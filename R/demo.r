##########################################################################
# DEMONSTRATION CODE FOR LA R USERS' MEETUP AUGUST 17, 2010
# Taking R to the Limit: Part 2, Large Datasets
# by Ryan Rosario
# rosario --*AT#-- stat dot ucla dot edu
# http://www.stat.ucla.edu/~rosario
# http://www.bytemining.com
# Code originates from sources cited in the last slide of my presentation.
#
# If this file is retransmitted or shared, please distribute with this
# header intact.
###########################################################################


#########################################
# bigmemory
#########################################

#Bigmemory and Shared Memory
#Open R in two sessions using GNU Screen

#Session 1
library(bigmemory)
library(biganalytics)
options(bigmemory.typecast.warning=FALSE)

A <- big.matrix(5000, 5000, type="char", init=0)
#Fill the matrix by randomly picking 20% of the positions for a 1.
x <- sample(1:5000,size=5000,replace=TRUE)
y <- sample(1:5000,size=5000,replace=TRUE)
for(i in 1:5000) {
    A[x[i],y[i]] <- 1
}
#Get the location in RAM of the pointer to A.
desc <- describe(A)
#Write it to disk.
dput(desc, file="/tmp/A.desc")
sums <- colsum(A, 1:20)

#Session 2
library(bigmemory)
library(biganalytics)

#Read the pointer from disk.
desc <- dget("/tmp/A.desc")
#Attach to the pointer in RAM.
A <- attach.big.matrix(desc)
#Check our results.
sums <- colsum(A, 1:20)


#The BIG example 1
library(bigmemory)
library(biganalytics)
x <- read.big.matrix("airline.csv", type="integer", header=TRUE,
     backingfile="airline.bin",
     descriptorfile="airline.desc",
     extraCols="Age")   #NOT done live. Requires 28 mins.

#If x has already been created, do the following:
desc <- dget('airline.desc')
x <- attach.big.matrix(desc)

birthmonth <- function(y) {
     minYear <- min(y[,'Year'], na.rm=TRUE)
     these <- which(y[,'Year']==minYear)
     minMonth <- min(y[these,'Month'], na.rm=TRUE)
     return(12*minYear + minMonth - 1)
}

#DO NOT run this!
#aircrafts <- unique(x[,'TailNum'])
#acStart <- rep(0, length(aircrafts))
#for (i in aircrafts) {
# acStart[i]<-birthmonth( x[mwhich(x, 'TailNum', i, 'eq'),
#               c('Year', 'Month'), drop=FALSE])
#}

library(bigtabulate)
acindices <- bigsplit(x, 'TailNum')

#Fast Method 1
acStart <- sapply(acindices, function(i) birthmonth(x[i, c('Year','Month'), drop=FALSE]))

#Fast Method 2
library(doMC)
registerDoMC(cores=2)
acStart <- foreach(i=acindices, .combine=c) %dopar% {
   return(birthmonth(x[i, c('Year', 'Month'), drop=FALSE]))
}

#NOT live: takes several minutes.
x[,'Age'] <- x[,'Year']*as.integer(12) + x[,'Month'] - as.integer(acStart[x[,'TailNum']])


#The BIG example 2
library(biganalytics)
blm <- biglm.big.matrix(ArrDelay ~ Age + Year, data=x)
summary(blm)


#########################################
# ff
#########################################
library(ff)
#creating the file
my.obj <- ff(vmode="double", length=10)
#modifying the file.
my.obj[1:10] <- iris$Sepal.Width[1:10]
#Show subsetting?

#creating a multidimensional file
multi.obj <- ff(vmode="double", dim=c(10, 3))
multi.obj[1:10, 1] <- iris[1:10,1]
#Show subsetting?

data(trees)
Girth <- ff(trees$Girth)
Height <- ff(trees$Height)
Volume <- ff(trees$Volume)
#Create sequences frame with some added parameters.
fftrees <- ffdf(Girth=Girth,Height=Height,Volume=Volume)

library(biganalytics)
model <- biglm(log(Volume)~log(Girth)+log(Height),data=fftrees)



#########################################
# mapReduce
#########################################
data(iris)
mapReduce( 
  map=Species, 
  mean.sepal.length=mean(Sepal.Length),
  max.sepal.length=max(Sepal.Length) ,
  data = iris
) 


#########################################
# HadoopStreaming
#########################################
### HadoopStreaming
#Put in its own file!

#Start file here.

#! /usr/bin/env Rscript

library(HadoopStreaming)

opts <- c()
op <- hsCmdLineArgs(opts, openConnections=TRUE)

if (op$mapper) {
    mapper <- function(x) {
        #tokenize each tweet
        words <- unlist(strsplit(x, " "))
        #remove words containing nothing.
        words <- words[!(words='')] 
        #Create a sequences frame with 1 column: the words.
        df <- data.frame(Word=words)
        #Add a column called count, initialized to 1.
        df[,'Count'] = 1
        #Send this out to the console for the reducer.
        hsWriteTable(df[,c('Word','Count')], file=op$outcon, sep=',')
    }
    #Read a line from IN.
    hsLineReader(op$incon, chunkSize=op$chunksize, FUN=mapper)
} else if (op$reducer) {
    #Define the reducer function.
    #It just prints the word, the sum of the counts
    #separated by comma.
    reducer <- function(d) {
        cat(d[1,'Word'], sum(d$Count), '\n', sep=',')
    }
    #Define the column names and types for output.
    cols = list(Word='', Count=0)
    hsTableReader(op$incon, cols, chunkSize=op$chunkSize, skip=0,
        sep=',', keyCol='Word', singleKey=T, ignoreKey=F,
        FUN=reducer)
}

if (!is.na(opts$infile)) {
  close(opt$incon)
}

if (!is.na(opt$outfile)) {
  close(opt$outcon)
}


#End file here.

#DEMONSTRATION

#Create input directory in HDFS. I just used ~/hdfs/in on the local filesystem.
hadoop fs -mkdir ~/hdfs/in
#Move to the directory containing the original twitter.tsv file.
mkdir pieces
cd pieces
split ../twitter.tsv --bytes=64M
cd ..
hadoop fs -put pieces/* ~/hdfs/in

#Sample command for Hadoop Streaming.
hadoop jar $HADOOP_HOME/contrib/streaming/hadoop-0.20.2-streaming.jar \
     -input /home/ryan/hdfs/in \
     -output ~/hdfs/out2 \
     -mapper "count.R -m" \
     -reducer "count.R -r" \
     -file ./count.R

