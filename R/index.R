
# includes
source("R/functions.R")
library("ggplot2")
library("dplyr")
require(data.table)

# load data
t <- read.table("data/track_features/tf_000000000000.csv", TRUE, ",", nrows=10000000000) %>% select(track_id, tempo, valence)
t <- rbind(t, read.table("data/track_features/tf_000000000001.csv", TRUE, ",", nrows=10000000000) %>% select(track_id, tempo, valence))
d <- read.table("data/training_set/log_mini.csv", TRUE, ",")

# pre-processing
d = d %>% inner_join(t, by=c("track_id_clean"="track_id")) %>% arrange(session_id, session_position)
df = data.table(d)
df[ , vdiff := valence - shift(valence), by = session_id]
df[ , tdiff := tempo - shift(tempo), by = session_id]
df[ , vdiff_abs := abs(valence - shift(valence)), by = session_id]
df[ , tdiff_abs := abs(tempo - shift(tempo)), by = session_id]
df[ , valid:=session_position-shift(session_position)==1, by = session_id]
d = setDF(df)
rm(df)
d = d %>% filter(valid==TRUE) %>% mutate(skipped=ifelse(not_skipped=="true", 0, 1))

# visualization
ggplot(d, aes(x=vdiff, color=factor(skipped, levels=c(1, 0), labels=c("true", "false")))) +
	geom_histogram(fill="white", alpha=0.5, position="identity")+
	scale_color_manual("skipped",values=c("red","blue"))

ggplot(d, aes(x=tdiff, color=factor(skipped, levels=c(1, 0), labels=c("true", "false")))) +
	geom_histogram(fill="white", alpha=0.5, position="identity")+
	scale_color_manual("skipped",values=c("red","blue"))

ggplot(d, aes(x=vdiff_abs, color=factor(skipped, levels=c(1, 0), labels=c("true", "false")))) +
	geom_histogram(fill="white", alpha=0.5, position="identity")+
	scale_color_manual("skipped",values=c("red","blue"))

ggplot(d, aes(x=tdiff_abs, color=factor(skipped, levels=c(1, 0), labels=c("true", "false")))) +
	geom_histogram(fill="white", alpha=0.5, position="identity")+
	scale_color_manual("skipped",values=c("red","blue"))

# statistics
m=glm(skipped ~ vdiff_abs + tdiff_abs, d, family = "binomial")
summary(m)
