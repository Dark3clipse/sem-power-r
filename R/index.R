
# includes
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
	source("R/functions.R")
	source("R/simulation.R");
	library("ggplot2")
	library("dplyr")
	library("psych")
	library("stats")
	library("pracma")
	library("whitening")
	library("Matrix")
	library("MASS")
	library("sem")
	library("lavaan")
	require(data.table)
})));

mc = MCSimulator();
mc = set(mc,
	150,
	4,
	t(c("explanation", "feedback")),
	t(c("gender", "age")),
	t(c("control", "effort", "transparency", "personalization", "rec_quality", "exp_quality", "trust", "usefulness", "satisfaction")),
	2,
	c(0, 0, 0, 1, 0, 0, 1, 1),
	.7, # factor loadings
	0,  # factor correlations
	.3 # regression path strength
);
#mc = generate(mc);
#mc = score(mc);
this=mc;

this = generate(this);
m = score(this);

cn=c("v", "N", "factor loadings", "regression coefficients", "factor-indicator se", "factor-indicator z", "factor-indicator p", "transparency ~ explanation [est]", "std.err", "z", "p", "trust ~ transparency [est]", "std.err", "z", "p", "personalization ~ exp_quality + feedback [est1]", "std.err", "z", "p", "[est2]", "std.err", "z", "p", "rec_quality ~ personalization [est]", "std.err", "z", "p" ,"effort ~ feedback [est]", "std.err", "z", "p", "control ~ feedback [est]", "std.err", "z", "p", "usefulness ~ trust + rec_quality + effort + control [est1]", "std.err", "z", "p", "[est2]", "std.err", "z", "p", "[est3]", "std.err", "z", "p", "[est4]", "std.err", "z", "p", "satisfaction ~ usefulness [est]", "std.err", "z", "p");
D = matrix(, ncol=55);
colnames(D) = cn;


colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)

N = c(50, 50, 100, 120, 150, 180, 200, 260, 300);
fl= c(.50, .60, .70);
rs= c(.25, .30, .40);
bootstraps = 25;
for(n in 1:9){
	this@N = N[n];
	for (l in 1:3){
		this@factor_loadings = fl[l];
		for (s in 1:3){
			this@regression_path_strength = rs[s];

			suppressWarnings(suppressMessages(suppressPackageStartupMessages({
			Db = matrix(, ncol=55);
			colnames(Db) = cn;
			for (b in 1:bootstraps){
				this = generate(this);
				m = score(this);
				Di = cbind(cbind(0, this@N, this@factor_loadings, this@regression_path_strength), m$PE[2, 6:8], m$PE[37, 5:8], m$PE[38, 5:8], m$PE[39, 5:8], m$PE[40, 5:8], m$PE[41, 5:8], m$PE[42, 5:8], m$PE[43, 5:8], m$PE[44, 5:8], m$PE[45, 5:8], m$PE[46, 5:8], m$PE[47, 5:8], m$PE[48, 5:8]);
				colnames(Di)=cn;
				Db = rbind(Db, Di);
			}
			cm = t(colMeans(Db, na.rm = T));
			sd = t(colSd(Db, na.rm=T));

			cm[1]=1
			Di = cm;
			colnames(Di)=cn;
			D = rbind(D, Di);

			sd[1] = 2;
			Di = sd;
			colnames(Di)=cn;
			D = rbind(D, Di);

			pow = 1.64 * sd;
			pow[1] = 3;
			Di = pow;
			colnames(Di)=cn;
			D = rbind(D, Di);
})));
		}
	}
}
write.csv(D, file="data/output.csv");


