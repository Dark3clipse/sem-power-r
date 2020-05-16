
this=mc;


n=this@regression_path_strength;
	p=this@regression_path_strength;
	FIM = matrix(c(	1,.5, 0, 0, p, 0, 0, 0, 0, 0, 0,
					.5,1, p, p, 0, p, 0, 0, 0, 0, 0,
					0, p, 1, 0, 0, 0, 0, 0, 0, p, 0,
					0, p, 0, 1, 0, 0, 0, 0, 0, n, 0,
					p, 0, 0, 0, 1, 0, 0, 0, p, 0, 0,
					0, p, 0, 0, 0, 1, p, p, 0, 0, 0,
					0, 0, 0, 0, 0, p, 1, 0, 0, p, 0,
					0, 0, 0, 0, 0, p, 0, 1, n, 0, 0,
					0, 0, 0, 0, p, 0, 0, n, 1, p, 0,
					0, 0, p, n, 0, 0, p, 0, p, 1, p,
					0, 0, 0, 0, 0, 0, 0, 0, 0, p, 1), nrow=this@N_factors_all, ncol=this@N_factors_all);
	r = t(matrix(c(0, 0, 0, 0, 0, 0, 0, 0, p, p, p, p, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	             p, p, p, p, p, p, p, p, 0, 0, 0, 0, p, p, p, p, p, p, p, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=this@N_measurements, ncol=this@N_control));




	X1 = t(matrix(rep(c(0, 0, 0, 1, 0, 0, 1, 1), this@N/2), nrow=2, ncol=this@N*2 ));
	X = X1;
	for(i in 1:(this@N_measurements-this@N_control)){
		RD = rnorm(this@N*2);
		rho = matrix(, nrow=1, ncol=this@N_control+i-1)
		for(j in 1:(this@N_control+i-1)){
			rho[j]=this@SCM[j, i+this@N_control];
		}
		X = cbind(X, complement(X, rho, RD));
	}
	xc=cor(X);


	RD = scale(whiten(replicate(this@N_measurements, rnorm(this@N*2)), method="PCA-cor"), center=T, scale=T);
	print(cor(X1, a));

	Xctr  <- scale(X, center=TRUE, scale=FALSE);
	Id   <- diag(this@N*this@conditions);
	Q    <- qr.Q(qr(Xctr[ , 1:this@N_control, drop=FALSE]))
	P    <- tcrossprod(Q);
	x2o  <- (Id-P) %*% Xctr[ , (this@N_control+1):this@N_variables];
	Xc2  <- cbind(Xctr[ , 1:this@N_control], x2o)                # bind to matrix
	Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

	S = Y[ , (1+this@N_control):(this@N_variables)];
	A = Y[ , 1:this@N_control];
	B = matrix(0, nrow=2, ncol=36);
	for(i in 1:this@N_control){
		for (j in 1:this@N_measurements){
			B[i, j] = tan(acos(r[i, j]));
		}
	}

	X = cbind(X1, S + A %*% B);
	xc=cor(X);





#x_between = t(c("gender", "age"))
#x_within = t(c("control", "effort", "transparency", "rec_quality", "trust", "usefulness"))

vars_control = t(c("explanation", "feedback"));
vars_between = t(c("gender", "age"));
vars_within =  t(c("control", "effort", "transparency", "personalization", "rec_quality", "exp_quality", "trust", "usefulness", "satisfaction"));

# insert N measurements per latent factor for the within variables
N_measurements_per_latent_factor = 4;
N_factors = length(vars_within);
N_measurements = N_factors*N_measurements_per_latent_factor;

vr = matrix(,nrow=N_measurements_per_latent_factor,ncol=length(vars_within));
k=1
for (i in 1:length(vars_within)){
	vc = array();
	for (j in 1:N_measurements_per_latent_factor){
		vc[j] = paste(vars_within[i], "_", j, sep="")
	}
	vr[,k] = vc
	k=k+1
}

vars = c(vars_control, vars_between, c(vr))
vartype = c(rep(0, length(vars_control)), rep(1, length(vars_between)), rep(2, length(c(vr))))
rm(vr, vc, k)

conditions = 2;
measurements_by_condition = function(x){
	if(x==0)
		return(matrix(c(0, 0, 0, 1), nrow=2, ncol=2))
	else{
		return(matrix(c(0, 0, 1, 1), nrow=2, ncol=2))
	}
}


cnames = c("participant", "condition", vars);

# parameters
N = 200;
factor_loadings = .5;
factor_correlations = 0L;


# compute Factor Loading Matrix
#H2 = matrix(factor_loadings^2, ncol=N_measurements, nrow=1);
#Lp = sqrt(factor_loadings^2/(N_measurements_per_latent_factor + (N_measurements-N_measurements_per_latent_factor) * factor_correlations^2));
#Lm = factor_correlations * Lp;


n=.15;
p=.15;
N_factors_all=11;
N_variables=38;
N_control=2;
FIM = matrix(c(	1,.5, 0, 0, p, 0, 0, 0, 0, 0, 0,
				.5,1, p, p, 0, p, 0, 0, 0, 0, 0,
				0, p, 1, 0, 0, 0, 0, 0, 0, p, 0,
				0, p, 0, 1, 0, 0, 0, 0, 0, n, 0,
				p, 0, 0, 0, 1, 0, 0, 0, p, 0, 0,
				0, p, 0, 0, 0, 1, p, p, 0, 0, 0,
				0, 0, 0, 0, 0, p, 1, 0, 0, p, 0,
				0, 0, 0, 0, 0, p, 0, 1, n, 0, 0,
				0, 0, 0, 0, p, 0, 0, n, 1, p, 0,
				0, 0, p, n, 0, 0, p, 0, p, 1, p,
				0, 0, 0, 0, 0, 0, 0, 0, 0, p, 1), nrow=N_factors_all, ncol=N_factors_all);

LL = matrix(0, nrow=N_variables, ncol=N_variables);
for(i in 1:N_variables){
	for(j in 1:N_variables){
		if (i <= N_control){
			factor1 = i;
		}else{
			factor1 = floor((i-1-N_control) / N_measurements_per_latent_factor)+3;
		}
		if (j <= N_control){
			factor2 = j;
		}else{
			factor2 = floor((j-1-N_control) / N_measurements_per_latent_factor)+3;
		}

		LL[i,j] = FIM[factor1, factor2];
		if (factor1 == factor2 && i != j){
			LL[i,j] = factor_loadings;
		}
		if (factor1 != factor2){
			LL[i,j] = LL[i,j] * factor_loadings^2;
		}
	}
}
SCM = LL;
diag(LL) = factor_loadings^2;



























p = .15;
n = .15;
FIM = matrix(c(	1, 0, 0, 0, 0, 0, 0, p, 0,
				0, 1, 0, 0, 0, 0, 0, n, 0,
				0, 0, 1, 0, 0, 0, p, 0, 0,
				0, 0, 0, 1, p, 0, 0, 0, 0,
				0, 0, 0, 0, 1, 0, 0, p, 0,
				0, 0, 0, p, 0, 1, n, 0, 0,
				0, 0, 0, 0, 0, 0, 1, p, 0,
				0, 0, 0, 0, 0, 0, 0, 1, p,
				0, 0, 0, 0, 0, 0, 0, 0, 1), nrow=N_factors, ncol=N_factors)
FIM = matrix(c(	1, 0, 0, 0, 0, 0, 0, p, 0,
				0, 1, 0, 0, 0, 0, 0, n, 0,
				0, 0, 1, 0, 0, 0, p, 0, 0,
				0, 0, 0, 1, p, p, 0, 0, 0,
				0, 0, 0, p, 1, 0, 0, p, 0,
				0, 0, 0, p, 0, 1, n, 0, 0,
				0, 0, p, 0, 0, n, 1, p, 0,
				p, n, 0, 0, p, 0, p, 1, p,
				0, 0, 0, 0, 0, 0, 0, p, 1), nrow=N_factors, ncol=N_factors)

# define Factor Inter-correlation Matrix
#FIM = matrix(factor_correlations, nrow=N_factors, ncol=N_factors);
#FIM[cbind(1:N_factors,1:N_factors)] <- 1

FLM = matrix(0,nrow=N_factors, ncol=N_measurements);
for(i in 1:N_factors){
	for(j in 1:N_factors){
		for(k in 1:N_measurements_per_latent_factor){
			FLM[i,(j-1)*N_measurements_per_latent_factor + k] = factor_loadings * FIM[i, j]
		}
	}
}

# generate Factor Data Matrix
LRV = t(chol(FIM)) # Latent Random Vector (random-vector to generate latent values with given correlation matrix)
RD = scale(whiten(replicate(N_factors, rnorm(N)), method="PCA-cor"), center=T, scale=T)
RD_COV = cov(RD)
print(colMeans(RD))
print(apply(RD, 2, sd))
FDM = t(LRV %*% t(RD))
FIMR = cor(FDM)

# construct sample correlation matrix
#LL = outer(1:N_measurements,1:N_measurements,FUN=function(i,j){
#	s = 0;
#	for (k in 1:N_factors){
#		s = s + FLM[k, i] * FLM[k, j];
#	}
#	return(s);
#});
LL = matrix(0, nrow=N_measurements, ncol=N_measurements);
for(i in 1:N_measurements){
	for(j in 1:N_measurements){
		factor1 = floor((i-1) / N_measurements_per_latent_factor)+1;
		factor2 = floor((j-1) / N_measurements_per_latent_factor)+1;
		LL[i,j] = FIM[factor1, factor2];
		if (factor1 == factor2 && i != j){
			LL[i,j] = factor_loadings;
		}
		if (factor1 != factor2){
			LL[i,j] = LL[i,j] * factor_loadings^2;
		}
	}
}
SCM = LL;
diag(SCM) = 1;
ECM = diag(N_measurements) * outer(rep(1, N_measurements), 1-diag(LL));

LRV = t(chol(SCM)) # Latent Random Vector (random-vector to generate latent values with given correlation matrix)
RD = scale(whiten(replicate(N_measurements, rnorm(N)), method="PCA-cor"), center=T, scale=T)
SDM = t(LRV %*% t(RD))
SCMR = cor(SDM)

DM2 = mvrnorm(n=N, mu=rep(0, N_measurements), Sigma=SCM/(N-1), empirical=T);
DM2_cor = cor(DM2)

# construct errors from error covariance matrix
E = mvrnorm(n=N, mu=rep(0, N_measurements), Sigma=ECM/(N-1), empirical=T);
ECMR = matrix(0L, nrow=N_measurements, ncol=N_measurements);
for (i in 1:N_measurements){
	for (j in 1:N_measurements){
		ECMR[i,j] = sum(E[,i] * E[,j])
	}
}


#E_cor = diag(N_measurements) - diag(LL) %*% diag(N_measurements);

#E = replicate(N_measurements, rnorm(N))
#E = matrix(0L, nrow=N, ncol=length(vars_within)*N_measurements_per_latent_factor)
#DM = t(t(FLM) %*% t(RD)) + E;
#DM = t(t(FLM) %*% t(FDM)) + E;
DM = matrix(0L, nrow=N, ncol=N_measurements);
DM = FDM %*% FLM + E; # even though factor loadings are 1, the items should vary with a specific correlation! (SCM?)
DM_cor = cor(DM);
#for (i in 1:N){
#	for (j in 1:N_measurements){
#		for (k in 1:N_factors){
#			DM[i,j] = DM[i,j] + factor_loadings * FDM[i, k];
#		}
#		DM[i,j] = DM[i,j] + E[i, j];
#	}
#}

# validate Data Matrix using conventional factor analysis
TEST_N=9;
TEST_PCA = prcomp(DM2, center=T, scale=T, tol=.02);
TEST_LRAW = TEST_PCA$rotation[,1:TEST_N] %*% diag(TEST_PCA$sdev, TEST_N, TEST_N);
TEST_LROT = varimax(TEST_LRAW)$loadings;
TEST_LINV = t(pracma::pinv(TEST_LROT));
TEST_SCORES = scale(DM2) %*% TEST_LINV;



categorical = c( T,  T);
truth_gamma = c(.5, 25);
truth_tau =   c(.1,  3);
#truth_beta<-t(mapply(function(x,y){rnorm(x,y,n=1)},x=truth_gamma,y=truth_tau))

truth_mean = c(
	.2, .4, .8, .4, .5, .1, .3, .3, .3
);
truth_cor = matrix(c(
	1,  0,  0,  0,  0,  0, 0, 0, 0,
	0,  1,  0,  0,  0,  0, 0, 0, 0,
	0,  0,  1,  0,  0,  0, 0, 0, 0,
	0,  0,  0,  1,  0,  0, 0, 0, 0,
	0,  0,  0,  0,  1,  0, 0, 0, 0,
	0,  0,  0,  0,  0,  1, 0, 0, 0,
	0,  0,  0,  0,  0,  0, 1, 0, 0,
	0,  0,  0,  0,  0,  0, 0, 1, 0,
	0,  0,  0,  0,  0,  0, 0, 0, 1), nrow=9, ncol=9);

#means = c(.5, 25, .1, .2, .3, .4, .5, .6);
#effects = matrix(c(),
#				nrow=5,
#				ncol=5);

data_x = matrix(, nrow = 0, ncol = length(cnames))
for (i in 1:N){

	data_xi = matrix(, nrow = 0, ncol = length(cnames))

	condition_for_participant = 1-i%%2
	x_between = t(mapply(function(x,y,c){v=rnorm(x,y,n=1);return(if_else(c, round(v), v))}, x=truth_gamma, y=truth_tau, c=categorical))
	#data_x_between = t(mapply(function(x,y,c){v=rnorm(x,y,n=1);return(if_else(c, round(v), v))}, x=truth_gamma, y=truth_tau, c=categorical))
	for (j in 1:conditions){
		data_xj = matrix(, nrow = 0, ncol = length(cnames))
		c_vars = t(measurements_by_condition(condition_for_participant)[,j]);
		x_within = t(seq(length(vars_within)*N_measurements_per_latent_factor));
		data_xj = cbind(i, j, c_vars, x_between, x_within)
		data_xi <- rbind(data_xi, data_xj);
	}
	data_x = rbind(data_x, data_xi)
}
df=data.frame(data_x)
colnames(df) = cnames;
View(df)


Sys.sleep(9999999999);
