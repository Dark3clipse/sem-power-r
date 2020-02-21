######################################################################
# MC Simulator for mixed-design studies
######################################################################

MCSimulator <- setClass(

	# Set the name for the class
	"MCSimulator",

	# Define the slots
	slots = c(

		# public
		N = "numeric",
		N_measurements_per_latent_factor = "numeric",
		factor_loadings = "numeric",
		factor_correlations = "numeric",
		vars_control = "vector",
		vars_between = "vector",
		vars_within =  "vector",
		conditions = "numeric",
		condition_vector = "vector",
		regression_path_strength = "numeric",

		#private
		vars = "vector",
		N_factors = "numeric",
		N_factors_all = "numeric",
		N_control = "numeric",
		N_variables = "numeric",
		N_measurements = "numeric",
		cnames = "vector",
		SCM = "matrix",

		# generated
		DM = "matrix",
		df = "data.frame",
		res = "data.frame"
	),

	# Set the default values for the slots. (optional)
	prototype=list(

		#public
		N = 200,
		factor_loadings = .5,
		factor_correlations = 0,
		vars_control = t(c("explanation", "feedback")),
		vars_between = t(c("gender", "age")),
		vars_within =  t(c("control", "effort", "transparency", "personalization", "rec_quality", "exp_quality", "trust", "usefulness", "satisfaction"))

		#private

	),

	# Make a function that can test to see if the data is consistent.
	# This is not called if you have an initialize function defined!
	validity=function(object){
		if((object@N%%1 != 0) || (object@N <= 0)) {
			return("A nonvalid sample size is given");
		}

		return(T);
	}
);

setGeneric(name="set", def=function(this, N, N_measurements_per_latent_factor, vars_control, vars_between, vars_within, conditions, condition_vector, factor_loadings, factor_correlations, regression_path_strength){standardGeneric("set")})
setMethod(f="set", signature="MCSimulator", definition=function(this, N, N_measurements_per_latent_factor, vars_control, vars_between, vars_within, conditions, condition_vector, factor_loadings, factor_correlations, regression_path_strength){
	cat("~~~ MCSimulator: set ~~~ \n")

	this@N = N;
	this@N_factors = length(vars_within);
	this@N_control = length(vars_control);
	this@N_measurements_per_latent_factor = N_measurements_per_latent_factor;
	this@N_measurements = this@N_factors*this@N_measurements_per_latent_factor;
	this@N_factors_all = this@N_factors + this@N_control;
	this@N_variables = this@N_measurements + this@N_control;
	this@vars_control = vars_control;
	this@vars_between = vars_between;
	this@vars_within = vars_within;
	this@conditions = conditions;
	this@condition_vector = condition_vector;
	this@factor_loadings = factor_loadings;
	this@factor_correlations = factor_correlations;
	this@regression_path_strength = regression_path_strength;

	# compute variable names
	vr = matrix(,nrow=N_measurements_per_latent_factor,ncol=length(vars_within));
	k=1;
	for (i in 1:length(vars_within)){
		vc = array();
		for (j in 1:N_measurements_per_latent_factor){
			vc[j] = paste(vars_within[i], "_", j, sep="");
		}
		vr[,k] = vc;
		k=k+1;
	}
	this@vars = c(vars_control, vars_between, c(vr));
	rm(vr, vc, k);
	this@cnames = c("participant", "condition", vars);

	return(this);
});


complement <- function(y, rho, x, threshold=1e-12) {
  #
  # Process the arguments.
  #
  if(!is.matrix(y)) y <- matrix(y, ncol=1)
  if (missing(x)) x <- rnorm(n)
  d <- ncol(y)
  n <- nrow(y)
  y <- scale(y, center=FALSE) # Makes computations simpler
  #
  # Remove the effects of `y` on `x`.
  #
  e <- residuals(lm(x ~ y))
  #
  # Calculate the coefficient `sigma` of `e` so that the correlation of
  # `y` with the linear combination y.dual %*% rho + sigma*e is the desired
  # vector.
  #
  y.dual <- with(svd(y), (n-1)*u %*% diag(ifelse(d > threshold, 1/d, 0)) %*% t(v))
	D=cov(y.dual)
  sigma2 <- c((1 - rho %*% cov(y.dual) %*% t(rho)) / var(e))
  #
  # Return this linear combination.
  #
  if (sigma2 >= 0) {
    sigma <- sqrt(sigma2)
    z <- y.dual %*% t(rho) + sigma*e
  } else {
    warning("Correlations are impossible.")
    z <- rep(0, n)
  }
  return(z)
}

setGeneric(name="generate", def=function(this){standardGeneric("generate")})
setMethod(f="generate", signature="MCSimulator", definition=function(this){
	cat("~~~ MCSimulator: generate ~~~ \n");

#FLM = matrix(0,nrow=this@N_factors, ncol=this@N_measurements);
	#for(i in 1:this@N_factors){
	#	for(j in 1:this@N_measurements_per_latent_factor){
	#		FLM[i,(i-1)*this@N_measurements_per_latent_factor + j] = this@factor_loadings
	#	}
	#}

	#LL = outer(1:this@N_measurements,1:this@N_measurements,FUN=function(i,j){
	#	s = 0;
	#	for (k in 1:this@N_factors){
	#		s = s + FLM[k, i] * FLM[k, j];
	#	}
	#	return(s);
	#});
	#SCM = LL;
	#diag(SCM) = 1;

	# method 1: directly generate data from sample correlation-matrix
	#this@DM = mvrnorm(n=this@N, mu=rep(0, this@N_measurements), Sigma=SCM/(this@N-1), empirical=T);
	#this@df = data.frame(this@DM);

	# method 2: generate factor values and errors, then compute data
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

	LL = matrix(0, nrow=this@N_variables, ncol=this@N_variables);
	for(i in 1:this@N_variables){
		for(j in 1:this@N_variables){
			if (i <= this@N_control){
				factor1 = i;
			}else{
				factor1 = floor((i-1-this@N_control) / this@N_measurements_per_latent_factor)+1+this@N_control;
			}
			if (j <= this@N_control){
				factor2 = j;
			}else{
				factor2 = floor((j-1-this@N_control) / this@N_measurements_per_latent_factor)+1+this@N_control;
			}

			LL[i,j] = FIM[factor1, factor2];
			if (factor1 == factor2 && i != j){
				LL[i,j] = this@factor_loadings;
			}
			if (factor1 != factor2){
				LL[i,j] = LL[i,j] * this@factor_loadings^2;
			}
		}
	}
	SCM = LL;
	this@SCM = SCM;
	diag(LL) = this@factor_loadings^2; 	# check correlation = rho

	X1 = t(matrix(rep(this@condition_vector, this@N/this@conditions), nrow=this@conditions, ncol=this@N*this@conditions ));
	X = X1;
	for(i in 1:(this@N_measurements)){
		RD = rnorm(this@N*2);
		rho = matrix(, nrow=1, ncol=this@N_control+i-1)
		for(j in 1:(this@N_control+i-1)){
			rho[j]=this@SCM[j, i+this@N_control];
		}
		X = cbind(X, complement(X, rho, RD));
	}
	SDM = X;

	#LRV = t(chol(SCM));
	#RD = scale(whiten(replicate(this@N_variables, rnorm(this@N*2)), method="PCA-cor"), center=T, scale=T);
	#SDM = t(LRV %*% t(RD));
	#for (i in 1:(this@N*2)){
	#	SDM[i, 1] = min(1, max(0, round(SDM[i, 1])));
	#	SDM[i, 2] = min(1, max(0, round(SDM[i, 2])));
	#}

	ECM = diag(this@N_variables) * outer(rep(1, this@N_variables), 1-diag(LL));
	E = mvrnorm(n=this@N*2, mu=rep(0, this@N_variables), Sigma=ECM/(this@N*2-1), empirical=T);
	for (i in 1:(this@N*2)){
		E[i, 1] = 0;
		E[i, 2] = 0;
	}

	this@DM = matrix(0L, nrow=this@N*2, ncol=this@N_variables);
	this@DM = SDM + E;
	this@df = data.frame(this@DM);



	# generate column names
	cnames = this@vars_control;
	for (i in 1:this@N_factors){
		for (j in 1:this@N_measurements_per_latent_factor){
			cnames = cbind(cnames, paste(this@vars_within[i], j, sep=""));
		}
	}
	colnames(this@df) = cnames;


	return(this) # return of the object
});

setGeneric(name="score", def=function(this){standardGeneric("score")});
setMethod(f="score", signature="MCSimulator", definition=function(this){
	cat("~~~ MCSimulator: score ~~~ \n")

	#TEST_PCA = prcomp(this@DM, center=T, scale=T, tol=.02);
	#TEST_LRAW = TEST_PCA$rotation[,1:this@N_factors] %*% diag(TEST_PCA$sdev, this@N_factors, this@N_factors);
	#TEST_LROT = varimax(TEST_LRAW)$loadings;
	#TEST_LINV = t(pracma::pinv(TEST_LROT));
	#factor_scores = scale(this@DM) %*% TEST_LINV;

	#vars_control = t(c("explanation", "feedback"));
	#vars_between = t(c("gender", "age"));
	#vars_within =  t(c("control", "effort", "transparency", "personalization", "rec_quality", "exp_quality", "trust", "usefulness", "satisfaction"));

	mSpec = "";
	for (i in 1:this@N_factors){
		mSpec = paste(mSpec, this@vars_within[i], " =~", sep="");
		for (j in 1:this@N_measurements_per_latent_factor){
			pre = "";
			if (j > 1){
				pre = "+ "
			}
			mSpec = paste(mSpec, " ", pre, this@vars_within[i], j, sep="");
		}
		mSpec = paste(mSpec, "\n", sep="");
	}
	mSpec = paste(mSpec, "\n", "transparency ~ explanation", sep="");
	mSpec = paste(mSpec, "\n", "trust ~ transparency", sep="");
	mSpec = paste(mSpec, "\n", "personalization ~ exp_quality + feedback", sep="");
	mSpec = paste(mSpec, "\n", "rec_quality ~ personalization", sep="");
	mSpec = paste(mSpec, "\n", "effort ~ feedback", sep="");
	mSpec = paste(mSpec, "\n", "control ~ feedback", sep="");
	mSpec = paste(mSpec, "\n", "usefulness ~ trust + rec_quality + effort + control", sep="");
	mSpec = paste(mSpec, "\n", "satisfaction ~ usefulness", sep="");
	mSpec = paste(mSpec, "\n", "exp_quality ~~ transparency", sep="");
	mSpec = paste(mSpec, "\n", "satisfaction ~~ trust + rec_quality + effort + control", sep="");
	mSpec = paste(mSpec, "\n", "personalization ~~ explanation", sep="");

	mFit <- sem(mSpec, data=this@df);# std.lv=T
	m=summary(mFit, fit.measures=T);
	return(m)
});

setGeneric(name="simulate", def=function(this){standardGeneric("simulate")});
setMethod(f="simulate", signature="MCSimulator", definition=function(this){
	cat("~~~ MCSimulator: simulate ~~~ \n")

	cn=c("N", "factor loadings", "regression coefficients", "factor-indicator se", "factor-indicator z", "factor-indicator p", "transparency ~ explanation [est]", "std.err", "z", "p", "trust ~ transparency [est]", "std.err", "z", "p", "personalization ~ exp_quality + feedback [est1]", "std.err", "z", "p", "[est2]", "std.err", "z", "p", "rec_quality ~ personalization [est]", "std.err", "z", "p" ,"effort ~ feedback [est]", "std.err", "z", "p", "control ~ feedback [est]", "std.err", "z", "p", "usefulness ~ trust + rec_quality + effort + control [est1]", "std.err", "z", "p", "[est2]", "std.err", "z", "p", "[est3]", "std.err", "z", "p", "[est4]", "std.err", "z", "p", "satisfaction ~ usefulness [est]", "std.err", "z", "p");
	D = matrix(, ncol=54);
	colnames(D) = cn;

	N = c(50, 75, 100, 125, 150, 175, 200, 250, 300);
	fl= c(.50, .65, .80);
	rs= c(.25, .40, .50);
	for(n in 1:9){
		this@N = N[n];
		for (l in 1:3){
			this@factor_loadings = fl[l];
			for (s in 1:3){
				this@regression_path_strength = rs[s];

				this = generate(this);
				m = score(this);
				Di = cbind(cbind(this@N, this@factor_loadings, this@regression_path_strength), m$PE[2, 6:8], m$PE[37, 5:8], m$PE[38, 5:8], m$PE[39, 5:8], m$PE[40, 5:8], m$PE[41, 5:8], m$PE[42, 5:8], m$PE[43, 5:8], m$PE[44, 5:8], m$PE[45, 5:8], m$PE[46, 5:8], m$PE[47, 5:8], m$PE[48, 5:8]);
				colnames(Di)=cn;
				D = rbind(D, Di);
			}
		}
	}

	D = D[-1,];

	return(D);
});
