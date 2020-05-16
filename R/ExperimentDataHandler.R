######################################################################
# Create the class that handles experiment data
# 
# Able to read data from file, or generate simulated data
#
######################################################################

ExperimentDataHandler <- setClass(
  
  # Set the name for the class
  "ExperimentDataHandler",
  
  # Define the slots
  slots = c(
    
    #public
    N = "numeric",
    N_within = "numeric",
    yvar = "vector",
    levelvar = "vector",
    xvar = "vector",
    xvar_active = "vector",
    truth_sigma = "numeric",
    truth_effect_size = "vector",
    truth_interaction_effects = "matrix",
    truth_tau = "numeric",
    
    data_frame = "data.frame",
    data_frame_wide = "data.frame",
    
    #private
    xvar_excluded = "vector",
    n_excluded = "numeric",
    truth_gamma_sample = "vector"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    
    #public
    N = 25,
    N_within = 20,
    yvar = c("y"),
    levelvar = c("participant", "measurement"),
    xvar = c("intercept", "rating_model", "group"),
    #xvar_active = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    truth_sigma = .05,
    truth_effect_size = c(.5, rep(0, 3)),
    truth_interaction_effects = matrix(rep(0, 3*3), nrow=3, ncol=3),
    truth_tau = .5
    
    #private
    #xvar_excluded = vector(),
    #n_excluded = 0
    
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if((object@N%%1 != 0) || (object@N <= 0)) {
      return("A nonvalid sample size is given")
    }
    
    return(TRUE)
  }
)

# define constructor
setMethod(f="initialize",
  signature="ExperimentDataHandler",
  definition=function(.Object){
    cat("~~~ ExperimentDataHandler: constructor ~~~ \n")
      
    
    #compute excluded variables
    #for (i in 1:length(.Object@truth_effect_size)){
    #  if (.Object@xvar_active[i]==0){
    #    .Object@xvar_excluded <- c(.Object@xvar_excluded, i)
    #    .Object@n_excluded<- .Object@n_excluded + 1
    #  }
    #}
    
    return(.Object) # return of the object
  }
)

# create a method to assign the value of a coordinate
setGeneric(name="generateData",
  def=function(.Object)
  {
   standardGeneric("generateData")
  }
)

setMethod(f="generateData",
  signature="ExperimentDataHandler",
  definition=function(.Object)
  {
    cat("~~~ ExperimentDataHandler: generateData ~~~ \n")
    
    #.Object <- ExperimentDataHandler()
    
    #derivatives of the parameters
    truth_gamma = .Object@truth_sigma * .Object@truth_effect_size;
    truth_tau = c(.Object@truth_tau, rep(0, length(.Object@truth_effect_size)-1))

    truth_beta<-mapply(function(x,y){rnorm(x,y,n=.Object@N)},x=truth_gamma,y=truth_tau)
    colnames(truth_beta) <- .Object@xvar
    truth_gamma_sample = vector()
    for (i in 1:ncol(truth_beta)){
      truth_gamma_sample = c(truth_gamma_sample, mean(truth_beta[,i]))
    }
    .Object@truth_gamma_sample = truth_gamma_sample
    
    #truth_beta<-truth_beta[, -.Object@xvar_excluded]

    #generate data
    #data_x = matrix(, nrow = 0, ncol = length(.Object@xvar)+length(.Object@yvar)+length(.Object@levelvar) -.Object@n_excluded)
    data_x = matrix(, nrow = 0, ncol = length(.Object@xvar)+length(.Object@yvar)+length(.Object@levelvar))
    for (i in 1:.Object@N){
      #i=1
      data_xi = matrix(nrow = 0, ncol = length(.Object@truth_effect_size))
      data_xi <- rbind(data_xi, cbind(rep(1, .Object@N_within), ratings, c(rep(0, 10), rep(1, 10))))
      #data_xi <- data_xi[-1,]
      
      # apply main effects
      data_yi_main = as.vector(as.matrix(data_xi) %*% as.matrix(truth_beta[i,])) # main effects
      
      # apply interaction effects
      data_yi_intm = array(0, dim=c(length(.Object@truth_effect_size), length(.Object@truth_effect_size), .Object@N_within))
      for(q in 1:length(.Object@truth_effect_size)){
        for(k in 1:length(.Object@truth_effect_size)){
          data_yi_intm[q,k,] = data_xi[,q] * data_xi[,k] * .Object@truth_interaction_effects[q,k]
        }
      }
      data_yi_int = array(0, dim=.Object@N_within)
      data_yi_int = apply(data_yi_intm, 3, sum)
      
      # noise
      data_yi_rand = rnorm(.Object@N_within, 0, .Object@truth_sigma)
      
      # combine main effects, interaction effects and noise to form the dependent variable
      data_yi = data_yi_main + data_yi_int + data_yi_rand
      
      # add the level variables
      data_xi = cbind(rep(i, .Object@N_within), seq(1, .Object@N_within), data_xi, data_yi)
      
      # add participant to full dataset
      data_x = rbind(data_x, data_xi)
    }
    
    .Object@data_frame <- data.frame(data_x)
    #colnames(.Object@data_frame) <- c(.Object@levelvar, .Object@xvar[-.Object@xvar_excluded], .Object@yvar)
    colnames(.Object@data_frame) <- c(.Object@levelvar, .Object@xvar, .Object@yvar)
    rownames(.Object@data_frame) <- c()
    rm(data_x, data_xi, data_yi, truth_beta, data_yi_main, data_yi_int, data_yi_rand, truth_gamma, truth_gamma_sample, truth_tau)
    
    return(.Object)
  }
)

setGeneric(name="addMissingValues",
  def=function(.Object, amount)
  {
   standardGeneric("addMissingValues")
  }
)

setMethod(f="addMissingValues",
  signature="ExperimentDataHandler",
  definition=function(.Object, amount)
  {
    cat("~~~ ExperimentDataHandler: addMissingValues ~~~ \n")
    
    for (i in 1:nrow(.Object@data_frame)){
      if (runif(1) < amount){
        .Object@data_frame$flow[i] = NA
      }
    }
    
    return(.Object)
  }
)

setGeneric(name="getVariableList",
           def=function(.Object)
           {
             standardGeneric("getVariableList")
           }
)

setMethod(f="getVariableList",
  signature="ExperimentDataHandler",
  definition=function(.Object)
  {
    cat("~~~ ExperimentDataHandler: getVariableList ~~~ \n")
    
    return(.Object@xvar[c(-1)])
  }
)

setGeneric(name="setSimulationParameters",
  def=function(.Object, N, N_within, sigma, tau, effect_size, interaction_effects)
  {
   standardGeneric("setSimulationParameters")
  }
)

setMethod(f="setSimulationParameters",
  signature="ExperimentDataHandler",
  definition=function(.Object, N, N_within, sigma, tau, effect_size, interaction_effects)
  {
    cat("~~~ ExperimentDataHandler: setSimulationParameters ~~~ \n")
    
    .Object@N = N
    .Object@N_within = N_within
    .Object@truth_sigma = sigma
    .Object@truth_tau = tau
    .Object@truth_effect_size = effect_size
    .Object@truth_interaction_effects = interaction_effects
    
    return(.Object)
  }
)

setGeneric(name="readData",
  def=function(.Object, normalize)
  {
   standardGeneric("readData")
  }
)

setMethod(f="readData",
  signature="ExperimentDataHandler",
  definition=function(.Object, normalize)
  {
    cat("~~~ ExperimentDataHandler: readData ~~~ \n")
  
    return(.Object)
  }
)
