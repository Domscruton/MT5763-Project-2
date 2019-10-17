
####Our new Bootstrap function here!! ------------------------------------

####  For parameters, "nboots" for setting the number of looping;
####  "inputData" is the dataset we use;
####  "covars" is the covariates we want to analyze, we can use arbitrary number of covariates;
####  "replace" is the option for sampling;
####  This function returns to the coefficient of the linear model so far;
####  I can change this function whatever you guys want or we need later;
NewBoots_coef <- function(nboots,inputData,covars,replace = T){
  ##import the necessary packages
  library(profvis)
  library(parallel)
  
  ## Setting a basic bootstrap for parallelizing; If you guys have a more efficient way, Just Change It.
  ## The index here is for later looping
  ## The other parameters are the same as above.
  innerBoot <- function(index,inputData,covars,replace = T){
    
    ##  Get the dimension for later sampling
    dataDim <- nrow(inputData)
    
    ##  Resample our data with the method we want
    bootData <- inputData[sample(1:dataDim,dataDim,replace = replace),]
    
    ##  Set the linear model
    bootLM <- lm(covars, data = bootData)
    
    ##  Store the coefs
    coef(bootLM)
  }
  
  ##### Paralleling ---------------------------------------------------------
  ##Set default Cluster parameter
  ncores <- detectCores()
  myCluster <- makeCluster(ncores - 1,type = 'PSOCK')
  clusterEvalQ(myCluster,library(dplyr))
  clusterEvalQ(myCluster,library(profvis))
  
  ####Record paralleled function running time.
  t <- system.time(
    bootCoefList <- parLapply(myCluster,1:nboots,innerBoot, inputData = inputData,covars = covars)
  )
  
  ###Getting the dataframe of results
  bootCoefs <- plyr::ldply(bootCoefList)
  
  #Closing the Cluster
  stopCluster(myCluster)
  
  #print the running time and results
  print(t)
  #return to a potentially wanted value
  return(bootCoefs)
}



# Testing -----------------------------------------------------------------

##  Create a data frame of random values from the uniform distribution
##  Assign my ID number to random seed to be reproduciable 
set.seed(190014610) 
x <- rnorm(10000)
y <- 20*x+100 
z <- 50*x+50 
randomdf <- data.frame(x, y, z) ## Set three variables for later test

#Estimate of the memory being used to store randomDataFrame
object.size(randomdf)/1024^2

test1 <- NewBoots_coef(100,randomdf,y~x) ## Let's use a small looping times first
test2 <- NewBoots_coef(100,randomdf,z~x)
