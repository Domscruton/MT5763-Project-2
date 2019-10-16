# Profile the original R Bootstrapping code

# Original Bootstrapping function in R ------------------------------------

lmBoot <- function(inputData, nBoot){
  for(i in 1:nBoot){
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    # fit the model under this alternative reality
    bootLM <- lm(y ~ x, data = bootData)
    # store the coefs
    if(i == 1){
      bootResults <- matrix(coef(bootLM), ncol = 2)
    } else {
      bootResults<- rbind(bootResults, matrix(coef(bootLM), ncol = 2))
    }
  } # end of i loop
  bootResults
}

# Do Some Profiling -------------------------------------------------------

#Create a data frame of random values from the uniform distribution
set.seed(4563)  
x <- runif(1e6/2)
y <- runif(1e6/2)
randomDataFrame <- data.frame(x, y)

#Estimate of the memory being used to store randomDataFrame
object.size(randomDataFrame)/1024^2

#Return CPU times that lmBoost() used
system.time(test <- lmBoot(randomDataFrame, 10))

#Profile the execution of lmBoost()
Rprof("storageFile")
lmBoot(randomDataFrame, 10)
Rprof()

#Summarise the output of the Rprof function 
summaryRprof("storageFile")

#Summarise the run time by running the expression 10 times
library(microbenchmark)
microbenchmark( lmBoot(randomDataFrame, 10), times=10)

#Use R-studio profiler
library(profvis)
profvis(lmBoot(randomDataFrame, 10))


