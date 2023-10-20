#*
#* plotRandomHistogram.R
#* 
#* Date: 10/20/2023
#* 
#* Author: Morgan Ryan
#* 
#* Purpose:
#*    A function that will generate a sample of normal 
#*     observations and plot the histogram of that data
#*     
#* Inputs:
#*   num_pts = the number of observations to 
#*           randomly generate
#*   mu = the theoretical mean of the Normal
#*           Distribution that generates my data
#*   sigma = the theoretical standard deviation
#*   seed = an optional random number generator
#*           seed value, defaults to NULL
#*   meanColor = the color of the line corresponding
#*           to the sample mean of the data
#* Output:
#*  A histogram of the generated 
#*      data is drawn.
#*  A list with the following elements:
#*      Random_obs = a vector with the randomly
#*                 generated data
#*      Mean_Random_obs = sample mean corresponding to
#*                the randomly generated data
#*      SD_Random_obs = corresponding standard deviation
#*                 
#*          
#
plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1) {
  x<- rnorm(num_pts, mean=mu, sd=sigma)
  hist(x)
}
 help(RNG) # explains how random number generators work in R
 ## We can tell the random number generator to go to the SAME SPOT
 ## by setting a SEED -- set.seed(1)
 set.seed(1)
 plotRandomHistogram()
  # not actually random, we set the seed to control it for reproducing and replication
 set.seed(123)
 plotRandomHistogram()
 
 plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1, seed=5) {
   set.seed(seed)
   x<- rnorm(num_pts, mean=mu, sd=sigma)
   hist(x)
 }
plotRandomHistogram() 
## now it will output the same histogram every time we run it
plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1, seed=NULL) {
  set.seed(seed)
  x<- rnorm(num_pts, mean=mu, sd=sigma)
  hist(x)
}
plotRandomHistogram() 
plotRandomHistogram(seed=5) 
plotRandomHistogram(seed=6)
plotRandomHistogram(seed=19)
## If you don't specify anything, then the output is more "random"
## What is the mean of this data that is plotted?
## theoretical mean is zero
## We will add a line where the SAMPLE MEAN is
 # using abline() within hist()--corresponds to the a and b in slope
plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1, seed=NULL) {
  set.seed(seed)
  x<- rnorm(num_pts, mean=mu, sd=sigma)
  mean_x <- mean(x)
  hist(x)
  abline(v=mean_x)
}
plotRandomHistogram()
plotRandomHistogram(seed=19)
##law of large numbers--as sample size increases, real mean value
##. gets closer to theoretical mean
plotRandomHistogram(seed=19, num_pts=1000000)
plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1, seed=NULL) {
  set.seed(seed)
  x<- rnorm(num_pts, mean=mu, sd=sigma)
  mean_x <- mean(x)
  hist(x)
  abline(v=mean_x, lwd=5, col="hotpink")
}
plotRandomHistogram()
plotRandomHistogram(seed=19, num_pts=1000000)
## Let user of the function pick color
plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1, seed=NULL, meanColor="royalblue") {
  set.seed(seed)
  x<- rnorm(num_pts, mean=mu, sd=sigma)
  mean_x <- mean(x)
  hist(x)
  abline(v=mean_x, lwd=5, col=meanColor)
}
plotRandomHistogram()
plotRandomHistogram(seed=19, num_pts=1000000, meanColor="forestgreen")

## How can we get the function to return the data set? 
##   purpose: what if we want to do something else with the data like a t-test?
plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1, seed=NULL, meanColor="royalblue") {
  set.seed(seed)
  x<- rnorm(num_pts, mean=mu, sd=sigma)
  mean_x <- mean(x)
  hist(x)
  abline(v=mean_x, lwd=5, col=meanColor)
  return(x)
}
## In R, in a function, the output of the last line
##.  is automatically returned, so
##
## we can do this: (just put an x)
plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1, seed=NULL, meanColor="royalblue") {
  set.seed(seed)
  x<- rnorm(num_pts, mean=mu, sd=sigma)
  mean_x <- mean(x)
  hist(x)
  abline(v=mean_x, lwd=5, col=meanColor)
  x
}
plotRandomHistogram()
## What is we also want to output the MEAN?
## We can only return one thing in R- so we CANNOT use return(x, mean_x)
##.  return multiple things?
      ## make a LIST--we can return a list 
plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1, seed=NULL, meanColor="royalblue") {
  set.seed(seed)
  x<- rnorm(num_pts, mean=mu, sd=sigma)
  mean_x <- mean(x)
  hist(x)
  abline(v=mean_x, lwd=5, col=meanColor)
  list(x, mean_x)
}
plotRandomHistogram()

plotRandomHistogram <- function(num_pts = 30, mu=0, sigma=1, seed=NULL, meanColor="royalblue") {
  set.seed(seed)
  x<- rnorm(num_pts, mean=mu, sd=sigma)
  mean_x <- mean(x)
  hist(x)
  abline(v=mean_x, lwd=5, col=meanColor)
  list(Random_obs = x,
       Mean_Random_obs = mean_x,
       SD_Random_obs = sd(x))
}
plotRandomHistogram()
my_output <- plotRandomHistogram(seed=5, num_pts=200, mu=70,
                                 sigma=2.5, meanColor="goldenrod")
my_output$Mean_Random_obs

#* note about READme file--
#*   meant to give an overview of what the repository entails
