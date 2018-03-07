# http://www.stat.wisc.edu/~larget/stat302/chap3.pdf

# also...
# https://www2.stat.duke.edu/courses/Fall12/sta101.002/Sec3-34.pdf


# Bootstrap Confidence intervals with standard errors
# using the Atlanta commute times dataset

library(Lock5Data)
data(CommuteAtlanta)
str(CommuteAtlanta)
# 'data.frame':	500 obs. of  5 variables:


# goal is to construct the confidence interval for 
# the mean commute time in Atlanta....
# we need to find first the point estimate (sample mean) from
# the original sample.

time.mean = with(CommuteAtlanta, mean(Time))

# to find the standard error let's create a matrix 1000 x 500
# 1000 rows: one for each bootstrap sample
# 500 columns: to match the original sample size
# ...then we apply mean() to each row
B = 1000
n = nrow(CommuteAtlanta)   # 500
boot.samples = matrix(sample(CommuteAtlanta$Time, size = B * n, replace=T), 
                      nrow=B, ncol=n)

# lets compute the statistics on bootstrap samples
boot.statistics = apply(boot.samples, 1, mean) # 1 refers to row


# let's plot...
hist(boot.statistics, prob=T)
lines(density(boot.statistics))


# not too asymetric..., more or less bell shaped
# the std deviation is
time.se = sd(boot.statistics)
time.se


# let's construct the confidence interval
me = ceiling(10 * 2 * time.se) / 10
round(time.mean) + c(-1, 1) * me
# [1] 27.1 30.9

# interpretation is
# We are 95% confident that the mean commute time in Atlanta is
# in the interval from 27.1 to 31 minutes



# Let's wrap up into a function...
boot.mean = function(x, B, binwidth=NULL) {
  n = length(x)
  boot.samples = matrix(sample(x, size=n*B, replace=T), nrow=B, ncol=n)
  boot.statistics = apply(boot.samples, 1, mean)
  se = sd(boot.statistics)
  
  interval = mean(x) + c(-1, 1) * 2 * se
  print(interval)

  hist(boot.statistics, prob=T)
  lines(density(boot.statistics))

  return (list(boot.statistics=boot.statistics,
               interval=interval,
               se=se))
}

boot.mean(CommuteAtlanta$Time, 1000)



# Let's start from scratch the bootstrap...
# using for loops, because why not?
x = CommuteAtlanta$Time
B = 1000
result = rep(NA, B)
for (i in 1:B) {
  result[i] = mean(sample(x, replace=T))
}
mean(x) + 2 * sd(result) * c
# [1] 27.21967 31.00033


#############################################################
## Proportions
#############################################################
# Using Bootstraping to solve the problem of proportions

# Let assume a student has 11 oranges and 19 non-oranges candies
# let us use bootstrap to find a 95% confidence interval for the interval of orange
# we will represent the sample data as a vector with 11 1s and 19 0s
# and use the same machinery as before
reeses = c(rep(1, 11), rep(0, 19))   # reeses are candies  google it!
reeses.boot = boot.mean(reeses, 10000, binwidth = 1/30)

# [1] 0.1864931 0.5468402
# so based only on this single sample...
# we are 95% confident that the true proportion of orange Reese's pieces
# is between .19 and .55

# Let's combine all the 48 students samples into one big sample
# the total is 741 oranges candies and 699 non-oranges ones
# the observed proportions is then .515
reeses = c(rep(1, 741), rep(0, 699))
reeses.boot = boot.mean(reeses, 10000, binwidth=.005)
# [1] 0.4872727 0.5418939
# the True proportion is then 50%... most likely




#############################################################
## Differences in Means
#############################################################
# let's use the StudendSurvey data set to illustrate using bootstrap
# to estimate differences in means, the varianle Exercise is the number of hours
# per week each student exercises.
data(StudentSurvey)
summary(StudentSurvey$Exercise)
summary(StudentSurvey$Gender)

with(StudentSurvey, by(Exercise, Gender, mean, na.rm=T))
# Gender: F
# [1] 8.110119
# ------------------------------------------------------------------ 
#   Gender: M
# [1] 9.875648

# from the example, men exercise more hours per week than women.
# if we treat this sample of students as randomly chosen from a population
# of college students, we can estimate the difference in time spent exercising
# for each sex.
# Note that without more information about the sampling process, any such inference
# may be prone to bias... if the students in the sample differ substantially
# from the population.


# first let's remove the missing values : NA
newStudent = with(StudentSurvey, StudentSurvey[!is.na(Exercise), ])
summary(newStudent$Exercise)
length(newStudent)
length(StudentSurvey)

# let's skip the unsuccessfull boxplotting for now...
# ...

# let's group M and F by length(), the sample size of each group
n = with(newStudent, by(Exercise, Gender, length))

# Gender: F
# [1] 168
# ------------------------------------------------------------------ 
#   Gender: M
# [1] 193


# let's apply the bootstrap drill to the F first and then the M
# we then use apply() to find the mean of each sample and take the difference to
# get the distribution of bootstrap statistics.
# then we graph to check for symmetry and a bell shape.
B=1000000
f.size = n[1]
f.samples = with(newStudent, 
                 matrix(sample(Exercise[Gender=="F"], size=f.size * B, replace=T),
                        nrow=B, ncol=f.size))
f.means = apply(f.samples, 1, mean)


m.size = n[2]
m.samples = with(newStudent,
                 matrix(sample(Exercise[Gender=="M"], size=m.size * B, replace=T),
                        nrow=B, ncol=m.size))
m.means = apply(m.samples, 1, mean)

boot.stat = m.means - f.means

hist(boot.stat, prob=T)
lines(density(boot.stat))

# let's compute the confidence interval
xbars = with(newStudent, by(Exercise, Gender, mean))
me = 2 * sd(boot.stat)
(xbars[2] - xbars[1]) + c(-1,1)*me
## [1] 0.5397 2.9914
# 95% chance the mean difference is in the given interval




##################################################################
## Let's redo everything using the boot() function of R
##################################################################
# the function boot() requires 3 arguments
# 1. the data from the original sample as dataframe or matrix
# 2. a function to compute the statistics from the data...
# 3. the number of bootstrap replicates

library(boot)
data(CommuteAtlanta)
my.mean = function(x, indices) {
  return(mean(mean(x[indices])))
}
time.boot = boot(CommuteAtlanta$Time, my.mean, 10000)
time.boot
time.boot$t


# boot() returns an object with several fields:
# * t0: which is the sample mean of the original data
# * t: the collection of bootstrap statistics
#...
# let's use boot.ci() to calculate the bootstrap confidence

boot.ci(time.boot)

## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 10000 bootstrap replicates
## 
## CALL : 
##   boot.ci(boot.out = time.boot)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   (27.33, 30.93 )   (27.28, 30.91 )  
## 
## Level     Percentile            BCa          
## 95%   (27.31, 30.94 )   (27.42, 31.10 )  
## Calculations and Intervals on Original Scale

# basic uses the standard error
# percentile uses percentiles
# BCa also uses percentiles, but adjusted to account for bias and skewness


# you use percentile if you want to be more than 95% confident





