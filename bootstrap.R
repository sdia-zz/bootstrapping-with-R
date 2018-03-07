# https://stats.idre.ucla.edu/r/library/r-library-introduction-to-bootstrapping/

# Bootsrapping is a nonparametric method which lets us
# compute estimated standard errors, confidence intervals
# and hypothesis testing.

# Basics steps:
# 1. resample a given data set a specified number of times
# 2. calculate the specific statistic from each sample
# 3. find the standard deviation of the distribution of that statistic


# The sampling function: a major component of bootstrapping
# sample(x, size, replace, prob)


# generate a permutation of the seq 1:10
sample(10)

# bootstrap sample from the same sequence
sample(10, replace=T)

# bootstrap sample with prob that favors the numbers 1-5
prob1 = c(rep(.15, 5), rep(.05, 5))
sample(10, replace=T, prob=prob1)


# sample of size 5 from elements of a matrix
y1 = matrix(round(rnorm(25, 5)), ncol=5)

# saving sample size of 5 in vector x1
x1 = y1[sample(25, 5)]


# sampling the rows of the matrix
y2 = matrix(round(rnorm(40, 5)), ncol=5)

# saving the sample of rows
x2 = y2[sample(8,3),]



# BOOTSTRAPPING

# Goal: obtain a standard error for the estimate of the median
# functions used: sample, lapply, sapply

# let's create the dataset
# by taking 100 observations from a norm dist
# with mean 5 and stdev 3, rounded to nearest integer
SAMPLE_SIZE = 100
data = round(rnorm(SAMPLE_SIZE, 5, 3))
resamples = lapply(1:20, function(i) sample(data, replace=T))
resamples[1]

# calculate the median for each bootstrap sample
r.median = sapply(resamples, median)
r.median

# calculate the standard deviation of the distribution of medians
sqrt(var(r.median))
hist(r.median)


# wrapping up everything into a function
b.median  = function(data, num) {
  resamples = lapply(1:num, function(i) sample(data, replace=T))  
  r.median = sapply(resamples, median)
  std.err = sqrt(var(r.median))
  list(std.err=std.err, resamples=resamples, medians=r.median)
}

data1 = round(rnorm(100, 5, 3))
b1 = b.median(data1, 30)
b1$resamples[1]
b1$std.err
hist(b1$medians)


# BUILT-INS BOOTSTRAP IN R
library(boot)

# load data from the package
data(city)

# defining the ratio function
ratio = function(d, w) sum(d$x * w) / sum(d$u * w)
boot(city, ratio, R=999, stype="w")
