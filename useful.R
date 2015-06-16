library(dplyr)
library(magrittr)
library(ggplot2)

## add some style elements for ggplot2
theme_set(theme_bw())

# @aen - luminance palette
# @mikabr - solarized?

## NA functions
na.mean <- function(x) {mean(x, na.rm=T)}
na.median <- function(x) {median(x, na.rm=T)}
na.sum <- function(x) {sum(x, na.rm=T)}
na.sd <- function(x) {sd(x, na.rm=T)}

## standard error of the mean
sem <- function (x) {
  na.sd(x) / sqrt(length(x))
}

## convert to number
to.n <- function(x) {
  as.numeric(as.character(x))
}

## number of unique subs
n.unique <- function (x) {
  length(unique(x))
}

ci.low <- function(x) {quantile(x, 0.025)}
ci.high <- function(x) {quantile(x, 0.975)}

## Computes arbitrary bootstrap statistics on univariate data. 
## By default, computes a 95% confidence interval of the mean of the data.

# Arguments
# x - a vector of data to bootstrap over
# summary.function - a string that is the name of a function to be computed over each
#                    set of samples. This function needs to take a vector and return
#                    a single number (e.g. mean, median)
# statisics - a vector of strings that are names of functions to be computed over the
#             set of summary values from all samples (e.g. sd, ci.high, ci.low) 
# nboot - the number of bootstrap samples to take
#
# Examples:
# Mean and 95% Confidence Interval for 1000 Samples from a Standard Normal
# x <- rnorm(1000, mean = 0, sd = 1)
#
# ci.low <- function(x) {quantile(x, 0.025)}
# ci.high <- function(x) {quantile(x, 0.975)}
# 
# estimates <- multi.boot(x,statistics = c("ci.low", "mean", "ci.high"))
multi.boot <- function(x, statistics = c("ci.low","ci.high"),
                           summary.function = "mean", nboot = 1000) {
  
  
  formulas <- sapply(statistics, function(x) {as.formula(paste0("~",x))})
  
  one.sample <- function() {
    do.call(summary.function,list(sample(x, replace = TRUE)))
  }
  
  all.samples <- data.frame(sample = replicate(nboot, one.sample())) %>%
      summarise_each(funs_(formulas), sample)
  
  if(length(formulas) == 1) {
    all.samples %<>%
      rename_(.dots = setNames("sample", statistics))
  }
  
  return(all.samples)

}

## A wrapper for multi.boot for use with dataframes
## Calls multi.boot for a specified column and returns all statistics as their
## own columns, while presevering group_by structure.

# Arguments
# df - A data.frame to bootstrap for
# col - A string indicating the column to bootstrap
# summary.function - a string that is the name of a function to be computed over each
#                    set of samples. This function needs to take a vector and return
#                    a single number (e.g. mean, median)
# statisics - a vector of strings that are names of functions to be computed over the
#             set of summary values from all samples (e.g. sd, ci.high, ci.low) 
# nboot - the number of bootstrap samples to take
#
# Examples:
# Mean and 95% Confidence Interval for 1000 Samples from a Standard Normal
# gauss.1 <- data.frame(value = rnorm(1000, mean = 0, sd = 1),
#                      condition = 1)
# gauss.2 <- data.frame(value = rnorm(1000, mean = 2, sd = 3),
#                      condition = 2)
# df <- bind_rows(gauss.1, gauss.2) %>% group_by(condition)
#
# ci.low <- function(x) {quantile(x, 0.025)}
# ci.high <- function(x) {quantile(x, 0.975)}
# 
# estimates <- multi.boot.df(df, "value", statistics = c("ci.low", "mean", "ci.high"))
multi.boot.df <- function(df, col, statistics = c("ci.low","ci.high"),
                         summary.function = "mean", nboot = 1000) {

  boot.df <- df %>%
    do_(boot = ~multi.boot(.[[col]],
                           statistics = statistics,
                           summary.function = summary.function,
                           nboot = nboot))
  
  for (fun in statistics) {
    dots = list(~boot[[fun]])
    boot.df %<>%
      mutate_(.dots = setNames(dots, c(fun)))
  }
  
  boot.df %>%
    select(-boot)

}

# theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
# ci.low <- function(x,na.rm=T) {
#   mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
# ci.high <- function(x,na.rm=T) {
#   quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

## get stars for significance testing
getstars <- function(x) {
  if (x > .1) {return("")}
  if (x < .1 & x >= .05) {return(".")}
  if (x < .05) {return("*")}
  if (x < .01) {return("**")}
  if (x < .001) {return("***")}
}
