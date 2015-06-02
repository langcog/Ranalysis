library(dplyr)
library(ggplot2)
library(bootstrap)

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

## for bootstrapping 95% confidence intervals
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

## get stars for significance testing
getstars <- function(x) {
  if (x > .1) {return("")}
  if (x < .1 & x >= .05) {return(".")}
  if (x < .05) {return("*")}
  if (x < .01) {return("**")}
  if (x < .001) {return("***")}
}
