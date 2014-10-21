library(ggplot2)
library(reshape2)
library(bootstrap)
library(lme4)
library(stringr)
library(lubridate)
library(plyr) # first load plyr then dplyr from https://github.com/hadley/dplyr
library(dplyr)

## add some style elements for ggplot2
theme_set(theme_bw())

## standard error of the mean
sem <- function (x) {
  sd(x,na.rm=TRUE) / sqrt(length(x))
}

## NA functions
na.mean <- function(x) {mean(x,na.rm=T)}
na.median <- function(x) {median(x,na.rm=T)}
na.sum <- function(x) {sum(x,na.rm=T)}
na.sd <- function(x) {sd(x,na.rm=T)}

## convert to number
to.n <- function(x) {
  as.numeric(as.character(x))
}

## inverse logistic
inv.logit <- function (x) {
  exp(x) / (1 + exp(x)) 
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
  if (x < .001) {return("***")}
  if (x < .01) {return("**")}
  if (x < .05) {return("*")}
}
