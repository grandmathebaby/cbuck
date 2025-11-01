#--Sacha Medjo-Akono
#--Fall2025
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
library(lme4)
library(pscl)
library(factoextra)
library("pwr")
library(MASS)
library(MoMAColors)
display.all.moma()
#--Question 1
rm(list=ls())
nitrate <- read_csv("Thesis/cbuck/NO3PlotsInc.csv")
View(nitrate)