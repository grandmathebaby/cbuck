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
#--Ammonia
rm(list=ls())
Ammonia <- read_csv("NH4PlotsInc.csv")
View(Ammonia)