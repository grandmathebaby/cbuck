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
ggplot(Ammonia, aes(x=concentration, y=absorbance)) + geom_point() + theme_bw()
#--Nitrate
rm(list=ls())
Nitrate <- read_csv("NO3PlotsInc.csv")
View(Nitrate)
ggplot(Nitrate, aes(x=concentration, y=absorbance)) + geom_point() + theme_bw()
#
# Then do each type of correlation test
cor.test(Nitrate$absorbance, Nitrate$concentration, method="pearson")
# r = 0.57, p = 0.006

cor.test(Nitrate$absorbance, Nitrate$concentration, method="spearman")
# rho = 0.39, p = 0.07

cor.test(Nitrate$absorbance, Nitrate$concentration, method="kendall")
# tau = 0.25, p = 0.11