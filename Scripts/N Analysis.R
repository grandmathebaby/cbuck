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
df <- read.csv("Data/NH4PlotsInc.csv")
View(df)
ggplot(df, aes(x=concentration, y=abs)) + geom_point() + theme_bw()

##Ammonia Concetration

# --- Separate calibration and samples ---
standards <- subset(df, site == "standard" & !is.na(concentration))
samples  <- subset(df, site != "standard" & site != "blank")

# --- 1. Fit calibration curve (Berthelot method) ---
fit <- lm(abs ~ concentration, data = standards)
summary(fit)

m <- coef(fit)[["concentration"]]  # slope
b <- coef(fit)[["(Intercept)"]]    # intercept

cat("Calibration equation: Absorbance =", round(m,4), "* Conc +", round(b,4), "\n")

# --- 2. Predict NH4+-N concentration for samples (mg N/L) ---
samples$conc_mgL <- (samples$abs - b) / m

# --- 3. Convert to mg N per kg soil ---
# if extractant and soil_g_dry columns are filled in your file, use them:
samples$mgN_per_kg <- with(samples, conc_mgL * (extractant_ml / 1000) / (soil_g_dry / 1000))

# If some samples use default 15 mL extractant per 2 g soil:
samples$mgN_per_kg[is.na(samples$mgN_per_kg)] <- samples$conc_mgL[is.na(samples$mgN_per_kg)] * 7.5

# --- 4. Export results ---
cat("\n===== NH4+-N Results =====\n")
print(samples[, c("site", "abs", "conc_mgL", "mgN_per_kg")], row.names = FALSE)


#--Nitrate
rm(list=ls())
Nitrate <- read_csv("Data/NO3PlotsInc.csv")
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
