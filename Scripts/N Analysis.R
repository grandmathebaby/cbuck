#####################################################
#          N Analysis.                              #
#          Sacha Medjo-Akono                        #
#          Fall 2025                                #
#####################################################
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
#####################################################
#                     Ammonia                       #
#####################################################

rm(list=ls())
NH4 <- read.csv("Data/NH4PlotsInc.csv")
View(NH4)
ggplot(NH4, aes(x=concentration, y=abs)) + geom_point() + theme_bw()

#--Filter for standards only, including blanks
standards <- subset(NH4, site %in% c("standard", "blank"))
standards
#--Fitting the calibration line,
#--it's a linear regression, giving the slope (m), intercept (b) and R^2 yay
model_stand <- lm(abs ~ concentration, data = standards)
summary(model_stand)

#--Making the calibration line plot of standards
m <- coef(model_stand)[2] #the slope from earlier
b <- coef(model_stand)[1] #the intercept
r2 <- summary(model_stand)$r.squared

#Plot
ggplot(standards, aes(x = concentration, y = abs)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", lwd = 1) +
  annotate("text",
           x = max(standards$concentration)*0.6,
           y = max(standards$abs)*0.2,
           label = paste0("A = ", round(m,3), "C + ", round(b,3),
                          "\nR² = ", round(r2,3))) + #annotation could be omitted but kinda nice
  theme_bw() +
  labs(
    title = "NH4+ Calibration Curve (Berthelot Reaction)",
    x = "Concentration (mg N/L)",
    y = "Absorbance (660 nm)"
  )

#--If I wanted to separate by date 
unique(standards$date)

stand21 <- subset(standards, date == "10/21/25")
model_21 <- lm(abs ~ concentration, data = stand21)
summary(model_21)

stand16 <- subset(standards, date == "10/16/25")
model_16 <- lm(abs~concentration, data = stand16)
summary(model_16)

#--By date summaries
m21 <- coef(model_21)
b21 <- coef(model_21)
r21 <- summary(model_21)$r.squared

m16 <- coef(model_16)
b16 <- coef(model_16)
r216 <- summary(model_16)$r.squared
#--Plot
ggplot(standards, aes(x = concentration, y = abs, color = date)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, lwd = 1.2) +
  theme_bw() +
  labs(
    title = "NH4+ Calibration Curves by Date (Berthelot Reaction)",
    x = "Concentration (mg N/L)",
    y = "Absorbance (660 nm)",
    color = "Date"
  )

#--Second plot, diff boxes
ggplot(standards, aes(x = concentration, y = abs)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_bw() +
  facet_wrap(~ date) +
  labs(
    title = "NH4+ Calibration Curves by Date",
    x = "Concentration (mg N/L)",
    y = "Absorbance (660 nm)"
  )

#--Finding the unknown concentration now
#-Filter samples only
samples <- subset(NH4, !(site %in% c("standard", "blank" )))
#-Find target concentration
samples$mgNH4_L <- (samples$abs - b) / m
samples$mgNH4_L
#-Convert to mg N per kg soil
samples$mgNH4_kg_soil <- (samples$mgNH4_L * samples$extractant_ml) / samples$soil_g_dry
samples$mgNH4_kg_soil

#-- Makin it a csv so i can seeeee and separate by date later
write.csv(samples, "NH4_Calculated_Result.csv", row.names = FALSE)
NH4_Result <- read.csv("Data/NH4_Calculated_Result.csv")
View(NH4_Result)

#####################################################
#                     Nitrate                       #
#####################################################
rm(list=ls())
NO3 <- read_csv("Data/NO3PlotsInc.csv")
View(NO3)
ggplot(NO3, aes(x=concentration, y=abs)) + geom_point() + theme_bw()

##--Filter for standards only, including blanks
standards <- subset(NO3, site %in% c("standard", "blank"))
standards
#--Fitting the calibration line,
#--it's a linear regression, giving the slope (m), intercept (b) and R^2 yay
model_stand <- lm(abs ~ concentration, data = standards)
summary(model_stand)

#--Making the calibration line plot of standards
m <- coef(model_stand)[2] #the slope from earlier
b <- coef(model_stand)[1] #the intercept
r2 <- summary(model_stand)$r.squared

#Plot
ggplot(standards, aes(x = concentration, y = abs)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", lwd = 1) +
  annotate("text",
           x = max(standards$concentration)*0.6,
           y = max(standards$abs)*0.2,
           label = paste0("A = ", round(m,3), "C + ", round(b,3),
                          "\nR² = ", round(r2,3))) + #annotation could be omitted but kinda nice
  theme_bw() +
  labs(
    title = "NO3 Calibration Curve (VCL3/Griess Reaction)",
    x = "Concentration (mg N/L)",
    y = "Absorbance (540 nm)"
  )

#--If I wanted to separate by date 
unique(standards$date)

stand21 <- subset(standards, date == "10/21/25")
model_21 <- lm(abs ~ concentration, data = stand21)
summary(model_21)

stand16 <- subset(standards, date == "10/16/25")
model_16 <- lm(abs~concentration, data = stand16)
summary(model_16)
#--By date
m21 <- coef(model_21)
b21 <- coef(model_21)
r21 <- summary(model_21)$r.squared

m16 <- coef(model_16)
b16 <- coef(model_16)
r216 <- summary(model_16)$r.squared
#--Plot
ggplot(standards, aes(x = concentration, y = abs, color = date)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, lwd = 1.2) +
  theme_bw() +
  labs(
    title = "NO3 Calibration Curves by Date (VCL3/Griess Reaction)",
    x = "Concentration (mg N/L)",
    y = "Absorbance (540 nm)",
  )

#--Second plot, diff boxes
ggplot(standards, aes(x = concentration, y = abs)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_bw() +
  facet_wrap(~ date) +
  labs(
    title = "NO3 Calibration Curves by Date (VCL3/Griess Reaction)",
    x = "Concentration (mg N/L)",
    y = "Absorbance (540 nm)",
  )

#--Finding the unknown concentration now
#-Filter samples only
samples <- subset(NO3, !(site %in% c("standard", "blank" )))
#-Find target concentration
samples$mgNO3_L <- (samples$abs - b) / m
samples$mgNO3_L
#-Convert to mg N per kg soil
samples$mgNO3_kg_soil <- (samples$mgNO3_L * samples$extractant_ml) / samples$soil_g_dry
samples$mgNO3_kg_soil

#-- Makin it a csv so i can seeeee and separate by date later
write.csv(samples, "NO3_Calculated_Result.csv", row.names = FALSE) #move manually to data folder
NO3_Result <- read.csv("Data/NO3_Calculated_Result.csv")
View(NO3_Result)

#####################################################
#             Correlation tests                     #
#####################################################

# Pearson
cor.test(Nitrate$absorbance, Nitrate$concentration, method="pearson")
# r = 0.57, p = 0.006
#Spearman
cor.test(Nitrate$absorbance, Nitrate$concentration, method="spearman")
# rho = 0.39, p = 0.07
#Kendall
cor.test(Nitrate$absorbance, Nitrate$concentration, method="kendall")
# tau = 0.25, p = 0.11

#####################################################
#             Target N per kg for Pots              #
#####################################################

#Ammonia
rm(list=ls())
NH4 <- read.csv("Data/NH4_Calculated_Result.csv")
View(NH4)
# Sorting N per kg of soil w date filter
NH4_21 <- subset(NH4, date == "10/21/25")

NH4_21_sorted <- NH4_21[order(NH4$mgNH4_kg_soil), ]
View(NH4_21_sorted)

#Helps to see first and last concentrations 
#head(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])
#tail(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])
summary(NH4_21_sorted$mgNH4_kg_soil)

#--Making the gradient finally
NH4_gradient <- quantile(NH4_21_sorted$mgNH4_kg_soil,
         probs = c(0.25, 0.5, 0.75), na.rm=TRUE) #don't let the NA's get u chile
NH4_gradient
# NH4 thresholds
cuts_NH4 <- quantile(NH4_21_sorted$mgNH4_kg_soil, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
NH4_21_sorted$target_level <- cut(NH4_21_sorted$mgNH4_kg_soil,
                                  breaks = c(-Inf, cuts_NH4, Inf),
                                  labels = c("Low", "Med-Low", "Med-High", "High"))

#Nitrate
rm(list=ls())
NO3 <- read.csv("Data/NO3_Calculated_Result.csv")
View(NO3)
# Sorting N per kg of soil w date filter
NO3_21 <- subset(NO3, date == "10/21/25")

NO3_21_sorted <- NO3_21[order(NO3$mgNO3_kg_soil), ]
View(NO3_21_sorted)

#Helps to see first and last concentrations 
#head(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])
#tail(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])
summary(NO3_21_sorted$mgNO3_kg_soil)

#--Making the gradient finally
NO3_gradient <- quantile(NO3_21_sorted$mgNO3_kg_soil,
                         probs = c(0.25, 0.5, 0.75), na.rm=TRUE) #don't let the NA's get u chile
NO3_gradient
# NH4 thresholds
cuts_NO3 <- quantile(NO3_21_sorted$mgNO3_kg_soil, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
NO3_21_sorted$target_level <- cut(NO3_21_sorted$mgNO3_kg_soil,
                                  breaks = c(-Inf, cuts_NO3, Inf),
                                  labels = c("Low", "Med-Low", "Med-High", "High"))