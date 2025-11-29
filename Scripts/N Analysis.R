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

#Ammonia 10/21
rm(list=ls())
NH4 <- read.csv("Data/NH4_Calculated_Result.csv")
View(NH4)
# Sorting N per kg of soil w date filter
NH4_21 <- subset(NH4, date == "10/21/25")
NH4_21_clean <- subset(NH4_21, !(site %in% c("Franklin", "Fryman")))
unique(NH4_21_clean$site)

NH4_21_sorted <- NH4_21_clean[order(NH4$mgNH4_kg_soil), ]
View(NH4_21_sorted)

#Helps to see first and last concentrations 
#head(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])
#tail(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])
summary(NH4_21_sorted$mgNH4_kg_soil)

#--Making the gradient finally
NH4_gradient <- quantile(NH4_21_sorted$mgNH4_kg_soil,
         probs = c(0.25, 0.5, 0.75), na.rm=TRUE) #don't let the NA's get u chile
NH4_gradient

#Min.   1st Qu.  Median  Mean   3rd Qu.   Max.  NA's 
#12.94   27.73   50.90   48.26   68.00   85.93   96 

# NH4 thresholds. Save for later could be helpful for figures or classifying groups idk?
cuts_NH4 <- quantile(NH4_21_sorted$mgNH4_kg_soil, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
NH4_21_sorted$target_level <- cut(NH4_21_sorted$mgNH4_kg_soil,
                                  breaks = c(-Inf, cuts_NH4, Inf),
                                  labels = c("Low", "Med-Low", "Med-High", "High"))

##############################################

#Ammonia 10/16 (incl Fryman/Franklin)
rm(list=ls())
NH4 <- read.csv("Data/NH4_Calculated_Result.csv")
View(NH4)

# Sorting N per kg of soil w date filter
NH4_16 <- subset(NH4, date == "10/16/25")
NH4_16_sorted <- NH4_16[order(NH4$mgNH4_kg_soil), ]
View(NH4_16_sorted)

#Helps to see first and last concentrations 
#head(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])
#tail(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])

summary(NH4_16_sorted$mgNH4_kg_soil)
#Min.  1st Qu.  Median    Mean  3rd Qu.    Max.  NA's 
#17.47  47.44   77.40    63.68   82.04    88.51   80 

#--Making the gradient finally
NH4_gradient <- quantile(NH4_16_sorted$mgNH4_kg_soil,
                         probs = c(0.25, 0.5, 0.75), na.rm=TRUE) #don't let the NA's get u chile
NH4_gradient

# NH4 thresholds. Save for later could be helpful for figures or classifying groups idk?
cuts_NH4 <- quantile(NH4_16_sorted$mgNH4_kg_soil, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
NH4_16_sorted$target_level <- cut(NH4_16_sorted$mgNH4_kg_soil,
                                  breaks = c(-Inf, cuts_NH4, Inf),
                                  labels = c("Low", "Med-Low", "Med-High", "High"))

#################################################

#--Nitrate 10/21 (-Franklin/Fryman)
rm(list=ls())
NO3 <- read.csv("Data/NO3_Calculated_Result.csv")
View(NO3)
# Sorting N per kg of soil w date filter
NO3_21 <- subset(NO3, date == "10/21/25")
NO3_21_clean <- subset(NO3_21, !(site %in% c("Franklin", "Fryman")))
unique(NO3_21_clean$site)

NO3_21_sorted <- NO3_21_clean[order(NO3$mgNO3_kg_soil), ]
View(NO3_21_sorted)

#Helps to see first and last concentrations 
#head(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])
#tail(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])

summary(NO3_21_sorted$mgNO3_kg_soil)
#Min.  1st Qu.  Median    Mean   3rd Qu.   Max.   NA's 
#2.251  12.365  29.298   26.329  37.428   53.991   96 

#--Making the gradient finally
NO3_gradient <- quantile(NO3_21_sorted$mgNO3_kg_soil,
                         probs = c(0.25, 0.5, 0.75), na.rm=TRUE) #don't let the NA's get u chile
NO3_gradient
# NO3 thresholds
cuts_NO3 <- quantile(NO3_21_sorted$mgNO3_kg_soil, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
NO3_21_sorted$target_level <- cut(NO3_21_sorted$mgNO3_kg_soil,
                                  breaks = c(-Inf, cuts_NO3, Inf),
                                  labels = c("Low", "Med-Low", "Med-High", "High"))

################################

#--Nitrate 10/16
rm(list=ls())
NO3 <- read.csv("Data/NO3_Calculated_Result.csv")
View(NO3)

# Sorting N per kg of soil w date filter
NO3_16 <- subset(NO3, date == "10/16/25")
NO3_16_sorted <- NO3_16[order(NO3$mgNO3_kg_soil), ]
View(NO3_16_sorted)

#---Helps to see first and last concentrations 
#head(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])
#tail(NH4_21_sorted[, c("site", "mgNH4_kg_soil")])

summary(NO3_16_sorted$mgNO3_kg_soil)
#Min.  1st Qu.  Median    Mean   3rd Qu.   Max.  NA's 
#2.066  19.232  27.280  27.639   38.222   53.673  80 

#--Making the gradient finally
NO3_gradient <- quantile(NO3_16_sorted$mgNO3_kg_soil,
                         probs = c(0.25, 0.5, 0.75), na.rm=TRUE) #don't let the NA's get u chile
NO3_gradient
# NH4 thresholds
cuts_NO3 <- quantile(NO3_16_sorted$mgNO3_kg_soil, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
NO3_16_sorted$target_level <- cut(NO3_16_sorted$mgNO3_kg_soil,
                                  breaks = c(-Inf, cuts_NO3, Inf),
                                  labels = c("Low", "Med-Low", "Med-High", "High"))
########################
#--Targets NO3 16
min_N <- 2.066
max_N <- 53.673

fertilizer_targets <- seq(from = min_N, to = max_N, length.out = 4)
fertilizer_targets #[1]  a)2.06600 b)19.26833 c)36.47067 d)53.67300


