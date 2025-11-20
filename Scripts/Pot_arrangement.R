#####################################################
#          Greenhouse Layout                        #
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
#display.all.moma()
#####################################################

rm(list=ls())
data <- read.csv("Data/NH4_Calculated_Result.csv")
view(data)
sites <- unique(data$site)
sites
#
mother <- 1:3
tags <- c("Red", "Green", "Orange", "Yellow")
reps <- 1:3
#Making the grid for the center bench
plants <- expand.grid(Site = sites,
                      Seed = mother,
                      Treatment = tags,
                      Rep = reps)
plants$Seed_ID <- paste(plants$Site, plants$Seed)
#Wanted to ask R for a random number so that the randomized arangement can be reproduced or 
#avoided idk
sample(1:360, 1)
#> sample(1:360, 1)
#Response [1] 292

set.seed(292)
plants$Position <- sample(1:nrow(plants))
plants <- plants[order(plants$Position), ]

n_rows <- 15
n_cols <- 24

plants$Row <- rep(1:n_rows, each = n_cols)
plants$Col <- rep(1:n_cols, times = n_rows)

#Plot that ho
ggplot(plants, aes(x = Col, y = Row, fill = Treatment)) +
  geom_tile(color = "black") +
  scale_y_reverse() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Randomized Greenhouse Layout",
       x = "Column", y = "Row") +
  scale_fill_moma_d(palette = "Klein", direction = 1)

