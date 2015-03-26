rm(list=ls())

library(xts)
library(sas7bdat)

setwd("C:/Users/jlappen/Desktop/R/sax_institute")


# load data ---------------------------------------------------------------
load_data <- read.sas7bdat("dummy_2.sas7bdat")


# Histogram of Ages
par(mfrow=c(2, 1))
hist(load_data[,3], freq = FALSE, xlab = "Age", 
     main = "Distribution of Ages", 
     col = "#bdbdbd", ylim = c(0, 0.04))

curve(dnorm(x, mean = mean(load_data[,3]), sd = sd(load_data[,3])), 
      add = TRUE, col = "#de2d26", lwd = 2) 

# Barplot of Sex
barplot(table(load_data[, "fup1_sex"]), main = "Males & Females")


# Barplots of Categorical Variables
par(mfrow=c(2, 2))
barplot(table(load_data[, "fup_probirritsble"]), main = "Grumpy")
barplot(table(load_data[, "fup_probworry"]), main = "Worried")
barplot(table(load_data[, "fup_probsleep"]), main = "Sleepy")
barplot(table(load_data[, "fup_probappetite"]), main = "Hungry")


par(mfrow=c(1, 1))
barplot(table(load_data[, c("fup1_sex", "fup_probsleep")]), 
        main = "Problems Sleeping vs. Gender")

