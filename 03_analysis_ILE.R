#Data Analysis
source("01_data_cleaning_ILE.R", echo=T)
source("02_descriptive_statistics_ILE.R", echo = T)
pacman::p_load(tidyverse, readr, readxl, lubridate, arsenal, openxlsx, logbin. ResourceSelection )
dim(moldova)
#1. fitting general log binomial 

logB.moldova.squarem <- logbin(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age_diag, 
                       data = moldova,
                       accelerate = "squarem")

glm.moldova <- glm(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age_diag, 
                               data = moldova,
                   family = binomial(link = "logit"))

#2. test model
summary(logB.moldova.squarem)
exp(coef(logB.moldova.squarem))
exp(confint(logB.moldova.squarem))
par(mfrow=c(2, 2))
plot(logB.moldova.squarem) # really bad

summary(glm.moldova)
exp(coef(glm.moldova))
exp(confint(glm.moldova))
par(mfrow=c(2, 2))
plot(glm.moldova) # really bad

library(ResourceSelection)
hoslem.test(as.numeric(moldova$resistant.f) - 1, 
            fitted(logB.moldova), g = 10)

skewness(glm.moldova$residuals)


#3. conduct fit testing 
stepAIC(glm.moldova, direction = 'both') #AIC - removes examinaton - final at 189.86
stepAIC(glm.moldova, direction = 'both', k = log(11687)) #BIC - removes examination final at 199

#4. Select best model

#5. add HIV as effect modifier 
glm.moldova.adj <- glm(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age_diag * hiv.f, 
                               data = moldova, family = binomial(link = "logit"))
summary(glm.moldova.adj)
exp(coef(glm.moldova.adj))
exp(confint(glm.moldova.adj))
par(mfrow=c(2, 2))
plot(glm.moldova.adj) # really bad


#6. test for effect modifications 

#7. report results