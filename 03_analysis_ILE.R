#Data Analysis
source("02_descriptive_statistics_ILE.R", echo = T)
pacman::p_load(tidyverse, readr, readxl, lubridate, arsenal, openxlsx, logbin, ResourceSelection, car, DescTools, lmtest)
dim(moldova)

Anova(glm.moldova.adj, type=c('III'), test="Wald")
lrtest(base.mod, glm.moldova.adj) #p = 0.7169 not significant
#1. fit general 

glm.moldova <- glm(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age_diag + hiv.f, 
                               data = moldova,
                   family = binomial(link = "logit"))

#test model 
summary(glm.moldova)
Cstat(glm.moldova) #0.649 poor discrimination
moldova$resistant.f.num <- as.numeric(moldova$resistant.f)
complete_cases <- as.numeric(rownames(model.frame(glm.moldova))) #gets only complete cases
hoslem.test(moldova$resistant.f[complete_cases], fitted(glm.moldova), g = 10) #p = 2.2e-6 bad (shows poor fit)

#2 Fit Test
#Order of p value - jobcat, educat, homeless, rural,
fit <- function(old, new){
  value <- (old-new) / old
  if(value > 1.1 | value < 0.9) {
     print("Passed")} else {
       print("Failed")
}
}
#starting p value = 0.035379

#remove jobcat
glm.moldova.q <- glm(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f + edu.f + age_diag + hiv.f, 
                   data = moldova,
                   family = binomial(link = "logit"))
summary(glm.moldova.q)
fit(0.035379, 0.012707) #passed

#removed educat
glm.moldova.w <- glm(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f  + age_diag + hiv.f, 
                     data = moldova,
                     family = binomial(link = "logit"))
summary(glm.moldova.w)
fit(0.012707, 0.004869) #passed

#remove homeless
glm.moldova.e <- glm(resistant.f ~ ever_deten.f + sexM.f + rural.f  + age_diag + hiv.f, 
                     data = moldova,
                     family = binomial(link = "logit"))
summary(glm.moldova.e)
fit(0.004869, 0.004021) #passed


#remove rural
glm.moldova.r <- glm(resistant.f ~ ever_deten.f + sexM.f + age_diag + hiv.f, 
                     data = moldova,
                     family = binomial(link = "logit"))
summary(glm.moldova.r)
fit(0.004021, 0.002302) #passed


#add hiv.f as interaction term
glm.moldova.t <- glm(resistant.f ~ ever_deten.f * hiv.f + sexM.f + age_diag, 
                     data = moldova,
                     family = binomial(link = "logit"))
summary(glm.moldova.t)

#normal model is best.

