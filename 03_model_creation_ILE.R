#Data Analysis
source("02_descriptive_statistics_ILE.R", echo = T)
pacman::p_load(tidyverse, readr, readxl, lubridate, arsenal, openxlsx, logbin, ResourceSelection, car, DescTools, lmtest)
dim(moldova)

#for each glm model 

#Assumptions testing 
#linearity 
#no perfect colinearity 
#no influential outliers
#interdependence of observations 

#model assessment 
#Overall model fit - Hosmer-Lemeshow test
#AIC for modeling comarriosn 
#Psuedo Rsqared
#LRT
#classification performance 
library(car)
library(ResourceSelection)
library(DescTools)
library(pROC)

moldova <- moldova %>% mutate(age.cat = case_when(
  age_diag < 25 ~ 1,
  age_diag >= 25 & age_diag < 44 ~ 2,
  age_diag >= 44 & age_diag < 65 ~ 3,
  age_diag >= 65 ~ 4,
))
moldova$age.cat.f <- factor(moldova$age.cat)

Anova(glm.moldova.adj, type=c('III'), test="Wald")
lrtest(base.mod, glm.moldova.adj) #p = 0.7169 not significant


#crude model 
crude.glm <- glm(resistant.f ~ ever_deten.f, data = moldova, family = binomial(link = "logit"))

exp(coef(crude.glm)) #1.79
exp(confint(crude.glm)) #(1.42, 2.24)


boxTidwell(resistant.f~age_diag, data = moldova)
vif(crude.glm)

hoslem.test(crude.glm$y, fitted(crude.glm))
AIC(crude.glm) #4416


roc_obj <- roc(crude.glm$y, fitted(crude.glm))
auc(roc_obj)
plot(roc_obj)


#Change in estimate approach
#forwards 
#age and sex will be in there regardless
start.glm <- glm(resistant.f ~ ever_deten.f + age.cat.f + sexM.f, 
                              data = moldova, family = binomial(link = "logit"))
exp(coef(start.glm)) #1.527

#1
#rural
start.glm.rural <- glm(resistant.f ~ ever_deten.f + age.cat.f + sexM.f + rural.f, 
                              data = moldova, family = binomial(link = "logit"))
exp(coef(start.glm.rural)) #1.484
((1.5484-1.527)/1.527) * 100 #1%

#homeless
start.glm.homeless <- glm(resistant.f ~ ever_deten.f + age.cat.f + sexM.f + homeless.f, 
                                    data = moldova, family = binomial(link = "logit"))
exp(coef(start.glm.homeless)) #1.577
((1.277-1.527)/1.527) * 100 #0%

#job
start.glm.jobcat <- glm(resistant.f ~ ever_deten.f + age.cat.f + sexM.f + jobcat.f, 
                                    data = moldova, family = binomial(link = "logit"))
exp(coef(start.glm.jobcat)) #1.479
((1.441-1.527)/1.527) * 100 #-5.6

#education
start.glm.edu <- glm(resistant.f ~ ever_deten.f + age.cat.f + sexM.f + edu.f, 
                                    data = moldova, family = binomial(link = "logit"))
exp(coef(start.glm.edu)) #1.511
((1.49-1.527)/1.527) * 100 #-2.42

#hiv
start.glm.hiv <- glm(resistant.f ~ ever_deten.f + age.cat.f + sexM.f + hiv.f, 
                                    data = moldova, family = binomial(link = "logit"))
exp(coef(start.glm.hiv)) #1.455
((1.423-1.527)/1.527) * 100 #-6.8




cie.glm <- start.glm


dag.glm <- glm(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age.cat.f + hiv.f, 
               data = moldova,
               family = binomial(link = "logit"))

#create interaction term model 
emm.glm <- glm(resistant.f ~ ever_deten.f * hiv.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age.cat.f, 
               data = moldova,
               family = binomial(link = "logit"))





#1. fit general 
glm.moldova <- glm(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age_diag + hiv.f, 
                               data = moldova,
                   family = binomial(link = "logit"))




#test model 
exp(summary(glm.moldova))
exp(coef(glm.moldova))
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
#Change in Estimate Approach 
1

