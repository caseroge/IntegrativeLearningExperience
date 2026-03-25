#Data Analysis
source("02_descriptive_statistics_ILE.R", echo = T)
pacman::p_load(tidyverse, readr, readxl, lubridate, arsenal, openxlsx, logbin, ResourceSelection, car, DescTools, lmtest)
dim(moldova)
#1. fitting general log binomial 

#logB.moldova.squarem <- logbin(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age_diag, 
                   #    data = moldova,
                   #    accelerate = "squarem")

glm.moldova <- glm(resistant.f ~ ever_deten.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age_diag, 
                               data = moldova,
                   family = binomial(link = "logit"))

#2. test model
#summary(logB.moldova.squarem)
#exp(coef(logB.moldova.squarem))
#exp(confint(logB.moldova.squarem))



summary(glm.moldova)
exp(coef(glm.moldova))
exp(confint(glm.moldova))
Cstat(glm.moldova) #0.649 poor discrimination
moldova$resistant.f.num <- as.numeric(moldova$resistant.f)
complete_cases <- as.numeric(rownames(model.frame(glm.moldova))) #gets only complete cases
hoslem.test(moldova$resistant.f[complete_cases], fitted(glm.moldova), g = 10) #p = 2.2e-6 bad (shows poor fit)



#3. conduct fit testing 
stepAIC(glm.moldova, direction = 'both') #AIC - removes examinaton - final at 189.86
stepAIC(glm.moldova, direction = 'both', k = log(11687)) #BIC - removes examination final at 199

#4. Select best model
#regular fit model is good

#5. add HIV as effect modifier 
glm.moldova.adj <- glm(resistant.f ~ ever_deten.f * hiv.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age_diag, 
                               data = moldova, family = binomial(link = "logit"))

base.mod <- glm(resistant.f ~ ever_deten.f + hiv.f + sexM.f + rural.f + homeless.f + jobcat.f + edu.f + age_diag, 
                data = moldova, family = binomial(link = "logit"))


#6. test for effect modifications
summary(glm.moldova.adj)
exp(coef(glm.moldova.adj))
exp(confint(glm.moldova.adj))
Cstat(glm.moldova.adj) #0.649 poor discrimination
hoslem.test(moldova$resistant.f[complete_cases], fitted(glm.moldova.adj), g = 10) #p = 2.2e-6 bad (shows poor fit)
Anova(glm.moldova.adj, type=c('III'), test="Wald")
lrtest(base.mod, glm.moldova.adj) #p = 0.7169 not significant

summary(base.mod)
exp(coef(base.mod))
exp(confint(base.mod))



#7. report results
summary(glm.moldova)
exp(coef(glm.moldova))
exp(confint(glm.moldova))
# OR = 2.277 95%CI (1.99, 2.605) p = 2.2e-16


