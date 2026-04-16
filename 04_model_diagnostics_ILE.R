#model diagnositics
library(car)
library(ResourceSelection)
library(DescTools)
library(lmtest)
library(pROC)

#4 different models
#crude, cie, dag, emm 

#assumptions , linearity, no perfect colinearity, no influential outliers, interdependence of observations 
#model assessment, overal fit (hosmerlemshow), AIC/BIC, LRT, classification performacy (C stat, AUC, plot)

moldova$resistant_num <- as.numeric(moldova$resistant.f) - 1
boxTidwell(resistant_num ~ age_diag, data = moldova, subset = age_diag >= 1) 
#for age, linearity assumption is violated so age must be categorized

#crude
outs <- function(glm_model) {
  print(summary(glm_model))
  print(exp(coef(glm_model)))
  print(exp(confint(glm_model)))
}

assume <- function(glm_model) {
  print(vif(glm_model))
  par(mfrow=c(1,2))
  plot(glm_model, which = 4)  # Cook's distance plot
  influencePlot(glm_model)
}


assess <- function(oldmodel, newmodel) {
  hl <- hoslem.test(newmodel$y, fitted(newmodel))
  lr <- lrtest(oldmodel, newmodel)
  hl
  lr
  
  print(BIC(oldmodel, newmodel))
  
  roc_obj1 <- roc(newmodel$y, fitted(newmodel))
  print(auc(roc_obj1))
  par(mfrow=c(1,1))
  plot(roc_obj1)
  ifelse(hl$p.value <= 0.05, print("HL Significant (Bad Fit)"), print("HL Non-significant (Good Fit)"))
  ifelse((lr$`Pr(>Chisq)`[2]) >= 0.05, print("LRT Significant"), print("LRT Non-Significant"))
}

#assess crude 

outs(crude.glm) #OR = 1.794 (1.418, 2.251) p = 6.65e-7
assume(crude.glm) #cant complete
BIC(crude.glm) #4430.5

#cie
outs(cie.glm) #1.52 (1.19, 1.92) p = .000499
assume(cie.glm) #idk
assess(crude.glm, cie.glm) #reduced BIC, C = 0.5915, HL showsgood fit, LRT non-significant 

#dag
outs(dag.glm) #1.29 (1.0037, 1.653) p = .0428
assume(dag.glm) #
assess(crude.glm, dag.glm) #r


#emm
outs(emm.glm) #ever in detention non-significant
assume(cie.glm) #idk
assess(crude.glm, cie.glm) #reduced BIC, C = 0.5915, HL showsgood fit, LRT non-significant 

1