#Final Results Page
library(ResourceSelection)
numbers <- function(var) {
  print(table(moldova[[var]]), useNA = 'ifany')
  print(round(prop.table(table(moldova[[var]])), 3))
}

numbers('ever_deten.f')
numbers('hiv.f')
numbers('resistant.f')





#Complete Case
#Crude Results 
summary(crude.glm) #p 6.65e-07
exp(coef(crude.glm)) #1.794
exp(confint(crude.glm)) #(1.418, 2.25)

par(mfrow=c(2,2))
plot(crude.glm)
roc_obj <- roc(crude.glm$y, fitted(crude.glm))
auc(roc_obj)
hoslem.test(crude.glm$y, fitted(crude.glm), g =3)


#Adjusted Results 
summary(dag.glm) # non-significant p value .1716
exp(coef(dag.glm)) #1.203
exp(confint(dag.glm)) #0.917, 1.56

plot(dag.glm)
roc_obja <- roc(dag.glm$y, fitted(dag.glm))
auc(roc_obja)
hoslem.test(dag.glm$y, fitted(dag.glm), g =10)

#EMM Results 
summary(emm.glm) # interaction term p value - 0.251
exp(coef(emm.glm)) #interaction term coef - 0.663
exp(confint(emm.glm)) #interaction term confint 0.323, 1.323

plot(emm.glm)

#No HIV Results 
summary(dag.glm.noH) # p = 0.01252
exp(coef(dag.glm.noH)) #1.368
exp(confint(dag.glm.noH)) #1.065, 1.743

plot(dag.glm.noH)


#Not Complete Case
#Imputed Value Results
summary(pooled) # p = 0.056
exp(0.241) #OR: 1.27
exp(0.24165 - (1.96 * 0.126)); exp(0.24165 + (1.96 * 0.126)) #0.994, 1.630


R.version
