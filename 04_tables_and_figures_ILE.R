#Tables
source("03_analysis_ILE.R")
#Table 2
mycontrols <- modelsum.control(show.intercept = FALSE,
                               show.adjust = TRUE)

table2 <- modelsum(resistant.f ~ ever_deten.f,
                   adjust = ~ sexM.f + rural.f + homeless.f + 
                     jobcat.f + edu.f + age_diag + hiv.f,
                   family = "binomial",
                   data = moldova,
                   control = mycontrols)

summary(table2, text = TRUE, digits = 3)



#figure (tree plot maybe)