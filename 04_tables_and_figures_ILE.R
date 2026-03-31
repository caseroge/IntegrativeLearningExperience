#Tables
source("03_analysis_ILE.R")
pacman::p_load(ggplot2, broom)
#Table 2
mycontrols <- modelsum.control(show.intercept = FALSE,
                               show.adjust = TRUE,
                               conf.int = TRUE,
                               digits = 3,
                               digits.p = 3)

table2 <- modelsum(resistant.f ~ ever_deten.f,
                   adjust = ~ hiv.f + sexM.f + rural.f + homeless.f + 
                     jobcat.f + edu.f + age_diag,
                   family = "binomial",
                   data = moldova,
                   control = mycontrols)

summary(table2, text = TRUE, digits = 3)

write2word(table2, "table2_ile_test.docx", title = "Table 2")

# First convert the arsenal table to a dataframe
table2_df <- as.data.frame(table2)

# Then write to Excel
write.xlsx(table2_df, "table2_ile_test.xlsx")


#figure (tree plot maybe)
tidy_mod <- tidy(base.mod, conf.int = T, exponentiate = T) # clean data frame, 1 row per predictor 
tidy_mod

tidy_mod %>% filter(term != "(Intercept)") %>% #remove intercept ggplot
  ggplot(aes(x=estimate, y=reorder(term, estimate))) + geom_point() + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") + 
  scale_x_log10() +
  labs(x="Odds Ratio (log scale)", y = NULL, title = 'Predictors of Drug Resistance')
           
           
         

