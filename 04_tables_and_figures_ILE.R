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


           
         

