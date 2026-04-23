#Table Creation ILE 
library(gtsummary)
library(broom)
library(dplyr)
#Table 1

library(flextable)
install.packages("broom")

table_1 %>%
  as_flex_table() %>%
  save_as_docx(path = "tabled_ILE.docx")


table_1 <- moldova %>%
  tbl_summary(
    by = ever_deten.f,
    include = c(sexM.f, age_diag, rural.f, 
                homeless.f, jobcat.f, edu.f, hiv.f),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_p() %>%
  bold_labels()

table_1 %>% as_tibble() %>% View()
#table 2
unadj <- crude.glm %>% tbl_regression(exponentiate = T)
unadj

adj <- dag.glm %>% tbl_regression(exponentiate = T)
adj

table_2 <- tbl_merge(
  tbls = list(unadj, adj),
  tab_spanner = c("Unadjusted", "Adjusted")
)
table_2

table_2 %>%
  as_flex_table() %>%
  save_as_docx(path = "table2_ILE.docx")
#Figure 1
library(ggplot2)

forest_data <- data.frame(
  model    = c("Crude", "Adjusted", "HIV Removed", "Imputed"),
  OR       = c(1.79, 1.20, 1.37, 1.27),        # paste your ORs here
  CI_lower = c(1.42, 0.92, 1.07, 0.99),        # paste lower CIs here
  CI_upper = c(2.25, 1.56, 1.74, 1.63)         # paste upper CIs here
)

ggplot(forest_data, aes(x = OR, y = model)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    x = "Odds Ratio (95% CI)",
    y = ""
  ) +
  theme_minimal()
