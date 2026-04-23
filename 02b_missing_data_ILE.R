#Missing Data Analysis 
source("01_data_cleaning_ILE.R")
library(mice)
library(naniar)
na_vars <- names(moldova)[-c(9,10)]
na_vars
mold_subset <- moldova[,na_vars]
par(mfrow = c(1,1))
md.pattern(mold_subset)

gg_miss_upset(mold_subset)


# 1. How much missingness per variable?
miss_summary <- moldova %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  arrange(desc(pct_missing))
print(miss_summary)

# 2. Is missingness in covariates associated with the outcome?
# (tests if missing data is random or systematic)
moldova %>%
  mutate(across(c(sexM.f, rural.f, homeless.f, jobcat.f, edu.f, hiv.f),
                ~ is.na(.), .names = "miss_{.col}")) %>%
  summarise(across(starts_with("miss_"),
                   ~ chisq.test(., moldova$resistant.f)$p.value))

# 3. Compare detained vs non-detained among complete vs incomplete cases
moldova$complete <- complete.cases(moldova[, c("ever_deten.f", "sexM.f", 
                                               "rural.f", "homeless.f",
                                               "jobcat.f", "edu.f", 
                                               "age.cat.f", "hiv.f",
                                               "resistant.f")])
table(moldova$complete, moldova$ever_deten.f)



