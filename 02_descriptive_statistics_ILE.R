#descriptive statistics
source("01_data_cleaning_ILE.R")
pacman::p_load(tidyverse, readr, readxl, lubridate, arsenal, openxlsx)

range(moldova1$diagnosis_day) #01/01/2008 - 12-31-2009
table(moldova1$iso, moldova1$rif, useNA = "always") #no NA values for these
table(moldova$resistant.f, moldova$ever_deten.f, useNA = "always") #174 NA values from detention status 

#creating table 1
table1_cohort <- tableby(
  ever_deten.f~ + sexM.f + age_diag + rural.f + homeless.f + jobcat.f + edu.f + hiv.f,
  data = moldova,
  control = tableby.control(
    test = TRUE,                 
    total = TRUE,
    numeric.stats = c("meansd", "median", "range"),
    cat.stats = c("countpct"),
    stats.labels = list(
      meansd = "Mean (SD)",
      median = "Median",
      range = "Range",
      countpct = "N (%)" ) ) )

tab1 <- summary(table1_cohort,
        title = "Table 1.",
        digits = 2,
        labelTranslations = c(
          ever_deten.f = "Ever Been In Detention",
          sexM.f= "Sex",
          age_diag= "Age at Diagnosis",
          rural.f="Urban or Rural Dwelling",
          homeless.f="Homelessness Status",
          jobcat.f="Occupational Status",
          edu.f="Educational Status",
          hiv.f="HIV Co-Infection Status"
          ))
tab1
write2word(table1_cohort, "ILE_table1_test.docx",
        title = "Table 1.",
        digits = 2,
        labelTranslations = c(
          ever_deten.f = "Ever Been In Detention",
          sexM.f= "Sex",
          age_diag= "Age at Diagnosis",
          rural.f="Urban or Rural Dwelling",
          homeless.f="Homelessness Status",
          jobcat.f="Occupational Status",
          edu.f="Educational Status",
          hiv.f="HIV Co-Infection Status"
        ))

#First convert the arsenal table to a dataframe
table1_df <- as.data.frame(tab1)

#Then write to Excel
write.xlsx(table1_df, "table1_test.xlsx")



