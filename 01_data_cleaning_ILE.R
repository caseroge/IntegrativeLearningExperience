#ILE Data Cleaning
pacman::p_load(tidyverse, readr, readxl, lubridate, arsenal, openxlsx)
moldova_raw <- read_xlsx("Moldova_data_for_ILE.xlsx") #read in file
dim(moldova_raw) #check dimension
#select only desired vars 
moldova_restrict_raw <- moldova_raw %>% filter(P24 == 1)
dim(moldova_restrict_raw)
dim(moldova_raw)[1] - dim(moldova_restrict_raw)[1] #deleted 3721 observations 

moldova1 <- moldova_restrict_raw %>% dplyr::select(
  ID = a.ID,
  detention = P11,
  iso = P21_H,
  rif = P21_R,
  bday = birth_day,
  diagnosis_day = P14,
  sex = Gender_P6,
  urbanrural = 'urban-rural_P5_c',
  house = homless_P5_d,
  job = ocupation_P9,
  edu = P9_b,
  hiv = aP28
)

#clean vars and create new vars off old cars
#look at vars before
sapply(moldova1[-c(1, 5, 6)], function(x) {table(x, useNA="always")})
class(moldova1$bday)
class(moldova1$diagnosis_day)

#mutate all the vars
moldova2 <- moldova1 %>% mutate(
  ever_deten = case_when( #detention status
  detention == 1 ~ 1,
  detention == 2 ~ 0,
  TRUE ~ NA_real_
  ),
  ever_deten.f = factor(ever_deten, levels = c(0,1), labels = c("Never In Dentention", "In Detention at Any Time")), #factor deten
  resistant = case_when(  #create resistance var
    iso == TRUE & rif == TRUE ~ 1,
    TRUE ~ 0
  ),
  resistant.f = factor(resistant, levels = c(0,1), labels = c("Not Resistant", "Resistant")), #factor resist 
  sexM = case_when( #sex male
    sex == "M" ~ 1,
    sex == "F" ~ 0,
    TRUE ~ NA_real_
  ),
  sexM.f = factor(sexM, levels = c(0,1), labels = c("Female", "Male")), #factor Sex
  rural = case_when( #urban rural
    urbanrural == "rural" ~ 1,
    urbanrural == "urban" ~ 0,
    TRUE ~ NA_real_
  ),
  rural.f = factor(rural, levels = c(0,1), labels = c("Urban", "Rural")), #factor rural
  homeless = case_when( #homeless
    house == "Y" ~ 0,
    house == "N" ~ 1,
    TRUE ~ NA_real_
  ),
  homeless.f = factor(homeless, levels = c(0,1), labels = c("Not Homeless", "Homelesss")), #factor homelessness
  jobcat = case_when( #occupation status
    job == "worker" ~ 1,
    job == "disabled" ~ 2,
    job == "pensioner" ~ 3,
    job == "student" ~ 4,
    job == "unemployed" ~ 5,
    TRUE ~ NA_real_
  ),
  jobcat.f = factor(jobcat, levels=c(1,2,3,4,5),  #factor it 
                    labels=c("Worker", "Disabled", "Pensioner", "Student", "Unemployed")),
  edu.f = factor(edu, levels=c(5,1,2,3,4), #factor education status
                    labels=c("None", "Primary", "Secondary", "Specialized_secondary", "Higher")),
  hiv = case_when( #create HIV var
    hiv == 2 ~ 1,
    TRUE ~ 0
  ),
  hiv.f = factor(hiv, levels = c(0,1), labels = c("No HIV Co-Infection", "HIV Co-Infection")), #factor HIV
  bday = base::as.Date(bday), #clean bday date
  diagnosis_day = base::as.Date(diagnosis_day), #clean diagnosis day date
  age_diag = floor(time_length(interval(bday, diagnosis_day), "years")) #age using lubridate package
)

moldova3 <- moldova2 %>% dplyr::select(-c("ID", "detention", "iso", "rif", "bday", #remove old vars 
                                   "diagnosis_day", "sex", "urbanrural", 
                                   "house", "job", "jobcat", "edu", "hiv", "ever_deten", 
                                   "resistant", "sexM", "rural", "homeless"))

moldova <- moldova3
sapply(moldova, function(x) {table(x, useNA="always")}) #check new vars for errors 


