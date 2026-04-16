#Missing Data Analysis 
library(mice)
library(naniar)
na_vars <- names(moldova)[-c(9,10)]
na_vars
mold_subset <- moldova[,na_vars]
par(mfrow = c(1,1))
md.pattern(mold_subset)

gg_miss_upset(mold_subset)
1