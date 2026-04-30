base_path <- "Datasets/V1"

baseline <- read.csv(file.path(base_path, "data_complete_baseline_V1.csv"))
knn_k5   <- read.csv(file.path(base_path, "knn_imputed_data_k5_V1.csv"))
knn_k10  <- read.csv(file.path(base_path, "knn_imputed_data_k10_V1.csv"))
meanmode <- read.csv(file.path(base_path, "mean_mode_imputed_dataset_V1.csv"))
mice1    <- read.csv(file.path(base_path, "mice_imputation_data1_V1.csv"))
mice2    <- read.csv(file.path(base_path, "mice_imputation_data2_V1.csv"))

# find mode for age, only one person said "Perfer not to state"
names(sort(table(baseline$age), decreasing = TRUE))[1]

# replace "Prefer not to state" with mode, only one such observation
# function to replace "Prefer not to state" with mode
replace_with_mode <- function(df) {
  mode_age <- names(sort(table(df$age), decreasing = TRUE))[1]
  
  df$age <- as.character(df$age)  # avoid factor issues
  df$age[df$age == "Prefer not to state"] <- mode_age
  df$age <- as.factor(df$age)
  
  return(df)
}

# apply to all datasets
baseline <- replace_with_mode(baseline)
knn_k5   <- replace_with_mode(knn_k5)
knn_k10  <- replace_with_mode(knn_k10)
meanmode <- replace_with_mode(meanmode)
mice1    <- replace_with_mode(mice1)
mice2    <- replace_with_mode(mice2)

# change gender with others/perfer not to state to "Others"
clean_gender <- function(df) {
  df$gender <- as.character(df$gender)  # avoid factor issues
  df$gender[df$gender == "Other / Prefer not to state"] <- "Other"
  df$gender <- as.factor(df$gender)
  return(df)
}
baseline <- clean_gender(baseline)
knn_k5   <- clean_gender(knn_k5)
knn_k10  <- clean_gender(knn_k10)
meanmode <- clean_gender(meanmode)
mice1    <- clean_gender(mice1)
mice2    <- clean_gender(mice2)

# change prefer not to state in income to NA
baseline$income[baseline$income == "Prefer not to state"] <- NA
knn_k5$income[knn_k5$income == "Prefer not to state"] <- NA
knn_k10$income[knn_k10$income == "Prefer not to state"] <- NA
meanmode$income[meanmode$income == "Prefer not to state"] <- NA
mice1$income[ mice1$income == "Prefer not to state"] <- NA
mice2$income[ mice2$income == "Prefer not to state"] <- NA

# check NAs
datasets <- list(
  baseline = baseline,
  knn_k5   = knn_k5,
  knn_k10  = knn_k10,
  meanmode = meanmode,
  mice1    = mice1,
  mice2    = mice2
)

na_summary <- lapply(datasets, function(df) {
  na_counts <- colSums(is.na(df))
  na_counts[na_counts > 0]
})

na_summary
# all of them only have NAs in income(11 rows) now

#mode imputation for inome
# 1. Mode function
get_mode <- function(x) {
  ux <- na.omit(x)
  names(sort(table(ux), decreasing = TRUE))[1]
}

# 2. Copy dataset (so you don’t overwrite original)
meanmode_v2 <- meanmode

# 3. Impute income with mode
mode_income <- get_mode(meanmode_v2$income)
meanmode_v2$income[is.na(meanmode_v2$income)] <- mode_income

# 4. Save to Datasets/V2
write.csv(
  meanmode_v2,
  file = "Datasets/V2/mean_mode_imputed_dataset.csv",
  row.names = FALSE
)
unique(meanmode_v2$income)

# knn for k=5 and k=10
library(VIM)

# convert to character first
knn_k5$income <- as.character(knn_k5$income)

# set correct ordinal order using the actual labels in your data
knn_k5$income <- ordered(knn_k5$income, levels = c(
  "Under $20,000",
  "$20,000 - $39,999",
  "$40,000 - $59,999",
  "$60,000 - $79,999",
  "$80,000 - $99,999",
  "Over $100,000"
))

# do KNN imputation for income
knn_k5_imputed <- kNN(knn_k5, variable = "income", k = 5)

# remove indicator column added by VIM
knn_k5_imputed$income_imp <- NULL

# save output
write.csv(
  knn_k5_imputed,
  "Datasets/V2/knn_imputed_data_k5.csv",
  row.names = FALSE
)

# check result
unique(knn_k5_imputed$income)
table(knn_k5_imputed$income, useNA = "ifany")

# save output
write.csv(
  knn_k5_imputed,
  "Datasets/V2/knn_imputed_data_k5.csv",
  row.names = FALSE
)


# k=10
# convert to character first
knn_k10$income <- as.character(knn_k10$income)

# set correct ordinal order using the actual labels in your data
knn_k10$income <- ordered(knn_k10$income, levels = c(
  "Under $20,000",
  "$20,000 - $39,999",
  "$40,000 - $59,999",
  "$60,000 - $79,999",
  "$80,000 - $99,999",
  "Over $100,000"
))

# do KNN imputation for income (k = 10)
knn_k10_imputed <- kNN(knn_k10, variable = "income", k = 10)

# remove indicator column added by VIM
knn_k10_imputed$income_imp <- NULL

# save output
write.csv(
  knn_k10_imputed,
  "Datasets/V2/knn_imputed_data_k10.csv",
  row.names = FALSE
)

# check result
unique(knn_k10_imputed$income)
table(knn_k10_imputed$income, useNA = "ifany")

# MICE
library(mice)

# convert income to character first
mice1$income <- as.character(mice1$income)

# set income as an ordered factor
mice1$income <- ordered(mice1$income, levels = c(
  "Under $20,000",
  "$20,000 - $39,999",
  "$40,000 - $59,999",
  "$60,000 - $79,999",
  "$80,000 - $99,999",
  "Over $100,000"
))

# set imputation methods
meth <- make.method(mice1)
meth["income"] <- "polr"   # ordinal logistic regression for ordered categorical
meth[names(meth) != "income"] <- ""   # only impute income

# run MICE to generate 5 imputed datasets
imp_income <- mice(mice1, method = meth, m = 5, seed = 123)

# randomly choose one of the 5 completed datasets
set.seed(123)
chosen_index <- sample(1:5, 1)
chosen_index

mice1_income_imputed <- complete(imp_income, chosen_index)

# save it
write.csv(
  mice1_income_imputed,
  "Datasets/V2/mice_imputation_data1.csv",
  row.names = FALSE
)

# check result
unique(mice1_income_imputed$income)
table(mice1_income_imputed$income, useNA = "ifany")


# convert income to character first
mice2$income <- as.character(mice2$income)

# set income as an ordered factor
mice2$income <- ordered(mice2$income, levels = c(
  "Under $20,000",
  "$20,000 - $39,999",
  "$40,000 - $59,999",
  "$60,000 - $79,999",
  "$80,000 - $99,999",
  "Over $100,000"
))

# set imputation methods
meth <- make.method(mice2)
meth["income"] <- "polr"          # ordinal logistic regression
meth[names(meth) != "income"] <- ""  # only impute income

# run MICE (5 datasets)
imp_income_v2 <- mice(mice2, method = meth, m = 5, seed = 123)

# randomly choose one
set.seed(123)
chosen_index <- sample(1:5, 1)

mice1_v2_income_imputed <- complete(imp_income_v2, chosen_index)

# save chosen dataset
write.csv(
  mice1_v2_income_imputed,
  "Datasets/V2/mice_imputation_data2.csv",
  row.names = FALSE
)

# (optional) save all 5 datasets
for (i in 1:5) {
  temp <- complete(imp_income_v2, i)
  write.csv(
    temp,
    paste0("Datasets/V2/mice_imputation_data1_V2_", i, ".csv"),
    row.names = FALSE
  )
}

# check result
unique(mice1_v2_income_imputed$income)
table(mice1_v2_income_imputed$income, useNA = "ifany")


