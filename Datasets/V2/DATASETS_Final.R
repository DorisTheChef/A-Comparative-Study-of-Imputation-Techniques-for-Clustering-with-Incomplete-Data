base_path <- "Datasets/V1"

baseline <- read.csv(file.path(base_path, "data_complete_baseline_V1.csv"))
knn_k5   <- read.csv(file.path(base_path, "knn_imputed_data_k5_V1.csv"))
knn_k10  <- read.csv(file.path(base_path, "knn_imputed_data_k10_V1.csv"))
meanmode <- read.csv(file.path(base_path, "mean_mode_imputed_dataset_V1.csv"))
mice1    <- read.csv(file.path(base_path, "mice_imputation_data1_V1.csv"))
mice2    <- read.csv(file.path(base_path, "mice_imputation_data2_V1.csv"))

# find mode
# Get mode
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

#
baseline$income[baseline$income == "Prefer not to state"] <- NA
knn_k5$income[knn_k5$income == "Prefer not to state"] <- NA
knn_k10$income[knn_k10$income == "Prefer not to state"] <- NA
meanmode$income[meanmode$income == "Prefer not to state"] <- NA
mice1$income[ mice1$income == "Prefer not to state"] <- NA
mice2$income[ mice2$income == "Prefer not to state"] <- NA

