# Generate V1 imputed datasets from the early cleaned dataset.
#
# Input:
#   Datasets/clean_data_V1.csv
#
# Outputs:
#   Datasets/V1/data_complete_baseline_V1.csv
#   Datasets/V1/mean_mode_imputed_dataset_V1.csv
#   Datasets/V1/knn_imputed_data_k5_V1.csv
#   Datasets/V1/knn_imputed_data_k10_V1.csv
#   Datasets/V1/mice_imputation_data1_V1.csv
#   Datasets/V1/mice_imputation_data2_V1.csv
#
# Notes:
# - This script reconstructs the missing first-stage imputation step.
# - It intentionally starts from clean_data_V1.csv, which still keeps
#   "Prefer not to state" as a category for age/income and
#   "Other / Prefer not to state" as a gender category.
# - DATASETS_Final.R is the next-stage script that cleans those special
#   categories and creates the V2 datasets.

input_path <- "Datasets/clean_data_V1.csv"
output_path <- "Datasets/V1"

dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

clean_data <- read.csv(input_path, check.names = FALSE)

get_mode <- function(x) {
  x_no_na <- x[!is.na(x)]
  names(sort(table(x_no_na), decreasing = TRUE))[1]
}

mean_mode_impute <- function(df) {
  out <- df

  for (col in names(out)) {
    missing <- is.na(out[[col]])

    if (!any(missing)) {
      next
    }

    if (is.numeric(out[[col]])) {
      out[[col]][missing] <- mean(out[[col]], na.rm = TRUE)
    } else {
      out[[col]][missing] <- get_mode(out[[col]])
    }
  }

  out
}

prepare_mixed_types <- function(df) {
  out <- df

  ordered_levels <- list(
    age = c(
      "18-29", "30-39", "40-49", "50-59", "60-69",
      "70 or over", "Prefer not to state"
    ),
    income = c(
      "Under $20,000",
      "$20,000 - $39,999",
      "$40,000 - $59,999",
      "$60,000 - $79,999",
      "$80,000 - $99,999",
      "Over $100,000",
      "Prefer not to state"
    ),
    `Q8 Aside from weddings and funerals, how often do you attend religious services?` = c(
      "Never",
      "Seldom",
      "A few times a year",
      "Once or twice a month",
      "Once a week",
      "More than once a week"
    )
  )

  for (col in names(ordered_levels)) {
    if (col %in% names(out)) {
      out[[col]] <- ordered(out[[col]], levels = ordered_levels[[col]])
    }
  }

  for (col in names(out)) {
    if (is.character(out[[col]])) {
      out[[col]] <- as.factor(out[[col]])
    }
  }

  out
}

rename_imputed_work_time_columns <- function(df) {
  out <- df

  old_names <- names(out)
  old_names[old_names == "WorkTimeInSeconds...9"] <- "WorkTimeInSeconds...1"
  old_names[old_names == "WorkTimeInSeconds...17"] <- "WorkTimeInSeconds...7"
  names(out) <- old_names

  out
}

write_dataset <- function(df, filename) {
  write.csv(
    df,
    file = file.path(output_path, filename),
    row.names = FALSE
  )
}

# 1. Complete-case baseline
baseline <- na.omit(clean_data)
write_dataset(baseline, "data_complete_baseline_V1.csv")

# 2. Mean/mode imputation
mean_mode_data <- mean_mode_impute(clean_data)
mean_mode_data <- rename_imputed_work_time_columns(mean_mode_data)
write_dataset(mean_mode_data, "mean_mode_imputed_dataset_V1.csv")

# 3. KNN imputation, k = 5 and k = 10
if (!requireNamespace("VIM", quietly = TRUE)) {
  stop("Package 'VIM' is required for KNN imputation. Install it with install.packages('VIM').")
}

knn_input <- prepare_mixed_types(clean_data)

knn_k5 <- VIM::kNN(knn_input, k = 5)
knn_k5 <- knn_k5[, !grepl("_imp$", names(knn_k5))]
knn_k5 <- rename_imputed_work_time_columns(knn_k5)
write_dataset(knn_k5, "knn_imputed_data_k5_V1.csv")

knn_k10 <- VIM::kNN(knn_input, k = 10)
knn_k10 <- knn_k10[, !grepl("_imp$", names(knn_k10))]
knn_k10 <- rename_imputed_work_time_columns(knn_k10)
write_dataset(knn_k10, "knn_imputed_data_k10_V1.csv")

# 4. MICE imputation
if (!requireNamespace("mice", quietly = TRUE)) {
  stop("Package 'mice' is required for MICE imputation. Install it with install.packages('mice').")
}

mice_input <- prepare_mixed_types(clean_data)
meth <- mice::make.method(mice_input)

for (col in names(mice_input)) {
  if (!any(is.na(mice_input[[col]]))) {
    meth[col] <- ""
  } else if (is.numeric(mice_input[[col]])) {
    meth[col] <- "pmm"
  } else if (is.ordered(mice_input[[col]])) {
    meth[col] <- "polr"
  } else {
    n_levels <- length(levels(mice_input[[col]]))
    meth[col] <- ifelse(n_levels == 2, "logreg", "polyreg")
  }
}

set.seed(123)
mice_fit <- mice::mice(
  mice_input,
  method = meth,
  m = 5,
  maxit = 10,
  seed = 123,
  printFlag = FALSE
)

mice_data1 <- mice::complete(mice_fit, 1)
mice_data1 <- rename_imputed_work_time_columns(mice_data1)
write_dataset(mice_data1, "mice_imputation_data1_V1.csv")

mice_data2 <- mice::complete(mice_fit, 2)
mice_data2 <- rename_imputed_work_time_columns(mice_data2)
write_dataset(mice_data2, "mice_imputation_data2_V1.csv")

cat("Generated V1 datasets in:", output_path, "\n")
