library(tidyverse)

clean_data <- read_csv("clean_data.csv")
data_complete_baseline <- read_csv("data_complete_baseline.csv")
knn_imputed_k5 <- read_csv("knn_imputed_data_k5.csv")
knn_imputed_k10 <- read_csv("knn_imputed_data_k10.csv")
mean_mode_imputed <- read_csv("mean_mode_imputed_dataset.csv")
mice_data1 <- read_csv("mice_imputation_data1.csv")
mice_data2 <- read_csv("mice_imputation_data2.csv")
raw_data <- read_csv("Raw Data.csv")


colnames(clean_data)
library(tidyverse)

library(tidyverse)

library(tidyverse)

data_list <- list(
  clean = clean_data,
  baseline = data_complete_baseline,
  knn5 = knn_imputed_k5,
  knn10 = knn_imputed_k10,
  mean_mode = mean_mode_imputed,
  mice1 = mice_data1,
  mice2 = mice_data2
)

num_vars <- c(1, 2, 3, 7, 8, 9)
cat_vars <- setdiff(seq_along(clean_data), num_vars)

for (i in num_vars) {
  
  var_name <- names(clean_data)[i]
  
  data_list %>%
    imap(~ {
      .x %>%
        ggplot(aes(x = .data[[var_name]])) +
        geom_histogram(bins = 30, fill = "steelblue") +
        labs(title = paste("Dataset:", .y, "| Variable:", var_name),
             x = var_name,
             y = "Count") +
        theme_minimal()
    }) %>%
    walk(print)
}

for (i in cat_vars) {
  
  var_name <- names(clean_data)[i]
  
  data_list %>%
    imap(~ {
      .x %>%
        ggplot(aes(x = as.factor(.data[[var_name]]))) +
        geom_bar(fill = "darkorange") +
        labs(title = paste("Dataset:", .y, "| Variable:", var_name),
             x = var_name,
             y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }) %>%
    walk(print)
}

library(tidyverse)

# clean
clean_num <- clean_data |>
  dplyr::select(where(is.numeric))

clean_corr <- clean_num |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  rownames_to_column("Var1") |>
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  )

ggplot(clean_corr, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(title = "Correlation Heatmap - Clean", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_equal()
# baseline
baseline_num <- data_complete_baseline |>
  dplyr::select(where(is.numeric))

baseline_corr <- baseline_num |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  rownames_to_column("Var1") |>
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  )

ggplot(baseline_corr, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(title = "Correlation Heatmap - Baseline", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_equal()
# knn5
knn5_num <- knn_imputed_k5 |>
  dplyr::select(where(is.numeric))

knn5_corr <- knn5_num |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  rownames_to_column("Var1") |>
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  )

ggplot(knn5_corr, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(title = "Correlation Heatmap - KNN k = 5", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_equal()

# knn10
knn10_num <- knn_imputed_k10 |>
  dplyr::select(where(is.numeric))

knn10_corr <- knn10_num |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  rownames_to_column("Var1") |>
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  )

ggplot(knn10_corr, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(title = "Correlation Heatmap - KNN k = 10", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_equal()

# mean_mode
mean_mode_num <- mean_mode_imputed |>
  dplyr::select(where(is.numeric))

mean_mode_corr <- mean_mode_num |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  rownames_to_column("Var1") |>
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  )

ggplot(mean_mode_corr, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(title = "Correlation Heatmap - Mean/Mode", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_equal()

# mice1
mice1_num <- mice_data1 |>
  dplyr::select(where(is.numeric))

mice1_corr <- mice1_num |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  rownames_to_column("Var1") |>
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  )

ggplot(mice1_corr, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(title = "Correlation Heatmap - MICE 1", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_equal()
# mice2
mice2_num <- mice_data2 |>
  dplyr::select(where(is.numeric))

mice2_corr <- mice2_num |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  rownames_to_column("Var1") |>
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  )

ggplot(mice2_corr, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(title = "Correlation Heatmap - MICE 2", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_equal()

