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


library(dplyr)
library(ggplot2)
# EDA comparison bw datasets
var_name <- "Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation."

combined_data <- bind_rows(
  data_baseline  %>% mutate(dataset = "baseline"),
  data_knn_k5    %>% mutate(dataset = "knn_k5"),
  data_knn_k10   %>% mutate(dataset = "knn_k10"),
  data_mean_mode %>% mutate(dataset = "mean_mode"),
  data_mice1     %>% mutate(dataset = "mice1"),
  data_mice2     %>% mutate(dataset = "mice2")
)

# votes
library(dplyr)
library(ggplot2)

var_vote <- "Q5.In.the.2016.Presidential.election..who.did.you.vote.for."

combined_data <- bind_rows(
  data_baseline  %>% mutate(dataset = "baseline"),
  data_knn_k5    %>% mutate(dataset = "knn_k5"),
  data_knn_k10   %>% mutate(dataset = "knn_k10"),
  data_mean_mode %>% mutate(dataset = "mean_mode"),
  data_mice1     %>% mutate(dataset = "mice1"),
  data_mice2     %>% mutate(dataset = "mice2")
)
ggplot(combined_data, aes(x = .data[[var_vote]])) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ dataset) +
  labs(
    title = "Voting Behavior Across Datasets",
    x = "Voting Choice",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# VAT
install.packages("factoextra")  # if not installed
library(factoextra)
library(dplyr)

get_numeric_scaled <- function(df) {
  df %>%
    select(
      WorkTimeInSeconds...9,
      multiplier,
      WorkTimeInSeconds...17,
      Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.,
      Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.
    ) %>%
    na.omit() %>%
    scale()
}

# =========================
# VAT-like plots for all datasets
# =========================
library(dplyr)
library(factoextra)
library(ggplot2)

# helper function: select numeric variables and scale
get_numeric_scaled <- function(df, wt1, wt2) {
  df %>%
    select(
      all_of(wt1),
      multiplier,
      amount,
      all_of(wt2),
      Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.,
      Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.
    ) %>%
    na.omit() %>%
    scale()
}

# baseline uses ...9 and ...17
baseline_scaled <- get_numeric_scaled(
  data_baseline,
  "WorkTimeInSeconds...9",
  "WorkTimeInSeconds...17"
)

# imputed datasets use ...1 and ...7
knn_k5_scaled <- get_numeric_scaled(
  data_knn_k5,
  "WorkTimeInSeconds...1",
  "WorkTimeInSeconds...7"
)

knn_k10_scaled <- get_numeric_scaled(
  data_knn_k10,
  "WorkTimeInSeconds...1",
  "WorkTimeInSeconds...7"
)

mean_mode_scaled <- get_numeric_scaled(
  data_mean_mode,
  "WorkTimeInSeconds...1",
  "WorkTimeInSeconds...7"
)

mice1_scaled <- get_numeric_scaled(
  data_mice1,
  "WorkTimeInSeconds...1",
  "WorkTimeInSeconds...7"
)

mice2_scaled <- get_numeric_scaled(
  data_mice2,
  "WorkTimeInSeconds...1",
  "WorkTimeInSeconds...7"
)

# =========================
# Plot each dataset
# =========================

fviz_dist(
  dist(baseline_scaled),
  gradient = list(low = "white", mid = "blue", high = "red"),
  show_labels = FALSE,
  order = TRUE
) +
  ggtitle("VAT-like Plot - Baseline")

fviz_dist(
  dist(knn_k5_scaled),
  gradient = list(low = "white", mid = "blue", high = "red"),
  show_labels = FALSE,
  order = TRUE
) +
  ggtitle("VAT-like Plot - KNN (k = 5)")

fviz_dist(
  dist(knn_k10_scaled),
  gradient = list(low = "white", mid = "blue", high = "red"),
  show_labels = FALSE,
  order = TRUE
) +
  ggtitle("VAT-like Plot - KNN (k = 10)")

fviz_dist(
  dist(mean_mode_scaled),
  gradient = list(low = "white", mid = "blue", high = "red"),
  show_labels = FALSE,
  order = TRUE
) +
  ggtitle("VAT-like Plot - Mean/Mode")

fviz_dist(
  dist(mice1_scaled),
  gradient = list(low = "white", mid = "blue", high = "red"),
  show_labels = FALSE,
  order = TRUE
) +
  ggtitle("VAT-like Plot - MICE 1")

fviz_dist(
  dist(mice2_scaled),
  gradient = list(low = "white", mid = "blue", high = "red"),
  show_labels = FALSE,
  order = TRUE
) +
  ggtitle("VAT-like Plot - MICE 2")
