data_baseline <- read.csv("data_complete_baseline.csv")
colSums(is.na(data_baseline))
unique(data_baseline$income)
table(data_baseline$income)
data_baseline$income[is.na(data_baseline$income)] <- "$40,000 - $59,999"


library(dplyr)
library(cluster)

# =========================
# 1. Select numeric variables only
# =========================
data_numeric <- data_baseline %>%
  select(
    WorkTimeInSeconds...9,
    multiplier,
    amount,
    WorkTimeInSeconds...17,
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.,
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.
  ) 

# =========================
# 2. Standardize numeric variables
# =========================
data_scaled <- scale(data_numeric)

# =========================
# 3. Elbow method
# =========================
set.seed(123)

k_values <- 1:10

wss <- sapply(k_values, function(k) {
  kmeans(data_scaled, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- data.frame(
  k = k_values,
  wss = wss
)

print(elbow_df)
# k=2 or k=3

par(mar = c(5, 4, 4, 2) + 0.1)

plot(
  k_values, wss, type = "b",
  xlab = "Number of Clusters (k)",
  ylab = "Total Within-Cluster Sum of Squares",
  main = "Elbow Method for K-means"
)

# =========================
# 4. Silhouette method
# =========================
k_sil <- 2:10

sil_scores <- sapply(k_sil, function(k) {
  km_model <- kmeans(data_scaled, centers = k, nstart = 25)
  sil <- silhouette(km_model$cluster, dist(data_scaled))
  mean(sil[, 3])
})

sil_df <- data.frame(
  k = k_sil,
  silhouette = sil_scores
)

print(sil_df)
# Statistically (pure silhouette): k = 5

# Practically (much better choice): k = 2 or 3


plot(
  k_sil, sil_scores, type = "b",
  xlab = "Number of Clusters (k)",
  ylab = "Average Silhouette Width",
  main = "Silhouette Method for K-means"
)

# =========================
# 5. Fit final k-means model
# =========================
set.seed(123)
# k=2 or k=3
kmeans_model <- kmeans(data_scaled, centers = 2, nstart = 25)

print(kmeans_model)
print(kmeans_model$size)
print(kmeans_model$centers)
# religious show significant differnt
print(kmeans_model$cluster)

# =========================
# 6. Add cluster labels back to the numeric data
# =========================
data_numeric_clustered <- data_numeric %>%
  mutate(cluster = factor(kmeans_model$cluster))

head(data_numeric_clustered)

# numerical only for hiarachical average
hc <- hclust(dist(data_scaled), method = "average")

plot(hc)
rect.hclust(hc, k = 2, border = "red")

library(cluster)
library(mclust)

# =========================
# 1. Prepare numeric data (same as k-means)
# =========================
data_numeric <- data_baseline %>%
  select(
    WorkTimeInSeconds...9,
    multiplier,
    amount,
    WorkTimeInSeconds...17,
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.,
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.
  ) %>%
  na.omit()

data_scaled <- scale(data_numeric)

# =========================
# 2. k-means (k = 2)
# =========================
set.seed(123)
km_model <- kmeans(data_scaled, centers = 2, nstart = 25)
km_cluster <- km_model$cluster

# =========================
# 3. Hierarchical (average linkage)
# =========================
hc <- hclust(dist(data_scaled), method = "average")
hc_cluster <- cutree(hc, k = 2)

# =========================
# 4. ARI comparison
# =========================
ari_value <- adjustedRandIndex(km_cluster, hc_cluster)

ari_value

library(dplyr)
library(cluster)
library(clustMixType)
library(mclust)

# =========================
# 1. State -> region mapping
# =========================
state_to_region <- c(
  "CT"="Northeast", "ME"="Northeast", "MA"="Northeast", "NH"="Northeast",
  "RI"="Northeast", "VT"="Northeast", "NJ"="Northeast", "NY"="Northeast", "PA"="Northeast",
  
  "IL"="Midwest", "IN"="Midwest", "IA"="Midwest", "KS"="Midwest", "MI"="Midwest",
  "MN"="Midwest", "MO"="Midwest", "NE"="Midwest", "ND"="Midwest", "OH"="Midwest",
  "SD"="Midwest", "WI"="Midwest",
  
  "AL"="South", "AR"="South", "DE"="South", "DC"="South", "FL"="South", "GA"="South",
  "KY"="South", "LA"="South", "MD"="South", "MS"="South", "NC"="South", "OK"="South",
  "SC"="South", "TN"="South", "TX"="South", "VA"="South", "WV"="South",
  
  "AK"="West", "AZ"="West", "CA"="West", "CO"="West", "HI"="West", "ID"="West",
  "MT"="West", "NV"="West", "NM"="West", "OR"="West", "UT"="West", "WA"="West", "WY"="West",
  
  "0"="West"
)

# =========================
# 2. Process baseline data
# =========================
data_baseline_kproto <- data_baseline %>%
  mutate(
    # clean state first
    Q9.What.State.do.you.live.in. =
      trimws(toupper(Q9.What.State.do.you.live.in.)),
    
    # map state to region
    Q9.What.State.do.you.live.in. =
      state_to_region[Q9.What.State.do.you.live.in.],
    
    # numeric variables
    WorkTimeInSeconds...9  = as.numeric(WorkTimeInSeconds...9),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...17 = as.numeric(WorkTimeInSeconds...17),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    
    # ordered factors
    age = factor(
      age,
      levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over"),
      ordered = TRUE
    ),
    
    income = factor(
      income,
      levels = c(
        "Under $20,000",
        "$20,000 - $39,999",
        "$40,000 - $59,999",
        "$60,000 - $79,999",
        "$80,000 - $99,999",
        "Over $100,000"
      ),
      ordered = TRUE
    ),
    
    Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =
      factor(
        Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.,
        levels = c(
          "Never",
          "Seldom",
          "A few times a year",
          "Once or twice a month",
          "Once a week",
          "More than once a week"
        ),
        ordered = TRUE
      ),
    
    # nominal factors
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. =
      as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. =
      as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

# check NA counts
colSums(is.na(data_baseline_kproto))

# remove rows with NA
data_baseline_kproto_complete <- na.omit(data_baseline_kproto)

# =========================
# 3. k-prototypes (k = 2)
# =========================
set.seed(123)
kproto_model <- kproto(data_baseline_kproto_complete, k = 2)
kproto_cluster <- kproto_model$cluster

# =========================
# 4. Hierarchical clustering
#    average linkage + Gower distance
# =========================
diss <- daisy(data_baseline_kproto_complete, metric = "gower")

hc_model <- hclust(diss, method = "average")
hc_cluster <- cutree(hc_model, k = 2)

# =========================
# 5. Compare ARI
# =========================
ari_value <- adjustedRandIndex(kproto_cluster, hc_cluster)

ari_value
library(cluster)
library(clustMixType)
library(mclust)

# =========================
# 1. Use your mixed dataset
# =========================
df <- data_baseline_kproto_complete   # already processed (numeric + factors)

# =========================
# 2. k-prototypes (k = 2)
# =========================
set.seed(123)
kproto_model <- kproto(df, k = 2)
kproto_cluster <- kproto_model$cluster

# =========================
# 3. Hierarchical (Gower + average)
# =========================
diss <- daisy(df, metric = "gower")

hc <- hclust(diss, method = "average")
hc_cluster <- cutree(hc, k = 2)

# =========================
# 4. ARI comparison
# =========================
ari_value <- adjustedRandIndex(kproto_cluster, hc_cluster)

ari_value

library(mclust)

# use your scaled numeric data
gmm_model <- Mclust(data_scaled)

summary(gmm_model)

# best k automatically selected
gmm_model$G

# cluster assignment
gmm_cluster <- gmm_model$classification
plot(gmm_model, what = "BIC")

library(mclust)

# =========================
# 1. Use your scaled numeric data
# =========================
# (same data you used for k-means earlier)
data_scaled <- data_scaled

# =========================
# 2. k-means (k = 2 and 3)
# =========================
set.seed(123)
km_2 <- kmeans(data_scaled, centers = 2, nstart = 25)
km_3 <- kmeans(data_scaled, centers = 3, nstart = 25)

# =========================
# 3. GMM (force k = 2 and 3)
# =========================
gmm_2 <- Mclust(data_scaled, G = 2)
gmm_3 <- Mclust(data_scaled, G = 3)

# cluster assignments
gmm_cluster_2 <- gmm_2$classification
gmm_cluster_3 <- gmm_3$classification

# =========================
# 4. ARI comparison
# =========================
ari_k2 <- adjustedRandIndex(km_2$cluster, gmm_cluster_2)
ari_k3 <- adjustedRandIndex(km_3$cluster, gmm_cluster_3)

# results
ari_results <- data.frame(
  k = c(2, 3),
  ARI = c(ari_k2, ari_k3)
)

ari_results

install.packages("mixsmsn")
library(mixsmsn)

# skew-t mixture
model <- smsn.mix(data_scaled, g = 2, family = "Skew.t")

clusters <- model$group


#baseline ARI
# =========================
# 0. Packages
# =========================
library(dplyr)
library(cluster)
library(clustMixType)
library(mclust)
library(fclust)
library(FPDclustering)

set.seed(123)

# =========================
# 1. State -> region mapping
# =========================
state_to_region <- c(
  "CT"="Northeast", "ME"="Northeast", "MA"="Northeast", "NH"="Northeast",
  "RI"="Northeast", "VT"="Northeast", "NJ"="Northeast", "NY"="Northeast", "PA"="Northeast",
  
  "IL"="Midwest", "IN"="Midwest", "IA"="Midwest", "KS"="Midwest", "MI"="Midwest",
  "MN"="Midwest", "MO"="Midwest", "NE"="Midwest", "ND"="Midwest", "OH"="Midwest",
  "SD"="Midwest", "WI"="Midwest",
  
  "AL"="South", "AR"="South", "DE"="South", "DC"="South", "FL"="South", "GA"="South",
  "KY"="South", "LA"="South", "MD"="South", "MS"="South", "NC"="South", "OK"="South",
  "SC"="South", "TN"="South", "TX"="South", "VA"="South", "WV"="South",
  
  "AK"="West", "AZ"="West", "CA"="West", "CO"="West", "HI"="West", "ID"="West",
  "MT"="West", "NV"="West", "NM"="West", "OR"="West", "UT"="West", "WA"="West", "WY"="West",
  
  "0"="West"
)

# =========================
# 2. Process baseline for k-prototypes
#    and keep the same rows for all methods
# =========================
data_baseline_processed <- data_baseline %>%
  mutate(
    # clean and map state
    Q9.What.State.do.you.live.in. =
      trimws(toupper(Q9.What.State.do.you.live.in.)),
    Q9.What.State.do.you.live.in. =
      state_to_region[Q9.What.State.do.you.live.in.],
    
    # numeric variables
    WorkTimeInSeconds...9  = as.numeric(WorkTimeInSeconds...9),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...17 = as.numeric(WorkTimeInSeconds...17),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    
    # ordered factors
    age = factor(
      age,
      levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over"),
      ordered = TRUE
    ),
    income = factor(
      income,
      levels = c(
        "Under $20,000",
        "$20,000 - $39,999",
        "$40,000 - $59,999",
        "$60,000 - $79,999",
        "$80,000 - $99,999",
        "Over $100,000"
      ),
      ordered = TRUE
    ),
    Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =
      factor(
        Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.,
        levels = c(
          "Never",
          "Seldom",
          "A few times a year",
          "Once or twice a month",
          "Once a week",
          "More than once a week"
        ),
        ordered = TRUE
      ),
    
    # nominal factors
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. =
      as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. =
      as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

# Keep only rows that are complete across all variables used by k-prototypes
rows_keep <- complete.cases(data_baseline_processed)

df_mixed <- data_baseline_processed[rows_keep, ]

# =========================
# 3. Numeric-only data for numeric methods
#    using the same rows as df_mixed
# =========================
df_numeric <- df_mixed %>%
  select(
    WorkTimeInSeconds...9,
    multiplier,
    amount,
    WorkTimeInSeconds...17,
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.,
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.
  )

df_numeric_scaled <- scale(df_numeric)

# =========================
# 4. Fit all methods at k = 2
# =========================

# 4.1 K-means
km_model <- kmeans(df_numeric_scaled, centers = 2, nstart = 25)
cl_kmeans <- km_model$cluster

# 4.2 Fuzzy k-means
# stand = 0 because data already scaled
fkm_model <- FKM(df_numeric_scaled, k = 2, m = 2, stand = 0)
cl_fuzzy <- apply(fkm_model$U, 1, which.max)

# 4.3 PD clustering
pd_model <- PDC(as.matrix(df_numeric_scaled), k = 2)
cl_pd <- pd_model$label

# 4.4 Hierarchical average linkage
hc_model <- hclust(dist(df_numeric_scaled), method = "average")
cl_hc <- cutree(hc_model, k = 2)

# 4.5 GMM
gmm_model <- Mclust(df_numeric_scaled, G = 2)
cl_gmm <- gmm_model$classification

# 4.6 k-prototypes
kp_model <- kproto(df_mixed, k = 2)
cl_kproto <- kp_model$cluster

# =========================
# 5. Collect cluster labels
# =========================
cluster_list <- list(
  kmeans      = cl_kmeans,
  fuzzy_kmeans= cl_fuzzy,
  pd_cluster  = cl_pd,
  hclust_avg  = cl_hc,
  gmm         = cl_gmm,
  kprototype  = cl_kproto
)

# Quick check: all must have same length
sapply(cluster_list, length)

# =========================
# 6. Pairwise ARI matrix
# =========================
method_names <- names(cluster_list)

ari_matrix <- matrix(
  NA,
  nrow = length(method_names),
  ncol = length(method_names),
  dimnames = list(method_names, method_names)
)

for (i in seq_along(method_names)) {
  for (j in seq_along(method_names)) {
    ari_matrix[i, j] <- adjustedRandIndex(
      cluster_list[[i]],
      cluster_list[[j]]
    )
  }
}

round(ari_matrix, 4)

# =========================
# 7. cluster sizes
# =========================
cluster_sizes <- list(
  kmeans       = table(cl_kmeans),
  fuzzy_kmeans = table(cl_fuzzy),
  pd_cluster   = table(cl_pd),
  hclust_avg   = table(cl_hc),
  gmm          = table(cl_gmm),
  kprototype   = table(cl_kproto)
)

cluster_sizes

# =========================
# 8. Baseline cluster profile comparison plot
#    Cluster labels are oriented so Cluster 2 has the higher
#    average religious orientation within each method.
# =========================
library(ggplot2)
library(tidyr)

profile_df <- df_mixed %>%
  mutate(
    Age = as.numeric(age),
    Amt = as.numeric(amount),
    Attend = as.numeric(Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.),
    Pol = as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Rel = as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    Vote = as.numeric(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.)
  ) %>%
  select(Age, Amt, Attend, Pol, Rel, Vote)

profile_variables <- c("Age", "Amt", "Attend", "Pol", "Rel", "Vote")

profile_long <- lapply(names(cluster_list), function(method_name) {
  tmp <- profile_df
  tmp$raw_cluster <- cluster_list[[method_name]]

  religion_means <- tapply(tmp$Rel, tmp$raw_cluster, mean, na.rm = TRUE)
  high_religion_cluster <- names(which.max(religion_means))

  tmp %>%
    mutate(
      Method = method_name,
      Cluster = ifelse(
        as.character(raw_cluster) == high_religion_cluster,
        "Cluster 2",
        "Cluster 1"
      )
    ) %>%
    select(Method, Cluster, all_of(profile_variables))
}) %>%
  bind_rows()

profile_average <- profile_long %>%
  pivot_longer(
    cols = all_of(profile_variables),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(Method, Cluster, Variable) %>%
  summarise(Average = mean(Value, na.rm = TRUE), .groups = "drop")

profile_difference <- profile_average %>%
  pivot_wider(names_from = Cluster, values_from = Average) %>%
  mutate(
    Difference = `Cluster 2` - `Cluster 1`,
    Direction = ifelse(Difference >= 0, "Cluster 2 higher", "Cluster 1 higher")
  )

difference_plot <- ggplot(
  profile_difference,
  aes(x = Difference, y = Method, fill = Direction)
) +
  geom_col() +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 3) +
  scale_fill_manual(
    values = c("Cluster 2 higher" = "#4C9F70", "Cluster 1 higher" = "#D2691E")
  ) +
  labs(
    title = "Difference Between Cluster 2 and Cluster 1 by Method",
    subtitle = "Positive means Cluster 2 has a higher average than Cluster 1",
    x = "Cluster 2 average - Cluster 1 average",
    y = "Method",
    fill = "Direction"
  ) +
  theme_minimal()

average_plot <- ggplot(
  profile_average,
  aes(x = Method, y = Average, fill = Cluster)
) +
  geom_col() +
  facet_grid(Variable ~ Cluster, scales = "free_y") +
  scale_fill_manual(values = c("Cluster 1" = "#D2691E", "Cluster 2" = "#4C9F70")) +
  labs(
    title = "Cluster Average Values by Method",
    x = "Method",
    y = "Average value",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dir.create("Clustering/outputs", recursive = TRUE, showWarnings = FALSE)

if (requireNamespace("gridExtra", quietly = TRUE)) {
  png(
    filename = "Clustering/outputs/baseline_cluster_profile_comparison_R.png",
    width = 1800,
    height = 850,
    res = 160
  )
  gridExtra::grid.arrange(difference_plot, average_plot, ncol = 2, widths = c(1, 1.35))
  dev.off()
} else {
  ggsave(
    "Clustering/outputs/baseline_cluster_profile_difference_R.png",
    difference_plot,
    width = 9,
    height = 6,
    dpi = 200
  )
  ggsave(
    "Clustering/outputs/baseline_cluster_profile_average_R.png",
    average_plot,
    width = 10,
    height = 7,
    dpi = 200
  )
}

difference_plot
average_plot
