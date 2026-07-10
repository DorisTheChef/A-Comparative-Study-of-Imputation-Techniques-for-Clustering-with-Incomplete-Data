# without amount
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
# 2. Process baseline data
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

# Keep only rows that are complete across all variables
rows_keep <- complete.cases(data_baseline_processed)

# Keep a clean full dataset INCLUDING amount for later interpretation
df_full <- data_baseline_processed[rows_keep, ]

# =========================
# 3. Mixed data for k-prototypes
#    EXCLUDE amount from clustering
# =========================
df_mixed <- df_full %>%
  select(-amount)

# =========================
# 4. Numeric-only data for numeric methods
#    EXCLUDE amount from clustering
# =========================
df_numeric <- df_full %>%
  select(
    WorkTimeInSeconds...9,
    multiplier,
    WorkTimeInSeconds...17,
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.,
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.
  )

df_numeric_scaled <- scale(df_numeric)

# =========================
# 5. Fit all methods at k = 3
# =========================

# 5.1 K-means
km_model <- kmeans(df_numeric_scaled, centers = 3, nstart = 25)
cl_kmeans <- km_model$cluster

# 5.2 Fuzzy k-means
fkm_model <- FKM(df_numeric_scaled, k = 3, m = 2, stand = 0)
cl_fuzzy <- apply(fkm_model$U, 1, which.max)

# 5.3 PD clustering
pd_model <- PDC(as.matrix(df_numeric_scaled), k = 3)
cl_pd <- pd_model$label

# 5.4 Hierarchical average linkage
hc_model <- hclust(dist(df_numeric_scaled), method = "average")
cl_hc <- cutree(hc_model, k = 3)

# 5.5 GMM
gmm_model <- Mclust(df_numeric_scaled, G = 3)
cl_gmm <- gmm_model$classification

# 5.6 k-prototypes
kp_model <- kproto(df_mixed, k = 3)
cl_kproto <- kp_model$cluster

# =========================
# 6. Collect cluster labels
# =========================
cluster_list <- list(
  kmeans       = cl_kmeans,
  fuzzy_kmeans = cl_fuzzy,
  pd_cluster   = cl_pd,
  hclust_avg   = cl_hc,
  gmm          = cl_gmm,
  kprototype   = cl_kproto
)

# Quick check: all must have same length
sapply(cluster_list, length)

# =========================
# 7. Pairwise ARI matrix
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
# 8. Optional: cluster sizes
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
# 9. Map clusters back to amount
#    for interpretation
# =========================
df_with_clusters <- df_full %>%
  mutate(
    cluster_kmeans   = cl_kmeans,
    cluster_fuzzy    = cl_fuzzy,
    cluster_pd       = cl_pd,
    cluster_hclust   = cl_hc,
    cluster_gmm      = cl_gmm,
    cluster_kproto   = cl_kproto
  )

# Example: compare mean amount by cluster for each method
df_with_clusters %>%
  group_by(cluster_kmeans) %>%
  summarise(mean_amount = mean(amount), n = n())

df_with_clusters %>%
  group_by(cluster_kproto) %>%
  summarise(mean_amount = mean(amount), n = n())

df_with_clusters %>%
  group_by(cluster_kproto) %>%
  summarise(
    mean_amount = mean(amount),
    sd_amount = sd(amount),
    n = n()
  )