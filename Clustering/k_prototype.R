# =========================
# 1. Working directory & load data
# =========================
getwd()
setwd("~/Documents/SJSU/study/Math 252_Cluster_Analysis/project")

data_baseline <- read.csv("data_complete_baseline.csv")

data_knn_k5    <- read.csv("knn_imputed_data_k5.csv")
data_knn_k10   <- read.csv("knn_imputed_data_k10.csv")
data_mean_mode <- read.csv("mean_mode_imputed_dataset.csv")
data_mice1     <- read.csv("mice_imputation_data1.csv")
data_mice2     <- read.csv("mice_imputation_data2.csv")

sapply(data_baseline, class)

# =========================
# 2. State -> region mapping
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
# 3. Load libraries
# =========================
library(dplyr)
library(ggplot2)
library(purrr)
library(clustMixType)

# =========================
# 4. Data preprocessing function
# =========================
prepare_kproto_data <- function(df) {
  
  df <- df %>%
    mutate(
      # ---------- region ----------
      Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
      
      # ---------- numeric ----------
      WorkTimeInSeconds...9  = as.numeric(WorkTimeInSeconds...9),
      multiplier             = as.numeric(multiplier),
      amount                 = as.numeric(amount),
      WorkTimeInSeconds...17 = as.numeric(WorkTimeInSeconds...17),
      Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
        as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
      Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
        as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
      
      # ---------- ordered factors ----------
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
      
      # ---------- nominal factors ----------
      gender = as.factor(gender),
      Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
        as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
      Q9.What.State.do.you.live.in. =
        as.factor(Q9.What.State.do.you.live.in.),
      Q7.Do.you.consider.yourself.a. =
        as.factor(Q7.Do.you.consider.yourself.a.),
      batch = as.factor(batch)
    )
  
  # optional: remove unused variables
  df_final <- df %>%
    select(
      -WorkTimeInSeconds...9,
      -WorkTimeInSeconds...17
    )
  
  return(df_final)
}

# =========================
# 5. Apply to baseline
# =========================
data_baseline_kproto <- prepare_kproto_data(data_baseline)

sapply(data_baseline_kproto, class)
summary(data_baseline_kproto)

# check NA values
colSums(is.na(data_baseline_kproto))

# remove rows with NA if needed for k-prototypes
data_baseline_kproto_complete <- na.omit(data_baseline_kproto)

# =========================
# 6. Elbow method
# =========================
set.seed(123)

k_values <- 1:10

costs <- map_dbl(k_values, function(k) {
  model <- kproto(data_baseline_kproto_complete, k = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = k_values,
  cost = costs
)

print(elbow_df)

ggplot(elbow_df, aes(x = k, y = cost)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Elbow Plot for K-Prototypes",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Distances"
  ) +
  theme_minimal()

# =========================
# 7. Fit final model
# =========================
set.seed(123)
kproto_model <- kproto(data_baseline_kproto_complete, k = 2)

print(kproto_model)
print(kproto_model$size)
print(kproto_model$centers)
print(kproto_model$cluster)

library(cluster)
library(clustMixType)

# use the processed dataset (with NA removed)
df <- data_baseline_kproto_complete

# compute Gower distance (suitable for mixed data)
diss <- daisy(df, metric = "gower")

# try different k values
k_values <- 2:6

sil_scores <- sapply(k_values, function(k) {
  model <- kproto(df, k = k)
  sil <- silhouette(model$cluster, diss)
  mean(sil[, 3])   # average silhouette width
})

# results
data.frame(k = k_values, silhouette = sil_scores)


# do mean_mode imputation dataset

data_mean_mode_kproto <- data_mean_mode %>%
  mutate(
    # ---------- region ----------
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    
    # ---------- numeric ----------
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    
    # ---------- ordered factors ----------
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
    
    # ---------- nominal factors ----------
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. =
      as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. =
      as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )
# =========================
# 5. Apply to mean_mode
# =========================

sapply(data_mean_mode_kproto, class)
summary(data_mean_mode_kproto)

# check NA values
colSums(is.na(data_mean_mode_kproto))


# =========================
# 6. Elbow method
# =========================
set.seed(123)

k_values <- 1:10

costs <- map_dbl(k_values, function(k) {
  model <- kproto(data_mean_mode_kproto, k = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = k_values,
  cost = costs
)

print(elbow_df)

ggplot(elbow_df, aes(x = k, y = cost)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Elbow Plot for K-Prototypes",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Distances"
  ) +
  theme_minimal()

# =========================
# 7. Fit final model
# =========================
set.seed(123)
kproto_model <- kproto(data_mean_mode_kproto, k = 2)

print(kproto_model)
print(kproto_model$size)
print(kproto_model$centers)
print(kproto_model$cluster)

library(cluster)
library(clustMixType)
# remove rows with NA if needed for k-prototypes
# na.omit(data_baseline_kproto)
# use the processed dataset
df <- na.omit(data_mean_mode_kproto)

# compute Gower distance (suitable for mixed data)
diss <- daisy(df, metric = "gower")

# try different k values
k_values <- 2:6

sil_scores <- sapply(k_values, function(k) {
  model <- kproto(df, k = k)
  sil <- silhouette(model$cluster, diss)
  mean(sil[, 3])   # average silhouette width
})

# results
data.frame(k = k_values, silhouette = sil_scores)


# =========================
# KNN k = 5 imputation dataset
# =========================
data_knn_k5_kproto <- data_knn_k5 %>%
  mutate(
    # ---------- region ----------
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    
    # ---------- numeric ----------
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    
    # ---------- ordered factors ----------
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
    
    # ---------- nominal factors ----------
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. =
      as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. =
      as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

sapply(data_knn_k5_kproto, class)
summary(data_knn_k5_kproto)
colSums(is.na(data_knn_k5_kproto))

set.seed(123)

k_values <- 1:10

costs <- map_dbl(k_values, function(k) {
  model <- kproto(data_knn_k5_kproto, k = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = k_values,
  cost = costs
)

print(elbow_df)

ggplot(elbow_df, aes(x = k, y = cost)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Elbow Plot for K-Prototypes",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Distances"
  ) +
  theme_minimal()

set.seed(123)
kproto_model <- kproto(data_knn_k5_kproto, k = 2)

print(kproto_model)
print(kproto_model$size)
print(kproto_model$centers)
print(kproto_model$cluster)

library(cluster)
library(clustMixType)

df <- data_knn_k5_kproto

diss <- daisy(df, metric = "gower")

k_values <- 2:6

sil_scores <- sapply(k_values, function(k) {
  model <- kproto(df, k = k)
  sil <- silhouette(model$cluster, diss)
  mean(sil[, 3])
})

data.frame(k = k_values, silhouette = sil_scores)


# =========================
# KNN k = 10 imputation dataset
# =========================
data_knn_k10_kproto <- data_knn_k10 %>%
  mutate(
    # ---------- region ----------
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    
    # ---------- numeric ----------
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    
    # ---------- ordered factors ----------
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
    
    # ---------- nominal factors ----------
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. =
      as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. =
      as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

sapply(data_knn_k10_kproto, class)
summary(data_knn_k10_kproto)
colSums(is.na(data_knn_k10_kproto))

set.seed(123)

k_values <- 1:10

costs <- map_dbl(k_values, function(k) {
  model <- kproto(data_knn_k10_kproto, k = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = k_values,
  cost = costs
)

print(elbow_df)

ggplot(elbow_df, aes(x = k, y = cost)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Elbow Plot for K-Prototypes",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Distances"
  ) +
  theme_minimal()

set.seed(123)
kproto_model <- kproto(data_knn_k10_kproto, k = 3)

print(kproto_model)
print(kproto_model$size)
print(kproto_model$centers)
print(kproto_model$cluster)

library(cluster)
library(clustMixType)

df <- data_knn_k10_kproto

diss <- daisy(df, metric = "gower")

k_values <- 2:6

sil_scores <- sapply(k_values, function(k) {
  model <- kproto(df, k = k)
  sil <- silhouette(model$cluster, diss)
  mean(sil[, 3])
})

data.frame(k = k_values, silhouette = sil_scores)


# =========================
# MICE imputation dataset 1
# =========================
data_mice1_kproto <- data_mice1 %>%
  mutate(
    # ---------- region ----------
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    
    # ---------- numeric ----------
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    
    # ---------- ordered factors ----------
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
    
    # ---------- nominal factors ----------
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. =
      as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. =
      as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

sapply(data_mice1_kproto, class)
summary(data_mice1_kproto)
colSums(is.na(data_mice1_kproto))

set.seed(123)

k_values <- 1:10

costs <- map_dbl(k_values, function(k) {
  model <- kproto(data_mice1_kproto, k = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = k_values,
  cost = costs
)

print(elbow_df)

ggplot(elbow_df, aes(x = k, y = cost)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Elbow Plot for K-Prototypes",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Distances"
  ) +
  theme_minimal()

set.seed(123)
kproto_model <- kproto(data_mice1_kproto, k = 2)

print(kproto_model)
print(kproto_model$size)
print(kproto_model$centers)
print(kproto_model$cluster)

library(cluster)
library(clustMixType)

df <- data_mice1_kproto

diss <- daisy(df, metric = "gower")

k_values <- 2:6

sil_scores <- sapply(k_values, function(k) {
  model <- kproto(df, k = k)
  sil <- silhouette(model$cluster, diss)
  mean(sil[, 3])
})

data.frame(k = k_values, silhouette = sil_scores)


# =========================
# MICE imputation dataset 2
# =========================
data_mice2_kproto <- data_mice2 %>%
  mutate(
    # ---------- region ----------
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    
    # ---------- numeric ----------
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    
    # ---------- ordered factors ----------
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
    
    # ---------- nominal factors ----------
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. =
      as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. =
      as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

sapply(data_mice2_kproto, class)
summary(data_mice2_kproto)
colSums(is.na(data_mice2_kproto))

set.seed(123)

k_values <- 1:10

costs <- map_dbl(k_values, function(k) {
  model <- kproto(data_mice2_kproto, k = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = k_values,
  cost = costs
)

print(elbow_df)

ggplot(elbow_df, aes(x = k, y = cost)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Elbow Plot for K-Prototypes",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Distances"
  ) +
  theme_minimal()

set.seed(123)
kproto_model <- kproto(data_mice2_kproto, k = 2)

print(kproto_model)
print(kproto_model$size)
print(kproto_model$centers)
print(kproto_model$cluster)

library(cluster)
library(clustMixType)

df <- data_mice2_kproto

diss <- daisy(df, metric = "gower")

k_values <- 2:6

sil_scores <- sapply(k_values, function(k) {
  model <- kproto(df, k = k)
  sil <- silhouette(model$cluster, diss)
  mean(sil[, 3])
})

data.frame(k = k_values, silhouette = sil_scores)

# =========================
# Compare ARI at k = 2 across imputed datasets
# =========================

library(dplyr)
library(clustMixType)
library(mclust)

set.seed(123)

# ---------------------------------
# 1. Preprocess each dataset
# ---------------------------------
data_knn_k5_kproto <- data_knn_k5 %>%
  mutate(
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    age = factor(age,
                 levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over"),
                 ordered = TRUE),
    income = factor(income,
                    levels = c("Under $20,000", "$20,000 - $39,999", "$40,000 - $59,999",
                               "$60,000 - $79,999", "$80,000 - $99,999", "Over $100,000"),
                    ordered = TRUE),
    Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =
      factor(Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.,
             levels = c("Never", "Seldom", "A few times a year",
                        "Once or twice a month", "Once a week", "More than once a week"),
             ordered = TRUE),
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. = as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. = as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

data_knn_k10_kproto <- data_knn_k10 %>%
  mutate(
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    age = factor(age,
                 levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over"),
                 ordered = TRUE),
    income = factor(income,
                    levels = c("Under $20,000", "$20,000 - $39,999", "$40,000 - $59,999",
                               "$60,000 - $79,999", "$80,000 - $99,999", "Over $100,000"),
                    ordered = TRUE),
    Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =
      factor(Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.,
             levels = c("Never", "Seldom", "A few times a year",
                        "Once or twice a month", "Once a week", "More than once a week"),
             ordered = TRUE),
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. = as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. = as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

data_mice1_kproto <- data_mice1 %>%
  mutate(
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    age = factor(age,
                 levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over"),
                 ordered = TRUE),
    income = factor(income,
                    levels = c("Under $20,000", "$20,000 - $39,999", "$40,000 - $59,999",
                               "$60,000 - $79,999", "$80,000 - $99,999", "Over $100,000"),
                    ordered = TRUE),
    Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =
      factor(Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.,
             levels = c("Never", "Seldom", "A few times a year",
                        "Once or twice a month", "Once a week", "More than once a week"),
             ordered = TRUE),
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. = as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. = as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

data_mice2_kproto <- data_mice2 %>%
  mutate(
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    age = factor(age,
                 levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over"),
                 ordered = TRUE),
    income = factor(income,
                    levels = c("Under $20,000", "$20,000 - $39,999", "$40,000 - $59,999",
                               "$60,000 - $79,999", "$80,000 - $99,999", "Over $100,000"),
                    ordered = TRUE),
    Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =
      factor(Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.,
             levels = c("Never", "Seldom", "A few times a year",
                        "Once or twice a month", "Once a week", "More than once a week"),
             ordered = TRUE),
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. = as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. = as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

data_mean_mode_kproto <- data_mean_mode %>%
  mutate(
    Q9.What.State.do.you.live.in. = state_to_region[Q9.What.State.do.you.live.in.],
    WorkTimeInSeconds...1  = as.numeric(WorkTimeInSeconds...1),
    multiplier             = as.numeric(multiplier),
    amount                 = as.numeric(amount),
    WorkTimeInSeconds...7  = as.numeric(WorkTimeInSeconds...7),
    Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views. =
      as.numeric(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.),
    Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation. =
      as.numeric(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.),
    age = factor(age,
                 levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over"),
                 ordered = TRUE),
    income = factor(income,
                    levels = c("Under $20,000", "$20,000 - $39,999", "$40,000 - $59,999",
                               "$60,000 - $79,999", "$80,000 - $99,999", "Over $100,000"),
                    ordered = TRUE),
    Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =
      factor(Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.,
             levels = c("Never", "Seldom", "A few times a year",
                        "Once or twice a month", "Once a week", "More than once a week"),
             ordered = TRUE),
    gender = as.factor(gender),
    Q5.In.the.2016.Presidential.election..who.did.you.vote.for. =
      as.factor(Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
    Q9.What.State.do.you.live.in. = as.factor(Q9.What.State.do.you.live.in.),
    Q7.Do.you.consider.yourself.a. = as.factor(Q7.Do.you.consider.yourself.a.),
    batch = as.factor(batch)
  )

# ---------------------------------
# 2. Fit k-prototypes with k = 2
# ---------------------------------
model_knn_k5    <- kproto(data_knn_k5_kproto, k = 2)
model_knn_k10   <- kproto(data_knn_k10_kproto, k = 2)
model_mice1     <- kproto(data_mice1_kproto, k = 2)
model_mice2     <- kproto(data_mice2_kproto, k = 2)
model_mean_mode <- kproto(data_mean_mode_kproto, k = 2)

clusters <- list(
  knn_k5    = model_knn_k5$cluster,
  knn_k10   = model_knn_k10$cluster,
  mice1     = model_mice1$cluster,
  mice2     = model_mice2$cluster,
  mean_mode = model_mean_mode$cluster
)

# ---------------------------------
# 3. Pairwise ARI matrix
# ---------------------------------
dataset_names <- names(clusters)

ari_matrix <- matrix(NA,
                     nrow = length(dataset_names),
                     ncol = length(dataset_names),
                     dimnames = list(dataset_names, dataset_names))

for (i in seq_along(dataset_names)) {
  for (j in seq_along(dataset_names)) {
    ari_matrix[i, j] <- adjustedRandIndex(
      clusters[[i]],
      clusters[[j]]
    )
  }
}

ari_matrix
round(ari_matrix, 4)

# ---------------------------------
# 4. Optional: convert to data frame
# ---------------------------------
ari_df <- as.data.frame(as.table(ari_matrix))
colnames(ari_df) <- c("Dataset_1", "Dataset_2", "ARI")

ari_df

# ---------------------------------
# 5. Optional: upper triangle only
# ---------------------------------
ari_pairs <- ari_df %>%
  filter(Dataset_1 < Dataset_2)

ari_pairs


# check the variables
# =========================
# Cluster profiling function for k-prototypes
# =========================

profile_kproto_clusters <- function(data, model, dataset_name) {
  
  data_with_cluster <- data %>%
    mutate(cluster = factor(model$cluster))
  
  cluster_summary <- data_with_cluster %>%
    group_by(cluster) %>%
    summarise(
      dataset = dataset_name,
      n = n(),
      
      # outcome variable for interpretation
      avg_amount = mean(amount, na.rm = TRUE),
      prop_give = mean(amount > 0, na.rm = TRUE),
      
      # continuous variables
      avg_multiplier = mean(multiplier, na.rm = TRUE),
      avg_pol_views = mean(
        Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.,
        na.rm = TRUE
      ),
      avg_relig_views = mean(
        Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.,
        na.rm = TRUE
      ),
      
      # ordinal variables
      avg_age_level = mean(as.numeric(age), na.rm = TRUE),
      avg_income_level = mean(as.numeric(income), na.rm = TRUE),
      avg_relig_attend_level = mean(
        as.numeric(Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.),
        na.rm = TRUE
      ),
      
      # categorical proportions
      prop_female = mean(gender == "Female", na.rm = TRUE),
      prop_voted_trump = mean(
        grepl("Trump", Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
        na.rm = TRUE
      ),
      prop_voted_clinton = mean(
        grepl("Clinton", Q5.In.the.2016.Presidential.election..who.did.you.vote.for.),
        na.rm = TRUE
      ),
      prop_democrat = mean(
        grepl("Democrat", Q7.Do.you.consider.yourself.a.),
        na.rm = TRUE
      ),
      prop_republican = mean(
        grepl("Republican", Q7.Do.you.consider.yourself.a.),
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  
  return(cluster_summary)
}
# =========================
# Apply cluster profiling to each dataset
# =========================

summary_knn_k5 <- profile_kproto_clusters(
  data_knn_k5_kproto,
  model_knn_k5,
  "KNN k=5"
)

summary_knn_k10 <- profile_kproto_clusters(
  data_knn_k10_kproto,
  model_knn_k10,
  "KNN k=10"
)

summary_mice1 <- profile_kproto_clusters(
  data_mice1_kproto,
  model_mice1,
  "MICE 1"
)

summary_mice2 <- profile_kproto_clusters(
  data_mice2_kproto,
  model_mice2,
  "MICE 2"
)

summary_mean_mode <- profile_kproto_clusters(
  data_mean_mode_kproto,
  model_mean_mode,
  "Mean/Mode"
)

kproto_cluster_summary_all <- bind_rows(
  summary_knn_k5,
  summary_knn_k10,
  summary_mice1,
  summary_mice2,
  summary_mean_mode
)

print(kproto_cluster_summary_all, width = Inf)
