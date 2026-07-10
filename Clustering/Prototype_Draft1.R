getwd()
setwd("~/Documents/SJSU/study/Math 252_Cluster_Analysis/project")
# 读取6个数据集
data_baseline <- read.csv("data_complete_baseline.csv")

data_knn_k5 <- read.csv("knn_imputed_data_k5.csv")
data_knn_k10 <- read.csv("knn_imputed_data_k10.csv")

data_mean_mode <- read.csv("mean_mode_imputed_dataset.csv")

data_mice1 <- read.csv("mice_imputation_data1.csv")
data_mice2 <- read.csv("mice_imputation_data2.csv")

sapply(data_baseline, class)

# change state to region
state_to_region <- c(
  "CT"="Northeast", "ME"="Northeast", "MA"="Northeast", "NH"="Northeast", 
  "RI"="Northeast", "VT"="Northeast", "NJ"="Northeast", "NY"="Northeast", "PA"="Northeast",
  "IL"="Midwest", "IN"="Midwest", "IA"="Midwest", "KS"="Midwest", "MI"="Midwest", 
  "MN"="Midwest", "MS"="Midwest", "NE"="Midwest", "ND"="Midwest", "OH"="Midwest", 
  "SD"="Midwest", "WI"="Midwest",
  "AL"="South", "AR"="South", "DE"="South", "DC"="South", "FL"="South", "GA"="South", 
  "KY"="South", "LA"="South", "MD"="South", "MS"="South", "NC"="South", "OK"="South", 
  "SC"="South", "TN"="South", "TX"="South", "VA"="South", "WV"="South",
  "AK"="West", "AZ"="West", "CA"="West", "CO"="West", "HI"="West", "ID"="West", 
  "MT"="West", "NV"="West", "NM"="West", "OR"="West", "UT"="West", "WA"="West", "WY"="West"
)
data_baseline$Q9.What.State.do.you.live.in. <- state_to_region[data_baseline$Q9.What.State.do.you.live.in.]

# change income to ordinal
income_to_number <- c("$80,000 - $99,999"=5, "$60,000 - $79,999"= 4, "Under $20,000"= 1, "Over $100,000"=6, "$40,000 - $59,999"=3 , "$20,000 - $39,999" = 2)
data_baseline$income <- income_to_number[data_baseline$income]

# change religion
religion_service_to_number <- c("Once or twice a month"=4, "Never"=1, "A few times a year"=3, "Once a week"=5, "Seldom"=2, "More than once a week"=6)
data_baseline$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. <- religion_service_to_number[
  data_baseline$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.
]

# change age to ordinal
age_to_number <- c("60-69"=5, "18-29"=1,"30-39"=2, "50-59"=4, "40-49" =3, "70 or over"=6)
data_baseline$age <- age_to_number[data_baseline$age]

sapply(data_baseline, class)

library(tidyverse)
data_baseline_kproto <- data_baseline |>
  mutate(
    # numeric variables
    `WorkTimeInSeconds...9` = as.numeric(`WorkTimeInSeconds...9`),
    multiplier = as.numeric(multiplier),
    amount = as.numeric(amount),
    `WorkTimeInSeconds...17` = as.numeric(`WorkTimeInSeconds...17`),
    `Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.` =
      as.numeric(`Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.`),
    `Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.` =
      as.numeric(`Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.`),
    
    # ordinal variables
    age = factor(
      age,
      levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over"),
      ordered = TRUE
    ),
    
    income = factor(
      income,
      levels = c(
        "Less than $20,000",
        "$20,000 - $39,999",
        "$40,000 - $59,999",
        "$60,000 - $79,999",
        "$80,000 - $99,999",
        "$100,000 or more"
      ),
      ordered = TRUE
    ),
    
    `Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.` =
      factor(
        `Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.`,
        levels = c(
          "Never",
          "Less than once a year",
          "Once or twice a year",
          "A few times a year",
          "Once or twice a month",
          "Once a week",
          "More than once a week"
        ),
        ordered = TRUE
      ),
    
    # nominal categorical variables
    gender = as.factor(gender),
    `Q5.In.the.2016.Presidential.election..who.did.you.vote.for.` =
      as.factor(`Q5.In.the.2016.Presidential.election..who.did.you.vote.for.`),
    `Q9.What.State.do.you.live.in.` =
      as.factor(`Q9.What.State.do.you.live.in.`),
    `Q7.Do.you.consider.yourself.a.` =
      as.factor(`Q7.Do.you.consider.yourself.a.`),
    batch = as.factor(batch),
  )
sapply(data_baseline_kproto, class)


data_baseline_kproto_final <- data_baseline_kproto |>
  dplyr::select(
    -age,
    -income,
    -`Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.`,
    -`WorkTimeInSeconds...9`,
    -`WorkTimeInSeconds...17`
  )
colnames(data_baseline_kproto_final)
library(clustMixType)

set.seed(123)

k_values <- 1:10

costs <- map_dbl(k_values, function(k) {
  kproto(data_baseline_kproto_final, k = k)$tot.withinss
})

elbow_df <- data.frame(k = k_values, cost = costs)

kproto_model <- kproto(data_baseline_kproto_final, k = 2)
kproto_model
kproto_model$size



library(ggplot2)

ggplot(elbow_df, aes(x = k, y = cost)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Elbow Method for k-prototypes",
    x = "Number of clusters (k)",
    y = "Total within-cluster cost"
  )
kproto_model <- kproto(data_baseline_kproto_final, k = 3)
kproto_model
kproto_model$size

kproto_model <- kproto(data_baseline_kproto_final, k = 4)
kproto_model
kproto_model$size
# siluette
library(cluster)
library(clustMixType)

# 👉 一定要统一数据
df <- na.omit(data_baseline_kproto_final)

# distance
diss <- daisy(df, metric = "gower")

# k values
k_values <- 2:6

sil_scores <- sapply(k_values, function(k) {
  model <- kproto(df, k = k)
  
  # 👉 强制检查
  stopifnot(length(model$cluster) == nrow(df))
  
  sil <- silhouette(model$cluster, diss)
  mean(sil[, 3])
})

data.frame(k = k_values, silhouette = sil_scores)


