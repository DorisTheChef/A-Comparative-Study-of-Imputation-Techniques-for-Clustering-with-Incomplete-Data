"Final Project - Christie Ngo"

# Sort States into geographic regions to reduce number of categories

state_to_region <- c(
  # NORTHEAST
  "CT"="Northeast", "ME"="Northeast", "MA"="Northeast", "NH"="Northeast", 
  "RI"="Northeast", "VT"="Northeast", "NJ"="Northeast", "NY"="Northeast", "PA"="Northeast",
  
  # MIDWEST
  "IL"="Midwest", "IN"="Midwest", "IA"="Midwest", "KS"="Midwest", "MI"="Midwest", 
  "MN"="Midwest", "MO"="Midwest", "NE"="Midwest", "ND"="Midwest", "OH"="Midwest", 
  "SD"="Midwest", "WI"="Midwest",
  
  # SOUTH
  "AL"="South", "AR"="South", "DE"="South", "DC"="South", "FL"="South", "GA"="South", 
  "KY"="South", "LA"="South", "MD"="South", "MS"="South", "NC"="South", "OK"="South", 
  "SC"="South", "TN"="South", "TX"="South", "VA"="South", "WV"="South",
  
  # WEST
  "AK"="West", "AZ"="West", "CA"="West", "CO"="West", "HI"="West", "ID"="West", 
  "MT"="West", "NV"="West", "NM"="West", "OR"="West", "UT"="West", "WA"="West", "WY"="West"
)

# Map strings to ordinal values 
income_to_number <- c("$80,000 - $99,999"=5, "$60,000 - $79,999"= 4, "Under $20,000"= 1, "Over $100,000"=6,  "$40,000 - $59,999"=3 , "$20,000 - $39,999" = 2)

religion_service_to_number <- c("Once or twice a month"=4, "Never"=1, "A few times a year"=3, "Once a week"=5, "Seldom"=2, "More than once a week"=6)

age_to_number <- c("60-69"=5, "18-29"=1,"30-39"=2, "50-59"=4, "40-49" =3, "70 or over"=6)


############################## Imputed Dataset for Ordinal & Categorical  ##############################

income_levels <- c("Under $20,000", "$20,000 - $39,999", "$40,000 - $59,999", 
                   "$60,000 - $79,999", "$80,000 - $99,999", "Over $100,000")
religion_levels <- c("Never", "Seldom", "A few times a year", 
                     "Once or twice a month", "Once a week", "More than once a week")
age_levels <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over")

baseline = read.csv("C:\\Users\\chris\\Documents\\MAT 252\\Project Data\\data_complete_baseline.csv")

baseline$Q9.What.State.do.you.live.in. <- state_to_region[baseline$Q9.What.State.do.you.live.in.]
baseline$Q9.What.State.do.you.live.in.[is.na(baseline$Q9.What.State.do.you.live.in.)] <- "Unknown"
baseline$batch <- NULL
# Select all categorical columns and convert into factors
baseline_cat = baseline[, c(4,5,6,10,11,12,13)]

mm = read.csv("C:\\Users\\chris\\Documents\\MAT 252\\Project Data\\mean_mode_imputed_dataset.csv")
mm$Q9.What.State.do.you.live.in. <- state_to_region[mm$Q9.What.State.do.you.live.in.]
mm$Q9.What.State.do.you.live.in.[is.na(mm$Q9.What.State.do.you.live.in.)] <- "Unknown"
mm$batch <- NULL
mm_cat = mm[, c(4,5,6,10,11,12,13)]


mice1 = read.csv("C:\\Users\\chris\\Documents\\MAT 252\\Project Data\\mice_imputation_data1.csv")
mice1$Q9.What.State.do.you.live.in. <- state_to_region[mice1$Q9.What.State.do.you.live.in.]
mice1$Q9.What.State.do.you.live.in.[is.na(mice1$Q9.What.State.do.you.live.in.)] <- "Unknown"
mice1$batch <- NULL
mice1_cat = mice1[, c(4,5,6,10,11,12,13)]


mice2 = read.csv("C:\\Users\\chris\\Documents\\MAT 252\\Project Data\\mice_imputation_data2.csv")
mice2$Q9.What.State.do.you.live.in. <- state_to_region[mice2$Q9.What.State.do.you.live.in.]
mice2$Q9.What.State.do.you.live.in.[is.na(mice2$Q9.What.State.do.you.live.in.)] <- "Unknown"
mice2$batch <- NULL
mice2_cat = mice2[, c(4,5,6,10,11,12,13)]


knn1 = read.csv("C:\\Users\\chris\\Documents\\MAT 252\\Project Data\\knn_imputed_data_k5.csv")
knn1$Q9.What.State.do.you.live.in. <- state_to_region[knn1$Q9.What.State.do.you.live.in.]
knn1$Q9.What.State.do.you.live.in.[is.na(knn1$Q9.What.State.do.you.live.in.)] <- "Unknown"
knn1$batch <- NULL
knn1_cat = knn1[, c(4,5,6,10,11,12,13)]


knn2 = read.csv("C:\\Users\\chris\\Documents\\MAT 252\\Project Data\\knn_imputed_data_k10.csv")
knn2$Q9.What.State.do.you.live.in. <- state_to_region[knn2$Q9.What.State.do.you.live.in.]
knn2$Q9.What.State.do.you.live.in.[is.na(knn2$Q9.What.State.do.you.live.in.)] <- "Unknown"
knn2$batch <- NULL
knn2_cat = knn2[, c(4,5,6,10,11,12,13)]

# Put datasets into a named list
dataset_list <- list(baseline = baseline_cat, mm = mm_cat, mice1 = mice1_cat, mice2 = mice2_cat, knn1 = knn1_cat, knn2 = knn2_cat)

# 2. Define your levels (ensure these are run first)
income_levels <- c("Under $20,000", "$20,000 - $39,999", "$40,000 - $59,999", 
                   "$60,000 - $79,999", "$80,000 - $99,999", "Over $100,000")

religion_levels <- c("Never", "Seldom", "A few times a year", 
                     "Once or twice a month", "Once a week", "More than once a week")

age_levels <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or over")

# 3. Loop through the list to apply the transformations
dataset_list <- lapply(dataset_list, function(df) {
  
  # Ordered Factors
  df$income <- factor(df$income, levels = income_levels, ordered = TRUE)
  
  df$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. <- 
    factor(df$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services., 
           levels = religion_levels, ordered = TRUE)
  
  df$age <- factor(df$age, levels = age_levels, ordered = TRUE)
  
  # Bulk convert remaining characters to regular factors
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
  
  return(df)
})

# Extract them back to individual objects
baseline_cat <- dataset_list$baseline
mm_cat <- dataset_list$mm
mice1_cat <- dataset_list$mice1
mice2_cat <- dataset_list$mice2
knn1_cat <- dataset_list$knn1
knn2_cat <- dataset_list$knn2


##################################################################################################

# Select only Continuous and Continuous + Ordinal 

baseline_num <- as.data.frame(scale(select_if(baseline, is.numeric)))
baseline$income = income_to_number[baseline$income]
baseline$income[is.na(baseline$income)] <- 3 # Set to mode group value for income if not stated
baseline$age = age_to_number[baseline$age]
# baseline$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =  religion_service_to_number[baseline$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.]
baseline_num_ord <- as.data.frame(scale(select_if(baseline, is.numeric)))

mm_num <- as.data.frame(scale(select_if(mm, is.numeric)))
mm$income = income_to_number[mm$income]
mm$age = age_to_number[mm$age]
# mm$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =  religion_service_to_number[mm$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.]
mm_num_ord <- as.data.frame(scale(select_if(mm, is.numeric)))

mice1_num <- as.data.frame(scale(select_if(mice1, is.numeric)))
mice1$income = income_to_number[mice1$income]
mice1$age = age_to_number[mice1$age]
# mice1$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =  religion_service_to_number[mice1$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.]
mice1_num_ord <- as.data.frame(scale(select_if(mice1, is.numeric)))

mice2_num <- as.data.frame(scale(select_if(mice2, is.numeric)))
mice2$income = income_to_number[mice2$income]
mice2$age = age_to_number[mice2$age]
# mice2$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =  religion_service_to_number[mice2$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.]
mice2_num_ord <- as.data.frame(scale(select_if(mice2, is.numeric)))


knn1_num <- as.data.frame(scale(select_if(knn1, is.numeric)))
knn1$income = income_to_number[knn1$income]
knn1$age = age_to_number[knn1$age]
# knn1$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =  religion_service_to_number[knn1$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.]
knn1_num_ord <- as.data.frame(scale(select_if(knn1, is.numeric)))

knn2_num <- as.data.frame(scale(select_if(knn2, is.numeric)))
knn2$income = income_to_number[knn2$income]
knn2$age = age_to_number[knn2$age]
# knn2$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. =  religion_service_to_number[knn2$Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.]
knn2_num_ord <- as.data.frame(scale(select_if(knn2, is.numeric)))

# Check the heatmaps for correlation between variables
# Helps debug singularity issues further below

library(corrplot)

cor_matrix <- cor(baseline_num_ord)

short_names <- abbreviate(colnames(baseline_num_ord), minlength = 4)
colnames(cor_matrix) <- rownames(cor_matrix) <- short_names
corrplot(cor_matrix, method = "color", type = "upper") 


library(mclust)
library(mixsmsn)
library(MixGHD)

############################## Skewed-t and GHD model fit  ##############################

run_models <- function(df, G_range = 2:8, ridge = 1e-4, iter.max = 100, nr = 50) {
  df <- as.data.frame(df)
  # initialize a GMM model to reduce singularity issues from skew t 
  make_init <- function(X, cl, ridge = 1e-4) {
    
    g <- length(unique(cl))
    mu <- Sigma <- shape <- vector("list", g)
    pii <- numeric(g)
    
    # get the parameter estimates for each cluster
    for (j in seq_len(g)) {
      Xj <- X[cl == j, , drop = FALSE]
      pii[j] <- nrow(Xj) / nrow(X)
      mu[[j]] <- colMeans(Xj)
      
      if (nrow(Xj) < 2) {
        Sigma[[j]] <- diag(ridge, ncol(X))
      } else {
        S <- cov(Xj)
        if (any(!is.finite(S))) {
          S <- diag(1, ncol(X))
        }
        # In make_init, scale ridge to cluster size
        ridge_j <- ridge * (nrow(X) / max(nrow(Xj), 1))
        Sigma[[j]] <- S + diag(ridge_j, ncol(X))
      }
      
      shape[[j]] <- rep(0, ncol(X))
    }
    
    list(mu = mu, Sigma = Sigma, shape = shape, pii = pii)
  }
  
  
  # Lower range for skew t because the EM algorithm returned unstable results at higher numbers
  fit_skewt_with_mclust_init <- function(X, G_range = 2:6,
                                         mclust_models = c("VVV","EEE"), # use only mclust models with full covariance
                                         family = "Skew.t",
                                         ridge = 1e-6,
                                         iter.max = 100) {
    X <- as.matrix(X)
    storage.mode(X) <- "double"
    
    # drop infinite values
    keep <- apply(is.finite(X), 1, all)
    X_fit <- X[keep, , drop = FALSE]
    
    # fit the initial GMM
    mc <- Mclust(X_fit, G = G_range, modelNames = mclust_models)
    init <- make_init(X_fit, mc$classification, ridge = ridge)
    
    # fit the skew t model with parameters from GMM
    st <- smsn.mmix(
      y = X_fit,
      g = mc$G,
      family = family,
      get.init = FALSE,
      mu = init$mu,
      Sigma = init$Sigma,
      shape = init$shape,
      pii = init$pii,
      group = TRUE,
      iter.max = iter.max,
      criteria = TRUE
    )
    
    list(
      best_G = mc$G,
      init_model = mc$modelName,
      init_bic = mc$bic,
      skewt_bic = st$bic,
      mclust_fit = mc,
      smsn_fit = st,
      keep_rows = keep,
      cluster = st$group
    )
  }
  
  # FIT A GHD MODEL
  run_ghd <- function(X, G_range = 2:8, nr = 50, modelSel = "BIC") {
    X <- as.matrix(X)
    storage.mode(X) <- "double"
    
    # drop inf values
    keep <- apply(is.finite(X), 1, all)
    X_fit <- X[keep, , drop = FALSE]
    
    out <- MGHD(X_fit, G = G_range, nr = nr, modelSel = modelSel)
    
    list(
      fit = out,
      G = length(unique(out@map)),
      BIC = out@BIC,
      loglik = out@loglik,
      cl = out@map,
      keep_rows = keep,
      best_model = "MGHD"
    )
  }
  
  # Helper function to add the cluster label column to dataframes
  add_cluster_column <- function(df, keep_rows, cluster, new_name) {
    df[[new_name]] <- NA_integer_
    df[[new_name]][keep_rows] <- cluster
    df
  }
  
  # Capture errors that would break loop
  res <- tryCatch(
    fit_skewt_with_mclust_init(df, G_range = G_range, ridge = ridge, iter.max = iter.max),
    error = function(e) e
  )
  
  ghd_best <- tryCatch(
    run_ghd(df, G_range = G_range, nr = nr),
    error = function(e) e
  )
  
  if (!inherits(res, "error")) {
    df <- add_cluster_column(df, res$keep_rows, res$cluster, "skew.t_cluster")
    df$skew.t_model <- "Skew.t"
    df$skew.t_init_model <- res$init_model
    df$skew.t_BIC <- res$skewt_bic
  }
  
  if (!inherits(ghd_best, "error")) {
    df <- add_cluster_column(df, ghd_best$keep_rows, ghd_best$cl, "ghd_cluster")
    df$ghd_model <- ghd_best$best_model
    df$ghd_BIC <- ghd_best$BIC
  }
  
  list(
    data = df,
    skewt = res,
    ghd = ghd_best
  )
}

results <- run_models(baseline_num)
baseline_num_out <- results$data


table(baseline_num_out$skew.t_cluster, useNA = "ifany")
table(baseline_num_out$ghd_cluster, useNA = "ifany")

# RUN RESULTS FOR EACH DATASET
results <- run_models(baseline_num_ord)
baseline_num_ord_out <- results$data


table(baseline_num_ord_out$skew.t_cluster, useNA = "ifany")
table(baseline_num_ord_out$ghd_cluster, useNA = "ifany")

set.seed(10)
results <- run_models(mm_num)
mm_num_out <- results$data


table(mm_num_out$skew.t_cluster, useNA = "ifany")
table(mm_num_out$ghd_cluster, useNA = "ifany")

results <- run_models(mm_num_ord)
mm_num_ord_out <- results$data


table(mm_num_ord_out$skew.t_cluster, useNA = "ifany")
table(mm_num_ord_out$ghd_cluster, useNA = "ifany")

results <- run_models(mice1_num)
mice1_num_out <- results$data


table(mice1_num_out$skew.t_cluster, useNA = "ifany")
table(mice1_num_out$ghd_cluster, useNA = "ifany")

results <- run_models(mice1_num_ord)
mice1_num_ord_out <- results$data


table(mice1_num_ord_out$skew.t_cluster, useNA = "ifany")
table(mice1_num_ord_out$ghd_cluster, useNA = "ifany")

results <- run_models(mice2_num)
mice2_num_out <- results$data


table(mice2_num_out$skew.t_cluster, useNA = "ifany")
table(mice2_num_out$ghd_cluster, useNA = "ifany")

results <- run_models(mice2_num_ord)
mice2_num_ord_out <- results$data


table(mice2_num_ord_out$skew.t_cluster, useNA = "ifany")
table(mice2_num_ord_out$ghd_cluster, useNA = "ifany")

results <- run_models(knn1_num)
knn1_num_out <- results$data


table(knn1_num_out$skew.t_cluster, useNA = "ifany")
table(knn1_num_out$ghd_cluster, useNA = "ifany")

results <- run_models(knn1_num_ord)
knn1_num_ord_out <- results$data


table(knn1_num_ord_out$skew.t_cluster, useNA = "ifany")
table(knn1_num_ord_out$ghd_cluster, useNA = "ifany")

results <- run_models(knn2_num)
knn2_num_out <- results$data


table(knn2_num_out$skew.t_cluster, useNA = "ifany")
table(knn2_num_out$ghd_cluster, useNA = "ifany")

results <- run_models(knn2_num_ord)
knn2_num_ord_out <- results$data


table(knn2_num_ord_out$skew.t_cluster, useNA = "ifany")
table(knn2_num_ord_out$ghd_cluster, useNA = "ifany")

library(FactoMineR)
library(factoextra)

# Perform Multiple Correspondence Analysis
res.mca <- MCA(mm_cat, graph = FALSE)

# Create the Scree Plot to select optimal number of clusters
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 20)) +
  ggtitle("Scree Plot: Determine ndimrange") +
  theme_minimal()

library(clustrd)

# 1. Create the list of dataframes to process
data_list <- list(
  baseline = baseline_cat,
  mm       = mm_cat, 
  mice1    = mice1_cat, 
  mice2    = mice2_cat, 
  knn1     = knn1_cat, 
  knn2     = knn2_cat
)

# 2.store the full model objects (for plots/summaries)
results_list <- list()

# 3. COMBINED LOOP: Run model + Assign Labels
for (name in names(data_list)) {
  message(paste("Processing dataset:", name))
  
  # This finds the best combination of clusters (3-8) and dimensions (2-3)
  res <- tuneclus(
    data_list[[name]], 
    nclusrange = 3:8, 
    ndimrange  = 2:3, 
    method     = "clusCA", 
    criterion  = "asw"
  )
  
  # Store the full result object in our list
  results_list[[name]] <- res
  
  # Get the labels from the winning cluster object
  best_labels <- res$clusobjbest$cluster
  
  # Add the labels back to the dataframe as a factor
  data_list[[name]]$cluster_labels <- as.factor(best_labels)
  
  message(paste("   - Optimal clusters found:", res$nclusbest))
  message(paste("   - Optimal dimensions found:", res$ndimbest))
}

# Extract the updated dataframes back
baseline_cat <- data_list$baseline
mm_cat           <- data_list$mm
mice1_cat        <- data_list$mice1
mice2_cat        <- data_list$mice2
knn1_cat         <- data_list$knn1
knn2_cat         <- data_list$knn2

# Plot the best solution for KNN1
plot(results_list$knn1$clusobjbest, cludesc = TRUE)

# See which categories define Cluster 1 in KNN1
results_list$knn1$clusobjbest$description

library(mclust)

# 1. Collect all the cluster label columns into a single dataframe
# This makes it easy to iterate through them
all_clusters <- data.frame(
  # baseline = baseline_cat$cluster_labels,
  mm       = mm_cat$cluster_labels,
  mice1    = mice1_cat$cluster_labels,
  mice2    = mice2_cat$cluster_labels,
  knn1     = knn1_cat$cluster_labels,
  knn2     = knn2_cat$cluster_labels
)

# 2. Initialize an empty square matrix
n <- ncol(all_clusters)
ari_matrix <- matrix(0, nrow = n, ncol = n)
colnames(ari_matrix) <- colnames(all_clusters)
rownames(ari_matrix) <- colnames(all_clusters)

# 3. Fill the matrix with pairwise ARI values
for (i in 1:n) {
  for (j in 1:n) {
    ari_matrix[i, j] <- adjustedRandIndex(all_clusters[, i], all_clusters[, j])
  }
}

# 4. View the matrix
print(round(ari_matrix, 3))
```

```{r}
# 1. Collect all the cluster label columns into a single dataframe
all_clusters <- data.frame(
  # BASELINE has a lower number of rows, cannot compare ARI
  # baseline = baseline_cat$cluster_labels,
  mm_ghd = mm_num_out$ghd_cluster,
  mice1_skew.t = mice1_num_out$skew.t_cluster,
  mice1_ghd = mice1_num_out$ghd_cluster,
  mice2_skew.t = mice2_num_out$skew.t_cluster,
  mice2_ghd = mice2_num_out$ghd_cluster,
  knn1_skew.t = knn1_num_out$skew.t_cluster,
  knn1_ghd = knn1_num_out$ghd_cluster,
  knn2_skew.t = knn2_num_out$skew.t_cluster,
  knn2_ghd = knn2_num_out$ghd_cluster
)

# 2. Initialize an empty square matrix
n <- ncol(all_clusters)
ari_matrix <- matrix(0, nrow = n, ncol = n)
colnames(ari_matrix) <- colnames(all_clusters)
rownames(ari_matrix) <- colnames(all_clusters)

# 3. Fill the matrix with pairwise ARI values
for (i in 1:n) {
  for (j in 1:n) {
    ari_matrix[i, j] <- adjustedRandIndex(all_clusters[, i], all_clusters[, j])
  }
}

# 4. View the matrix
print(round(ari_matrix, 3))

# 1. Collect all the cluster label columns into a single dataframe
all_clusters <- data.frame(
  # baseline = baseline_cat$cluster_labels,
  mm_ghd = mm_num_ord_out$ghd_cluster,
  mice1_skew.t = mice1_num_ord_out$skew.t_cluster,
  mice1_ghd = mice1_num_ord_out$ghd_cluster,
  mice2_skew.t = mice2_num_ord_out$skew.t_cluster,
  mice2_ghd = mice2_num_ord_out$ghd_cluster,
  knn1_skew.t = knn1_num_ord_out$skew.t_cluster,
  knn1_ghd = knn1_num_ord_out$ghd_cluster,
  knn2_skew.t = knn2_num_ord_out$skew.t_cluster,
  knn2_ghd = knn2_num_ord_out$ghd_cluster
)

# 2. Initialize an empty square matrix
n <- ncol(all_clusters)
ari_matrix <- matrix(0, nrow = n, ncol = n)
colnames(ari_matrix) <- colnames(all_clusters)
rownames(ari_matrix) <- colnames(all_clusters)

# 3. Fill the matrix with pairwise ARI values
for (i in 1:n) {
  for (j in 1:n) {
    ari_matrix[i, j] <- adjustedRandIndex(all_clusters[, i], all_clusters[, j])
  }
}

# 4. View the matrix
print(round(ari_matrix, 3))

### Mixed Finite Mixture Model

# Difficulty fitting model due to various errors

# Data quality checks showed pointed to no glaring issues

# library(clustMD)
# # collapse
# mm$Q7.Do.you.consider.yourself.a. <- ifelse(mm$Q7.Do.you.consider.yourself.a. %in% c("Independent", "Something else"), "Other", mm$Q7.Do.you.consider.yourself.a.)
# mm$Q5.In.the.2016.Presidential.election..who.did.you.vote.for.  <- ifelse(mm$Q5.In.the.2016.Presidential.election..who.did.you.vote.for. %in% c("Someone else", "Didn't vote"), "Other", mm$Q5.In.the.2016.Presidential.election..who.did.you.vote.for.)
# mm$Q9.What.State.do.you.live.in.  <- ifelse(mm$Q9.What.State.do.you.live.in. == "Unknown", "West", mm$Q9.What.State.do.you.live.in.)
# mm$gender[is.na(mm$gender)] <- "0"
# 
# 
# 
# # then rebuild your pipeline
# 
# # 1. Identify column indices (already done)
# cns_cols <- c(1, 2, 3, 7, 8, 9)
# ord_cols <- c(4, 5, 6, 13)
# # Identify nominal (anything left over)
# all_cols <- 1:ncol(mm)
# nom_cols <- setdiff(all_cols, c(cns_cols, ord_cols))
# 
# # 2. Reorder the dataframe so columns follow the Continuous -> Ordinal -> Nominal pattern
# mm_reordered <- mm[, c(cns_cols, ord_cols, nom_cols)]
# 
# # 3. Define the COUNT of variables
# CnsCount <- length(cns_cols) 
# OrdCount <- length(ord_cols)+CnsCount
# 
# # number of Monte Carlo samples
# Nnorms <- 100000
# 
# set.seed(113)
# 
# clust_fits <- vector("list", 6)
# for (g in 2:6) {
#   clust_fits[[g]] <- clustMD(
#     X = data.matrix(mm_reordered), # convert nominal into integers
#     G = g,
#     CnsIndx = CnsCount,
#     OrdIndx = OrdCount,
#     Nnorms = Nnorms,
#     model = "EVI",         # can change; start with a parsimonious one
#     MaxIter = 500,
#     scale = TRUE,
#     store.params = TRUE, startCL = "kmeans"
#   )
# }
# 
# clust_stats <- data.frame(
#   G = 2:6,
#   BIC = sapply(2:6, function(g) clust_fits[[g]]$BIC)
# )
# 
# clust_stats[order(-clust_stats$BIC), ]   # check package convention for "better" BIC
# best_g_clust <- clust_stats$G[which.max(clust_stats$BIC)]
# best_clust <- clust_fits[[best_g_clust]]
# 
# table(best_clust$cl)


### GENERATE CLUSTER DEMOGRAPHIC SUMMARIES

library(dplyr)
knn2_combined <- cbind(knn2, skew.t_cluster=knn2_num_out[,7])

cluster_summary <- knn2_combined %>%
  group_by(skew.t_cluster) %>%
  summarise(
    n = n(),
    
    # --- CONTINUOUS VARIABLES (Means) ---
    avg_age = mean(age, na.rm = TRUE),
    avg_amount = mean(amount, na.rm = TRUE),
    
    avg_income = mean(income, na.rm = TRUE),
    avg_pol_views = mean(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views., na.rm = TRUE),
    avg_relig_views = mean(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation., na.rm = TRUE),
    avg_work_time1 = mean(WorkTimeInSeconds...1, na.rm = TRUE),
    avg_work_time7 = mean(WorkTimeInSeconds...7, na.rm = TRUE),
    
    # --- CATEGORICAL VARIABLES (Proportions) ---
    # Gender (Example: Proportion Female)
    prop_female = mean(gender == "Female", na.rm = TRUE),
    
    # Politics (Example: Proportion voted for Trump/Clinton)
    prop_voted_trump = mean(grepl("Trump", Q5.In.the.2016.Presidential.election..who.did.you.vote.for.), na.rm = TRUE),
    
    # Religious Attendance (Example: Proportion "Never")
    prop_never_attend = mean(Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. == "Never", na.rm = TRUE),
    
    # Political Identity (Example: Proportion Democrat)
    prop_democrat = mean(grepl("Democrat", Q7.Do.you.consider.yourself.a.), na.rm = TRUE)
  )

# View the result
print(cluster_summary)
```

```{r}
mice1_combined <- cbind(mice1, skew.t_cluster=mice1_num_ord_out[,9])

cluster_summary <- mice1_combined %>%
  group_by(skew.t_cluster) %>%
  summarise(
    n = n(),
    
    # --- CONTINUOUS VARIABLES (Means) ---
    avg_age = mean(age, na.rm = TRUE),
    avg_amount = mean(amount, na.rm = TRUE),
    
    avg_income = mean(income, na.rm = TRUE),
    avg_pol_views = mean(Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views., na.rm = TRUE),
    avg_relig_views = mean(Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation., na.rm = TRUE),
    avg_work_time1 = mean(WorkTimeInSeconds...1, na.rm = TRUE),
    avg_work_time7 = mean(WorkTimeInSeconds...7, na.rm = TRUE),
    
    # --- CATEGORICAL VARIABLES (Proportions) ---
    # Gender (Example: Proportion Female)
    prop_female = mean(gender == "Female", na.rm = TRUE),
    
    # Politics (Example: Proportion voted for Trump/Clinton)
    prop_voted_trump = mean(grepl("Trump", Q5.In.the.2016.Presidential.election..who.did.you.vote.for.), na.rm = TRUE),
    
    # Religious Attendance (Example: Proportion "Never")
    prop_never_attend = mean(Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services. == "Never", na.rm = TRUE),
    
    # Political Identity (Example: Proportion Democrat)
    prop_democrat = mean(grepl("Democrat", Q7.Do.you.consider.yourself.a.), na.rm = TRUE)
  )

# View the result
print(cluster_summary)
