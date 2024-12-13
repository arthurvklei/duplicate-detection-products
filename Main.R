#BOOTSTRAPPING
# Split true duplicates into training and test sets
split_true_duplicates <- function(true_duplicates, true_duplicates_matrix, train_indices, test_indices) {
  # Ensure indices are sorted
  train_indices <- sort(train_indices)
  test_indices <- sort(test_indices)
  
  # Create lists to store results
  train_true_duplicates <- list()
  test_true_duplicates <- list()
  train_true_duplicates_matrix <- matrix(nrow = 0, ncol = 2)
  test_true_duplicates_matrix <- matrix(nrow = 0, ncol = 2)
  
  # Loop through true duplicates
  for (i in seq_along(true_duplicates)) {
    pair <- true_duplicates[[i]]
    matrix_pair <- true_duplicates_matrix[i, ]
    
    # Check if both products in the pair are in training set
    if (all(pair %in% train_indices)) {
      train_true_duplicates <- append(train_true_duplicates, list(pair))
      train_true_duplicates_matrix <- rbind(train_true_duplicates_matrix, matrix_pair)
    }
    # Check if both products in the pair are in test set
    else if (all(pair %in% test_indices)) {
      test_true_duplicates <- append(test_true_duplicates, list(pair))
      test_true_duplicates_matrix <- rbind(test_true_duplicates_matrix, matrix_pair)
    }
  }
  
  # Return results
  list(
    train_true_duplicates = train_true_duplicates,
    test_true_duplicates = test_true_duplicates,
    train_true_duplicates_matrix = train_true_duplicates_matrix,
    test_true_duplicates_matrix = test_true_duplicates_matrix
  )
}

set.seed(42)  # Ensure reproducibility
num_bootstraps <- 5
num_products <- ncol(signature_matrix)

train_size <- 1023  # Number of products for the training set
test_size <- num_products - train_size  # Remaining products for the test set

# Initialize a list to store bootstrap results
bootstrap_results <- list()

# Loop through each bootstrap iteration
for (bootstrap in 1:num_bootstraps) {
  cat("\nStarting Bootstrap:", bootstrap, "\n")
  
  # Step 1: Create training and test indices
  train_indices <- sort(sample(1:num_products, train_size, replace = FALSE))
  test_indices <- sort(setdiff(1:num_products, train_indices))
  
  # Create training and test signature matrices
  train_signature_matrix <- signature_matrix[, train_indices, drop = FALSE]
  test_signature_matrix <- signature_matrix[, test_indices, drop = FALSE]
  
  # Update column names to product indices
  colnames(train_signature_matrix) <- train_indices
  colnames(test_signature_matrix) <- test_indices
  
  # Split true duplicates
  split_results <- split_true_duplicates(true_duplicates, true_duplicates_matrix, train_indices, test_indices)
  
  train_true_duplicates <- split_results$train_true_duplicates
  test_true_duplicates <- split_results$test_true_duplicates
  train_true_duplicates_matrix <- split_results$train_true_duplicates_matrix
  test_true_duplicates_matrix <- split_results$test_true_duplicates_matrix
  
  # Step 2: Grid Search for Best b and r
  b_values <- c(18, 15, 10, 9, 6, 5, 3, 2, 1)
  r_values <- c(90, 45, 30, 18, 15, 10, 9, 6, 5)
  
  valid_combinations <- expand.grid(b = b_values, r = r_values)
  valid_combinations <- valid_combinations[valid_combinations$b * valid_combinations$r == 90, ]
  
  best_b <- NULL
  best_r <- NULL
  best_F1_LSH <- -Inf
  
 lsh_msm_results <- list()
  
  for (i in 1:nrow(valid_combinations)) {
    b <- valid_combinations$b[i]
    r <- valid_combinations$r[i]
    
    cat("Running LSH for b =", b, "and r =", r, "\n")
    
    train_candidate_pairs <- perform_lsh(train_signature_matrix, b, r)
    train_results_lsh <- evaluate_lsh(train_candidate_pairs, train_true_duplicates_matrix, length(train_indices))
    
    # Calculate the Dissimilarity Matrix and Clean it
    dissimilarity_matrix_train <- DissimilarityMatrix(train_candidate_pairs, 0.75, 0.7, 0.5, 
                                                      product_features_df, Shop_product, 
                                                      selected_product_features_df, train_indices)
    cleaned_dissimilarity_matrix_train <- replace_invalid_with_large_value(dissimilarity_matrix_train)
    
    # Grid search for epsilon
    eps_values <- seq(0.1, 1.0, by = 0.1)
    f1_scores <- numeric(length(eps_values))
    
    for (j in seq_along(eps_values)) {
      eps <- eps_values[j]
      
      # Perform hierarchical clustering
      train_clusters <- hierarchicalClustering(cleaned_dissimilarity_matrix_train, eps)
      
      # Evaluate clustering
      train_clustermetrics <- evaluate_clustering(train_clusters, train_true_duplicates, train_indices)
      
      # Store the F1 score
      f1_scores[j] <- train_clustermetrics$F1MSM
    }
    
    # Find the best epsilon with the highest F1 score
    best_eps <- eps_values[which.max(f1_scores)]
    best_f1_msm <- max(f1_scores)
    print(best_eps)
    print(best_f1_msm)
    
    # Store the results for this combination of b, r, and epsilon
    lsh_msm_results[[paste0("b", b, "_r", r)]] <- list(
      b = b,
      r = r,
      Best_Epsilon = best_eps,
      F1_LSH = train_lsh_metrics$F1lsh,
      F1_MSM = best_f1_msm,
      Pair_Quality = train_lsh_metrics$Pair_Quality,
      Pair_Completeness = train_lsh_metrics$Pair_Completeness,
      Fraction_of_Comparisons = train_lsh_metrics$fraction_of_comparisons
    )
  }
    
    # Perform LSH
    #train_candidate_pairs <- perform_lsh(train_signature_matrix, b, r)
    
    # Evaluate LSH
    #train_lsh_metrics <- evaluate_lsh(train_candidate_pairs, train_true_duplicates_matrix, length(train_indices))
    #print(train_lsh_metrics)
    
    # Check if this is the best F1_LSH so far
    #if (train_lsh_metrics$F1lsh > best_F1_LSH) {
     # best_b <- b
      #best_r <- r
      #best_F1_LSH <- train_lsh_metrics$F1lsh
    #}
  
  
  #cat("Best combination of b =", best_b, ", r =", best_r, "with F1_LSH =", best_F1_LSH,  "\n")
  #best_train_lsh <- perform_lsh(train_signature_matrix, best_b, best_r)
  #best_train_lsh_results <- evaluate_lsh(best_train_lsh, train_true_duplicates_matrix, length(train_indices))
  
  # Step 3: Perform LSH with the best parameters
 # train_candidate_pairs_best <- perform_lsh(train_signature_matrix, best_b, best_r)
  
  # Step 4: Calculate Dissimilarity Matrix and Clean it
  #dissimilarity_matrix_train <- DissimilarityMatrix(train_candidate_pairs_best, 0.75, 0.7, 0,
   #                                                 product_features_df, Shop_product, selected_product_features_df, train_indices)
  #cleaned_dissimilarity_matrix_train <- replace_invalid_with_large_value(dissimilarity_matrix_train)
  
  # Step 5: Grid Search for Best Epsilon
  #eps_values <- seq(0.1, 1.0, by = 0.1)
  #f1_scores <- numeric(length(eps_values))
  
  #for (i in seq_along(eps_values)) {
   # eps <- eps_values[i]
    
    #train_clusters <- hierarchicalClustering(cleaned_dissimilarity_matrix_train, eps)
    #train_clustermetrics <- evaluate_clustering(train_clusters, train_true_duplicates, train_indices)
    
    #f1_scores[i] <- train_clustermetrics$F1MSM
  #}
  
  #best_eps <- eps_values[which.max(f1_scores)]
  #cat("Best epsilon =", best_eps, "with F1 score =", max(f1_scores), "\n")
  
  # Step 6: Perform LSH and MSM on the Test Data
  #test_candidate_pairs_best <- perform_lsh(test_signature_matrix, best_b, best_r)
  #test_results_lsh <- evaluate_lsh(test_candidate_pairs_best, test_true_duplicates_matrix, length(test_indices))
  
  #dissimilarity_matrix_test <- DissimilarityMatrix(test_candidate_pairs_best, 0.75, 0.7, 0,
   #                                                product_features_df, Shop_product, selected_product_features_df, test_indices)
  #cleaned_dissimilarity_matrix_test <- replace_invalid_with_large_value(dissimilarity_matrix_test)
  
  #test_clusters_best <- hierarchicalClustering(cleaned_dissimilarity_matrix_test, best_eps)
  #test_clustermetrics <- evaluate_clustering(test_clusters_best, test_true_duplicates, test_indices)
  
  # Step 7: Store Results for This Bootstrap
  #bootstrap_results[[bootstrap]] <- list(
    #best_b = best_b,
    #best_r = best_r,
    #best_eps = best_eps,
    #train_LSH_results = best_train_lsh_results,
    #test_LSH_results = test_results_lsh
    #test_MSM_results = test_clustermetrics
  #)
  # Store the results for this bootstrap iteration
  bootstrap_results[[paste0("Bootstrap_", bootstrap)]] <- lsh_msm_results
}

# Print the bootstrap results
print(bootstrap_results)

# Step 1: Compute the Average Epsilon for Each Combination of b and r
average_epsilons <- list()

# Loop through each combination of b and r
for (combination in unique(unlist(lapply(bootstrap_results, names)))) {
  epsilons <- sapply(bootstrap_results, function(x) x[[combination]]$Best_Epsilon)
  avg_eps <- mean(epsilons)
  
  average_epsilons[[combination]] <- avg_eps
}

set.seed(42)  # Ensure reproducibility
num_bootstraps <- 5
num_products <- ncol(signature_matrix)

train_size <- 1023  # Number of products for the training set
test_size <- num_products - train_size  # Remaining products for the test set

# Initialize a list to store test results for each bootstrap
all_bootstrap_test_results <- list()

for (bootstrap in 1:num_bootstraps) {
  cat("\nStarting Bootstrap:", bootstrap, "\n")
  
  # Step 1: Create training and test indices
  train_indices <- sort(sample(1:num_products, train_size, replace = FALSE))
  test_indices <- sort(setdiff(1:num_products, train_indices))
  
  # Create training and test signature matrices
  train_signature_matrix <- signature_matrix[, train_indices, drop = FALSE]
  test_signature_matrix <- signature_matrix[, test_indices, drop = FALSE]
  
  # Update column names to product indices
  colnames(train_signature_matrix) <- train_indices
  colnames(test_signature_matrix) <- test_indices
  
  # Split true duplicates
  split_results <- split_true_duplicates(true_duplicates, true_duplicates_matrix, train_indices, test_indices)
  
  train_true_duplicates <- split_results$train_true_duplicates
  test_true_duplicates <- split_results$test_true_duplicates
  train_true_duplicates_matrix <- split_results$train_true_duplicates_matrix
  test_true_duplicates_matrix <- split_results$test_true_duplicates_matrix
  
  # Step 2: Grid Search for Best b and r
  b_values <- c(18, 15, 10, 9, 6, 5, 3, 2, 1)
  r_values <- c(90, 45, 30, 18, 15, 10, 9, 6, 5)
  
  valid_combinations <- expand.grid(b = b_values, r = r_values)
  valid_combinations <- valid_combinations[valid_combinations$b * valid_combinations$r == 90, ]
  
  # Initialize a list to store results for each combination of b and r
  bootstrap_test_results <- list()
  
  for (i in 1:nrow(valid_combinations)) {
    b <- valid_combinations$b[i]
    r <- valid_combinations$r[i]
    
    cat("Running LSH for b =", b, "and r =", r, "\n")
    
    # Perform LSH on the test data
    test_candidate_pairs <- perform_lsh(test_signature_matrix, b, r)
    
    # Evaluate LSH on the test data
    test_lsh_metrics <- evaluate_lsh(test_candidate_pairs, test_true_duplicates_matrix, length(test_indices))
    
    # Calculate the dissimilarity matrix and clean it
    dissimilarity_matrix_test <- DissimilarityMatrix(test_candidate_pairs, 0.75, 0.7, 0.5, 
                                                     product_features_df, Shop_product, 
                                                     selected_product_features_df, test_indices)
    cleaned_dissimilarity_matrix_test <- replace_invalid_with_large_value(dissimilarity_matrix_test)
    
    # Perform hierarchical clustering using the average epsilon from the training phase
    avg_eps <- average_epsilons[[paste0("b", b, "_r", r)]]
    
    test_clusters <- hierarchicalClustering(cleaned_dissimilarity_matrix_test, avg_eps)
    
    # Evaluate MSM on the test data
    test_msm_metrics <- evaluate_clustering(test_clusters, test_true_duplicates, test_indices)
    
    # Store the results for this combination
    bootstrap_test_results[[paste0("b", b, "_r", r)]] <- list(
      b = b,
      r = r,
      Avg_Epsilon = avg_eps,
      Pair_Quality = test_lsh_metrics$Pair_Quality,
      Pair_Completeness = test_lsh_metrics$Pair_Completeness,
      F1_LSH = test_lsh_metrics$F1lsh,
      Fraction_of_Comparisons = test_lsh_metrics$fraction_of_comparisons,
      F1_MSM = test_msm_metrics$F1MSM,
      MSM_Pair_Quality = test_msm_metrics$Pair_Quality,
      MSM_Pair_Completeness = test_msm_metrics$Pair_Completeness
    )
  }
  
  # Store the test results for this bootstrap iteration
  all_bootstrap_test_results[[paste0("Bootstrap_", bootstrap)]] <- bootstrap_test_results
}

# Step 3: Aggregate Results Across All Bootstraps
# Initialize a data frame to store aggregated results
results_list <- list()

for (combination in unique(unlist(lapply(all_bootstrap_test_results, names)))) {
  # Extract metrics for each bootstrap
  metrics <- lapply(all_bootstrap_test_results, function(x) x[[combination]])
  
  # Compute the average of each metric
  avg_results <- data.frame(
    Combination = combination,
    b = metrics[[1]]$b,
    r = metrics[[1]]$r,
    Avg_Epsilon = mean(sapply(metrics, function(x) x$Avg_Epsilon)),
    Avg_Pair_Quality = mean(sapply(metrics, function(x) x$Pair_Quality)),
    Avg_Pair_Completeness = mean(sapply(metrics, function(x) x$Pair_Completeness)),
    Avg_F1_LSH = mean(sapply(metrics, function(x) x$F1_LSH)),
    Avg_Fraction_of_Comparisons = mean(sapply(metrics, function(x) x$Fraction_of_Comparisons)),
    Avg_F1_MSM = mean(sapply(metrics, function(x) x$F1_MSM)),
    Avg_MSM_Pair_Quality = mean(sapply(metrics, function(x) x$MSM_Pair_Quality)),
    Avg_MSM_Pair_Completeness = mean(sapply(metrics, function(x) x$MSM_Pair_Completeness))
  )
  
  results_list[[combination]] <- avg_results
}

# Combine all results into a single data frame
final_test_results <- do.call(rbind, results_list)

# Display the final aggregated results
print(final_test_results)
