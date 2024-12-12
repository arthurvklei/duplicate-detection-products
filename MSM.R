#Load necessary library
library(stringi)
library(stringdist)
setwd("C:/Users/arthu/OneDrive/Bureaublad/CSBAscalableDupDet")

calculate_matching_model_words_percentage <- function(non_matching_features_1, non_matching_features_2,
                                                      product_1_index, product_2_index, product_features_df) {
  # Get model words for both products using their indices and non-matching features
  model_words_product_1 <- unlist(product_features_df[product_1_index, non_matching_features_1])
  model_words_product_2 <- unlist(product_features_df[product_2_index, non_matching_features_2])
  
  # Remove any NA values from the model words
  model_words_product_1 <- model_words_product_1[!is.na(model_words_product_1)]
  model_words_product_2 <- model_words_product_2[!is.na(model_words_product_2)]
  
  # Find matching model words
  matching_model_words <- intersect(model_words_product_1, model_words_product_2)
  
  # Calculate the total number of unique non-matching model words
  total_non_matching_model_words <- length(unique(c(model_words_product_1, model_words_product_2)))
  
  # Calculate percentage of matching model words
  percentage_matching <- if (total_non_matching_model_words > 0) {
    (length(matching_model_words) / total_non_matching_model_words)
  } else {
    0
  }
  
  # Return only the percentage
  return(percentage_matching)
}


colnames(product_features_df) <- gsub("^featuresmap\\.", "", colnames(product_features_df))
# Replace spaces in column names with underscores
colnames(product_features_df) <- gsub(" ", "_", colnames(product_features_df))

# Initialize a list to store non-NA features for each product
non_na_features_per_product <- vector("list", nrow(product_features_df))

# Function to clean feature values
clean_feature_value <- function(value) {
  if (is.na(value)) {
    return(NA)
  }
  # Convert to lowercase
  value <- tolower(value)
  # Remove all whitespace
  value <- gsub("\\s+", "", value)
  # Remove all non-alphanumeric characters
  value <- gsub("[^a-z0-9]", "", value)
  return(value)
}

# Apply the cleaning function to all cells in the dataframe
product_features_df <- as.data.frame(lapply(product_features_df, function(col) {
  sapply(col, clean_feature_value)
}))

# Sort the candidate_pairs matrix by the first column
candidate_pairs <- candidate_pairs[order(candidate_pairs[, 1]), ]

# Loop through each product and store features with non-NA values
for (i in 1:nrow(product_features_df)) {
  # Extract the non-NA features for the current product
  product_features <- product_features_df[i, ]
  non_na_features <- names(product_features)[!is.na(product_features) & product_features != ""]
  
  # Store the non-NA features for this product
  non_na_features_per_product[[i]] <- non_na_features
}

# Name each list entry for clarity
names(non_na_features_per_product) <- paste0("Product_", 1:nrow(product_features_df))


DissimilarityMatrix <- function(CandidatePairs, gamma, mu, beta, product_features_df, Shop_product, selected_product_features_df, indices) {
  # Create a vector of product numbers from the given indices
  product_numbers <- length(indices)
  
  # Initialize a square matrix with Inf and set the dimnames to the indices
  dissimilarity_matrix <- matrix(Inf, nrow = product_numbers, ncol = product_numbers, dimnames = list(indices, indices))
  
  # Loop through each candidate pair
  for (i in 1:nrow(CandidatePairs)) {
    # Extract product indices
    product_1_index <- CandidatePairs[i, 1]
    product_2_index <- CandidatePairs[i, 2]
    product_1_index_char <- as.character(product_1_index)
    product_2_index_char <- as.character(product_2_index)
    
    # Check if the indices are in the specified range
    if (!(product_1_index_char %in% indices && product_2_index_char %in% indices)) {
      next
    }
    
    # Extract shops and brands
    shop_1 <- Shop_product[product_1_index]
    shop_2 <- Shop_product[product_2_index]
    brand_1 <- selected_product_features_df$brand[product_1_index]
    brand_2 <- selected_product_features_df$brand[product_2_index]
    
    # Skip products from the same shop or with different brands
    if (shop_1 == shop_2 || brand_1 != brand_2) {
      distance_candidatepair <- Inf
      dissimilarity_matrix[product_1_index_char, product_2_index_char] <- distance_candidatepair
      dissimilarity_matrix[product_2_index_char, product_1_index_char] <- distance_candidatepair 
      next
    }
    
    # Initialize variables as described in the paper
    sim <- 0       # Overall similarity
    avgSim <- 0    # Average similarity
    m <- 0         # Number of matches
    w <- 0         # Weight of matches
    # Lists to track unmatched features
    product_1_features <- non_na_features_per_product[[product_1_index]]
    product_2_features <- non_na_features_per_product[[product_2_index]]
    non_matching_features_1 <- product_1_features
    non_matching_features_2 <- product_2_features
    
    # Double loop to compare features and corresponding values
    for (feature_1 in product_1_features) {
      for (feature_2 in product_2_features) {
        # Calculate the feature name similarity
        feature_name_similarity <- stringsim(feature_1, feature_2, method = "jaccard", q = 2)
        
        if (feature_name_similarity > gamma) {
          # Get corresponding values from product_features_df
          value_1 <- as.character(product_features_df[product_1_index, feature_1])
          value_2 <- as.character(product_features_df[product_2_index, feature_2])
          
          # Calculate the corresponding value similarity
          value_similarity <- stringsim(value_1, value_2, method = "jaccard", q = 2)
          
          # Update variables
          sim <- sim + value_similarity
          m <- m + 1
          w <- w + feature_name_similarity
          # Remove matched features from non-matching lists
          non_matching_features_1 <- setdiff(non_matching_features_1, feature_1)
          non_matching_features_2 <- setdiff(non_matching_features_2, feature_2)
        }
      }
    }
    
    if (w > 0) {
      avgSim <- sim / w
    }
    
    # Calculate percentage of matching model words corresponding to non-matching keys
    mwPerc <- calculate_matching_model_words_percentage(non_matching_features_1, non_matching_features_2, product_1_index, product_2_index, product_features_df)
    
    # Calculate the title similarity
    title_similarity <- stringsim(final_titles[product_1_index], final_titles[product_2_index], method = "jaccard", q = 2)
    
    amount_features_product1 <- length(non_na_features_per_product[[product_1_index]])
    amount_features_product2 <- length(non_na_features_per_product[[product_2_index]])
    minFeatures <- min(amount_features_product1, amount_features_product2)
    
    if (title_similarity < beta) {
      theta1 <- m / minFeatures
      theta2 <- 1 - theta1
      hSim <- (theta1 * avgSim) + (theta2 * mwPerc)
    } else {
      theta1 <- (1 - mu) * (m / minFeatures)
      theta2 <- 1 - mu - theta1
      hSim <- (theta1 * avgSim) + (theta2 * mwPerc) + (mu * title_similarity)
    }
    
    distance_candidatepair <- 1 - hSim
    
    # Set the calculated dissimilarity
    dissimilarity_matrix[product_1_index_char, product_2_index_char] <- distance_candidatepair
    dissimilarity_matrix[product_2_index_char, product_1_index_char] <- distance_candidatepair  # Symmetry
    
    
    # Print progress every 1000 iterations
    if (i %% 1000 == 0) {
      print(paste("Iteration:", i))
    }
  }
  
  return(dissimilarity_matrix)
}

# Function to replace NA, NaN, and Inf values with a large finite value
replace_invalid_with_large_value <- function(dissimilarities) {
  # Find the maximum finite value
  max_finite_value <- max(dissimilarities[is.finite(dissimilarities)], na.rm = TRUE)
  large_value <- max_finite_value * 5  # Use a multiplier to ensure it's clearly larger
  
  # Replace Inf, NA, and NaN with the large finite value
  dissimilarities[!is.finite(dissimilarities)] <- large_value
  return(dissimilarities)
}

hierarchicalClustering <- function(cleaned_dissimilarities, eps){
  
  # Convert the dissimilarity matrix to a distance object
  dissimilarity_dist <- as.dist(cleaned_dissimilarities)
  
  # Perform hierarchical clustering with single linkage
  hc <- hclust(dissimilarity_dist, method = "single")

  # Cut the dendrogram at the specified epsilon threshold
  clusters <- cutree(hc, h = eps)
  return(clusters)
}

evaluate_clustering <- function(hclustering_dups, true_duplicates, original_indices) {
  
  # Assign original indices to the clustering results
  names(hclustering_dups) <- original_indices
  
  # Convert the clustering result into pairs of products
  found_pairs <- list()
  cluster_ids <- unique(hclustering_dups)
  
  for (cluster_id in cluster_ids) {
    product_indices <- names(hclustering_dups[hclustering_dups == cluster_id])
    
    if (length(product_indices) > 1) {
      # Generate all unique pairs within the cluster and add them to the list
      cluster_pairs <- combn(product_indices, 2, simplify = TRUE)
      
      # Append each pair as a list to the found_pairs
      for (j in 1:ncol(cluster_pairs)) {
        found_pairs <- append(found_pairs, list(sort(as.numeric(cluster_pairs[, j]))))
      }
    }
  }
  
  # Step 2: Convert lists of pairs to character vectors for easy comparison
  found_pairs_str <- sapply(found_pairs, function(x) paste(x, collapse = "-"))
  true_duplicates_str <- sapply(true_duplicates, function(x) paste(sort(x), collapse = "-"))
  
  # Step 3: Count the number of true duplicate pairs found (Df)
  Df <- sum(found_pairs_str %in% true_duplicates_str)
  
  # Total number of found pairs
  Nc <- length(found_pairs)
  
  # Total number of true duplicate pairs
  Dn <- length(true_duplicates)
  
  # Calculate PQ and PC
  PQ <- ifelse(Nc > 0, Df / Nc, 0)
  PC <- ifelse(Dn > 0, Df / Dn, 0)
  
  # Calculate F1-score
  F1 <- ifelse((PQ + PC) > 0, (2 * PQ * PC) / (PQ + PC), 0)
  
  # Return metrics
  return(list(Pair_Quality = PQ, Pair_Completeness = PC, dups_found = Df, F1MSM = F1))
}