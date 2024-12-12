# Initialize an empty list to store duplicate pairs
true_duplicates <- list()

# Initialize a product counter
product_counter <- 1

# Iterate through the Data list
for (modelID in names(Data)) {
  # Check if the corresponding data frame has more than one row
  if (nrow(Data[[modelID]]) > 1) {
    # Extract the row indices for the current data frame
    product_indices <- seq(product_counter, by = 1, length.out = nrow(Data[[modelID]]))
    
    # Generate all combinations of product indices (pairs)
    duplicate_pairs <- combn(product_indices, 2, simplify = FALSE)
    
    # Append to the true_duplicates list
    true_duplicates <- append(true_duplicates, duplicate_pairs)
  }
  
  # Update the product counter to account for the rows processed
  product_counter <- product_counter + nrow(Data[[modelID]])
}

# Convert the list of duplicate pairs to a matrix
true_duplicates_matrix <- do.call(rbind, true_duplicates)

perform_lsh <- function(signature_matrix, bands, rows_per_band) {
  # Get the actual product indices from the column names of the signature matrix
  product_indices <- as.numeric(colnames(signature_matrix))
  num_products <- length(product_indices)
  candidate_pairs <- list()
  
  for (band in 1:bands) {
    # Extract the rows for the current band
    start_row <- (band - 1) * rows_per_band + 1
    end_row <- band * rows_per_band
    band_matrix <- signature_matrix[start_row:end_row, , drop = FALSE]
    
    # Create a hash table for this band
    hash_table <- list()
    for (product_index in 1:num_products) {
      # Get the product's actual index
      actual_product_index <- product_indices[product_index]
      
      # Hash the band vector for the current product
      band_vector <- band_matrix[, product_index]
      hash_key <- paste(band_vector, collapse = "-")
      
      # Add the product to the corresponding bucket
      if (!hash_key %in% names(hash_table)) {
        hash_table[[hash_key]] <- c()
      }
      hash_table[[hash_key]] <- c(hash_table[[hash_key]], actual_product_index)
    }
    
    # Identify candidate pairs in each bucket
    for (bucket in hash_table) {
      if (length(bucket) > 1) {
        for (i in 1:(length(bucket) - 1)) {
          for (j in (i + 1):length(bucket)) {
            pair <- sort(c(bucket[i], bucket[j]))
            candidate_pairs <- append(candidate_pairs, list(pair))
          }
        }
      }
    }
  }
  
  # Return unique candidate pairs
  unique(do.call(rbind, candidate_pairs))
}

# Evaluation function for candidate pairs
evaluate_lsh <- function(candidate_pairs, true_duplicates_matrix, n) {
  # Convert true_duplicates and candidate_pairs to sets for comparison
  true_pairs_set <- apply(true_duplicates_matrix, 1, function(x) paste(sort(x), collapse = "_"))
  candidate_pairs_set <- apply(candidate_pairs, 1, function(x) paste(sort(x), collapse = "_"))
  
  # Calculate Df: the number of duplicates found in candidate pairs
  Df <- sum(candidate_pairs_set %in% true_pairs_set)
  
  # Calculate Nc: the total number of candidate pairs
  Nc <- nrow(candidate_pairs)
  
  # Calculate Dn: the total number of true duplicates
  Dn <- nrow(true_duplicates_matrix)
  
  # Metrics
  PQ <- if (Nc > 0) Df / Nc else 0  # Avoid division by zero
  PC <- if (Dn > 0) Df / Dn else 0  # Avoid division by zero
  
  F1 <- ((2 * PQ * PC) / (PQ + PC))
  
  N <- n
  
  total_possible_comparisons <- choose(N, 2)
  
  # Fraction of comparisons
  fraction_of_comparisons <- Nc / total_possible_comparisons
  
  # Return the metrics as a list
  return(list(Pair_Quality = PQ, Pair_Completeness = PC, Duplicates_Found = Df, Total_Candidate_Pairs = Nc, Total_True_Duplicates = Dn, fraction_of_comparisons = fraction_of_comparisons, F1lsh = F1))
}