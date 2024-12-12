# Initialize a list to store model words for each product
model_words_per_product <- vector("list", nrow(selected_product_features_df))

# Iterate through each product (row)
for (i in 1:nrow(selected_product_features_df)) {
  # Extract the non-NA feature values for the current product
  product_features <- selected_product_features_df[i, ]
  non_na_features <- product_features[!is.na(product_features) & product_features != ""]
  
  # Create model words by combining cleaned feature names and their corresponding non-NA values
  product_model_words <- paste0(colnames(selected_product_features_df)[!is.na(product_features) & product_features != ""], "_", non_na_features)
  
  # Store the model words for this product
  model_words_per_product[[i]] <- product_model_words
}

# Name each list entry with its product index for clarity
names(model_words_per_product) <- paste0("Product_", 1:nrow(selected_product_features_df))

# Ensure rows in the binary matrix are sorted as in all_model_words
sorted_model_words <- sort(all_model_words)

# Initialize the binary matrix with sorted model words as rows
binary_matrix <- matrix(0, nrow = length(sorted_model_words), ncol = length(model_words_per_product),
                        dimnames = list(sorted_model_words, names(model_words_per_product)))

# Fill the matrix
for (product_index in seq_along(model_words_per_product)) {
  # Get the model words for the current product
  product_words <- model_words_per_product[[product_index]]
  
  # Mark corresponding rows for the current product as 1
  binary_matrix[product_words, product_index] <- 1
}

# View a portion of the matrix for verification
binary_matrix[1:10, 1:5]

