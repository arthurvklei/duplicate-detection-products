install.packages("gmp")
library(gmp)

# Set seed for reproducibility
set.seed(42)

# Number of permutations
num_hashes <- 90

# Initialize signature matrix with Inf
signature_matrix <- matrix(Inf, nrow = num_hashes, ncol = ncol(binary_matrix))

# Generate MinHashes
set.seed(42) # For reproducibility
for (i in 1:num_hashes) {
  # Generate a random permutation of row indices
  permutation <- sample(1:nrow(binary_matrix))
  
  # Iterate over each product (column)
  for (product in 1:ncol(binary_matrix)) {
    # Find the first row in the permutation where the binary matrix has a 1
    for (row in permutation) {
      if (binary_matrix[row, product] == 1) {
        signature_matrix[i, product] <- row
        break # Stop after finding the first 1
      }
    }
  }
}


# Function to calculate Jaccard similarity between two product vectors
jaccard_similarity <- function(vector1, vector2) {
  # Ensure the vectors are binary (contain only 0s and 1s)
  if (!all(vector1 %in% c(0, 1)) || !all(vector2 %in% c(0, 1))) {
    stop("Both vectors must be binary (0s and 1s).")
  }
  
  # Calculate the intersection and union of the two vectors
  intersection <- sum(vector1 & vector2)  # Logical AND
  union <- sum(vector1 | vector2)         # Logical OR
  
  # If the union is 0 (both vectors are empty), return 0
  if (union == 0) return(0)
  
  # Calculate and return Jaccard similarity
  return(intersection / union)
}

# Function to calculate Signature Similarity between two signature vectors
signature_similarity <- function(signature1, signature2) {
  # Check if the vectors have the same length
  if (length(signature1) != length(signature2)) {
    stop("Signature vectors must have the same length.")
  }
  
  # Calculate the number of matching elementss
  matches <- sum(signature1 == signature2)
  
  # Calculate the total number of elements in the signature
  total_elements <- length(signature1)
  
  # Calculate and return the similarity
  return(matches / total_elements)
}

