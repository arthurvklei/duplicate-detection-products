# duplicate-detection-products

In this project I perform LSH and MSM on television data from 4 different webshops.

In ModelWords we perform multiple procedures to finally extract all unique model words. 
We clean the key-value pairs and titles. We count the frquency of the keys and select high frequent keys and clean special cases among values for these keys.
We combine a special case for screen size class and screen size, where we round the screen size values.

In BinaryVectorRep we use the model words to create binary vector representations for each product.

In Minhashing we create the signature matrix and determine the number of permutations.

In LSH we have two functions, perform_LSH obtains the candidate pairs and evaluate_lsh() calculates the evaluation metrics for LSH

In MSM we have 4 functions, DissimilarityMatrix() calculates the dissimilarity matrix, calculate_matching_model_words_percentage calculates part 2 of the similarity function and is used in the DissimilarityMatrix(). hierarchicalClustering performs the clustering. evaluate_clustering calculates the metrics for MSM.

In Main we run the bootstraps and split the data into training and test data and call all the functions and store the resullts.
