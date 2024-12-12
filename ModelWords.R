setwd("C:/Users/arthu/OneDrive/Bureaublad/CSBAscalableDupDet")
library(jsonlite)
library(dplyr)
library(stringr)


#load data
Data <- fromJSON("C:/Users/arthu/OneDrive/Bureaublad/CSBAscalableDupDet/Functions/TVs-all-merged.json", flatten = TRUE)

#number of products <- 1624
#unique products <- 1262

  
  # Initialize vectors to store titles, shops, and model IDs
  Title_product <- c()
  Shop_product  <- c()
  Model_ID_product <- c()
  
  # Iterate through each product
  for (i in 1:length(Data)) {
    # Get the number of duplicates (rows) for the current product
    num_duplicates <- dim(Data[[i]])[1]
    
    # Loop through each duplicate (row) and extract data
    for (j in 1:num_duplicates) {
      Title_product <- c(Title_product, Data[[i]]$title[j])
      Shop_product <- c(Shop_product, Data[[i]]$shop[j])
      Model_ID_product <- c(Model_ID_product, Data[[i]]$modelID[j])
    }
  }
  
  Model_ID_product <- tolower(Model_ID_product)
  
  # Extract all unique features from the dataset
  features_location <- 5
  
  # Get all unique features across products
  unique_features <- unique(unlist(lapply(Data, function(product) {
    # Loop through each duplicate for the product and extract feature names
    do.call(c, lapply(1:dim(product)[1], function(row) names(product[features_location:length(product)][row, ])))
  })))
  
  # Normalize unique_features for consistency
  unique_features <- tolower(trimws(unique_features))
  
  # Initialize an empty data frame with columns as features
  product_features_df <- data.frame(matrix(NA, nrow = 0, ncol = length(unique_features)))
  
  # Iterate through each product to populate the data frame
  for (i in 1:length(Data)) {
    # Determine the number of duplicates (rows) for the current product
    num_duplicates <- dim(Data[[i]])[1]
    
    # Loop through duplicates
    for (j in 1:num_duplicates) {
      # Extract features and values for the current duplicate
      features <- tolower(trimws(names(Data[[i]][features_location:length(Data[[i]])][j, ])))
      values <- Data[[i]][features_location:length(Data[[i]])][j, ]
      
      # Create a named vector of feature values
      feature_values <- setNames(as.character(values), features)
      
      # Create a row with NA values for all features
      row <- rep(NA, length(unique_features))
      names(row) <- unique_features
      
      # Match and populate the row with feature values
      matched_features <- intersect(features, unique_features)
      row[matched_features] <- feature_values[matched_features]
      
      # Add the row to the data frame
      product_features_df <- rbind(product_features_df, row)
    }
  }
  colnames(product_features_df) <- unique_features
  
  
  # Define a mapping of redundant features to unified feature names
  feature_mapping <- list(
    "featuresmap.maximum resolution" = c("featuresmap.maximum resolution", "featuresmap.maximum resolution:"),
    "featuresmap.aspect ratio" = c("featuresmap.aspect ratio", "featuresmap.aspect ratio:", "featuresmap.image aspect ratio"),
    "featuresmap.brand" = c("featuresmap.brand", "featuresmap.brand name", "featuresmap.brand name:"),
    "featuresmap.screen size" = c("featuresmap.screen size", "featuresmap.screen size (measured diagonally)", "featuresmap.screen size:"),
    "featuresmap.usb port" = c("featuresmap.usb port", "featuresmap.usb input"),
    "featuresmap.ethernet port" = c("featuresmap.ethernet port", "featuresmap.ethernet:"),
    "featuresmap.screen refresh rate" = c("featuresmap.screen refresh rate", "featuresmap.refresh rate"),
    "featuresmap.speakers" = c("featuresmap.speakers", "featuresmap.speakers:"),
    "featuresmap.energy star certified" = c(
      "featuresmap.energy star certified", 
      "featuresmap.energy star compliant", 
      "featuresmap.energy star qualified"
    ),
    "featuresmap.component video" = c("featuresmap.component video", "featuresmap.component video inputs", "featuresmap.component video:"),
    "featuresmap.speaker output power" = c("featuresmap.speaker output power", "featuresmap.output power"),
    "featuresmap.width" = c("featuresmap.width", "featuresmap.product width", "featuresmap.width:"),
    "featuresmap.warranty terms - parts" = c("featuresmap.warranty terms - parts", "featuresmap.warranty term - parts")
  )
  
  # Combine redundant features into unified columns
  for (target_feature in names(feature_mapping)) {
    # Get the list of redundant features for this target feature
    redundant_features <- feature_mapping[[target_feature]]
    
    # Combine values into the target feature column
    product_features_df[[target_feature]] <- Reduce(function(x, y) {
      ifelse(!is.na(x), x, y)  # Keep existing values or use the next column's values
    }, product_features_df[redundant_features])
    
    # Remove the redundant columns, except the new combined column
    product_features_df <- product_features_df[, !colnames(product_features_df) %in% redundant_features[-1]]
  }
  
  # Function to clean and normalize screen size values
  clean_screen_size <- function(size) {
    # Case 1: Handle NA values
    if (is.na(size)) return(NA)
    
    # Normalize spaces in the string
    size <- str_squish(size)  # Removes extra spaces
    
    # Case 2: Standard sizes starting with an integer and a quote (e.g., "58\"", "64.5\"")
    if (str_detect(size, "^\\d+\\.?\\d*\"$")) {
      return(size)  # Return as-is
    }
    
    # Case 3: Fractional sizes (e.g., "28-1/2\"")
    if (str_detect(size, "\\d+-\\d+/\\d+")) {
      # Convert fraction to decimal
      fraction <- str_extract(size, "\\d+-\\d+/\\d+")
      decimal_value <- eval(parse(text = gsub("-", "+", fraction)))
      return(paste0(ceiling(decimal_value), "\""))  # Round up and append inches
    }
    
    # Case 4: Sizes with nested details (e.g., "51\" Class (50.7\" Diag.)")
    if (str_detect(size, "^\\d+\"")) {
      # Extract the integer part before the first quote
      primary_size <- str_extract(size, "^\\d+")
      return(paste0(primary_size, "\""))  # Return the primary size
    }
    
    # Failsafe: Return original value if no patterns match
    return(size)
  }
  
  product_features_df$`featuresmap.screen size` <- sapply(
    product_features_df$`featuresmap.screen size`, 
    clean_screen_size
  )
  
  clean_special_cases <- function(size) {
    # Handle NA values
    if (is.na(size)) return(NA)
    
    # Normalize spaces
    size <- str_squish(size)
    
    # Case 1: Values starting with double single quotes (e.g., "32' ' Class (31.5\" Diag.)")
    if (str_detect(size, "^\\d+' '")) {
      primary_size <- str_extract(size, "^\\d+")
      return(paste0(primary_size, "\""))
    }
    
    # Case 2: Values starting with an integer but no quote (e.g., "47 Class (46.9\" Actual size)")
    if (str_detect(size, "^\\d+\\s")) {
      primary_size <- str_extract(size, "^\\d+")
      return(paste0(primary_size, "\""))
    }
    
    # Failsafe: Return original value if no patterns match
    return(size)
  }
  
  product_features_df$`featuresmap.screen size` <- sapply(
    product_features_df$`featuresmap.screen size`, 
    clean_special_cases
  )
  
  # Combine 'featuresmap.screen size' into 'featuresmap.screen size class'
  product_features_df$`featuresmap.screen size class` <- ifelse(
    is.na(product_features_df$`featuresmap.screen size class`), 
    product_features_df$`featuresmap.screen size`,  # Use Screen Size if Screen Size Class is NA
    product_features_df$`featuresmap.screen size class`  # Keep existing Screen Size Class values
  )
  
  # Remove the 'featuresmap.screen size' column if no longer needed
  product_features_df$`featuresmap.screen size` <- NULL
  
  # Count non-NA values for each feature in the dataset
  non_na_counts <- colSums(!is.na(product_features_df))
  
  # Convert the result into a data frame
  non_na_features_df <- data.frame(
    Feature = names(non_na_counts),
    NonNA_Count = non_na_counts,
    row.names = NULL
  )
  
  # Sort the data frame by the count of non-NA values in descending order
  non_na_features_df <- non_na_features_df[order(-non_na_features_df$NonNA_Count), ]
  
  # Count unique non-NA values for each feature
  unique_non_na_counts <- sapply(product_features_df, function(column) {
    length(unique(column[!is.na(column)]))
  })
  
  # Convert the result into a data frame
  unique_non_na_features_df <- data.frame(
    Feature = names(unique_non_na_counts),
    Unique_NonNA_Values = unique_non_na_counts,
    row.names = NULL
  )
  
  # Sort unique_non_na_features_df to match the order of non_na_features_df
  unique_non_na_features_df <- unique_non_na_features_df[match(non_na_features_df$Feature, unique_non_na_features_df$Feature), ]
  
  # Merge non_na_features_df and unique_non_na_features_df by "Feature"
  metrics_df <- merge(
    non_na_features_df, 
    unique_non_na_features_df, 
    by = "Feature"
  )
  
  # Rename columns for clarity (if needed)
  colnames(metrics_df) <- c("Feature", "NonNA_Count", "Unique_NonNA_Values")
  
  # Calculate additional metrics
  total_products <- nrow(product_features_df)  # Total number of products
  metrics_df <- metrics_df %>%
    mutate(
      Frequency_Ratio = NonNA_Count / total_products,  # Ratio of products with non-NA values
      Distinct_Value_Ratio = Unique_NonNA_Values / NonNA_Count  # Proportion of distinct values
    )
  
  # Define thresholds for feature selection
  frequency_threshold <- 0.39  # At least 39% of products have the feature
  distinct_value_threshold <- 0.05  # Fewer than 5% distinct values relative to non-NA count
  
  # Filter features based on thresholds
  selected_features <- metrics_df %>%
    filter(Frequency_Ratio >= frequency_threshold & Distinct_Value_Ratio <= distinct_value_threshold)
  
  selected_product_features_df <- product_features_df[, selected_features$Feature]
  
  # Convert all values in the selected_product_features_df to lowercase
  selected_product_features_df <- as.data.frame(
    apply(selected_product_features_df, 2, function(x) {
      if (is.character(x)) tolower(x) else x  # Convert to lowercase if the column is character
    })
  )
  
  colnames(selected_product_features_df) <- gsub("^featuresmap\\.", "", colnames(selected_product_features_df))
  # Replace spaces in column names with underscores
  colnames(selected_product_features_df) <- gsub(" ", "_", colnames(selected_product_features_df))
  
  # Function to clean and normalize Aspect Ratio values
  clean_aspect_ratio <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Normalize valid aspect ratios
    value <- str_replace_all(value, "\\b16:09\\b", "16:9")
    value <- str_replace_all(value, "\\b4:3 and 16:9\\b", "4:3|16:9")
    value <- str_replace_all(value, "\\b4:3, 14:9 and 16:9\\b", "4:3|14:9|16:9")
    value <- str_replace_all(value, "\\b4:3 and 16:10\\b", "4:3|16:10")
    
    # Remove invalid or corrupted values
    if (value %in% c("1899-12-31t16:09:00.000", "0.16875", "0:0", "40000:1")) {
      return(NA)
    }
    
    # Normalize combined ratios by sorting them
    if (str_detect(value, "\\|")) {
      ratios <- unlist(str_split(value, "\\|"))
      value <- paste(sort(ratios), collapse = "|")
    }
    
    # Final cleanup to normalize spacing
    value <- str_squish(value)
    
    return(value)
  }
  
  # Apply the cleaning function to the Aspect Ratio column
  selected_product_features_df$`aspect_ratio` <- sapply(
    selected_product_features_df$`aspect_ratio`, 
    clean_aspect_ratio
  )
  
  # Function to clean component video values
  clean_component_video <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Normalize clear numeric values (e.g., "1", "2 in", "3 in")
    if (str_detect(value, "^\\d+(\\s+in)?$")) {
      return(str_extract(value, "^\\d+"))  # Extract the numeric part
    }
    
    # Normalize shared or hybrid inputs
    if (str_detect(value, "shared|hybrid|common|combo")) {
      count <- str_extract(value, "\\d+")
      return(ifelse(!is.na(count), paste0(count, " (shared)"), "shared"))
    }
    
    # Normalize RCA descriptions to numeric
    if (str_detect(value, "rca x \\d+")) {
      count <- str_extract(value, "\\d+")
      return(count)
    }
    
    # Normalize positional inputs (e.g., "1 (rear)")
    if (str_detect(value, "\\d+\\s*\\(")) {
      count <- sum(as.numeric(str_extract_all(value, "\\d+")[[1]]))
      return(as.character(count))  # Return the total count
    }
    
    # Handle 'yes' as a single input
    if (value == "yes") return("1")
    
    # Handle invalid or ambiguous values
    if (value %in% c("0", "none")) return("0")
    
    # Failsafe: Return NA for any unclear values
    return(NA)
  }
  
  # Apply the cleaning function to the Component Video column
  selected_product_features_df$`component_video` <- sapply(
    selected_product_features_df$`component_video`, 
    clean_component_video
  )
  # Function to further simplify shared entries
  simplify_shared_entries <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Remove the "(shared)" part and keep the numeric count
    value <- str_replace(value, "\\s*\\(shared\\)", "")
    
    # Return the simplified value
    return(value)
  }
  
  # Apply the simplification function to the Component Video column
  selected_product_features_df$`component_video` <- sapply(
    selected_product_features_df$`component_video`, 
    simplify_shared_entries
  )
  
  # Function to clean Energy Star Certified feature
  clean_energy_star <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Extract version number if it exists
    if (str_detect(value, "\\d+(\\.\\d+)?")) {
      version <- str_extract(value, "\\d+(\\.\\d+)?")
      # Ensure consistent formatting with one decimal place
      version <- format(as.numeric(version), nsmall = 1)
      return(version)
    }
    
    # Normalize "yes" responses (without a version)
    if (str_detect(value, "\\byes\\b")) return("yes")
    
    # Normalize "no" responses
    if (value == "no") return("no")
    
    # Treat "unknown" as NA
    if (value == "unknown") return(NA)
    
    # Handle invalid or unclear entries
    if (value %in% c("cec")) return(NA)
    
    # Default: Mark anything unhandled as NA
    return(NA)
  }
  
  # Apply the cleaning function to the Energy Star Certified column
  selected_product_features_df$`energy_star_certified` <- sapply(
    selected_product_features_df$`energy_star_certified`, 
    clean_energy_star
  )
  
  # Function to clean Maximum Resolution feature
  clean_maximum_resolution <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Remove commas and any extra text like "(native)"
    value <- str_replace_all(value, ",", "")
    value <- str_replace_all(value, "\\s*\\(.*?\\)", "")
    
    # Remove spaces from the resolution (e.g., "1024 x 768" -> "1024x768")
    value <- str_replace_all(value, "\\s", "")
    
    # Check if the value matches the resolution pattern "<width>x<height>"
    if (str_detect(value, "^\\d+x\\d+$")) {
      return(value)  # Valid resolution format
    }
    
    # If not valid, return NA
    return(NA)
  }
  
  # Apply the cleaning function to the Maximum Resolution column
  selected_product_features_df$maximum_resolution <- sapply(
    selected_product_features_df$`maximum_resolution`, 
    clean_maximum_resolution
  )
  
  # Function to clean Screen Refresh Rate feature
  clean_refresh_rate <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Extract numeric refresh rate
    refresh_rate <- str_extract(value, "\\d+hz")
    
    # If a valid refresh rate is found, return it
    if (!is.na(refresh_rate)) return(refresh_rate)
    
    # Default: Mark anything unhandled as NA
    return(NA)
  }
  
  # Apply the cleaning function to the Screen Refresh Rate column
  selected_product_features_df$`screen_refresh_rate` <- sapply(
    selected_product_features_df$`screen_refresh_rate`, 
    clean_refresh_rate
  )
  
  # Function to clean and round Screen Size Class feature
  clean_screen_size_class <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Extract the numeric screen size, allowing for decimals
    size <- str_extract(value, "\\d+(\\.\\d+)?")
    
    # If a valid size is found, round it
    if (!is.na(size)) {
      rounded_size <- ceiling(as.numeric(size))  # Round up values ending in .5
      return(as.character(rounded_size))
    }
    
    # Default: Mark anything unhandled as NA
    return(NA)
  }
  
  # Apply the cleaning function to the Screen Size Class column
  selected_product_features_df$`screen_size_class` <- sapply(
    selected_product_features_df$`screen_size_class`, 
    clean_screen_size_class
  )
  
  clean_speaker_output_power <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Extract all numeric wattages from the value
    wattages <- as.numeric(str_extract_all(value, "\\d+\\.?\\d*")[[1]])
    
    # If wattages are found, sum them up
    if (length(wattages) > 0) {
      total_power <- sum(wattages)
      total_power <- round(total_power)  # Round to the nearest integer
      return(paste0(total_power, "W"))  # Return the total as "<number>W"
    }
    
    # Handle invalid or unrelated entries
    if (str_detect(value, "power consumption")) return(NA)
    
    # Default: Return NA for unhandled cases
    return(NA)
  }
  
  # Apply the cleaning function to the Speaker Output Power column
  selected_product_features_df$`speaker_output_power` <- sapply(
    selected_product_features_df$`speaker_output_power`, 
    clean_speaker_output_power
  )
  # Function to clean TV Type feature
  clean_tv_type <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Remove "flat-panel" from relevant entries
    value <- str_replace(value, "\\s*flat-panel", "")
    
    # Remove spaces in "tv/dvd combo"
    value <- gsub("tv/dvd combo", "tv/dvdcombo", value, ignore.case = TRUE)
    
    # Ensure all values are lowercase
    value <- tolower(value)
    
    return(value)
  }
  
  # Apply the cleaning function to the TV Type column
  selected_product_features_df$tv_type <- sapply(
    selected_product_features_df$tv_type, 
    clean_tv_type
  )
  
  # Function to clean USB Port feature
  clean_usb_port <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Normalize to "yes" for all positive indications
    if (str_detect(value, "yes")) return("yes")
    
    # Normalize "no" as is
    if (str_detect(value, "no")) return("no")
    
    # Treat "not specified" as NA
    if (value == "not specified") return(NA)
    
    # Default: Return NA for unhandled cases
    return(NA)
  }
  
  # Apply the cleaning function to the USB Port column
  selected_product_features_df$`usb_port` <- sapply(
    selected_product_features_df$`usb_port`, 
    clean_usb_port
  )
  
  # Function to clean V-Chip feature
  clean_v_chip <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Set all non-NA values to "yes"
    return("yes")
  }
  
  # Apply the cleaning function to the V-Chip column
  selected_product_features_df$`v-chip` <- sapply(
    selected_product_features_df$`v-chip`, 
    clean_v_chip
  )
  
  # Function to clean Warranty Terms - Parts feature
  clean_warranty_terms <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Extract duration in years or months without spaces
    if (str_detect(value, "\\d+\\s*year")) {
      years <- as.numeric(str_extract(value, "\\d+"))
      return(paste0(years, "year", ifelse(years > 1, "s", "")))  # Remove spaces and handle singular/plural
    }
    if (str_detect(value, "\\d+\\s*month|\\d+\\s*day")) {
      months <- as.numeric(str_extract(value, "\\d+"))
      if (str_detect(value, "day")) {
        months <- ceiling(months / 30)  # Convert days to months (approximate)
      }
      return(paste0(months, "month", ifelse(months > 1, "s", "")))  # Remove spaces and handle singular/plural
    }
    
    # Default: Mark anything unhandled as NA
    return(NA)
  }
  
  # Apply the cleaning function to the Warranty Terms - Parts column
  selected_product_features_df$`warranty_terms_-_parts` <- sapply(
    selected_product_features_df$`warranty_terms_-_parts`, 
    clean_warranty_terms
  )
  
  # Clean Vertical Resolution Feature
  clean_vertical_resolution <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Remove spaces inside parentheses
    value <- gsub("\\s*\\(\\s*", "(", value)
    value <- gsub("\\s*\\)", ")", value)
    
    return(value)
  }
  
  # Apply cleaning to the vertical resolution column
  selected_product_features_df$vertical_resolution <- sapply(selected_product_features_df$vertical_resolution, clean_vertical_resolution)

  # Extracting brands from features
  unique_brands_features <- unique(selected_product_features_df$brand)
  
  # Get the indices of products with no brand
  missing_brand_indices <- which(is.na(selected_product_features_df$`brand`))
  
  # Extract the titles corresponding to these indices from cleaned_titles
  titles_with_missing_brands <- cleaned_titles[missing_brand_indices]

  # Extract potential brands from the titles with missing brands
  missing_brands <- unique(str_extract(tolower(titles_with_missing_brands), "^[a-z]+"))
  
  # Combine with existing unique_brands_features and ensure no duplicates
  updated_brands <- unique(c(unique_brands_features, missing_brands))

  # Iterate over the rows where the brand is NA
  for (i in which(is.na(selected_product_features_df$`brand`))) {
    # Get the title of the product
    title_prod <- cleaned_titles[i]
    
    # Initialize detected brand as NA
    detected_brand <- NA
    
    # Check if any brand in unique_brands_features exists in the title
    for (brand in updated_brands) {
      if (str_detect(title_prod, paste0("\\b", brand, "\\b"))) {
        detected_brand <- brand  # If found, assign the brand
        break  # Stop checking once a match is found
      }
    }
    # Update the brand column with the detected brand
    selected_product_features_df$`brand`[i] <- detected_brand
  }
  
  # Function to clean the Brand feature
  clean_brand <- function(value) {
    if (is.na(value)) return(NA)  # Retain NA as is
    
    # Replace specific patterns with desired values
    value <- gsub("lg electronics", "lg", value, ignore.case = TRUE)
    value <- gsub("jvc tv", "jvc", value, ignore.case = TRUE)
    value <- gsub("hello kitty", "hellokitty", value, ignore.case = TRUE)
    value <- gsub("sceptre inc\\.", "sceptre", value, ignore.case = TRUE)
    
    # Return cleaned value
    return(value)
  }
  
  # Apply the cleaning function to the Brand feature column
  selected_product_features_df$brand <- sapply(selected_product_features_df$brand, clean_brand)
  
  # Function to clean titles
  clean_title <- function(title) {
    # Handle NA values
    if (is.na(title)) return(NA)
    
    # Convert to lowercase
    title <- tolower(title)
    
    # Remove retailer names and unnecessary words
    title <- str_replace_all(title, "\\b(newegg|com|best buy|refurbished)\\b", "")
    
    # Normalize spaces
    title <- str_squish(title)  # Remove extra spaces
    
    # Remove unnecessary long numbers (e.g., "2812\"")
    title <- str_replace_all(title, "\\b\\d{3,}\"\\b", "")
    
    # Normalize 'inch'
    title <- str_replace_all(title, "\\b(inch(es)?|”|\"|-inch| inch)\\b", "inch")
    
    # Normalize 'hz' and 'p'
    title <- str_replace_all(title, "\\b(hertz|hz| hz)\\b", "hz")
    title <- str_replace_all(title, "\\bp\\b", "p")
    
    # Normalize numbers with decimals (e.g., "69 5\"")
    title <- str_replace_all(title, "\\b(\\d+) (\\d+)\"\\b", "\\1.\\2inch")
    
    # Remove non-alphanumeric characters except for spaces, 'inch', and 'hz'
    title <- str_replace_all(title, "[^a-z0-9\\s]", "")
    
    # Final space normalization
    title <- str_squish(title)
    
    return(title)
  }
  
  # Apply cleaning function to Title_product vector
  cleaned_titles <- sapply(Title_product, clean_title, USE.NAMES = FALSE)
  
  unique_tv_types <- unique(selected_product_features_df$tv_type)
  unique_tv_types <- unique_tv_types[!is.na(unique_tv_types)]
  
  # Step 2: Fill missing tv types from titles
  missing_tv_type_indices <- which(is.na(selected_product_features_df$tv_type))
  for (idx in missing_tv_type_indices) {
    title <- cleaned_titles[idx]
    detected <- unique_tv_types[sapply(unique_tv_types, function(val) any(grepl(val, title)))]
    if (length(detected) > 0) {
      selected_product_features_df$tv_type[idx] <- detected[1] # Use the first detected value
    }
  }
  
  # Define the regular expression for model words
  model_word_regex <- "([a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*)"
  
  # Apply the regex to each title in Titles_products
  cleaned_titles1 <- lapply(Title_product, function(title) {
    if (is.na(title)) {
      return(NA)  # Retain NA values as is
    }
    
    # Extract all matches for model words in the title
    matches <- str_extract_all(title, model_word_regex)[[1]]
    
    # Combine matches into a single cleaned title string
    cleaned_title1 <- paste(matches, collapse = " ")
    
    return(cleaned_title1)
  })
  
  # Convert the cleaned titles list back to a character vector
  cleaned_titles1 <- unlist(cleaned_titles1)
  cleaned_titles1 <- tolower(cleaned_titles1)
  
  clean_model_ids <- gsub("[^a-zA-Z0-9]", "", Model_ID_product)
  
  # Define a function to clean the titles
  clean_titles_further <- function(titles) {
    titles <- gsub("[0-9]+\\.[0-9]+", "", titles)  # Remove decimals (e.g., 54.6)
    titles <- gsub("\\\"|-inch| inch|inches|inch|\\(", "", titles, ignore.case = TRUE)  # Remove variations of "inch"
    titles <- gsub("[^a-zA-Z0-9 ]", "", titles)  # Remove non-alphanumeric characters
    titles <- trimws(titles)  # Remove leading and trailing spaces
    return(titles)
  }
  
  # Apply the cleaning function
  cleaned_titles2 <- clean_titles_further(cleaned_titles1)
  
  # Remove the model ID from the corresponding title
  cleaned_titles2 <- mapply(function(title, model_id) {
    # Use gsub to remove the model ID if it's present in the title
    gsub(pattern = model_id, replacement = "", x = title, ignore.case = TRUE)
  }, cleaned_titles2, clean_model_ids, USE.NAMES = FALSE)
  
  # Trim whitespace from the resulting titles
  cleaned_titles2 <- trimws(cleaned_titles2)

  # Function to clean and refine titles
  refine_titles <- function(title) {
    # Split the title into words
    words <- unlist(strsplit(title, " "))
    
    # Identify valid numeric values
    numeric_values <- grep("^[0-9]+$", words, value = TRUE)
    
    # Keep only the first valid screen size
    if (length(numeric_values) > 0) {
      screen_size <- numeric_values[1] # The first number is treated as the screen size
    } else {
      screen_size <- NULL
    }
    
    # Remove redundant numbers or sequences following the screen size
    refined_words <- unique(words[!words %in% numeric_values[-1]]) # Exclude extra numeric sequences
    
    # Ensure the screen size appears first
    if (!is.null(screen_size)) {
      refined_words <- c(screen_size, refined_words[!refined_words %in% screen_size])
    }
    
    # Combine back into a single string
    paste(refined_words, collapse = " ")
  }
  
  # Apply the refinement to all titles
  final_titles <- sapply(cleaned_titles2, refine_titles, USE.NAMES = FALSE)
  
  # Step 1: Get unique non-NA values for each feature
  unique_screen_sizes <- unique(selected_product_features_df$`screen_size_class`)
  unique_screen_sizes <- unique_screen_sizes[!is.na(unique_screen_sizes)]
  
  unique_resolutions <- unique(selected_product_features_df$`vertical_resolution`)
  unique_resolutions <- unique_resolutions[!is.na(unique_resolutions)]
  
  unique_refresh_rates <- unique(selected_product_features_df$`screen_refresh_rate`)
  unique_refresh_rates <- unique_refresh_rates[!is.na(unique_refresh_rates)]
  
  # Step 2: Fill missing "ScreenSize"
  missing_screen_size_indices <- which(is.na(selected_product_features_df$`screen_size_class`))
  for (idx in missing_screen_size_indices) {
    title <- final_titles[idx]
    detected <- unique_screen_sizes[sapply(unique_screen_sizes, function(val) any(grepl(val, title)))]
    if (length(detected) > 0) {
      selected_product_features_df$`screen_size_class`[idx] <- detected[1] # Use the first detected value
    }
  }
  
  # Step 3: Fill missing "Resolution"
  missing_resolution_indices <- which(is.na(selected_product_features_df$`vertical_resolution`))
  for (idx in missing_resolution_indices) {
    title <- final_titles[idx]
    detected <- unique_resolutions[sapply(unique_resolutions, function(val) any(grepl(val, title)))]
    if (length(detected) > 0) {
      selected_product_features_df$`vertical_resolution`[idx] <- detected[1] # Use the first detected value
    }
  }
  
  # Step 4: Fill missing "RefreshRate"
  missing_refresh_rate_indices <- which(is.na(selected_product_features_df$`screen_refresh_rate`))
  for (idx in missing_refresh_rate_indices) {
    title <- final_titles[idx]
    detected <- unique_refresh_rates[sapply(unique_refresh_rates, function(val) any(grepl(val, title)))]
    if (length(detected) > 0) {
      selected_product_features_df$`screen_refresh_rate`[idx] <- detected[1] # Use the first detected value
    }
  }
  
  # Initialize a dataframe to store the final metrics for each feature
  final_feature_metrics <- data.frame(
    Feature = colnames(selected_product_features_df),
    Frequency = numeric(length(colnames(selected_product_features_df))),
    Distinct_Value_Ratio = numeric(length(colnames(selected_product_features_df)))
  )
  # Calculate frequency and distinct value ratio for each feature
  for (feature in colnames(selected_product_features_df)) {
    
    # Extract the values for the current feature
    feature_values <- selected_product_features_df[[feature]]
    
    # Calculate frequency: percentage of non-NA values
    non_na_count <- sum(!is.na(feature_values) & feature_values != "")
    total_count <- length(feature_values)
    frequency <- non_na_count / total_count
    
    # Calculate distinct value ratio: number of unique non-NA values / total non-NA count
    distinct_values <- length(unique(feature_values[!is.na(feature_values) & feature_values != ""]))
    distinct_value_ratio <- ifelse(non_na_count > 0, distinct_values / non_na_count, 0)
    
    # Store the metrics
    final_feature_metrics[final_feature_metrics$Feature == feature, "Frequency"] <- frequency
    final_feature_metrics[final_feature_metrics$Feature == feature, "Distinct_Value_Ratio"] <- distinct_value_ratio
  }
  # Identify features with frequency > 0.65 in final_feature_metrics
  selected_features_final <- final_feature_metrics$Feature[final_feature_metrics$Frequency > 0.75]
  
  # Subset the dataframe to keep only the selected features
  selected_product_features_df <- selected_product_features_df[, selected_features_final]

  # Initialize an empty vector to store all model words
  all_model_words <- c()
  
  # Iterate through each feature to create model words
  for (feature in colnames(selected_product_features_df)) {
    # Extract unique non-NA values for the feature
    unique_values <- unique(selected_product_features_df[[feature]])
    unique_values <- unique_values[!is.na(unique_values)] # Remove NA values
    
    # Create model words for the feature
    model_words <- paste0(feature, "_", unique_values)
    
    # Append the model words to the all_model_words vector
    all_model_words <- c(all_model_words, model_words)
  }
  # Replace spaces with underscores in all model words
  all_model_words <- gsub(" ", "_", all_model_words)
  

  

  
  
  
  
  



  
