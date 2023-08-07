# Load Library
library(lmap) # for lpca() function

# Load data set
load("dataset_person_period.Rdata")

# Select the complete cases from data for Logistic Reduced Rank Regression (LRRR) analysis
dpp <- dataset_person_period[complete.cases(dataset_person_period[,24:32]),]

# Obtain the block names and numbers
block_names <- unique(dpp[, "CASEID"]) # unique IDs names
block_total <- length(block_names) # unique IDs (5381)

# Obtain the block sizes
block_sizes <- table(dpp[, "CASEID"]) # number of repetitions for IDs

# Determine the number of bootstrap samples
R <- 100

# Initialize an empty list to store the block data sets
block_boot_data <- NULL

# characterize the block names to select (repeat) the observation in a cluster
nu_block_names <- as.character(dpp[,"CASEID"])  

# Initialize an empty list to store samples results for LRRR analysis
bootstrap_results <- vector("list", length = R)  

# Set the seed to create reproducible results
set.seed(1234)

# Start to creating random bootstrap samples
for (r in 1:R) {
  
  # First select random blocks
  sample_block <- sample(x = block_names, size = block_total, replace = TRUE) 
  
  # Obtain frequencies of blocks
  sample_block_tab <- table(sample_block)
  
  
  for(n in 1:max(sample_block_tab)){
    names_n <- names(sample_block_tab[sample_block_tab == n])
    selected <- nu_block_names %in% names_n
    
    # Obtain samples according to the block sizes
    clustered_data <- dpp[selected,][rep(seq_len(sum(selected)), n), ]
    
    # Specify the row names
    row.names(clustered_data)[grep("\\.", row.names(clustered_data), invert = TRUE)] <- paste0(grep("\\.", row.names(clustered_data), invert = TRUE, value = TRUE),".0")
    clustered_data[,"CASEID"] <- paste0(clustered_data[,"CASEID"], sub(".*\\.","_",row.names(clustered_data)))
    
    # stack the bootstrap samples iteratively 
    block_boot_data <- rbind(block_boot_data, clustered_data) 
    
  }
 
  # Prepare data for LRRR
  bootstrap_Y <- block_boot_data[, 4:23]  # Subset the response variables
  bootstrap_X <- model.matrix(~ poly(Age, 4)  + Sex + PMI + PSA + FV + PA + Neglect + OPL 
                              + PIllness + EA, data = block_boot_data) # Subset the predictor variables
  bootstrap_X <- bootstrap_X[, -1]  # Remove the intercept column from predictor variables matrix
  
  # LRRR analysis
  output <- lpca(bootstrap_Y, bootstrap_X, S = 3) # Run LRRR function
  implied_coef <- data.frame(exp(output$B %*% t(output$V))) # Obtain exponentiated logistic reduced rank regression
  bootstrap_results[[r]] <- implied_coef  # Store the results in the list
  block_boot_data <- NULL # reset the selection for new bootstrap data
  
}



# Define a function to calculate the confidence interval for a given element across matrices
calculate_ci <- function(data, element_row, element_col, alpha = 0.95) {
  element_values <- sapply(data, "[[", element_row, element_col)
  lower_quantile <- (1 - alpha) / 2
  upper_quantile <- 1 - lower_quantile
  lower_bound <- quantile(element_values, lower_quantile)
  upper_bound <- quantile(element_values, upper_quantile)
  return(c(lower_bound, upper_bound))
}


# Calculate confidence intervals for each element in the matrices
num_rows <- 13 # number of predictor variables
num_cols <- 20 # number of response variables
confidence_intervals <- list() # create an empty list to assign the confidence intervals

for (row in 1:num_rows) {
  for (col in 1:num_cols) {
    element_ci <- calculate_ci(bootstrap_results, row, col)
    confidence_intervals[[paste0("Element", row, "x", col)]] <- element_ci
  }
}


# Print the confidence intervals for each element
for (row in 1:num_rows) {
  for (col in 1:num_cols) {
    element_name <- paste0("Element", row, "x", col)
    cat("Confidence interval for", element_name, ": ", confidence_intervals[[element_name]], "\n")
  }
}
