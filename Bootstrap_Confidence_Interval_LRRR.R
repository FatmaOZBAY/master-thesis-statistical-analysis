# Determine the number of bootstrap samples
R <- 100  

# Initialize an empty list to store bootstrap results
bootstrap_results <- vector("list", length = R)  

# Run "lpca" function for each sample
for (r in 1:R) {
  bootstrap_indices <- sample(nrow(dataset_V1.1), replace = TRUE)  # Randomly sample indices with replacement
  bootstrap_sample <- dataset_V1.1[bootstrap_indices, ]
  bootstrap_Y <- bootstrap_sample[, 4:23]  # Subset the response variables
  bootstrap_X <- model.matrix(~ poly(Age, 4)  + Sex + PMI + PSA + FV + PA + Neglect + PDivorce + OPL 
                              + PIllness + EA, data = bootstrap_sample)
  bootstrap_X <- bootstrap_X[, -1]  # Remove the intercept column from X, if present
  output <- lpca(bootstrap_Y, bootstrap_X, S = 3)
  implied_coef <- data.frame(exp(output$B %*% t(output$V)))
  
  bootstrap_results[[r]] <- implied_coef  # Store the result in the list
}

# Save the bootstrap results
save(bootstrap_results, file = "bootstrap_results.Rdata")


# Define the list of matrices
my_list <- list()
for (i in 1:100) {
  my_list[[i]] <- matrix(runif(15*20), nrow = 15, ncol = 20)
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
num_rows <- 14
num_cols <- 20
confidence_intervals <- list()
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
