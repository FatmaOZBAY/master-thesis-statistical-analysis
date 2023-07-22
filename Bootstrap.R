# Bootstrap
set.seed(112358) # produces the exact same random data every time you run the code
n <- length(dataset_V1.1) # the number of observations to sample
n
B <- 10000 # the number of bootstrap samples
variable <- .... # the variable we will resample from? what will be in my analysis
# change the variable name!

# Now, get those bootstrap samples (without loops!)
bootstrapsample <- matrix(sample(variable, size = n*B, replace = TRUE),
                          nrow = n, ncol = B)

?boot
######################## Example ###########################
# Load required packages
library(boot)

# Generate example longitudinal data with unbalanced repeated measurements
set.seed(123)
n <- 100  # Number of individuals
TT <- c(5, 100)  # Number of measurements per individual (unbalanced)
subject_ids <- rep.int(1:n, times = TT)  # Generate subject IDs based on number of measurements
measurements <- rnorm(sum(TT))  # Generate measurements
data <- data.frame(
  subject = subject_ids,
  measurement = measurements
)

# Define the function to calculate the statistic of interest

# Load required packages
library(boot)

# Define the function to calculate the statistic of interest
calc_statistic <- function(data, indices) {
  sampled_data <- data[indices, ]  # Resample the data
  
  # Perform your calculations here
  # Replace the following code with your desired analysis
  Y <- sampled_data[, 4:23]
  X <- model.matrix(~ poly(Age, 4)  + Sex + PMI + PSA + FV + PA + Neglect + PDivorce + OPL 
                    + PIllness + EA, data = sampled_data)[, -1]
  output <- lpca(Y, X, S = 3)
  implied_coef <- data.frame(exp(output$B %*% t(output$V)))
  
  return(implied_coef[7,1])
}

# Perform clustered bootstrap resampling
clustered_bootstrap <- boot(dataset_V1.1, statistic = calc_statistic, R = 2, strata = dataset_V1.1$CASEID)

# View the bootstrap results
print(clustered_bootstrap)


# Obtain bootstrap confidence intervals
boot_ci <- boot.ci(clustered_bootstrap, type = "basic")  # Change the type as needed
print(boot_ci)
?boot.ci

# 
library(car)

Y <- dataset_V1.1[, 4:23]
X <- model.matrix(~ poly(Age, 4)  + Sex + PMI + PSA + FV + PA + Neglect + PDivorce + OPL 
                  + PIllness + EA, data = dataset_V1.1)[, -1]


n <- 9282
R <- ncol(Y)
P <- ncol(X)
S <- 3

# Logistic Reduced Rank Regression
output <- lpca(Y, X, S = S)

car::Boot(output, R=100)

?Boot


















