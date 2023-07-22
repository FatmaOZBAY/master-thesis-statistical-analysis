# Load Library
library(lmap) # lpca function
library(openxlsx) # write (save) data into excel file

# Load data set
load("dataset_V1.1.Rdata") # person-period dataset that saved in the "Person_Period_Dataset.R" file


# Select "complete cases" for X matrix! Because glm is doing it "complete cases". 
# Be careful, glm is doing it for all X and Y. But here, it is done for X.
dataset_V1.1 <- dataset_V1.1[complete.cases(dataset_V1.1[, 24:34]), ]


####################### Dimensionality Selection ###############################
################################################################################
# All dimensions were tried on this code (not separate codes) 
# S (dimension) changed from 1 to 9
Y <- dataset_V1.1[, 4:23] # matrix for response variables
X <- model.matrix(~ poly(Age, 9) + Sex + PMI + PSA + PC + FV + 
                    PA + Neglect + PDeath + PDivorce + OPL + PIllness + EA, data = dataset_V1.1)[, -1] # matrix for predictor variables

n <- 9282 # number of unique observation
(R <- ncol(Y)) # number of responses
(P <- ncol(X)) # number of predictors
S <- 9 # dimension (it is changed from 1 to 9 before running "lpca" function)


# Logistic Reduced Rank Regression
output <- lpca(Y, X, S = S)
(dev <- output$deviance)

# Number of parameters
(number_param <-(P + R - S)*S + R) 

# AIC/BIC Statistics
(AIC <- dev + 2*number_param) 
(BIC <- output$deviance + log(n)*number_param)


#################### Selection of the "Age" form ###############################
################################################################################
# After deciding the dimensionality, that is "3", then the form of the "Age" variable is checked
# poly(Age, .) changed from 1 to 9
Y <- dataset_V1.1[, 4:23]
X <- model.matrix(~ poly(Age, 9) + Sex + PMI + PSA + PC + FV + 
                    PA + Neglect + PDeath + PDivorce + OPL + PIllness + EA, data = dataset_V1.1)[, -1]

n <- 9282
R <- ncol(Y)
P <- ncol(X)
S <- 3

# Logistic Reduced Rank Regression
output <- lpca(Y, X, S = S)
(dev <- output$deviance) # deviance


# Number of parameters
(number_param <-(P + R - S)*S + R)

# AIC/BIC Statistics
(AIC <- dev + 2*number_param) 
(BIC <- output$deviance + log(n)*number_param)




########################## 1 predictor left-out ################################
################################################################################
# We check the influence of the predictor variables with "poly(Age, 4)" on the 3rd dimension
# All predcitors were removed from the model one by one (first)
Y <- dataset_V1.1[, 4:23]
X <- model.matrix(~ poly(Age, 4) + Sex + 
                    PMI + PSA + FV + PA + Neglect + PDeath + 
                    PDivorce + OPL + PIllness + EA, data = dataset_V1.1)[, -1]


n <- 9282
R <- ncol(Y)
P <- ncol(X)
S <- 3

# Logistic Reduced Rank Regression
output <- lpca(Y, X, S = S)
(dev <- output$deviance) # deviance


# Number of parameters
(number_param <-(P + R - S)*S + R) 

# AIC/BIC Statistics
(AIC <- dev + 2*number_param) 
(BIC <- output$deviance + log(n)*number_param)


##################### 2 or more predictors left-out ############################
################################################################################
# After deciding which removed predictors (alone) decreases the AIC/BIC value, 
# we need to check their combination how they affect the AIC/BIC value. 
# So, we will check the AIC/BIC values for PC and PDeath combinations

Y <- dataset_V1.1[, 4:23]
X <- model.matrix(~ poly(Age, 4)  + Sex + PMI + PSA + FV + PA + Neglect +
                    PDivorce + PIllness + EA, data = dataset_V1.1)[, -1]


n <- 9282
R <- ncol(Y)
P <- ncol(X)
S <- 3

# Logistic Reduced Rank Regression
output <- lpca(Y, X, S = S)
(dev <- output$deviance) # deviance


# Number of parameters
(number_param <-(P + R - S)*S + R)

# AIC/BIC Statistics
(AIC <- dev + 2*number_param) 
(BIC <- output$deviance + log(n)*number_param)


########################### Decision (Final Model) #############################
################################################################################
# We removed 2 predictors (PC and PDeath) with lowest AIC/BIC scores (together) 
# So we should calculate the last model with "lpca" function to obtain the "implied"
# coefficients in the last model

# Final Model 
Y <- dataset_V1.1[, 4:23]
X <- model.matrix(~ poly(Age, 4)  + Sex + PMI + PSA + FV + PA + Neglect + PDivorce + OPL 
                  + PIllness + EA, data = dataset_V1.1)[, -1]


n <- 9282
R <- ncol(Y)
P <- ncol(X)
S <- 3

# Logistic Reduced Rank Regression
output <- lpca(Y, X, S = S)


############################## Implied Coefficients #######################################
###########################################################################################
# Check the coefficient values
# B is for childhood adversities, namely, predictors
# V is for response variables, namely, disorders
# output$B %*% t(output$V)
implied_coef <- data.frame(exp(output$B %*% t(output$V)))
write.xlsx(implied_coef, file = "implied_coef.xlsx", sheetName = "Sheet1", rowNames = FALSE)


############################## Quality of Representation ##############################
#######################################################################################
# L_r: It is the part of our loss function for response variable r
# we multiply with -2 to obtain deviance value
L_r <- -2 * colSums(output$Y * log(output$probabilities) + (1 - output$Y) * log(1 - output$probabilities), na.rm = T)


# L_(0,r): It is the deviance of the intercept-only logistic regression model for response variable r
L_0r <- NA
for(i in 4:23){
  output_0r <- glm(dataset_V1.1[, i] ~ 1, data = dataset_V1.1, family = binomial(link = "logit"))
  L_0r[i-3] <- output_0r$deviance
}
names(L_0r) <- colnames(dataset_V1.1[, 4:23])
L_0r


# L_lr: It is the deviance from a logistic regression with the same predictors
L_lr <- NA
for (i in 4:23) {
  output_lr <- glm(dataset_V1.1[, i] ~ poly(Age, 4)  + Sex + PMI + PSA + FV + 
                     PA + Neglect + PDivorce + OPL + PIllness + EA, data = dataset_V1.1, family = binomial(link = "logit"))
  L_lr[i-3] <- output_lr$deviance
}
names(L_lr) <- colnames(dataset_V1.1[, 4:23])
L_lr


# The Quality of Representation Q_r
(L_0r - L_r) / (L_0r - L_lr)



################################ Biplots #######################################
################################################################################
# Remove polynomial poly(Age,4) predictor and Sex for visualizations
# Columns to remove
columns_to_remove <- c(1, 2, 3, 4, 5)

# Remove columns from each part of the list
output$Xoriginal <- output$Xoriginal[, -columns_to_remove]
output$X <- output$X[, -columns_to_remove]
output$mx <- output$mx[-columns_to_remove]
output$sdx <- output$sdx[-columns_to_remove]
output$xnames <- output$xnames[-columns_to_remove]
output$B <- output$B[-columns_to_remove, ]

# Copy the matrices for checking the dimensions in pair
output_dimension_12 <- output 
output_dimension_13 <- output
output_dimension_23 <- output

# Remove columns from output matrix and create U, B, V with dimension 1-2
for (i in seq_along(output_dimension_12)) {
  if (i %in% c(10, 11, 12)) {
    output_dimension_12[[i]] <-output_dimension_12[[i]][, -3]
  }
}

# Remove columns from output matrix and create U, B, V with dimension 1-3
for (i in seq_along(output_dimension_13)) {
  if (i %in% c(10, 11, 12)) {
    output_dimension_13[[i]] <-output_dimension_13[[i]][, -2]
  }
}


# Remove columns from output matrix and create U, B, V with dimension 2-3
for (i in seq_along(output_dimension_23)) {
  if (i %in% c(10, 11, 12)) {
    output_dimension_23[[i]] <-output_dimension_23[[i]][, -1]
  }
}


plot_12 <- plot(output_dimension_12) # dimension 1 and dimension 2
plot_13 <- plot(output_dimension_13) # dimension 1 and dimension 3
plot_23 <- plot(output_dimension_23) # dimension 2 and dimension 3



