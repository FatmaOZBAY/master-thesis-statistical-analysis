# Load dataset
load("dataset_stacked_person_period.Rdata")

# Select the complete cases
dataset_stacked_person_period <- dataset_stacked_person_period[complete.cases( dataset_stacked_person_period[, 6:14]), ]

######################### Selection of the "Age" form ##########################
################################################################################
# Form of the polynomial for "Age" variable is checked from 1 to 9
model2_V2 <- glm(Event ~ poly(Age, 9) + Sex + PMI + PSA + PC + 
                   FV + PA + Neglect + PIllness + OPL + EA,
                 data = dataset_stacked_person_period, family = "binomial")

# Summary of LR model
smulti <- summary(model2_V2)


# Number of unique observations and parameters
n <- 5381
(number_param <- 10 + 1 + 9) # 12 is for predictors, 1 is for intercept and 9 is for Age 

# AIC/BIC Statistics
(deviance <- smulti$deviance)
(AIC <- smulti$deviance + 2*number_param) 
(BIC <- smulti$deviance + log(n)*number_param)

# Decision: The degree of 7 was selected for Age variable!!

######################## 1 or more predictors left-out #########################
################################################################################
# We check the influence of the predictor variables with "poly(Age, 7)"
model2_V2 <- glm(Event ~ poly(Age, 7) + Sex + PMI + PSA + PC +
                     FV + PA + Neglect + OPL + PIllness ,
                 data = dataset_stacked_person_period, family = "binomial")

# Summary of LR model
smulti <- summary(model2_V2)


# Number of unique observations and parameters
n <- 5381
(number_param <- 9 + 1 + 7) # 9 is for predictors, 1 is for intercept and 7 is for Age 

# AIC/BIC Statistics
(deviance <- smulti$deviance)
(AIC <- smulti$deviance + 2*number_param) 
(BIC <- smulti$deviance + log(n)*number_param)


############################## Decision (Final Model) ##########################
################################################################################
# We removed PC because of the lowest AIC/BIC value
model2_V2 <- glm(Event ~ poly(Age, 7) + Sex + PMI + 
                   PSA + FV + PA + Neglect + OPL + PIllness + EA ,
                 data = dataset_stacked_person_period, family = "binomial")

# Summary of LR model
smulti <- summary(model2_V2)

# ORs (odds ratios)
cmulti <- smulti$coefficients[10:17, 1]
exp(cmulti)

#     PMI      PSA       FV       PA  Neglect      OPL PIllness       EA 
#1.524834 1.273248 1.321163 1.246346 1.195916 1.221909 1.465760 1.306119 

# Confidence Intervals
sdmulti <- smulti$coefficients[10:17, 2]
CImulti <- cmulti + matrix(rep(c(-1, 1),8), nrow = 8, ncol = 2, byrow = T) * abs(qnorm(0.025)) * sdmulti
exp(CImulti)

# PMI      1.459424 1.593177
# PSA      1.217063 1.332027
# FV       1.264306 1.380577
# PA       1.175760 1.321168
# Neglect  1.144581 1.249554
# OPL      1.150672 1.297556
# PIllness 1.409740 1.524006
# EA       1.236534 1.379618









