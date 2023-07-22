# Load dataset
load("dataset_V2.Rdata")

######################### Selection of the "Age" form ##########################
################################################################################
# Form of the polynomial for "Age" variable is checked from 1 to 9
model2_V2 <- glm(Event ~ poly(Age, 9) + Sex + PMI + PSA + PC + 
                   FV + PA + Neglect + PDeath + PDivorce + PIllness + OPL + EA,
                 data = dataset_V2, family = "binomial")

# Summary of LR model
smulti <- summary(model2_V2)


# Number of unique observations and parameters
n <- 9282
(number_param <- 12 + 1 + 9) # 12 is for predictors, 1 is for intercept and 9 is for Age 

# AIC/BIC Statistics
(deviance <- smulti$deviance)
(AIC <- smulti$deviance + 2*number_param) 
(BIC <- smulti$deviance + log(n)*number_param)

# Decision: The degree of 7 was selected for Age variable!!

######################## 1 or more predictors left-out #########################
################################################################################
# We check the influence of the predictor variables with "poly(Age, 7)"
model2_V2 <- glm(Event ~ poly(Age, 7)  + Sex + PMI +
                   PSA + FV + PA + Neglect + PDivorce + OPL + PIllness + EA ,
                 data = dataset_V2, family = "binomial")

# Summary of LR model
smulti <- summary(model2_V2)


# Number of unique observations and parameters
n <- 9282
(number_param <- 10 + 1 + 7) # 10 is for predictors, 1 is for intercept and 7 is for Age 

# AIC/BIC Statistics
(deviance <- smulti$deviance)
(AIC <- smulti$deviance + 2*number_param) 
(BIC <- smulti$deviance + log(n)*number_param)


############################## Decision (Final Model) ##########################
################################################################################
# We removed PC and PDeath because of the lowest AIC/BIC value
model2_V2 <- glm(Event ~ poly(Age, 7) + Sex + PMI + 
                   PSA + FV + PA + Neglect + PDivorce + OPL + PIllness + EA ,
                 data = dataset_V2, family = "binomial")

# Summary of LR model
smulti <- summary(model2_V2)

# ORs (odds ratios)
cmulti <- smulti$coefficients[10:18, 1]
exp(cmulti)

#      PMI      PSA       FV       PA  Neglect PDivorce      OPL PIllness       EA 
# 1.267651 1.116601 1.187849 1.141287 1.087844 1.270058 1.070744 1.354327 1.307110

# Confidence Intervals
sdmulti <- smulti$coefficients[10:18, 2]
CImulti <- cmulti + matrix(rep(c(-1, 1),8), nrow = 8, ncol = 2, byrow = T) * abs(qnorm(0.025)) * sdmulti
exp(CImulti)

# PMI          1.215352 1.325258
# PSA          1.069094 1.169320
# FV           1.138031 1.243154
# PA           1.080628 1.215162
# Neglect      1.046988 1.142984
# PDivorce     1.217738 1.340054
# PIllness     1.301481 1.410564
# EA           1.245609 1.392815




