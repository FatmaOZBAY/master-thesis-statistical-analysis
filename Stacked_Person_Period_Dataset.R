################################################################################
########################## STACKED PERSON PERIOD DATA SET ######################
################################################################################
# This version is like that: there, the onset age of disorder is used for the upper bound
# ID AGE EVENT DISORDER
# 1 4  0 d1
# 1 5  0 d1
# 1 6  0 d1 
# 1 7  1 d1
# 1 4  0 d2 
# 1 5  0 d2
# 1 6  1 d2
# 1 7 NA d2
# 1 4  0 d3
# 1 5  0 d3
# 1 6  0 d3
# 1 7  0 d3
# 2 15 0 None

#Load Library
library(dplyr)

# Load the required data set
main_data_NCS <- read.delim("20240-0002-Data.tsv")

# Load merged CAs dataset
load("merged_CAs.Rdata")

# Select twenty disorders
twenty_disorders <- main_data_NCS[, c("CASEID", "Age", "ADD_OND", "CD_OND", "IED_OND",
                                      "ODD_OND", "PD_OND", "PTS_OND", "GAD_OND",
                                      "SAD_OND", "AGO_OND", "SP_OND", "SO_OND", 
                                      "ALA_OND", "ALD_OND", "DRA_OND", "DRD_OND",
                                      "BIPOLARI_OND", "BIPOLARII_OND", "BIPOLARSUB_OND",
                                      "DYS_OND", "MDDH_OND")]

# Create new version of data set
new_frame_3 <- matrix(nrow = 0, ncol = 4)
last_frame <- matrix(nrow = 0, ncol = 4)

for (patient in 1:nrow(twenty_disorders)) {
  
  new_frame_4 <- matrix(nrow = 0, ncol = 4) 
  
  if (sum(is.na(twenty_disorders[patient, 3:22])) != 20) {
    m <- max(twenty_disorders[patient, 3:22], na.rm = TRUE)
  } else {
    m <- max(twenty_disorders[patient, 3:22])
  }
  
  if (is.na(m)) {
    m <- 1
    empty_patient_3 <- matrix(nrow = m, ncol = 4)
    empty_patient_3[, 1] <- twenty_disorders[patient, 1]
    empty_patient_3[, 2] <- twenty_disorders[patient, 2]
    empty_patient_3[, 3] <- 0
    empty_patient_3[, 4] <- "None of them"
    new_frame_3 <- rbind(new_frame_3, empty_patient_3)
  } else {
    empty_patient_4_list <- list()
    for (j in 3:ncol(twenty_disorders)) {
      if (!is.na(twenty_disorders[patient, j]) && twenty_disorders[patient, j] > 0) {
        empty_patient_4 <- matrix(nrow = m - 3, ncol = 4)
        for (i in 1:nrow(empty_patient_4)) {
          if (i + 3 == twenty_disorders[patient, j]) {
            empty_patient_4[i, 3] <- 1
          } else if (i + 3 > twenty_disorders[patient, j]) {
            empty_patient_4[i, 3] <- NA
          } else {
            empty_patient_4[i, 3] <- 0
          }
        }
        empty_patient_4[, 1] <- twenty_disorders[patient, 1]
        empty_patient_4[, 2] <- seq(from = 4, to = m)
        empty_patient_4[, 4] <- colnames(twenty_disorders)[j]
        empty_patient_4_list[[j - 2]] <- empty_patient_4
      } else {
        empty_patient_4_list[[j - 2]] <- matrix(c(rep(twenty_disorders[patient, 1], m - 3), 
                                                  4:m, 
                                                  rep(0, m - 3), 
                                                  rep(colnames(twenty_disorders)[j], m - 3)),
                                                ncol = 4)
      }
    }
    new_frame_4 <- do.call(rbind, empty_patient_4_list)  
    new_frame_4[, 1] <- twenty_disorders[patient, 1]
    
  }
  
  new_frame <- rbind(new_frame_3, new_frame_4)
  last_frame <- rbind(last_frame, new_frame)
  new_frame_3 <- matrix(nrow = 0, ncol = 4)
  new_frame_4 <- matrix(nrow = 0, ncol = 4)
}

# Convert into data.frame in order to get rid of the quotes
last_frame = data.frame(last_frame) 

# Assign the column names
colnames(last_frame) <- c("CASEID", "Age", "Event", "Disorder")


# Convert variables into numeric, factor and integer otherwise they are characters
last_frame$CASEID <- as.numeric(last_frame$CASEID)
last_frame$Event <- as.factor(last_frame$Event)
last_frame$Age <- as.integer(last_frame$Age)


##################### Merge 20 disorders and CAs and Sex #######################
################################################################################

merged_CAs_and_disorders_V2 <- merge(last_frame, merged_CAs, by = "CASEID", all = TRUE)

dataset_V2 <- merge(merged_CAs_and_disorders_V2, main_data_NCS[, c("CASEID", "Sex")], by = "CASEID", all = TRUE)

# Convert "Sex" variable into factor
dataset_V2$Sex <- as.factor(dataset_V2$Sex)

# 
dataset_V2 <- dataset_V2 %>%
  select("CASEID", "Age", "Sex", everything())


# Save the data set
save(dataset_V2, file = "dataset_V2.Rdata")

