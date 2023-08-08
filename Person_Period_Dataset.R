################################################################################
################################ Person Period Dataset #########################
################################################################################
# This version is like that: all disorders are used at the different columns (multiple columns for disorders)
# Upper bond for age variable is current age
# Additionally, NA values are converted into zero by "lpca" function
# ID AGE EVENT1 EVENT2 ... EVENT20
# 1   4      0      0           0
# 1   5      1      0           0
# 1   6      NA     1           0
# 1   7      NA     NA          0
# 1   .      .      .           .
# 1  30      NA     NA          0
# 2   4      0      0           0
# 2   5      0      0           0
# 2   6      0      0           0
# .   .      .      .           .
# 2  20      0      0           0

# Load Library
library(dplyr)


# Load the required datasets
main_data_NCS <- read.delim("20240-0002-Data.tsv")

# Load merged CAs with NAs dataset
load("merged_CAs.Rdata")


# Select twenty disorders
twenty_disorders <- main_data_NCS[, c("CASEID", "Age", "ADD_OND", "CD_OND", "IED_OND",
                                      "ODD_OND", "PD_OND", "PTS_OND", "GAD_OND",
                                      "SAD_OND", "AGO_OND", "SP_OND", "SO_OND", 
                                      "ALA_OND", "ALD_OND", "DRA_OND", "DRD_OND",
                                      "BIPOLARI_OND", "BIPOLARII_OND", "BIPOLARSUB_OND",
                                      "DYS_OND", "MDDH_OND")]



# Create matrices to store the new data
new_frame_1 <- matrix(nrow = 0, ncol = ncol(twenty_disorders))
new_frame_2 <- matrix(nrow = 0, ncol = ncol(twenty_disorders))
final_frame <- matrix(nrow = 0, ncol = ncol(twenty_disorders))

# Loop through each patient
for (patient in 1:nrow(twenty_disorders)) {
  
  if (sum(is.na(twenty_disorders[patient, 3:22])) != 20) {
    m <- max(twenty_disorders[patient, 3:22], na.rm = TRUE)
  } else {
    m <- max(twenty_disorders[patient, 3:22])
  }
  
  
  # Check if the maximum value is less than or equal to 0
  if (is.na(m)) {
    # If so, create an empty patient with all 0s except for patient ID and visit number
    n <- twenty_disorders[patient, 2]
    empty_patient_1 <- matrix(nrow = n - 3, ncol = ncol(twenty_disorders))
    empty_patient_1[, 1] <- twenty_disorders[patient, 1]
    empty_patient_1[, 2] <- 4:n
    empty_patient_1[, 3:(ncol(twenty_disorders))] <- 0
    new_frame_1 <- rbind(new_frame_1, empty_patient_1)
  } else {
    # If the maximum value is greater than 0, create a patient with a row for each visit
    # and a column for each disorder
    n <- twenty_disorders[patient, 2]
    empty_patient_2 <- matrix(nrow = n-3, ncol = ncol(twenty_disorders))
    for (j in 3:ncol(empty_patient_2)) {
      if (!is.na(twenty_disorders[patient, j]) && twenty_disorders[patient, j] > 0) {
        # If the disorder is present at this visit, fill in the rows of the matrix
        for (i in 1:nrow(empty_patient_2)) {
          empty_patient_2[i, 1] <- twenty_disorders[patient, 1]
          if (i + 3 == twenty_disorders[patient, j]) {
            empty_patient_2[i, j] <- 1
          } else if (i + 3 < twenty_disorders[patient, j]) {
            empty_patient_2[i, j] <- 0
          }
        } 
      } else {
        # If the disorder is not present at this visit, fill in the rows with 0s
        for(i in 1:n-3){
          empty_patient_2[i, j] <- 0
        }
      }
    }
    # Fill in the patient ID and visit numbers for each row
    empty_patient_2[, 1] <- twenty_disorders[patient, 1]
    empty_patient_2[, 2] <- 4:n
    new_frame_2 <- rbind(new_frame_2, empty_patient_2)
  }
  
  # Combine the two frames for each patient and add to the final data matrix
  combined_patients <- rbind(new_frame_1, new_frame_2)
  final_frame <- rbind(final_frame, combined_patients)
  
  # Reset the temporary matrices for the next patient
  new_frame_1 <- matrix(nrow = 0, ncol = ncol(twenty_disorders))
  new_frame_2 <- matrix(nrow = 0, ncol = ncol(twenty_disorders))
}


# Assign columns names for new dataset_V1.1
colnames(final_frame) <- c("CASEID", "Age", "ADD", "CD", "IED",
                           "ODD", "PD", "PTS", "GAD",
                           "SAD", "AGO", "SP", "SO", 
                           "ALA", "ALD", "DRA", "DRD",
                           "BIPOLARI", "BIPOLARII", "BIPOLARSUB",
                           "DYS", "MDDH")

################### Merge 20 disorders and CAs and Sex  ########################
################################################################################
# Select the complete cases for merged_CA_with_NAs
merged_CAs_and_disorders <- merge(final_frame, merged_CAs_with_NAs, by = "CASEID", all = TRUE)

dataset_person_period <- merge(merged_CAs_and_disorders, main_data_NCS[, c("CASEID", "Sex")], by = "CASEID", all = TRUE)

dataset_person_period <- dataset_person_period %>%
  select("CASEID", "Age", "Sex", everything())


# Convert gender labels into 1 -> 0 (female) and 2 -> 1 (male)
dataset_person_period$Sex <- ifelse(dataset_person_period$Sex == 1, 0, 
                           ifelse(dataset_person_period$Sex == 2, 1, NA))

# Convert Age variable into integer
dataset_person_period$Age <- as.integer(dataset_person_period$Age)

# Save the dataset
save(dataset_person_period, file = "dataset_person_period.Rdata")



