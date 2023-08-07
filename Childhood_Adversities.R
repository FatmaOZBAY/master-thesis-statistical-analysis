# Load the data set
main_data_NCS <- read.delim("C:/Users/ozbyf/OneDrive/Desktop/Master_Thesis_Working/NCS_R/ICPSR_20240-V8-Delimited/ICPSR_20240/DS0002/20240-0002-Data.tsv")

######################## Select Childhood Adversities (CAs) ####################
################################################################################
# Maladaptive family functioning CAs
parental_mental_illness <- main_data_NCS[, c("CASEID", "CH44", "CH44a", "CH46", "CH74", "CH74a", "CH76")]
parental_substance_abuse <- main_data_NCS[, c("CASEID", "CH52", "CH59", "CH59a", "CH82", "CH89", "CH89a")] 
parental_criminality <- main_data_NCS[, c("CASEID", "CH64", "CH67", "CH94", "CH97")] 
family_violence <- main_data_NCS[, c("CASEID", "CH28", "CH29")] 
physical_abuse <- main_data_NCS[, c("CASEID", "CH63", "CH93")] 
neglect <-  main_data_NCS[, c("CASEID", "CH30_1a", "CH30_1b", "CH30_1c", "CH30_1d", "CH30_1e")] 
other_parental_loss <- main_data_NCS[, c("CASEID", "CH6")]
physical_illness <- main_data_NCS[, c("CASEID","CC1a", "CC1b", "CC1c", "CC1d", "CC1e", "CC1f",
                                      "CC1g", "CC1h", "CC1i", "CC1j", "CC1l", "CC1n", "CC1o", "CC1s", "CC1t",
                                      "CC3a", "CC3b","CC3c", "CC3d","CC3e", "CC3f", "CC3g", "CC3h", "CC3i", 
                                      "CC3j", "CC3l","CC3n", "CC3o", "CC3s", "CC3t")] 
economic_adversity <- main_data_NCS[, c("CASEID", "CH19")] 


################################ 1- Parental Mental Illness ####################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(parental_mental_illness)) {
  parental_mental_illness[, i] <- ifelse(parental_mental_illness[, i] %in% c(-8, -9), NA, parental_mental_illness[, i])
}


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
parental_mental_illness_CAs <- as.data.frame(ifelse(parental_mental_illness[, -1] == 1, 1, 0)) 

# Check if each row contains all NA values
all_na_rows <- apply(is.na(parental_mental_illness_CAs), 1, all)

# Set "total" column to NA for all NA rows
parental_mental_illness_CAs$total <- ifelse(all_na_rows, NA, 0)  # Fill with 0 temporarily, will update later

# Calculate row sums, ignoring NA values, for non-NA rows
non_na_rows <- which(!all_na_rows)
parental_mental_illness_CAs$total[non_na_rows] <- rowSums(parental_mental_illness_CAs[non_na_rows, ], na.rm = TRUE)

# Select the person with at least one parental mental illness
parental_mental_illness_CAs$total <- ifelse(parental_mental_illness_CAs$total == 0, 0, 1)

# Merge final dataset for parental mental illness with "CaseID" and "total" (whether there is parental mental illness or not)
parental_mental_illness_final <- cbind(parental_mental_illness$CASEID, parental_mental_illness_CAs$total)
colnames(parental_mental_illness_final) <- c("CASEID", "PMI")



##################### 2- Parental Substance Abuse ##############################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(parental_substance_abuse)) {
  parental_substance_abuse[, i] <- ifelse(parental_substance_abuse[, i] %in% c(-8, -9), NA, parental_substance_abuse[, i])
}


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
parental_substance_abuse_CAs <- as.data.frame(ifelse(parental_substance_abuse[, -1] == 1, 1, 0)) 

# Check if each row contains all NA values
all_na_rows <- apply(is.na(parental_substance_abuse_CAs), 1, all)

# Set "total" column to NA for all NA rows
parental_substance_abuse_CAs$total <- ifelse(all_na_rows, NA, 0)  # Fill with 0 temporarily, will update later

# Calculate row sums, ignoring NA values, for non-NA rows
non_na_rows <- which(!all_na_rows)
parental_substance_abuse_CAs$total[non_na_rows] <- rowSums(parental_substance_abuse_CAs[non_na_rows, ], na.rm = TRUE)


# Select the person with at least one parental substance abuse
parental_substance_abuse_CAs$total <- ifelse(parental_substance_abuse_CAs$total == 0, 0, 1) 

# Merge final dataset for parental substance abuse with "CaseID" and "total" 
# (whether there is at least one parental substance abuse or not)
parental_substance_abuse_final <- cbind(parental_substance_abuse$CASEID, parental_substance_abuse_CAs$total)
colnames(parental_substance_abuse_final) <- c("CASEID", "PSA")


####################### 3- Parental Criminality ################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(parental_criminality)) {
  parental_criminality[, i] <- ifelse(parental_criminality[, i] %in% c(-8, -9), NA, parental_criminality[, i])
}


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
parental_criminality_CAs <- as.data.frame(ifelse(parental_criminality[, -1] == 1, 1, 0)) 

# Check if each row contains all NA values
all_na_rows <- apply(is.na(parental_criminality_CAs), 1, all)

# Set "total" column to NA for all NA rows
parental_criminality_CAs$total <- ifelse(all_na_rows, NA, 0)  # Fill with 0 temporarily, will update later

# Calculate row sums, ignoring NA values, for non-NA rows
non_na_rows <- which(!all_na_rows)
parental_criminality_CAs$total[non_na_rows] <- rowSums(parental_criminality_CAs[non_na_rows, ], na.rm = TRUE)


# Select the person with at least one parental criminality
parental_criminality_CAs$total <- ifelse(parental_criminality_CAs$total == 0, 0, 1) 

# Merge final dataset for parental criminality abuse with "CaseID" and "total" 
# (whether there is at least one parental criminality or not)
parental_criminality_final <- cbind(parental_criminality$CASEID, parental_criminality_CAs$total)
colnames(parental_criminality_final) <- c("CASEID", "PC")



######################### 4- Family Violence ##################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(family_violence)) {
  family_violence[, i] <- ifelse(family_violence[, i] %in% c(-8, -9), NA, family_violence[, i])
}


# Convert the values with 1,2,3 into one and the value with 4 into 0 ((1 (often), 2 (sometimes), 3(rarely), 4(never))
for (i in 2:ncol(family_violence)) {
  family_violence[, i] <- ifelse(family_violence[, i] %in% c(1, 2, 3), 1, 
                                          ifelse(family_violence[, i] == 4, 0, family_violence[, i]))
}

# Check if each row contains all NA values
all_na_rows <- apply(is.na(family_violence[,-1]), 1, all)

# Set "total" column to NA for all NA rows
family_violence$total <- ifelse(all_na_rows, NA, 0)  # Fill with 0 temporarily, will update later

# Calculate row sums, ignoring NA values, for non-NA rows
non_na_rows <- which(!all_na_rows)
family_violence$total[non_na_rows] <- rowSums(family_violence[non_na_rows,-1], na.rm = TRUE)


# Select the person with at least one parental criminality
family_violence$total <- ifelse(family_violence$total == 0, 0, 1) 

# Select final family violence data with "CaseID" and "total" 
# (whether there is at least one family violence or not)
family_violence_final <- family_violence[, c(1, 4)]
colnames(family_violence_final) <- c("CASEID", "FV")



############################ 5- Physical Abuse #################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(physical_abuse)) {
  physical_abuse[, i] <- ifelse(physical_abuse[, i] %in% c(-8, -9), NA, physical_abuse[, i])
}


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
physical_abuse_CAs <- as.data.frame(ifelse(physical_abuse[, -1] == 1, 1, 0)) 


# Check if each row contains all NA values
all_na_rows <- apply(is.na(physical_abuse_CAs), 1, all)

# Set "total" column to NA for all NA rows
physical_abuse_CAs$total <- ifelse(all_na_rows, NA, 0)  # Fill with 0 temporarily, will update later

# Calculate row sums, ignoring NA values, for non-NA rows
non_na_rows <- which(!all_na_rows)
physical_abuse_CAs$total[non_na_rows] <- rowSums(physical_abuse_CAs[non_na_rows, ], na.rm = TRUE)


# Select the person with growing with parental physical abuse
physical_abuse_CAs$total <- ifelse(physical_abuse_CAs$total == 0, 0, 1) 

# Select final dataset for parental physical abuse with "CaseID" and "total"
physical_abuse_final <- cbind(physical_abuse$CASEID, physical_abuse_CAs$total)
colnames(physical_abuse_final) <- c("CASEID", "PA")




############################## 6- Neglect ######################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(neglect)) {
  neglect[, i] <- ifelse(neglect[, i] %in% c(-8, -9), NA, neglect[, i])
}


# Convert the values with 1,2,3 into one and the value with 4 into 0 ((1 (often), 2 (sometimes), 3(rarely), 4(never))
for (i in 2:ncol(neglect)) {
  neglect[, i] <- ifelse(neglect[, i] %in% c(1, 2, 3), 1, 
                                  ifelse(neglect[, i] == 4, 0, neglect[, i]))
}

# Check if each row contains all NA values
all_na_rows <- apply(is.na(neglect[,-1]), 1, all)

# Set "total" column to NA for all NA rows
neglect$total <- ifelse(all_na_rows, NA, 0)  # Fill with 0 temporarily, will update later

# Calculate row sums, ignoring NA values, for non-NA rows
non_na_rows <- which(!all_na_rows)
neglect$total[non_na_rows] <- rowSums(neglect[non_na_rows,-1], na.rm = TRUE)


# Select the person with at least one neglect
neglect$total <- ifelse(neglect$total == 0, 0, 1) 

# Select final dataset for neglect with "CaseID" and "total"
neglect_final <- neglect[, c(1, 7)]
colnames(neglect_final) <- c("CASEID", "Neglect")





####################### 7- Other Parental Loss ################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(other_parental_loss)) {
  other_parental_loss[, i] <- ifelse(other_parental_loss[, i] %in% c(-8, -9), NA, other_parental_loss[, i])
}


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
other_parental_loss$total <- ifelse(other_parental_loss[, -1] == 1, 1, 0)


# Select the final dataset for other parental loss with "CaseID" and "total"
other_parental_loss_final <- other_parental_loss[, c(1,3)]
colnames(other_parental_loss_final) <- c("CASEID", "OPL")




######################## 8- Physical Illness ##################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(physical_illness)) {
  physical_illness[, i] <- ifelse(physical_illness[, i] %in% c(-8, -9), NA, physical_illness[, i])
}

# Select data set that includes ever not had physical illness and had illness before 18
subset_physical_illness <- subset(physical_illness, 
                                  (CC1a %in% 5 & CC1b %in% 5 & CC1c %in% 5 & CC1d %in% 5 &
                                     CC1e %in% 5 & CC1f %in% 5 & CC1g %in% 5 & CC1h %in% 5 &
                                     CC1i %in% 5 & CC1j %in% 5 & CC1l %in% 5 & CC1n %in% 5 &
                                     CC1o %in% 5 & CC1s %in% 5 & CC1t %in% 5) |
                                    (!is.na(CC3a) | !is.na(CC3b) | !is.na(CC3c) | !is.na(CC3d) | !is.na(CC3e) |
                                       !is.na(CC3f) | !is.na(CC3g) | !is.na(CC3h) | !is.na(CC3i) | !is.na(CC3j) |
                                       !is.na(CC3l) | !is.na(CC3n) | !is.na(CC3o) | !is.na(CC3s) | !is.na(CC3t)))

# Convert the dataset into 1-0 (physical illness before 18 or not)
subset_physical_illness$total <- 0
for (i in 1:nrow(subset_physical_illness)) {
  if (any(subset_physical_illness[i, 17:31] < 18, na.rm = T)) {
    subset_physical_illness$total[i] <- 1
  }
}

# Select final physical illness data with "CaseID" and "total" 
physical_illness_final <- subset_physical_illness[, c(1, 32)]
colnames(physical_illness_final) <- c("CASEID", "PIllness")



######################### 9- Economic Adversity ###############################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(economic_adversity)) {
  economic_adversity[, i] <- ifelse(economic_adversity[, i] %in% c(-8, -9), NA, economic_adversity[, i])
}


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
economic_adversity$total <- ifelse(economic_adversity[, -1] == 1, 1, 0)


# Select final economic adversity data with "CaseID" and "total"
economic_adversity_final <- economic_adversity[, c(1,3)]
colnames(economic_adversity_final) <- c("CASEID", "EA")




########################## Merge Childhood Adversities #########################
################################################################################
nrow(parental_mental_illness_final); nrow(parental_substance_abuse_final)
df1 <- merge(parental_mental_illness_final, parental_substance_abuse_final, by = "CASEID", all = TRUE)
df2 <- merge(df1, parental_criminality_final, by = "CASEID", all = TRUE)
df3 <- merge(df2, family_violence_final, by = "CASEID", all = TRUE )
df4 <- merge(df3, physical_abuse_final, by = "CASEID", all = TRUE)
df5 <- merge(df4, neglect_final, by = "CASEID", all = TRUE)
#df6 <- merge(df5, parental_death_final, by = "CASEID", all = TRUE)
#df7 <- merge(df6, parental_divorce_final, by = "CASEID", all = TRUE)
df6 <- merge(df5, other_parental_loss_final, by = "CASEID", all = TRUE)
df7 <- merge(df6, physical_illness_final, by = "CASEID", all = TRUE)
merged_CAs_with_NAs <- merge(df7, economic_adversity_final, by = "CASEID", all = TRUE)

# Save the merged_CAs
save(merged_CAs_with_NAs, file = "merged_CAs_with_NAs.Rdata")


