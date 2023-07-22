# Load the data set
main_data_NCS <- read.delim("20240-0002-Data.tsv")


######################## Select Childhood Adversities (CAs) ####################
################################################################################
# Maladaptive family functioning CAs
parental_mental_illness <- main_data_NCS[, c("CASEID", "CH44", "CH44a", "CH46", "CH74", "CH74a", "CH76")]
parental_substance_abuse <- main_data_NCS[, c("CASEID", "CH52", "CH59", "CH59a", "CH82", "CH89", "CH89a")] 
parental_criminality <- main_data_NCS[, c("CASEID", "CH64", "CH67", "CH94", "CH97")] 
family_violence <- main_data_NCS[, c("CASEID", "CH28", "CH29")] 
physical_abuse <- main_data_NCS[, c("CASEID", "CH63", "CH93")] 
# sexual abuse is empty (Insufficient information in the codebook)
neglect <-  main_data_NCS[, c("CASEID", "CH30_1a", "CH30_1b", "CH30_1c", "CH30_1d", "CH30_1e")] 

# Other CAs
parental_death <- main_data_NCS[, c("CASEID", "CH2a", "CH2b")] 
parental_divorce <- main_data_NCS[, c("CASEID", "CH2c")] 
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

# Then NA values were removed
m1 <- c()
for(i in 1:nrow(parental_mental_illness)){
  if(sum(is.na(parental_mental_illness[i, -1])) == (ncol(parental_mental_illness) - 1)){
    m1 <- c(m1, i)
  }
}
parental_mental_illness_filtered <- parental_mental_illness[-m1, ] # remove all NA values


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
parental_mental_illness_filtered_CAs <- as.data.frame(ifelse(parental_mental_illness_filtered[, -1] == 1, 1, 0)) 

# Create new column by summing the rows
parental_mental_illness_filtered_CAs$total <- rowSums(parental_mental_illness_filtered_CAs, na.rm = TRUE)

# Select the person with at least one parental mental illness
parental_mental_illness_filtered_CAs$total <- ifelse(parental_mental_illness_filtered_CAs$total == 0, 0, 1)

# Merge final dataset for parental mental illness with "CaseID" and "total" (whether there is parental mental illness or not)
parental_mental_illness_final <- cbind(parental_mental_illness_filtered$CASEID, parental_mental_illness_filtered_CAs$total)
colnames(parental_mental_illness_final) <- c("CASEID", "PMI")



##################### 2- Parental Substance Abuse ##############################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(parental_substance_abuse)) {
  parental_substance_abuse[, i] <- ifelse(parental_substance_abuse[, i] %in% c(-8, -9), NA, parental_substance_abuse[, i])
}

# Then NA values were removed
m2 <- c()
for(i in 1:nrow(parental_substance_abuse)){
  if(sum(is.na(parental_substance_abuse[i, -1])) == (ncol(parental_substance_abuse) - 1)){
    m2 <- c(m2, i)
  }
}
parental_substance_abuse_filtered <- parental_substance_abuse[-m2, ] # remove all NA values

# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
parental_substance_abuse_filtered_CAs <- as.data.frame(ifelse(parental_substance_abuse_filtered[, -1] == 1, 1, 0)) 

# Create new column by summing the rows
parental_substance_abuse_filtered_CAs$total <- rowSums(parental_substance_abuse_filtered_CAs, na.rm = TRUE)

# Select the person with at least one parental substance abuse
parental_substance_abuse_filtered_CAs$total <- ifelse(parental_substance_abuse_filtered_CAs$total == 0, 0, 1) 

# Merge final dataset for parental substance abuse with "CaseID" and "total" 
# (whether there is at least one parental substance abuse or not)
parental_substance_abuse_final <- cbind(parental_substance_abuse_filtered$CASEID, parental_substance_abuse_filtered_CAs$total)
colnames(parental_substance_abuse_final) <- c("CASEID", "PSA")



####################### 3- Parental Criminality ################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(parental_criminality)) {
  parental_criminality[, i] <- ifelse(parental_criminality[, i] %in% c(-8, -9), NA, parental_criminality[, i])
}

# Then NA values were removed
m3 <- c()
for(i in 1:nrow(parental_criminality)){
  if(sum(is.na(parental_criminality[i, -1])) == (ncol(parental_criminality) - 1)){
    m3 <- c(m3, i)
  }
}
parental_criminality_filtered <- parental_criminality[-m3, ] # remove all NA values

# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
parental_criminality_filtered_CAs <- as.data.frame(ifelse(parental_criminality_filtered[, -1] == 1, 1, 0)) 

# Create new column by summing the rows
parental_criminality_filtered_CAs$total <- rowSums(parental_criminality_filtered_CAs, na.rm = TRUE)

# Select the person with at least one parental criminality
parental_criminality_filtered_CAs$total <- ifelse(parental_criminality_filtered_CAs$total == 0, 0, 1) 

# Merge final dataset for parental criminality abuse with "CaseID" and "total" 
# (whether there is at least one parental criminality or not)
parental_criminality_final <- cbind(parental_criminality_filtered$CASEID, parental_criminality_filtered_CAs$total)
colnames(parental_criminality_final) <- c("CASEID", "PC")



######################### 4- Family Violence ##################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(family_violence)) {
  family_violence[, i] <- ifelse(family_violence[, i] %in% c(-8, -9), NA, family_violence[, i])
}

# Then NA values were removed
m4 <- c()
for(i in 1:nrow(family_violence)){
  if(sum(is.na(family_violence[i, -1])) == (ncol(family_violence) - 1)){
    m4 <- c(m4, i)
  }
}
family_violence_filtered <- family_violence[-m4, ] # remove all NA values


# Convert the values with 1,2,3 into one and the value with 4 into 0 ((1 (often), 2 (sometimes), 3(rarely), 4(never))
for (i in 2:ncol(family_violence_filtered)) {
  family_violence_filtered[, i] <- ifelse(family_violence_filtered[, i] %in% c(1, 2, 3), 1, 
                                          ifelse(family_violence_filtered[, i] == 4, 0, family_violence_filtered[, i]))
}

# Create new column by summing the rows
family_violence_filtered$total <- rowSums(family_violence_filtered[, -1], na.rm = TRUE)

# Select the person with at least one parental criminality
family_violence_filtered$total <- ifelse(family_violence_filtered$total == 0, 0, 1) 

# Merge final dataset for family violence with "CaseID" and "total" 
# (whether there is at least one family violence or not)
family_violence_final <- cbind(family_violence_filtered$CASEID, family_violence_filtered$total)
colnames(family_violence_final) <- c("CASEID", "FV")




############################ 5- Physical Abuse #################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(physical_abuse)) {
  physical_abuse[, i] <- ifelse(physical_abuse[, i] %in% c(-8, -9), NA, physical_abuse[, i])
}

# Then NA values were removed
m5 <- c()
for(i in 1:nrow(physical_abuse)){
  if(sum(is.na(physical_abuse[i, -1])) == (ncol(physical_abuse) - 1)){
    m5 <- c(m5, i)
  }
}
physical_abuse_filtered <- physical_abuse[-m5, ] # remove all NA values


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
physical_abuse_filtered_CAs <- as.data.frame(ifelse(physical_abuse_filtered[, -1] == 1, 1, 0)) 

# Create new column by summing the rows
physical_abuse_filtered_CAs$total <- rowSums(physical_abuse_filtered_CAs, na.rm = TRUE)

# Select the person with growing with parental physical abuse
physical_abuse_filtered_CAs$total <- ifelse(physical_abuse_filtered_CAs$total == 0, 0, 1) 

# Select final dataset for parental physical abuse with "CaseID" and "total"
physical_abuse_final <- cbind(physical_abuse_filtered$CASEID, physical_abuse_filtered_CAs$total)
colnames(physical_abuse_final) <- c("CASEID", "PA")




############################## 7- Neglect ######################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(neglect)) {
  neglect[, i] <- ifelse(neglect[, i] %in% c(-8, -9), NA, neglect[, i])
}

# Then NA values were removed
m7 <- c()
for(i in 1:nrow(neglect)){
  if(sum(is.na(neglect[i, -1])) == (ncol(neglect) - 1)){
    m7 <- c(m7, i)
  }
}
neglect_filtered <- neglect[-m7, ] # remove all NA values


# Convert the values with 1,2,3 into one and the value with 4 into 0 ((1 (often), 2 (sometimes), 3(rarely), 4(never))
for (i in 2:ncol(neglect_filtered)) {
  neglect_filtered[, i] <- ifelse(neglect_filtered[, i] %in% c(1, 2, 3), 1, 
                                          ifelse(neglect_filtered[, i] == 4, 0, neglect_filtered[, i]))
}

# Create new column by summing the rows
neglect_filtered$total <- rowSums(neglect_filtered[, -1], na.rm = TRUE)

# Select the person with at least one parental criminality
neglect_filtered$total <- ifelse(neglect_filtered$total == 0, 0, 1) 

# Select final dataset for neglect with "CaseID" and "total"
neglect_final <- cbind(neglect_filtered$CASEID, neglect_filtered$total)
colnames(neglect_final) <- c("CASEID", "Neglect")



######################### 8- Parental Death ####################################
# Eliminate the Missing Values
m8 <- c() # create empty vector to assign the order of rows with missing values
for (i in 1:nrow(parental_death)) {
  if (-9 %in% parental_death[i, ] || -8 %in% parental_death[i, ]) {
    m8 <- c(m8, i)
  }
}
parental_death_filtered <- parental_death[-m8, ] # remove the rows with missing values

# Convert the age into the binary values (if somebody has age information, it takes 1; otherwise 0)
parental_death_filtered_CAs <- as.data.frame(ifelse(is.na(parental_death_filtered[, 2:ncol(parental_death_filtered)]), 0, 1))

# Create new column for selecting person with at least one parental death
parental_death_filtered_CAs$total <- ifelse(parental_death_filtered_CAs$CH2a + parental_death_filtered_CAs$CH2b == 0, 0, 1)

# Merge final dataset for parental death with "CaseID" and "total" (have at least one parental death)
parental_death_final <- cbind(parental_death_filtered$CASEID, parental_death_filtered_CAs$total)
colnames(parental_death_final) <- c("CASEID", "PDeath")




########################## 9- Parental Divorce #################################
# Eliminate the Missing Values
m9 <- c() # create empty vector to assign the order of rows with missing values
for (i in 1:nrow(parental_divorce)) {
  if (-9 %in% parental_divorce[i, ] || -8 %in% parental_divorce[i, ]) {
    m9 <- c(m9, i)
  }
}
parental_divorce_filtered <- parental_divorce[-m9, ] # remove the rows with missing values

# Create new column for selecting person with parental divorce
parental_divorce_filtered$total <- ifelse(is.na(parental_divorce_filtered[, 2:ncol(parental_divorce_filtered)]), 0, 1)

# Merge final dataset for parental death with "CaseID" and "total" (whether there is parental death or not)
parental_divorce_final <- cbind(parental_divorce_filtered$CASEID, parental_divorce_filtered$total)
colnames(parental_divorce_final) <- c("CASEID", "PDivorce")




####################### 10- Other Parental Loss ################################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(other_parental_loss)) {
  other_parental_loss[, i] <- ifelse(other_parental_loss[, i] %in% c(-8, -9), NA, other_parental_loss[, i])
}

# Then NA values were removed
m10 <- c()
for(i in 1:nrow(other_parental_loss)){
  if(sum(is.na(other_parental_loss[i, -1])) == (ncol(other_parental_loss) - 1)){
    m10 <- c(m10, i)
  }
}
other_parental_loss_filtered <- other_parental_loss[-m10, ] # remove all NA values


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
other_parental_loss_filtered$total <- ifelse(other_parental_loss_filtered[, -1] == 1, 1, 0)


# Select the final dataset for other parental loss with "CaseID" and "total"
other_parental_loss_final <- other_parental_loss_filtered[, c(1,3)]
colnames(other_parental_loss_final) <- c("CASEID", "OPL")




######################## 11- Physical Illness ##################################
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
  if (any(subset_physical_illness[i, 17:31] < 16, na.rm = T)) {
    subset_physical_illness$total[i] <- 1
  }
}

# Merge final dataset for physical illness with "CaseID" and "total" (total number of physical illness)
physical_illness_final <- cbind(subset_physical_illness$CASEID, subset_physical_illness$total)
colnames(physical_illness_final) <- c("CASEID", "PIllness")
  


######################### 12- Economic Adversity ###############################
# First -8 (do not know) and -9 (refused) were converted into NA values
for (i in 2:ncol(economic_adversity)) {
  economic_adversity[, i] <- ifelse(economic_adversity[, i] %in% c(-8, -9), NA, economic_adversity[, i])
}

# Then I removed NA values
m12 <- c()
for(i in 1:nrow(economic_adversity)){
  if(sum(is.na(economic_adversity[i, -1])) == (ncol(economic_adversity) - 1)){
    m12 <- c(m12, i)
  }
}
economic_adversity_filtered <- economic_adversity[-m12, ] # remove all NA values


# Convert the values 1 and 5 into 1 and zero (it is only taken 1 (YES) answers, and 5 is accepted zero)
economic_adversity_filtered$total <- ifelse(economic_adversity_filtered[, -1] == 1, 1, 0)


# Merge final dataset for parental physical abuse with "CaseID" and "total"
economic_adversity_final <- economic_adversity_filtered[, c(1,3)]
colnames(economic_adversity_final) <- c("CASEID", "EA")




########################## Merge Childhood Adversities #########################
################################################################################
nrow(parental_mental_illness_final); nrow(parental_substance_abuse_final)
df1 <- merge(parental_mental_illness_final, parental_substance_abuse_final, by = "CASEID", all = TRUE)
df2 <- merge(df1, parental_criminality_final, by = "CASEID", all = TRUE)
df3 <- merge(df2, family_violence_final, by = "CASEID", all = TRUE )
df4 <- merge(df3, physical_abuse_final, by = "CASEID", all = TRUE)
df5 <- merge(df4, neglect_final, by = "CASEID", all = TRUE)
df6 <- merge(df5, parental_death_final, by = "CASEID", all = TRUE)
df7 <- merge(df6, parental_divorce_final, by = "CASEID", all = TRUE)
df8 <- merge(df7, other_parental_loss_final, by = "CASEID", all = TRUE)
df9 <- merge(df8, physical_illness_final, by = "CASEID", all = TRUE)
merged_CAs <- merge(df9, economic_adversity_final, by = "CASEID", all = TRUE)

# Save the merged_CAs
save(merged_CAs, file = "merged_CAs.Rdata")


