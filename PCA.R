#A script that looks at the correlation of various glass parameters and crystallisation
#clearing crap
rm(list=ls())

#setting wd
wd="C://Users/Jack/Documents/University/PhD/Code/Correlation and PC R/"
setwd(wd)

#required libraries
library(ggplot2)

#importing data
data <- read.csv("comp_mol_export_CCC_t_280424.csv")
data <- read.csv("comp_mol_export_CCC_vol_010524.csv")

#removing columns where all  values are identical
remove_cols<-c() #creating cols to be removed

#looping through each column to find ones with standard dev =0 (i.e. all constatnt)
for (i in 1:ncol(data)){
  
  #calculating standard deviation of column
  sd <- sd(data[,i])
  
  #checking if standard deviation is zero
  if (sd == 0){
    
    #appending
    remove_cols <- c(remove_cols, i)
    
  }
  
}

#removing cols with a standard deviation of zero
data <- data[,-remove_cols]

#removing crystallisation column
data <- data[,-ncol(data)]

# Perform PCA
pca <- prcomp(data, scale. = TRUE)

#pca summary
pca_summary <- summary(pca)

#calculations for porportional and cumulative variance plots

#calculating
prop_var <- (pca$sdev^2) / sum(pca$sdev^2) 
cum_var <- cumsum(prop_var)

pc_numbers <- 1:length(prop_var) #listing PC numbers


cum_var <- data.frame(PC = pc_numbers, Cumulative_Variance = cum_var) #Cumulative variance plot data
prop_var <- data.frame(PC = pc_numbers, Proportional_Variance = prop_var) #Proportional variance plot data

#proportoional variance plot
prop_var_plot <- ggplot(prop_var, aes(x = PC, y = Proportional_Variance)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1, length(pc_numbers), by = 2)) +  # Adjust the breaks argument
  labs(
    x = "Principal Component",
    y = "Proportional Variance",
    title = "Proportional variance per PC"
  )

#cumulative variance plot
cum_var_plot <- ggplot(cum_var, aes(x = PC, y = Cumulative_Variance)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1, length(pc_numbers), by = 2)) +  # Adjust the breaks argument
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +  # Add horizontal line at 80% cumulative variance
  labs(
    x = "Principal Component",
    y = "Cumulative Variance",
    title = "Cumulative variance per PC"
  )


#displaying plots
print(prop_var_plot)
print(cum_var_plot)

#calculating coeficcients for all PCs
stand_data <- scale(data) #stardsing data

#empty matrices
pc_coeff <- matrix(NA, nrow = length(pc_numbers), ncol = length(pc_numbers))
pc_values <- matrix(NA, nrow = nrow(stand_data), ncol = ncol(pca$rotation))

#looping through each PC
for(i in pc_numbers){

  # Extract the rotation matrix for the current principal component
  pc_rot <-pca$rotation[,i]
  
  #storing coefficinets
  pc_coeff[i, ] <- pc_rot
  
  #calculating PC values
  pc_values[, i] <- stand_data %*% pc_rot
  
}

#finxing column names
colnames(pc_coeff) <- colnames(data)
rownames(pc_coeff) <- paste0("PC", pc_numbers)
colnames(pc_values) <- paste0("PC", pc_numbers)

#storing all PC values for all compositions
write.csv(pc_values, "pc_values_export.csv")

######Testing just for PC for Nth composition
# pc  <- 2  # PC to test
# nth <- 2 #composition number
# pc_calc <- 0 #resetting value
# for (i in pc_numbers){
#   
#   pc_calc <- pc_calc + stand_data[nth,i] * pc_coeff[pc,i]
#   
# }
# print(pc_calc)

# #####code to calculate PCs for new data points (normalisation/standardisation required.)
# 
# #importing new data that is in the same format as data!
# #importing data
# new_data <- read.csv("new_data.csv")
# 
# #removing columns where all  values are identical
# remove_cols<-c() #creating cols to be removed
# 
# #looping through each column to find ones with standard dev =0 (i.e. all constatnt)
# for (i in 1:ncol(new_data)){
# 
#   #calculating standard deviation of column
#   sd <- sd(new_data[,i])
# 
#   #checking if standard deviation is zero
#   if (sd == 0){
# 
#     #appending
#     remove_cols <- c(remove_cols, i)
# 
#   }
# 
# }
# 
# #removing cols with a standard deviation of zero
# new_data <- new_data[,-remove_cols]
# 
# #removing crystallisation column
# new_data <- new_data[,-ncol(new_data)]
# 
# # 1. Standardize the new data using the same scaling parameters as the original data
# stand_new_data <- scale(new_data, center = pca$center, scale = pca$scale)
# 
# # 2. Transform the standardized new data using the PCA model
# new_pc_scores <- as.matrix(standardized_new_data) %*% pca$rotation