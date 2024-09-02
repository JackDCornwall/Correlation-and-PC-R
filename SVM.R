#A script that uses SVM to for crystallisation
#clearing crap
rm(list=ls())

#setting wd
wd="C://Users/Jack/Documents/University/PhD/Code/Correlation and PC R/"
setwd(wd)

#required libraries
#install.packages("e1071")
library(e1071)

#importing data
#data <- read.csv("comp_mol_export_CCC_t_280424.csv")
data <- read.csv("comp_mol_export_CCC_vol_010524.csv")
#data <- read.csv("pc_non_vol_2_values_export.csv")
#data <- read.csv("pc_vol_10_values_export.csv")

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

#removing columns if there are some to remove
if (length(remove_cols) != 0){
  
  #removing cols with a standard deviation of zero
  data <- data[,-remove_cols]
}

#checking whether binary or contunuous to store as factor
if(length(unique(data[,ncol(data)]))<3){
  
  #storing NP_CCC as factor if necessary
  data[,ncol(data)] <- as.factor(data[,ncol(data)])
}


#selecting features (aka oxides and glass making parameters) and target (aka crystallisation)
#or PC's
features <- data[, -ncol(data)]

target <- as.vector(data[, ncol(data)])
#target <- data[, ncol(data), drop = TRUE]

#randomly training dataset
#set.seed(123)#setting a seed for reproducibility
train_index <- sample(1:nrow(data), 0.7*nrow(data)) #random indices for the training set
train_data <- data[train_index, ] #selecting training data
test_data <- data[-train_index, ] #verification data

svm_model <- svm(NP_CCC ~ ., data = train_data, kernel = "radial")

#Evaliating model if binary, allowing 10% error if continuous
if(length(unique(data[,ncol(data)]))<3){
  
  #Evaluate Model with no leway
  predicted <- predict(svm_model, newdata = test_data)
  accuracy <- mean(predicted == test_data$NP_CCC)
  
}else{
  #tolerance level
  tolerance <- 0.1
  
  # Compute predictions
  predicted <- predict(svm_model, newdata = test_data)
  
  # Compute accuracy with tolerance range
  accuracy <- mean(abs(predicted - test_data$NP_CCC) <= tolerance)
  
}

#Evaluate Model 
print(accuracy)

##### EXAMPLE FOR PREDICTIONS
# # Make predictions on new data
# predictions <- predict(svm_model, newdata = new_data)
# 
# # View the predictions
# print(predictions)