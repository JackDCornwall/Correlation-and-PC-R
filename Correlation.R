#A script that looks at the correlation of various glass parameters and crystallisation
#clearing crap
rm(list=ls())

#setting wd
wd="C://Users/Jack/Documents/University/PhD/Code/Correlation and PC R//"
setwd(wd)

#iimporting required packages
library(ggplot2)

#importing data
data <- read.csv("comp_mol_export_CCC_t_280424.csv")
#data <- read.csv("comp_mol_export_CCC_vol_010524.csv")

#removing columns where all  values are identical
remove_cols<-c() #creating cols to be removed

#looping through each column to find ones with standard dev =0 (i.e. all constatnt)
for (i in 1:ncol(data)){
  
  #calculating standard deviation of column
  sd <- sd(data[,i])
  
  print(sd)
  
  #checking if standard deviation is zero
  if (sd == 0){
    
    #appending
    remove_cols <- c(remove_cols, i)
    
    print(i)
    
  }
  
}

#removing cols with a standard deviation of zero
data <- data[,-remove_cols]

#correlation
correlation_matrix <- cor(data)

#summary
data_summary <- summary(data)

#plotting correlation matrix
corr_plot <- ggplot(data = as.data.frame(as.table(correlation_matrix)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red") +
  labs(x = "Variables", y = "Variables", title = "Correlation Matrix ~800 CCC Glasses Without Nepheline Vol%", fill = "Corr. Coeff.") +
  #labs(x = "Variables", y = "Variables", title = "Correlation Matrix ~300 CCC Glasses With Nepheline Vol%", fill = "Corr. Coeff.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(corr_plot)

#extracting desired correlations
NP_CCC <- as.data.frame(correlation_matrix)
NP_CCC <- NP_CCC[,c(nrow(NP_CCC) -1 ,nrow(NP_CCC))]
