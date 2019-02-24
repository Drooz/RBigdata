#################################################################
## Student ID : 1*****5                                        ##
## Name : Fares Saleh                          				         ##
##                                                             ##
##                                                             ## 
#################################################################



# Get the working directory. If needed, you can set the working directory to another folder.
getwd()
#setwd("C:/")


# Read the Data files from a directory  
data<-read.csv("C:****\\EuropeanSales.csv",header=T)


#Show attributes  
attributes(data)

# Show the Head
head(data) 


# Delete Country from the data 
dataNoCount <- subset(data , select = - Country)

#Correlation for all attributes
cor(dataNoCount[,])

# Fit All Arttibute Without the country
model2 <- lm(ComputerSales ~ ., data=dataNoCount)
summary(model2)

library(MASS)

# Step AIC in Both
#lm(formula = ComputerSales ~ Population + SalesPerCapita, data = dataNoCount) AIC =302.99
stepAIC(F,direction = "both")


# Fit the data 
F <- lm(formula = ComputerSales ~ Population + SalesPerCapita, data = dataNoCount)
summary(F)


plot(dataNoCount$ComputerSales,dataNoCount$Population)
plot(dataNoCount$ComputerSales,dataNoCount$SalesPerCapita)

# We fine 1 Outlier in OutVals1 and it should be removed
OutVals1 = boxplot(dataNoCount$SalesPerCapita)$out
OutVals2 = boxplot(dataNoCount$Population)$out
dataNoOut <- dataNoCount[-6,]

# Apply model again 
F <- lm(formula = ComputerSales ~ Population + SalesPerCapita, data = dataNoOut)
summary(F)
