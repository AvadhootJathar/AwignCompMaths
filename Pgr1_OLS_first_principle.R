# Estimate least Square Regression

rm(list = ls(all.names = TRUE))
setwd("D://Personal//Awign//whyMaths")

install.packages("matlib")
library(matlib)
library(Matrix)
library(glmnet)

volume_data = read.csv("volume_price.csv")

# Sample size
N = dim(volume_data)[1]

# Let us denote the dependent, independent variables
# Volume is dependent variable
y = matrix(data = volume_data$Volume_.ln.,ncol = 1)

# The intercept in the model must be coded as a column of 1s
# This is first column in matrix X, and the second column is the log price variable
X = cbind(rep(1, dim(volume_data)[1]),volume_data$Price_.ln.)
p = dim(X)[2]

# OLS estimates
beta = inv(t(X)%*%X)%*%(t(X)%*%y)
beta

# Check (X'X)^-1  
inv(t(X)%*%X)

# Calculation of standard errors
sigma2 = (1/(N-p-1))*sum((y - mean(y))^2)
Var_beta = inv(t(X)%*%X)*sigma2
std_error = sqrt(diag(Var_beta))
std_error

cbind(beta,std_error)

# Regression
attach(volume_data)
out_lm = lm(Volume_.ln. ~ Price_.ln., data = volume_data)
summary(out_lm)

# Let us add another variable for price of a larger SKU of same brand
# We want to see if its price changes affect, the volumes of focus SKU
# y variable does not change, X has now 3 columns, p = 3
X = cbind(rep(1, dim(volume_data)[1]),volume_data$Price_.ln., volume_data$Large_SKU_Price_.ln.)
p = dim(X)[2]

beta = inv(t(X)%*%X)%*%(t(X)%*%y)

# Examine X'X
A = t(X)%*%X
rankMatrix(A)

# We know the system is not full-rank
# Let us examine how to handle non full-rank system
# One method is Ridge/LASSO regression as well as Elastic nets
# Let us include also competitor prices in the model
X = cbind(volume_data$Price_.ln., volume_data$Large_SKU_Price_.ln.,
          volume_data$Competitor1_Price_.ln., volume_data$Competitor2_Price_.ln.)
p = dim(X)[2]

# LASSO Regression
out2 = glmnet(X, y, alpha = 1)
print(out2)
round(coef(out2, s=0.000436),2)

# Ridge regression
out3 = glmnet(X, y, alpha = 0)
print(out3)
round(coef(out3, s=0.009),2)

# OLS
attach(volume_data)
out_lm2 = lm(Volume_.ln. ~ Price_.ln. + Large_SKU_Price_.ln. 
             + Competitor1_Price_.ln. + Competitor2_Price_.ln., data=volume_data)
summary(out_lm2)
