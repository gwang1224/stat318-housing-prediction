data <- read.csv("~/Courses/STAT 318/Project/Data/cleaned_ames.csv")
attach(data)

library(MASS)
library(tree)
library(randomForest)
library(asbio)
library(sklearn)

# Variable Screening- Stepwise Automatic Selection
model.full <- lm(SalePrice~., data=data)

  # AIC model using stepwise selection
model.aic <- step(model.full, direction="both", k=2)
formula(model.aic) #formula for AIC model

  # BIC model using stepwise selection
model.bic <- step(model.full, direction="both", k=log(length(SalePrice)))
formula(model.bic) #formula for BIC model


# Regression Tree
  # fitting the tree
ames.tree <- tree(SalePrice~., data=data)
plot(ames.tree)
text(ames.tree,pretty=0,cex = 0.4)

  # pruning the tree
result <- cv.tree(ames.tree,K=10,FUN=prune.tree)
best_size <- result$size[which.min(result$dev)]
plot(result)

ames.tree.new <- prune.tree(ames.tree, best=best_size)
plot(ames.tree.new)
text(ames.tree.new,pretty=0,cex = 0.4)

# Using Random Forest Ensemble Method
model.randomForest <- randomForest(SalePrice~.,data=data,ntree=500,mtry=(ncol(data)-1)/3)


# LEAVE-ONE-OUT CROSS VALIDATION - comparing AIC, BIC, Regression Tree, and Random Forest models

CV_AIC <- press(lm(formula(model.aic), data=data))*(1/n) 

CV_BIC <- press(lm(formula(model.bic), data=data))*(1/n) 

CV_tree <- press(tree(SalePrice~., data=data))*(1/n)
  # k-fold cross validation caused issues, as the training set doesn't contain all the levels
  # of the predictors

#CV_randomForest <- press(randomForest(SalePrice~.,data=data,ntree=500,mtry=(ncol(data)-1)/3))*(1/n)

#AIC model has lowest leave-one-out CV score out of AIC and BIC

fitted.AIC <- lm(formula(model.aic), data=data)
summary(fitted.AIC)

fitted.BIC <- lm(formula(model.bic), data=data)
summary(fitted.BIC)
