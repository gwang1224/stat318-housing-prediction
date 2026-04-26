data <- read.csv("/Users/gracewang/stat318-housing-prediction/data/cleaned_ames.csv")
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


# ---------------------------- MLR ------------------------------

#SalePrice ~ Lot.Area + Street + Land.Slope + Neighborhood + Condition.2 + 
#  Bldg.Type + Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add + 
#  Roof.Matl + Mas.Vnr.Area + Exter.Qual + Bsmt.Qual + Total.Bsmt.SF + 
#  Gr.Liv.Area + Bsmt.Full.Bath + Bedroom.AbvGr + Kitchen.AbvGr + 
#  Kitchen.Qual + Functional + Fireplaces + Garage.Area + Wood.Deck.SF + 
#  Screen.Porch + Pool.QC + Misc.Feature + Sale.Condition

ames.train = read.csv("/Users/gracewang/stat318-housing-prediction/data/ames_train.csv", header=TRUE)
ames.test = read.csv("/Users/gracewang/stat318-housing-prediction/data/ames_test.csv", header=TRUE)


model = lm(SalePrice ~ Lot.Area + Street + Land.Slope + Neighborhood + Condition.2 + 
             Bldg.Type + Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add + 
             Roof.Matl + Mas.Vnr.Area + Exter.Qual + Bsmt.Qual + Total.Bsmt.SF + 
             Gr.Liv.Area + Bsmt.Full.Bath + Bedroom.AbvGr + Kitchen.AbvGr + 
             Kitchen.Qual + Functional + Fireplaces + Garage.Area + Wood.Deck.SF + 
             Screen.Porch + Pool.QC + Misc.Feature + Sale.Condition, data=ames.test)

test.pred = predict(model, newdata = ames.test, type="response")

residuals = ames.test$SalePrice - test.pred

MSE = mean(residuals^2) #536653526
RMSE = sqrt(MSE) #23165.78

plot(ames.test$SalePrice, test.pred,
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price",
     main = "Predicted vs Actual Sale Price")




# ---------------------------- Regression Tree ------------------------------




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
#model.randomForest <- randomForest(SalePrice~.,data=data,ntree=500,mtry=(ncol(data)-1)/3)


# LEAVE-ONE-OUT CROSS VALIDATION - comparing AIC, BIC, Regression Tree models
# getting rid of all the samples with single observation per categorical level
categorical <- names(data)[sapply(data, is.character)]
df_no_singletons <- data

repeat {
  before_n <- nrow(df_no_singletons)
  
  for (column_name in categorical) {
    counts <- table(df_no_singletons[[column_name]])
    keep_levels <- names(counts[counts > 1])
    
    df_no_singletons <- df_no_singletons[
      df_no_singletons[[column_name]] %in% keep_levels,
      ,
      drop = FALSE
    ]
  }
  
  if (nrow(df_no_singletons) == before_n) break
}


  # LOOCV for AIC 
attach(df_no_singletons)
N <- nrow(df_no_singletons)
diff <- rep(NA,N)

for (i in 1:N) {
  fit <- lm(formula(model.aic), data=df_no_singletons[-i,])
  Y.hat <- predict(fit, newdata = df_no_singletons[i,] )
  diff[i] <- SalePrice[i]-Y.hat
}

AIC_CV = mean(diff^2) #605886404
AIC_CV

  # LOOCV for BIC 
attach(df_no_singletons)
N <- nrow(df_no_singletons)
diff <- rep(NA,N)

for (i in 1:N) {
  fit <- lm(formula(model.bic), data=df_no_singletons[-i,])
  Y.hat <- predict(fit, newdata = df_no_singletons[i,] )
  diff[i] <- SalePrice[i]-Y.hat
}

BIC_CV = mean(diff^2) #620420014
BIC_CV

# LOOCV for regression tree
attach(df_no_singletons)
N <- nrow(df_no_singletons)
diff <- rep(NA,N)

for (i in 1:N) {
  fit <- tree(SalePrice~., data=df_no_singletons[-i,])
  Y.hat <- predict(fit, newdata = df_no_singletons[i,] )
  diff[i] <- SalePrice[i]-Y.hat
}

tree_CV = mean(diff^2) #1541472114
tree_CV

#AIC model has lowest leave-one-out CV score out of AIC and BIC

fitted.AIC <- lm(formula(model.aic), data=data)
summary(fitted.AIC)

fitted.BIC <- lm(formula(model.bic), data=data)
summary(fitted.BIC)
