data <- read.csv("~/Courses/STAT 318/Project/Data/ames_fully_cleaned.csv")

attach(data)

library(MASS)
library(tree)
library(randomForest)
library(asbio)
library(dplyr)
library(tidyverse)

# Variable Screening- Stepwise Automatic Selection
model.full <- lm(SalePrice~., data=data)

  # AIC model using stepwise selection
model.aic <- step(model.full, direction="both", k=2)
formula(model.aic) #formula for AIC model

  # BIC model using stepwise selection      !!!!! The model we chose !!!!!
model.bic <- step(model.full, direction="both", k=log(length(SalePrice)))
formula(model.bic) #formula for BIC model

# Predictor Variables
predictors_aic <- all.vars(formula(model.aic)[-2])
predictors_bic <- all.vars(formula(model.bic)[-2])

length(predictors_aic) #36
length(predictors_bic) #26

# is the BIC model nested in the AIC model? YES
length(intersect(predictors_aic,predictors_bic)) #length 26


# K-Fold CV
n <- 2590
K <- 5   #each fold contains 5 observations
n.fold <- floor(n/K)

n.shuffle <- sample(1:n, n, replace=FALSE)
index.fold <- list()

# shuffling the indices around so that it is randomly grouped into 5's
for(i in 1:K) {
  if(i<K)
  {
    index.fold[[i]] <- n.shuffle[((i-1)*n.fold+1):(i*n.fold)]
  }else
  {
    index.fold[[i]] <- n.shuffle[((K-1)*n.fold+1):n]
  }
}

# Computing for AIC
CV.score.AIC <- 0
Adj.R2.AIC <- 0
for(i in 1:K) {
  #fit the full model based on the data excluding the ith fold
  fit <- lm(formula(model.aic), data=data[-index.fold[[i]],])
  
  #make prediction on each observation in the ith fold
  pred <- predict(fit, data[index.fold[[i]],])
  
  #compute average squared error for the ith fold
  CV.score.AIC <- CV.score.AIC+(1/n)*sum((SalePrice[index.fold[[i]]]-pred)^2)
  
  #computing adj r2
  Adj.R2.AIC <- Adj.R2.AIC + (1/K)*summary(fit)$adj.r.squared
}
CV.score.AIC #780134763
Adj.R2.AIC #0.8926555

# Computing for BIC
CV.score.BIC <- 0
Adj.R2.BIC <- 0
for(i in 1:K) {
  #fit the full model based on the data excluding the ith fold
  fit <- lm(formula(model.bic), data=data[-index.fold[[i]],])
  
  #make prediction on each observation in the ith fold
  pred <- predict(fit, data[index.fold[[i]],])
  
  #compute average squared error for the ith fold
  CV.score.BIC <- CV.score.BIC+(1/n)*sum((SalePrice[index.fold[[i]]]-pred)^2)
  
  #computing adj r2
  Adj.R2.BIC <- Adj.R2.BIC + (1/K)*summary(fit)$adj.r.squared
}
CV.score.BIC #788669326
Adj.R2.BIC #0.8890774




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





# Getting highly significant predictors P<0.001
coefs <- summary(lm(formula(model.aic), data=data))$coefficients[,4]
length(coefs)
coefs_sig_aic <- coefs[coefs < 0.001]
which(coefs_sig_aic == min(coefs_sig_aic))

coefs_bic <- summary(lm(formula(model.bic), data=data))$coefficients[,4]
length(coefs_bic)
coefs_sig_bic <- coefs_bic[coefs_bic < 0.001]
which(coefs_sig_bic == min(coefs_sig_bic))

coefs_aic_df <- data.frame(coefs_sig_aic)
coefs_aic_df.index.name <- "coefs"
ggplot(coefs_aic_df, aes(x = class)) +
  geom_bar()

most_sig_AIC <- head(sort(coefs_sig_aic[-1]))
most_sig_BIC <- head(sort(coefs_sig_bic[-1]))

#plotting most significant predictors of AIC and BIC models

