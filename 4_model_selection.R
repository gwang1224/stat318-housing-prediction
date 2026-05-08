# ------------------------------------
# Model selection
# ------------------------------------

data <- read.csv("data/ames_fully_cleaned.csv")

attach(data)
library(MASS)
library(dplyr)

# Variable Screening- Stepwise Automatic Selection
model.full <- lm(SalePrice~., data=data)

  # AIC model using stepwise selection
model.aic <- step(model.full, direction="both", k=2)
AIC(model.aic)
BIC(model.aic)
formula(model.aic) #formula for AIC model

  # BIC model using stepwise selection      !!!!! The model we chose !!!!!
model.bic <- step(model.full, direction="both", k=log(length(SalePrice)))
AIC(model.bic)
BIC(model.bic)
formula(model.bic) #formula for BIC model

# Predictor Variables
predictors_aic <- all.vars(formula(model.aic)[-2])
predictors_bic <- all.vars(formula(model.bic)[-2])

length(predictors_aic) #37
length(predictors_bic) #23

# ------------------------------------
# Regression Assumptions for BIC Model
# ------------------------------------
ames.train = read.csv("data/ames_fully_cleaned.csv", header=TRUE)

bic.model = lm(SalePrice ~ Lot.Area + Lot.Config + Land.Slope + Neighborhood + 
                 Condition.1 + Bldg.Type + Overall.Qual + Overall.Cond + Exter.Qual + 
                 Foundation + Bsmt.Qual + Total.Bsmt.SF + Bsmt.Full.Bath + 
                 Full.Bath + Half.Bath + Kitchen.AbvGr + Kitchen.Qual + TotRms.AbvGrd + 
                 Fireplaces + Garage.Area + Wood.Deck.SF + Screen.Porch + 
                 Misc.Val, data=ames.train)

summary(bic.model) #adj r^2 = 0.885

par(mfrow = c(2,2))
plot(bic.model)


cooks <- cooks.distance(bic.model)
n <- nrow(ames.train)
p <- length(coef(bic.model))


# 50th percentile of F distribution
cutoff <- qf(0.50, df1 = p, df2 = n - p)

plot(cooks, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance",
     xlab = "Observation Index"
)
abline(h=cutoff, col="red")

ames.train.clean <- ames.train[-c(1931), ]

new.bic.model <- lm( formula(bic.model), data = ames.train.clean)


plot(resid(new.bic.model)~fitted(new.bic.model), xlab="Y.hat", ylab="Residuals", main = "Residual Plot")
abline(h=0, col="red")

# Outlier 2
cooks <- cooks.distance(new.bic.model)
n <- nrow(ames.train.clean)
p <- length(coef(new.bic.model))


# 50th percentile of F distribution
cutoff <- qf(0.50, df1 = p, df2 = n - p)

plot(cooks, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance",
     xlab = "Observation Index"
)
abline(h=cutoff, col="red")

ames.train.clean <- ames.train[-c(1333), ]

new.bic.model <- lm( formula(bic.model), data = ames.train.clean)


plot(resid(new.bic.model)~fitted(new.bic.model), xlab="Y.hat", ylab="Residuals", main = "Residual Plot")
abline(h=0, col="red")

summary(new.bic.model)

# Still non-constant variance

library(MASS)
par(mfrow = c(1,1))
bc = boxcox(new.bic.model)
bc$x[which.max(bc$y)] #0.26

new.bic.model = lm(SalePrice^0.18 ~ Lot.Area + Lot.Config + Land.Slope + Neighborhood + 
                     Condition.1 + Bldg.Type + Overall.Qual + Overall.Cond + Exter.Qual + 
                     Foundation + Bsmt.Qual + Total.Bsmt.SF + Bsmt.Full.Bath + 
                     Full.Bath + Half.Bath + Kitchen.AbvGr + Kitchen.Qual + TotRms.AbvGrd + 
                     Fireplaces + Garage.Area + Wood.Deck.SF + Screen.Porch + 
                     Misc.Val, data=ames.train.clean)

summary(new.bic.model)

par(mfrow = c(2,2))
plot(new.bic.model)

par(mfrow = c(1,1))
# Validate time series
res <- residuals(new.bic.model)
plot(ames.train.clean$Yr.Sold, res,
     type = "p",
     pch = 19,
     xlab = "Year Sold",
     ylab = "Residual",
     main = "Residuals Versus Year Sold")

abline(h = 0, lty = 2)

summary(new.bic.model)

# is the BIC model nested in the AIC model? YES
length(intersect(predictors_aic,predictors_bic)) #length 23

anova(model.bic, model.aic)

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
CV.score.AIC #924509276
Adj.R2.AIC #0.872163

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
CV.score.BIC #920106898
Adj.R2.BIC #0.8653614



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

most_sig_AIC <- head(sort(coefs_sig_aic[-1]))
most_sig_BIC <- head(sort(coefs_sig_bic[-1]))

