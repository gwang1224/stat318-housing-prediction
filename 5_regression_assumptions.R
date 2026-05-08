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
