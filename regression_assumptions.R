# ------------------------------------
# Regression Assumptions for BIC Model
# ------------------------------------
ames.train = read.csv("/Users/gracewang/stat318-housing-prediction/data/AmesHousing_no_na.csv", header=TRUE)

bic.model = lm(SalePrice ~ Lot.Area + Lot.Config + Land.Slope + Neighborhood + 
                 Condition.1 + Bldg.Type + House.Style + Overall.Qual + Overall.Cond + 
                 Year.Built + Mas.Vnr.Type + Mas.Vnr.Area + Exter.Qual + Bsmt.Qual + 
                 Gr.Liv.Area + Bsmt.Full.Bath + Full.Bath + Bedroom.AbvGr + 
                 Kitchen.Qual + Functional + Fireplaces + Garage.Cars + Wood.Deck.SF + 
                 Screen.Porch + Misc.Val + Sale.Condition, data=ames.train)

summary(bic.model) #adj r^2 = 0.888


# Residual plot
plot(resid(bic.model)~fitted(bic.model), xlab="Y.hat", ylab="Residuals")
abline(h=0, col="red")

# Normality
qqnorm(resid(bic.model))
qqline(resid(bic.model), col = "red")



# Outliers
cooks <- cooks.distance(bic.model)


n <- nrow(ames.train)
p <- length(coef(bic.model))


# 50th percentile of F distribution
cutoff <- qf(0.50, df1 = p, df2 = n - p)

plot(
  cooks,
  type = "h",
  main = "Cook's Distance",
  ylab = "Cook's Distance",
  xlab = "Observation Index"
)
abline(h=cutoff, col="red")

ames.train.clean <- ames.train[-c(1931, 1333, 1673), ]

new.bic.model.clean <- update(
  bic.model,
  data = ames.train.clean
)

summary(new.bic.model.clean) #Adj R^2 = 0.9148

plot(resid(new.bic.model.clean)~fitted(new.bic.model.clean), xlab="Y.hat", ylab="Residuals")
abline(h=0, col="red")



library(MASS)
bc = boxcox(new.bic.model.clean)
bc$x[which.max(bc$y)] #0.26

new.bic.model = lm(SalePrice^0.26 ~ Lot.Area + Lot.Config + Land.Slope + Neighborhood + 
                     Condition.1 + Bldg.Type + House.Style + Overall.Qual + Overall.Cond + 
                     Year.Built + Mas.Vnr.Type + Mas.Vnr.Area + Exter.Qual + Bsmt.Qual + 
                     Gr.Liv.Area + Bsmt.Full.Bath + Full.Bath + Bedroom.AbvGr + 
                     Kitchen.Qual + Functional + Fireplaces + Garage.Cars + Wood.Deck.SF + 
                     Screen.Porch + Misc.Val + Sale.Condition, data=ames.train.clean)

summary(new.bic.model)
anova(new.bic.model)

# Resid plot
plot(resid(new.bic.model)~fitted(new.bic.model), xlab="Y.hat", ylab="Residuals")
abline(h=0, col="red")

# FINAL
qqnorm(resid(new.bic.model))
qqline(resid(new.bic.model), col = "red")


