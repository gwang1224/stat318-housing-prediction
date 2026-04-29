# ------------------------------------
# Regression Assumptions for AIC Model
# ------------------------------------
ames.train = read.csv("/Users/gracewang/stat318-housing-prediction/data/ames_train.csv", header=TRUE)
ames.test = read.csv("/Users/gracewang/stat318-housing-prediction/data/ames_test.csv", header=TRUE)

bic.model = lm(SalePrice ~ Lot.Area + Street + Land.Slope + Neighborhood + Condition.2 + 
                 Bldg.Type + Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add + 
                 Roof.Matl + Mas.Vnr.Area + Exter.Qual + Bsmt.Qual + Total.Bsmt.SF + 
                 Gr.Liv.Area + Bsmt.Full.Bath + Bedroom.AbvGr + Kitchen.AbvGr + 
                 Kitchen.Qual + Functional + Fireplaces + Garage.Area + Wood.Deck.SF + 
                 Screen.Porch + Pool.QC + Misc.Feature + Sale.Condition, data=ames.train)

summary(bic.model) #adj r^2 = 0.9135


# Residual plot

plot(resid(bic.model)~fitted(bic.model), xlab="Y.hat", ylab="Residuals")
abline(h=0, col="red")

library(MASS)
bc = boxcox(bic.model)
bc$x[which.max(bc$y)] #0.30

new.bic.model = lm(SalePrice^0.3 ~ Lot.Area + Street + Land.Slope + Neighborhood + Condition.2 + 
                 Bldg.Type + Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add + 
                 Roof.Matl + Mas.Vnr.Area + Exter.Qual + Bsmt.Qual + Total.Bsmt.SF + 
                 Gr.Liv.Area + Bsmt.Full.Bath + Bedroom.AbvGr + Kitchen.AbvGr + 
                 Kitchen.Qual + Functional + Fireplaces + Garage.Area + Wood.Deck.SF + 
                 Screen.Porch + Pool.QC + Misc.Feature + Sale.Condition, data=ames.train)

summary(new.bic.model) #Adj R^2 = 0.9255

plot(resid(new.bic.model) ~ fitted(new.bic.model), xlab="Y.hat", ylab="Residuals")
abline(h=0, col="red")


# Normality
qqnorm(resid(new.bic.model))
qqline(resid(new.bic.model), col = "red")

# Outliers
cooks <- cooks.distance(new.bic.model)

sort(cooks, decreasing = TRUE)[1:10]
influential_index <- which.max(cooks)

plot(
  cooks,
  type = "h",
  main = "Cook's Distance",
  ylab = "Cook's Distance",
  xlab = "Observation Index"
)


ames.train.clean <- ames.train[-2275, ]

new.bic.model.clean <- update(
  new.bic.model,
  data = ames.train.clean
)

summary(new.bic.model.clean) #Adj R^2 = 0.9318
anova(new.bic.model.clean)

# Resid plot
plot(resid(new.bic.model.clean)~fitted(new.bic.model.clean), xlab="Y.hat", ylab="Residuals")
abline(h=0, col="red")

# FINAL
qqnorm(resid(new.bic.model.clean))
qqline(resid(new.bic.model), col = "red")


