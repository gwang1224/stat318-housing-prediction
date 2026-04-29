# ------------------------------------
# Regression Assumptions for AIC Model
# ------------------------------------
ames.train = read.csv("/Users/gracewang/stat318-housing-prediction/data/ames_train.csv", header=TRUE)
ames.test = read.csv("/Users/gracewang/stat318-housing-prediction/data/ames_test.csv", header=TRUE)


aic.model = lm(SalePrice ~ Lot.Area + Street + Land.Contour + Lot.Config + Land.Slope + 
                 Neighborhood + Condition.1 + Condition.2 + Bldg.Type + Overall.Qual + 
                 Overall.Cond + Year.Built + Year.Remod.Add + Roof.Matl + 
                 Exterior.1st + Mas.Vnr.Type + Mas.Vnr.Area + Exter.Qual + 
                 Bsmt.Qual + Total.Bsmt.SF + Gr.Liv.Area + Bsmt.Full.Bath + 
                 Bsmt.Half.Bath + Full.Bath + Bedroom.AbvGr + Kitchen.AbvGr + 
                 Kitchen.Qual + Functional + Fireplaces + Fireplace.Qu + Garage.Type + 
                 Garage.Cars + Garage.Area + Wood.Deck.SF + Screen.Porch + 
                 Pool.QC + Misc.Feature + Yr.Sold + Sale.Type + Sale.Condition, data=ames.train)
summary(aic.model)

plot(resid(aic.model)~fitted(aic.model), xlab="Y.hat", ylab="Residuals")
abline(h=0, col="red")

library(MASS)
bc = boxcox(aic.model)
bc$x[which.max(bc$y)] #0.35

trans.aic.model = lm(SalePrice^0.35 ~ Lot.Area + Street + Land.Contour + Lot.Config + Land.Slope + 
                 Neighborhood + Condition.1 + Condition.2 + Bldg.Type + Overall.Qual + 
                 Overall.Cond + Year.Built + Year.Remod.Add + Roof.Matl + 
                 Exterior.1st + Mas.Vnr.Type + Mas.Vnr.Area + Exter.Qual + 
                 Bsmt.Qual + Total.Bsmt.SF + Gr.Liv.Area + Bsmt.Full.Bath + 
                 Bsmt.Half.Bath + Full.Bath + Bedroom.AbvGr + Kitchen.AbvGr + 
                 Kitchen.Qual + Functional + Fireplaces + Fireplace.Qu + Garage.Type + 
                 Garage.Cars + Garage.Area + Wood.Deck.SF + Screen.Porch + 
                 Pool.QC + Misc.Feature + Yr.Sold + Sale.Type + Sale.Condition, data=ames.train)
summary(trans.aic.model)

plot(resid(trans.aic.model) ~ fitted(trans.aic.model), xlab="Y.hat", ylab="Residuals")
abline(h=0, col="red")
