# ------------------------------------
# Data Cleaning- Removing Missingness
# ------------------------------------
data = read.csv("data/AmesHousing.csv", header=TRUE)
summary(data)

# Inventory of NA's
colSums(is.na(data))
sum(colSums(is.na(data)) > 0) #25 features have missing values

# NA's in Pool.QC indicate that house does not have a pool, replace all NA's with NP = Not Present
data$Pool.QC[is.na(data$Pool.QC)] = "NP"

# Same with variables Fence, Misc.Feature, Fireplace.Qu, Garage.Type, Garage.Finish, Alley
data$Fence[is.na(data$Fence)] = "NP"
data$Misc.Feature[is.na(data$Misc.Feature)] = "NP"
data$Fireplace.Qu[is.na(data$Fireplace.Qu)] = "NP"
data$Garage.Type[is.na(data$Garage.Type)] = "NP"
data$Garage.Finish[is.na(data$Garage.Finish)] = "NP"
data$Alley[is.na(data$Alley)] = "NP"

# Indicator variable for when Garage is not present
data$Garage.Yr.Blt[is.na(data$Garage.Yr.Blt)] = "NP"
data$Garage.Qual[is.na(data$Garage.Qual)] = "NP"
data$Garage.Cond[is.na(data$Garage.Cond)] = "NP"

# Indicator variable for when Basement is not Present
data$Bsmt.Qual[is.na(data$Bsmt.Qual)] = "NP"
data$Bsmt.Cond[is.na(data$Bsmt.Cond)] = "NP"
data$Bsmt.Exposure[is.na(data$Bsmt.Exposure)] = "NP"
data$BsmtFin.Type.1[is.na(data$BsmtFin.Type.1)] = "NP"
data$BsmtFin.Type.2[is.na(data$BsmtFin.Type.2)] = "NP"

# Indicator variable when there is no Masonry veneer 
data$Mas.Vnr.Area[is.na(data$Mas.Vnr.Area)] = "NP"


# Lot.Frontage 490 samples is 16.7% of the data, opt to remove Lot.Frontage
data = subset(data, select = -Lot.Frontage)


# 3 samples that comtain many missing values
rows = data[is.na(data$BsmtFin.SF.1), ]
colnames(rows)[colSums(is.na(rows)) > 0]
data = data[!is.na(data$BsmtFin.SF.2),]

rows = data[is.na(data$Bsmt.Full.Bath),]
colnames(rows)[colSums(is.na(rows)) > 0]
data = data[!is.na(data$Bsmt.Full.Bath),]

rows = data[is.na(data$Garage.Cars), ]
colnames(rows)[colSums(is.na(rows)) > 0]
data = data[!is.na(data$Garage.Cars),]


# Removing columns for Order, PID - identifiers not used for prediction
data = data[,-c(1,2)]

colSums(is.na(data))

write.csv(data, "/Users/gracewang/stat318-housing-prediction/data/AmesHousing_no_na.csv", row.names = FALSE)



# ------------------------------------
# Multicollinearity screening
# ------------------------------------

library(car)

ames = read.csv("data/AmesHousing_no_na.csv", header=TRUE)

factor_vars <- c(
  "MS.SubClass",
  "MS.Zoning",
  "Street",
  "Alley",
  "Lot.Shape",
  "Land.Contour",
  "Utilities",
  "Lot.Config",
  "Land.Slope",
  "Neighborhood",
  "Condition.1",
  "Condition.2",
  "Bldg.Type",
  "House.Style",
  "Roof.Style",
  "Roof.Matl",
  "Exterior.1st",
  "Exterior.2nd",
  "Mas.Vnr.Type",
  "Exter.Qual",
  "Exter.Cond",
  "Foundation",
  "Bsmt.Qual",
  "Bsmt.Cond",
  "Bsmt.Exposure",
  "BsmtFin.Type.1",
  "BsmtFin.Type.2",
  "Heating",
  "Heating.QC",
  "Central.Air",
  "Electrical",
  "Kitchen.Qual",
  "Functional",
  "Fireplace.Qu",
  "Garage.Type",
  "Garage.Finish",
  "Garage.Qual",
  "Garage.Cond",
  "Paved.Drive",
  "Pool.QC",
  "Fence",
  "Misc.Feature",
  "Sale.Type",
  "Sale.Condition"
)

ames[factor_vars] <- lapply(ames[factor_vars], factor)

# Check for aliases
model = lm(SalePrice~., data=ames)

al <- alias(model)
rownames(al$Complete)

colnames(al$Complete)[which.max(al$Complete["Bldg.TypeDuplex", ])]
colnames(al$Complete)[which.max(al$Complete["Exterior.2ndPreCast", ])]
colnames(al$Complete)[which.max(al$Complete["Bsmt.CondNP", ])]
colnames(al$Complete)[which.max(al$Complete["Bsmt.ExposureNP", ])]
colnames(al$Complete)[which.max(al$Complete["BsmtFin.Type.1NP", ])]
colnames(al$Complete)[which.max(al$Complete["BsmtFin.Type.2NP", ])]
colnames(al$Complete)[which.max(al$Complete["Total.Bsmt.SF", ])]
colnames(al$Complete)[which.max(al$Complete["Gr.Liv.Area", ])]
colnames(al$Complete)[which.max(al$Complete["Garage.FinishNP", ])]
colnames(al$Complete)[which.max(al$Complete["Garage.FinishUnf", ])]
colnames(al$Complete)[which.max(al$Complete["Garage.QualNP", ])]
colnames(al$Complete)[which.max(al$Complete["Garage.CondNP", ])]

ames = subset(ames, select = -c(MS.SubClass, 
                                Exterior.2nd, 
                                Bsmt.Cond, Bsmt.Exposure, BsmtFin.SF.1, BsmtFin.SF.2, Bsmt.Unf.SF, BsmtFin.Type.1, BsmtFin.Type.2, 
                                X1st.Flr.SF, X2nd.Flr.SF, Low.Qual.Fin.SF, 
                                Garage.Cond, Garage.Qual, Garage.Finish, Garage.Yr.Blt, Mas.Vnr.Area))

# Multicollinearity screening with GVIF
model = lm(SalePrice~., data=ames)
gvif = car::vif(model)
gvif[,3]^2

# Year.Built = 12.707
ames = subset(ames, select = -c(Year.Built))

model = lm(SalePrice~., data=ames)
gvif = car::vif(model)
gvif[,3]^2

#Pool.Area = 23.719
ames = subset(ames, select = -c(Pool.Area))

model = lm(SalePrice~., data=ames)
gvif = car::vif(model)
gvif[,3]^2

#Gr.Liv.Area = 11.171
ames = subset(ames, select = -c(Gr.Liv.Area))

model = lm(SalePrice~., data=ames)
gvif = car::vif(model)
gvif[,3]^2

write.csv(ames, "/Users/gracewang/stat318-housing-prediction/data/cleaned_ames.csv", row.names = FALSE)



# ------------------------------------
# Categorical Variable Preprocessing
# ------------------------------------

data <- read.csv("data/cleaned_ames.csv")
attach(data)

# MS.Zoning
data = data[-which(data$MS.Zoning=="A (agr)"),] #getting rid of A (agr):   2
data = data[-which(data$MS.Zoning=="C (all)"),] #getting rid of C (all): 25
data = data[-which(data$MS.Zoning=="I (all)"),] #getting rid of I (all): 2
data[which(data$MS.Zoning=="RH"),]$MS.Zoning = "RM"  #merging RH and RM

# Street: removing since all but 6 are "Pave"
data$Street = NULL


# Alley: Creating 2 levels: NP (there is no Alley) vs P (there is an Alley)
data[which(data$Alley=="Pave"),]$Alley = "P"
data[which(data$Alley=="Grvl"),]$Alley = "P"

# Lot.Shape: Creating 2 levels: Reg (regular) vs Irreg (irregular)
data[which(data$Lot.Shape=="IR1"),]$Lot.Shape = "Irreg"
data[which(data$Lot.Shape=="IR2"),]$Lot.Shape = "Irreg"
data[which(data$Lot.Shape=="IR3"),]$Lot.Shape = "Irreg"

# Land.Contour: Creating 2 levels: Lvl (level) vs NonLvl (non level)
data[which(data$Land.Contour=="Bnk"),]$Land.Contour = "NonLvl"
data[which(data$Land.Contour=="HLS"),]$Land.Contour = "NonLvl"
data[which(data$Land.Contour=="Low"),]$Land.Contour = "NonLvl"

# Utilities: removing since there are only 2 observations that aren't AllPub
data$Utilities = NULL

# Lot.Config: merging FR2 and FR3 to be FR
data[which(data$Lot.Config=="FR2"),]$Lot.Config = "FR"
data[which(data$Lot.Config=="FR3"),]$Lot.Config = "FR"

# Land.Slope: merging Mod and Sev to be Sloped
data[which(data$Land.Slope=="Mod"),]$Land.Slope = "Sloped"
data[which(data$Land.Slope=="Sev"),]$Land.Slope = "Sloped"

# Neighborhood: fine because each level is above 5%
#getting rid of rows with less than 10 observations for specific levels
data = data[-which(data$Neighborhood=="Landmrk"),] #getting rid of Landmrk:   1
data = data[-which(data$Neighborhood=="GrnHill"),] #getting rid of GrnHill:   2
data = data[-which(data$Neighborhood=="Greens"),]  #getting rid of Greens:   8
data = data[-which(data$Neighborhood=="Blueste"),]  #getting rid of Blueste:   10

#merging the rows with less than 100 observations into "other"
data[which(data$Neighborhood=="Blmngtn"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="BrDale"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="ClearCr"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="IDOTRR"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="NoRidge"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="NPkVill"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="StoneBr"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="SWISU"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="Timber"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="Veenker"),]$Neighborhood = "Other"
data[which(data$Neighborhood=="MeadowV"),]$Neighborhood = "Other"

# Condition.1: merging Feedr and Artery => Street
# Merging RRAe,RRAn,RRNe,RRNn (railroads) => RR
data[which(data$Condition.1=="Feedr"),]$Condition.1 = "Street"
data[which(data$Condition.1=="Artery"),]$Condition.1 = "Street"

data[which(data$Condition.1=="RRAn"),]$Condition.1 = "RR"
data[which(data$Condition.1=="RRAe"),]$Condition.1 = "RR"
data[which(data$Condition.1=="RRNe"),]$Condition.1 = "RR"
data[which(data$Condition.1=="RRNn"),]$Condition.1 = "RR"

data = data[-which(data$Condition.1=="PosA"),] #getting rid of PosA:   20
data = data[-which(data$Condition.1=="PosN"),] #getting rid of PosN:   39

# Condition.2: removing because basically all are Norm
data$Condition.2 = NULL

# Bldg.Type: Merging 2FmCon + Duplex => multifam, Twnhs + TwnhsE => townhouse
data[which(data$Bldg.Type=="2fmCon"),]$Bldg.Type = "multifam"
data[which(data$Bldg.Type=="Duplex"),]$Bldg.Type = "multifam"

data[which(data$Bldg.Type=="Twnhs"),]$Bldg.Type = "townhouse"
data[which(data$Bldg.Type=="TwnhsE"),]$Bldg.Type = "townhouse"

# House.Style: Removing 1.5Unf, 2.5Unf, 2.5Fin
data = data[-which(data$House.Style=="1.5Unf"),] 
data = data[-which(data$House.Style=="2.5Fin"),] 
data = data[-which(data$House.Style=="2.5Unf"),] 

# Roof.Style: Removing Flat, Gambrel, Mansard, Shed
data = data[-which(data$Roof.Style=="Flat"),] 
data = data[-which(data$Roof.Style=="Gambrel"),] 
data = data[-which(data$Roof.Style=="Mansard"),]
data = data[-which(data$Roof.Style=="Shed"),] 

# Roof.Matl: removing because basically all are CompShg
data$Roof.Matl = NULL

# Exterior.1st: Merging BrkComm and BrkFace => Brick
data[which(data$Exterior.1st=="BrkComm"),]$Exterior.1st = "Brick"
data[which(data$Exterior.1st=="BrkFace"),]$Exterior.1st = "Brick"

#Merging Stucco and ImStucc => Stucco
data[which(data$Exterior.1st=="ImStucc"),]$Exterior.1st = "Stucco"

#Merging Wd Sdng and WdShing => Wood
data[which(data$Exterior.1st=="Wd Sdng"),]$Exterior.1st = "Wood"
data[which(data$Exterior.1st=="WdShing"),]$Exterior.1st = "Wood"

#Dropping with levels **********************
data = data[-which(data$Exterior.1st=="AsphShn"),]
data = data[-which(data$Exterior.1st=="CBlock"),]
data = data[-which(data$Exterior.1st=="Stone"),] 
data = data[-which(data$Exterior.1st=="AsbShng"),] 

# Mas.Vnr.Type: Merging BrkComm and BrkFace => Brick, adding empty string to None
data[which(data$Mas.Vnr.Type=="BrkCmn"),]$Mas.Vnr.Type = "Brick"
data[which(data$Mas.Vnr.Type=="BrkFace"),]$Mas.Vnr.Type = "Brick"

data[which(data$Mas.Vnr.Type==""),]$Mas.Vnr.Type = "None"

# Exter.Qual: Merging Gd and Fa (good and fair) into good
data[which(data$Exter.Qual=="Fa"),]$Exter.Qual = "Gd"

# Exter.Cond: Merging Fa, Ta => Ta, Merging Ex and Gd => Gd, removing Po
data[which(data$Exter.Cond=="Ex"),]$Exter.Cond = "Gd"
data[which(data$Exter.Cond=="Fa"),]$Exter.Cond = "TA"
data = data[-which(data$Exter.Cond=="Po"),]

# Foundation: removing slab, stone, wood
data = data[-which(data$Foundation=="Slab"),]
data = data[-which(data$Foundation=="Stone"),]
data = data[-which(data$Foundation=="Wood"),]

# Bsmt.Qual: Nothing

# Heating: removing because basically all observations are GasA
data$Heating = NULL

# Heating.QC: removing Po, merging Ta and Fa
data[which(data$Heating.QC=="Fa"),]$Heating.QC = "Ta"
data = data[-which(data$Heating.QC=="Po"),]

# Electrical: removing '' and mix, merging FuseP and FuseF
data[which(data$Electrical=="FuseP"),]$Electrical = "FuseF"
data = data[-which(data$Electrical==""),]
data = data[-which(data$Electrical=="Mix"),]

# Kitchen.Qual: Merging Fa and TA, getting rid of Po
data[which(data$Kitchen.Qual=="Fa"),]$Kitchen.Qual = "TA"
data = data[-which(data$Kitchen.Qual=="Po"),]

# Functional: Merging to create two groups: ATyp + Typ
data[which(data$Functional=="Maj1"),]$Functional = "ATyp"
data[which(data$Functional=="Maj2"),]$Functional = "ATyp"
data[which(data$Functional=="Min1"),]$Functional = "ATyp"
data[which(data$Functional=="Min2"),]$Functional = "ATyp"
data[which(data$Functional=="Mod"),]$Functional = "ATyp"
data[which(data$Functional=="Sal"),]$Functional = "ATyp"
data[which(data$Functional=="Sev"),]$Functional = "ATyp"

# Fireplace.Qu: Merging Ex and Gd, Merging Fa and TA
data[which(data$Fireplace.Qu=="Ex"),]$Fireplace.Qu = "Gd"
data[which(data$Fireplace.Qu=="Fa"),]$Fireplace.Qu = "TA"

# Garage.Type: Creating an other category
data[which(data$Garage.Type=="2Types"),]$Garage.Type = "Other"
data[which(data$Garage.Type=="Basment"),]$Garage.Type = "Other"
data[which(data$Garage.Type=="CarPort"),]$Garage.Type = "Other"

# Paved.Drive: Merging P and Y => Y
data[which(data$Paved.Drive=="P"),]$Paved.Drive = "Y"

# Pool.QC: Removing since most houses don't have a pool
data$Pool.QC = NULL

# Fence: Removing MnWw
data = data[-which(data$Fence=="MnWw"),]

# Misc.Feature: Removing since most houses don't have misc features
data$Misc.Feature = NULL

# Sale.Type: Merging Con + ConLw + ConLI + ConLD => Contract, WD + CWD + VWD => Warenty
data[which(data$Sale.Type=="Con"),]$Sale.Type = "Contract"
data[which(data$Sale.Type=="ConLw"),]$Sale.Type = "Contract"
data[which(data$Sale.Type=="ConLI"),]$Sale.Type = "Contract"
data[which(data$Sale.Type=="ConLD"),]$Sale.Type = "Contract"

data[which(data$Sale.Type=="CWD"),]$Sale.Type = "WD "
data[which(data$Sale.Type=="VWD"),]$Sale.Type = "WD "

#Merging COD, Oth
data[which(data$Sale.Type=="COD"),]$Sale.Type = "Oth"

# Sale.Condition: Removing AdjLand and Alloca
data = data[-which(data$Sale.Condition=="AdjLand"),]
data = data[-which(data$Sale.Condition=="Alloca"),]

cat_df <- data[, sapply(data, is.character)]
cat_df[] <- lapply(cat_df, as.factor)
summary(cat_df)
summary(cat_df$Sale.Type)


# Saving to CSV => ames_fully_cleaned.csv
write.csv(data, "data/ames_fully_cleaned.csv", row.names = FALSE)


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

# Calculating the final RMSE
pred_orig <- predict(new.bic.model)^(1/0.18)
MSE <- mean((ames.train.clean$SalePrice - pred_orig)^2)
sqrt(MSE)

