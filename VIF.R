library(car)

ames = read.csv("/Users/gracewang/stat318-housing-prediction/data/ames_no_na.csv", header=TRUE)

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



# Check for multicollinearity
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
                                Garage.Cond, Garage.Qual, Garage.Finish, Garage.Yr.Blt))



model = lm(SalePrice~., data=ames)
car::vif(model)

write.csv(ames, "/Users/gracewang/stat318-housing-prediction/data/cleaned_ames.csv", row.names = FALSE)
