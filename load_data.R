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

drop_vars <- c("Order", "PID")

ames[factor_vars] <- lapply(ames[factor_vars], factor)
ames <- subset(ames, select = -c(Order, PID))


