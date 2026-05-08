data <- read.csv("~/Courses/STAT 318/Project/Data/cleaned_ames.csv")
attach(data)

# creating a dataframe of just the categorical predictors and making them factors
#cat_df <- data[, sapply(data, is.character)]
#cat_df[] <- lapply(cat_df, as.factor)

# viewing all the counts of the different levels of categorical predictors
#summary(cat_df)

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
write.csv(data, "~/Courses/STAT 318/Project/Data/ames_fully_cleaned.csv", row.names = FALSE)
