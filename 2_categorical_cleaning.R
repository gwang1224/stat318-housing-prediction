data <- read.csv("data/AmesHousing_no_na.csv")
attach(data)

# creating a dataframe of just the categorical predictors and making them factors
cat_df <- data[, sapply(data, is.character)]
cat_df[] <- lapply(cat_df, as.factor)

# viewing all the counts of the different levels of categorical predictors
summary(cat_df)

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

# Condition.1: merging Feedr and Artery => Street
# Merging RRAe,RRAn,RRNe,RRNn (railroads) => RR
data[which(data$Condition.1=="Feedr"),]$Condition.1 = "Street"
data[which(data$Condition.1=="Artery"),]$Condition.1 = "Street"

data[which(data$Condition.1=="(Other)"),]$Condition.1 = "RR"
data[which(data$Condition.1=="RRAe"),]$Condition.1 = "RR"
data[which(data$Condition.1=="RRNe"),]$Condition.1 = "RR"
data[which(data$Condition.1=="RRNn"),]$Condition.1 = "RR"

# Condition.2: removing because basically all are Norm
data$Condition.2 = NULL

# Bldg.Type: Merging 1Fam and 2fmCon => fam
data[which(data$Bldg.Type=="1Fam"),]$Bldg.Type = "fam"
data[which(data$Bldg.Type=="2fmCon"),]$Bldg.Type = "fam"

# House.Style: Merging SFoyer and (Other), Merging 2.5Unf with 2Story
data[which(data$House.Style=="SFoyer"),]$House.Style = "(Other)"
data[which(data$House.Style=="2fmCon"),]$House.Style = "fam"

summary(cat_df$Neighborhood)

