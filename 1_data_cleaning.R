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

