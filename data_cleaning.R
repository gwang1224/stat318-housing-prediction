# ------------------------------------
# Data Cleaning
# ------------------------------------
library(mice)

data = read.csv("data/AmesHousing.csv", header=TRUE)
summary(data)


# Inventory of NA's
na.counts <- colSums(is.na(data))
txt <- capture.output(print(na.counts))

png("figures/na_counts.png", width = 1200, height = 800)
par(mar = c(1, 1, 1, 1))
plot.new()
text(0, 1, paste(txt, collapse = "\n"),
     adj = c(0, 1), family = "mono", cex = 1)
dev.off()

# Missingness in Data
# Pool.QC — 2916
# Alley — 2731
# Misc.Feature — 2823
# Fence — 2358
# Fireplace.Qu — 1421
# Lot.Frontage — 490
# Garage.Qual — 158
# Garage.Cond — 158
# Garage.Type — 157
# Garage.Finish — 157
# Bsmt.Qual — 79
# Bsmt.Cond — 79
# Bsmt.Exposure — 79
# BsmtFin.Type.1 — 79
# BsmtFin.Type.2 — 79
# Mas.Vnr.Area — 23
# Bsmt.Full.Bath — 1
# Bsmt.Half.Bath — 1
# Garage.Cars — 1
# Garage.Area — 1

# NA's in Pool.QC indicate that house does not have a pool, replace all NA's with NP = Not Present
data$Pool.QC[is.na(data$Pool.QC)] = "NP"

# Same with variables Fence, Misc.Feature, Fireplace.Qu, Garage.Type, Garage.Finish, Alley
data$Fence[is.na(data$Fence)] = "NP"
data$Misc.Feature[is.na(data$Misc.Feature)] = "NP"
data$Fireplace.Qu[is.na(data$Fireplace.Qu)] = "NP"
data$Garage.Type[is.na(data$Garage.Type)] = "NP"
data$Garage.Finish[is.na(data$Garage.Finish)] = "NP"
data$Alley[is.na(data$Alley)] = "NP"

# Lot.Frontage 490 samples is 16.7% of the data, opt to remove Lot.Frontage
data = subset(data, select = -Lot.Frontage)


# Most of the rest of the missing variable occurs in one sample
rows = data[is.na(data$BsmtFin.SF.1), ]
colnames(rows)[colSums(is.na(rows)) > 0]
# Remove the row with the missing data
data = data[!is.na(data$BsmtFin.SF.2),]


rows = data[is.na(data$Bsmt.Full.Bath),]
colnames(rows)[colSums(is.na(rows)) > 0]
data = data[!is.na(data$Bsmt.Full.Bath),]

rows = data[is.na(data$Garage.Cars), ]
colnames(rows)[colSums(is.na(rows)) > 0]
# Remove the row with the missing data
data = data[!is.na(data$Garage.Cars),]

# Indicator variable for when Garage is not present
data$Garage.Yr.Blt[is.na(data$Garage.Yr.Blt)] = 0
data$Garage.Qual[is.na(data$Garage.Qual)] = 0
data$Garage.Cond[is.na(data$Garage.Cond)] = 0

# Indicator variable for when Basement is not Present
data$Bsmt.Qual[is.na(data$Bsmt.Qual)] = 0
data$Bsmt.Cond[is.na(data$Bsmt.Cond)] = 0
data$Bsmt.Exposure[is.na(data$Bsmt.Exposure)] = 0
data$BsmtFin.Type.1[is.na(data$BsmtFin.Type.1)] = 0

colSums(is.na(data))

write.csv(data, "data/ames_no_na.csv", row.names = FALSE)
