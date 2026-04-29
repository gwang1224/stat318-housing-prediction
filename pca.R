library(pls)

data = read.csv("/Users/gracewang/stat318-housing-prediction/data/ames_no_na.csv", header=TRUE)

summary(data)
train.idx = sample(1:nrow(data), nrow(data)*0.8, replace=FALSE)
train.data = data[train.idx,]
test.data = data[-train.idx,]

pca.fit = pcr(SalePrice ~ ., data=train.data, scale=FALSE, validation="CV")

summary(pca.fit)

