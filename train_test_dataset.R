data = read.csv("/Users/gracewang/stat318-housing-prediction/data/cleaned_ames.csv")

train_index = sample(1:nrow(data), 2341, replace=FALSE )

data.train = data[train_index,]
write.csv(data.train, "/Users/gracewang/stat318-housing-prediction/data/ames_train.csv")

data.test = data[-train_index,]
write.csv(data.test, "/Users/gracewang/stat318-housing-prediction/data/ames_test.csv")


