data = read.csv("/Users/gracewang/stat318-housing-prediction/data/AmesHousing.csv")

predictors <- ames.train[, names(ames.train) != "SalePrice"]

num_quant <- sum(sapply(predictors, is.numeric))
num_cat <- sum(sapply(predictors, function(x) is.factor(x) || is.character(x)))

num_quant
num_cat



# Plot
library(ggplot2)
library(scales)

ggplot(ames.train, aes(x = SalePrice)) +
  geom_histogram(bins = 30, fill = "#2A9D8F", color = "white") +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Distribution of Home Sale Prices",
    subtitle = "Most homes are concentrated at lower to mid-range sale prices",
    x = "Sale Price",
    y = "Number of Homes"
  ) +
  theme_minimal(base_size = 14)