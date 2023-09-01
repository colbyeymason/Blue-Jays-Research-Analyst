training <- read.csv("D:/Colby/Blue Jays Research Analyst/training.csv")

str(training)

training$SpinRate <- as.numeric(training$SpinRate)

logistic_model <- glm(InPlay ~ ., data = training, family = "binomial")

summary(logistic_model)

new_logistic_model <- glm(InPlay ~ Velo + HorzBreak + InducedVertBreak, data = training, family = "binomial")

summary(new_logistic_model)

deploy <- read.csv("D:/Colby/Blue Jays Research Analyst/deploy.csv") 

str(deploy)

deploy$SpinRate <- as.numeric(deploy$SpinRate)

library(dplyr)

deploy_with_prediction <- deploy %>%
  mutate(InPlayPrediction = predict(new_logistic_model, deploy, type = "response"))

write.csv(deploy_with_prediction, "D:/Colby/Blue Jays Research Analyst/deploy_with_prediction.csv")



library(ggplot2)
library(reshape2)

training_long <- melt(training, id = "InPlay")

ggplot(training_long, aes(x = variable, y = value)) + geom_boxplot(aes(fill = as.character(InPlay)), outlier.shape = NA) + facet_wrap( ~ variable, scales = "free")
