library(corrplot)
library(nnet)
library(caret)
setwd("F:/16 Fall/Sketch Recognition/Final Project")
datCTS <- data.frame(cbind(Rotation, Smoothness, Sharpness, LHRatio, MaxLengthPos, TopStrokeRatio, BotStrokeRatio, TopBotRatio))

data(trainingData)
Train <- createDataPartition(trainingData$Class, p = 0.6, list = FALSE)
training <- trainingData[ Train, ]
testing <- trainingData[ -Train, ]




M <- cor(datCTS)
corrplot(M,method = "circle")

model1 <- multinom(Class~Sharpness+LHRatio+MaxLengthPos+TopStrokeRatio+BotStrokeRatio+TopBotRatio,data=training)

summary(model1)

model2 <- multinom(Class~Rotation+Smoothness+Sharpness+LHRatio+MaxLengthPos+TopStrokeRatio+BotStrokeRatio+TopBotRatio, data=training)
summary(model2)



exp(coef(model1))

exp(coef(model2))

#train_control <- trainControl(method="cv", number = 10)

pred = predict(model1, newdata=testing)
accuracy <- table(pred, testing[,"Class"])
sum(diag(accuracy))/sum(accuracy)
## [1] 0.705
pred = predict(model1, newdata=testing)
confusionMatrix(data=pred, testing$Class)

pred = predict(model2, newdata=testing)
accuracy <- table(pred, testing[,"Class"])
sum(diag(accuracy))/sum(accuracy)
## [1] 0.705
pred = predict(model2, newdata=testing)
confusionMatrix(data=pred, testing$Class)






