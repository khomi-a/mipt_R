library(ggplot2)
library(GGally)
library(pROC)
library(MLmetrics)

ir_data <- iris[1:100,]
set.seed(42)
samp<-sample(1:100,67)
ir_train <-ir_data[samp,]
ir_test <-ir_data[-samp,]
ggpairs(ir_train)

model0 <-glm(Species~Petal.Length, family = 'binomial', data = ir_train)
predicted <- predict(model0,ir_test, type = "response")
sum((ir_test$Species == 'setosa') == (predicted < 0.5))/length(predicted) # accuracy = 1
roc(ir_test$Species, predicted) # 1
F1_Score((ir_test$Species == 'setosa') * 1.0, (predicted < 0.5) * 1) # 1

model1 <-glm(Species~Sepal.Length, family = 'binomial', data = ir_train)
predicted <- predict(model1,ir_test, type = "response")
sum((ir_test$Species == 'setosa') == (predicted < 0.5))/length(predicted) # 0.88
roc(ir_test$Species, predicted) #0.9405
F1_Score((ir_test$Species == 'setosa') * 1.0, (predicted < 0.5) * 1) # 0.9

model2 <-glm(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, family = 'binomial', data = ir_train)
predicted <- predict(model2,ir_test, type = "response")
sum((ir_test$Species == 'setosa') == (predicted < 0.5))/length(predicted) # 1
roc(ir_test$Species, predicted) # 1
F1_Score((ir_test$Species == 'setosa') * 1.0, (predicted < 0.5) * 1) # 1
