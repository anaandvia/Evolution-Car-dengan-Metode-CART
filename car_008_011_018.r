getwd()
data.car <- read.csv("car.csv", sep = ",")
dim(data.car)
str(data.car)
counts <- table(data.car$class)
barplot(counts,col=c("#ffc97d","#003b66","#008478","#2ed194"),
        legend = rownames(counts), 
        main = "Class Value in the Dataset")
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)
sample_n(data.car,10)
set.seed(123)
training.samples <- data.car$class %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- data.car[training.samples, ]
test.data <- data.car[-training.samples, ]
model.class <- rpart(class ~., data = train.data, method = "class",
               control = rpart.control(minsplit = 100, cp =0))
printcp(model.class)
summary(model.class)
rpart.plot(model.class, yesno = 2, type = 2, extra = 4)
predicted.classes <- model %>% 
  predict(test.data, type = "class")
str(predicted.classes)
summary(predicted.classes)
head(predicted.classes)
head(test.data$class)
mean(predicted.classes == test.data$class)
mean(predicted.classes != test.data$class)
