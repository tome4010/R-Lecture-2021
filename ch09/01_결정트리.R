# 분류(Classification)
# 결정 트리(Decsion Tree)
library(rpart)

head(iris)
dtc <- rpart(Species~., iris)    # iris 데이터를 결정 트리로 학습
summary(dtc)
dtc

# 결정 트리 시각화
par(mfrow=c(1,1), xpd=NA)
plot(dtc)
text(dtc, use.n=T)

# 예측
pred <- predict(dtc, iris, type='class')
table(pred, iris$Species)

# 평가
library(caret)
confusionMatrix(pred,iris$Species)    # Accuracy : 0.96

# 시각화
library(rpart.plot)
rpart.plot(dtc)
rpart.plot(dtc, type=4)

# 훈련/테스트 셋으로 분리하여 시행, sample(1:10, 4)
set.seed(2021)
iris_index <- sample(1:nrow(iris), 0.8*nrow(iris))
iris_train <- iris[iris_index,]
iris_test <- iris[setdiff(1:nrow(iris), iris_index),]
iris_test <- iris[-iris_index,]
dim(iris_train)
dim(iris_test)
table(iris_train$Species)
table(iris_test$Species)

# 모델링
dtc <- rpart(Species~., iris_train)

# 예측
pred <- predict(dtc, iris_test, type='class')

# 평가
confusionMatrix(pred, iris_test$Species)

