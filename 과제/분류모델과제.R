# 1. ucla 4가지 모델 예측/평가(DT, RF, SVM, KNN)


# 분류(Classification)
# 결정 트리(Decsion Tree)
library(rpart)

ucla <- read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit <- factor(ucla$admit)
head(ucla)

dtc <- rpart(admit~., ucla)    # ucla 데이터를 결정 트리로 학습
summary(dtc)
dtc

# 결정 트리 시각화
r <- rpart(admit~., data=ucla)
par(mfrow=c(1,1), xpd=NA)
plot(r)
text(r, use.n=T)

# 예측
pred <- predict(r, ucla, type='class')
table(pred, ucla$admit)

# 평가
library(caret)
confusionMatrix(pred, ucla$admit)    # Accuracy : 0.7575

# 시각화
library(rpart.plot)
rpart.plot(r)
rpart.plot(r, type=4)

# 훈련/테스트 셋으로 분리하여 시행, sample(1:10, 4)
set.seed(2021)
ucla_index <- sample(1:nrow(ucla), 0.8*nrow(ucla))
ucla_train <- ucla[ucla_index,]
# iris_test <- iris[setdiff(1:nrow(iris), iris_index),]
ucla_test <- ucla[-ucla_index,]
dim(ucla_train)
dim(ucla_test)
table(ucla_train$admit)
table(ucla_test$admit)

# 모델링
r <- rpart(admit~., ucla_train)

# 예측
pred <- predict(r, ucla_test, type='class')

# 평가
confusionMatrix(pred, ucla_test$admit)

------------------------------------------------------

# 2. RF(Random Forest)

library(caret)
library(randomForest)
head(ucla)

set.seed(2021)
train_index <- createDataPartition(ucla$admit,p=0.8,list=F)
ucla_train <-ucla[train_index,]
ucla_test <- ucla[-train_index,]

# 모델링/학습
rf <- randomForest(admit~ ., ucla_train)
rf

# 예측
pred <- predict(rf, ucla_test, type='class')

# 평가
confusionMatrix(pred, ucla_test$admit)

# 시각화
plot(rf)

# 하이퍼 파라메터
small_forest <- randomForest(admit ~ ., ucla_train,
                             ntree=100, nodesize=4)
s_pred <- predict(small_forest, ucla_test, type='class')
confusionMatrix(s_pred, ucla_test$admit)
plot(small_forest)


------------------------------------------------------
    
# 3. SVM(Support Vector Machine)
    

library(caret)
library(e1071)

set.seed(2021)
train_index <- createDataPartition(ucla$admit,p=0.8,list=F)
ucla_train <- ucla[train_index,]
ucla_test <- ucla[-train_index,]

# 모델링
svc <- svm(admit~., ucla_train)

# 예측
pred <- predict(svc, ucla_test, type='class')

# 평가
table(pred, ucla_test$admit)
confusionMatrix(pred, ucla_test$admit)

# 하이퍼 파라미터 C(cost)
svc100 <- svm(admit~., ucla_train, cost=100)
pred100 <- predict(svc100, ucla_test, type='class')
table(pred100, ucla_test$admit)

svc001 <- svm(admit~., ucla_train, cost=0.01)
pred001 <- predict(svc001, ucla_test, type='class')
table(pred001, ucla_test$admit)

# 모델을 훈련했을 때의 데이터로 예측
self100 <- predict(svc100, ucla_train, type='class')
table(self100, ucla_train$admit)

self001 <- predict(svc001, ucla_train, type='class')
table(self001, ucla_train$admit)


------------------------------------------------------
    
    
# K-NN(Nearest Neighbor) - K-최근접 이웃 
library(class)
head(ucla)
k <- knn(ucla_train[, 1:4], ucla_test[, 1:4], 
         ucla_train$admit, k=5)
k
ucla_test$admit
confusionMatrix(k, ucla_test$admit)



------------------------------------------------------
 
    
# 2. wine 4가지 모델 예측/평가(DT, RF, SVM, KNN)

# 분류(Classification)
# 결정 트리(Decsion Tree)
library(rpart)

wine <- read.table("data/wine.data.txt", header=T, sep=',')
wine
head(wine)

dtc <- rpart(~., wine.data)    # iris 데이터를 결정 트리로 학습
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



------------------------------------------------------
    
    
    


