library(e1071)
library(ElemStatLearn)
data <- read.csv('/Users/dengkun/Desktop/578/data.csv')
str(data)
data <- data[,-c(1,33)]
data = na.omit(data)


set.seed(123)
split <- data$diagnosis %>% createDataPartition(p = 0.75, list = FALSE)
train_data <- data[split,]
test_data <- data[-split,]

ggpairs(data[,c(3:12)], title = "Cancer Mean")
ggpairs(data[,c(13:22)], title = "Cancer SE")
ggpairs(data[,c(23:32)], title = "Cancer Worst")

model <- svm(diagnosis~., data = train_data,
             type = "C-classification",
             kernel = "linear", cost = 10)
summary(model)
test_pred <- predict(model, newdata = test_data)
table(test_data$diagnosis,test_pred)
mean(test_pred == test_data$diagnosis)


model2 <-svm(diagnosis~., data = train_data,
             type = "C-classification",
             kernel = "radial", cost = 10)
pred2 <- predict(model2, test_data)
summary(model2)
table(test_data$diagnosis, pred2)
mean(pred2 == test_data$diagnosis)
plot(cmdscale(dist(train_data[,-1])),
     col = as.integer(train_data[,1]),
     pch = c("o","+")[1:150 %in% model$index + 1])

## parameter infect(cost)
num.SV = sapply(X=1:0.1:100, FUN=function(C) {
  svm(diagnosis~., data, kernel = "radial", cost=C, epsilon =.1)$tot.nSV
  })
plot(x=1:0.1:100, y=num.SV,
     xlab="C value", ylab="# of support vectors",
     pch=16, cex=.5, main="# of SVs in soft-margin SVM")

## parameter infect(epsilon)
num.SV1 = lapply(X=seq(0,1,0.01), FUN=function(e) {
  svm(diagnosis~., data, kernel = "radial",
      cost=1, epsilon =e)$tot.nSV
  })
plot(x=seq(0,1,0.01), y=num.SV1, 
     xlab="ε value", ylab="# of support vectors", 
     pch=16, cex=.5, main="# of SVs in SVR")

#gamma
train.accuracy = sapply(X=seq(0.1,10,0.1), 
                        FUN=function(g){
                          model4 = svm(diagnosis~., train_data, gamma=g, epsilon =.1)
                          pred4 = predict(model4, train_data)
                          confus.matrix = table(real=train_data$diagnosis, predict=pred4)
                          sum(diag(confus.matrix))/sum(confus.matrix)
                        } 
)

test.accuracy = sapply(X=seq(0.1,10,0.1), 
                       FUN=function(g){
                         model4 = svm(diagnosis~., train_data, gamma=g, epsilon =.1)
                         pred4 = predict(model4, test_data)
                         confus.matrix = table(real=test_data$diagnosis, predict=pred4)
                         sum(diag(confus.matrix))/sum(confus.matrix)
                       } 
)
plot(x=seq(0.1,10,0.1), y=train.accuracy, 
     pch=16, cex=.5, col="red", ylim=c(0,1),
     xlab="gamma value", ylab="Class Accuracy", 
     main="Accuracy in soft-margin SVM")
points(x=seq(0.1,10,0.1), y=test.accuracy, 
       pch=16, cex=.5, col="blue")
legend("bottomright", pch = 16, 
       col = c("red","blue"),
       legend=c("Train-Accuracy", "Test-Accuracy"))

## cost
train.accuracy = sapply(X=1:0.1:100, 
                        FUN=function(C){
                          model4 = svm(diagnosis~., train_data, cost = C, epsilon =.1)
                          pred4 = predict(model4, train_data)
                          confus.matrix = table(real=train_data$diagnosis, predict=pred4)
                          sum(diag(confus.matrix))/sum(confus.matrix)
                        } 
)

test.accuracy = sapply(X=1:0.1:100, 
                       FUN=function(C){
                         model4 = svm(diagnosis~., train_data, cost = C, epsilon =.1)
                         pred4 = predict(model4, test_data)
                         confus.matrix = table(real=test_data$diagnosis, predict=pred4)
                         sum(diag(confus.matrix))/sum(confus.matrix)
                       } 
)
plot(x=1:0.1:100, y=train.accuracy, 
     pch=16, cex=.5, col="red", ylim=c(0,1),
     xlab="cost value", ylab="Class Accuracy", 
     main="Accuracy in soft-margin SVM")
points(x=1:0.1:100, y=test.accuracy, 
       pch=16, cex=.5, col="blue")
legend("bottomright", pch = 16, 
       col = c("red","blue"),
       legend=c("Train-Accuracy", "Test-Accuracy"))



## 10-fold cv
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25,5,10))
model3 <- train(
  diagnosis ~., data = train_data, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  tuneGrid = grid,
  tuneLength = 10
)
model3$bestTune

tune.model <- tune(svm,diagnosis~., data = train_data, 
                       type="C-classification",kernel="radial",    
                       range=list(cost=10^(-2:2), gamma=c(.5,1,2))
                       )
plot(tune.model)
tune.model$best.model
summary(tune.model)

#regression

suppressWarnings(model_reg<-train(diagnosis~., data=train_data, method="glm", family="binomial",
            trControl = trainControl("cv", number = 10)))
varImp(model_reg)
summary(model_reg)
regpred <- predict(model_reg, test_data)
table(test_data$diagnosis, regpred)
mean(regpred == test_data$diagnosis)



