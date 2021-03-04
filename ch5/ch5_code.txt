# 데이터 분석가 _ kevin          \
#                                \
# 스크립트 실행(Run a script)    \
##  : Windows : 'Ctrl + Enter'   \
##  : MAC     : 'Command + Enter'\
#---------------------------------

#########################
# 범주형 데이터 예측하기#
#########################
# tree 강의

install.packages("C50") 
library(C50)

# 데이터 생성하기
class<-c(1,1,1,1,2,
         2,2,2,2,2)

gender <- c("M","M","M","F","M",
            "M","M","M","F","F")

survievd <- c("Y","Y","Y","Y","N",
              "N","N","Y","Y","Y")

# 생성한 데이터를 합치기
train_data <- data.frame(class,gender,survievd,stringsAsFactors = T)
str(train_data)

# test데이터 생성하기 (모델 예측확인)
class <- c(1,2,2)
gender <- c("F","F","M")

test_data <- data.frame(class,gender,stringsAsFactors = T)
# stringsAsFactors - 문자형을 범주형으로 모두 변환

#모델 적용
# 모델 적용은 C5.0으로 매우 쉽게 적용 가능하다
model <- C5.0(survievd~.,data=train_data)
summary(model) #적용 결과값을 쉽게 요약

plot(model) #시각화하기

#만든 모델 적용하기
#predict를 통해서 예측이 가능하다
result <- predict(object = model, newdata = test_data)
result

# 예측한 값을 테스트 데이터에 적재하기
test_data$survievd <- result



#iris 적용해보기
set.seed(1234)
i <- sample(1:nrow(iris), size = nrow(iris)*0.8)     
train_iris<- iris[i,]         # 80%의 랜덤  (train)데이터
test_iris <- iris[-i,]         #  # 나머지 20%의 랜덤 (test)데이터


iris_model <- C5.0(Species~.,data=train_iris) #트리 모델 적용
summary(iris_model)

iris_result <- predict(object = iris_model, newdata = test_iris) #예측하기


#모델 평가
install.packages("caret")
library(caret)
confusionMatrix(iris_result, test_iris$Species)



#############################
#############################
# 랜덤포레스트
install.packages("randomForest")
library(randomForest)

iris_rfmodel <- randomForest(Species~.,data=train_iris,ntree=700, mtry=2) #랜덤포레스트 적용
#ntree : 만들 나무의 개수
#mtry : 사용할 변수의 수

iris_rfresult <- predict(iris_rfmodel, newdata = test_iris) #예측하기
confusionMatrix(iris_rfresult, test_iris$Species)

#### 강의에서는 뒤에서 다시 설명!
varImpPlot(iris_rfmodel) #중요도 시각화
#######################



#########################
# 연속형 데이터 예측하기#
#########################

#데이터 호출
# install.packages("MASS")
library(MASS)
# install.packages("party")
library(party)
# install.packages("randomForest")
library(randomForest)
library(dplyr)  # 데이터 요약
library(ggplot2)# 데이터 시각화

# 데이터 셋 나누기

set.seed(1234) #랜덤 고정
i=sample(1:nrow(Boston),round(nrow(Boston)*0.7)) #70% 행번호
Boston.train = Boston[i,] #train데이터(70% 데이터 추출)
Boston.test = Boston[-i,] #test 데이터(30% 데이터 추출)

# 회귀나무
fit.tr <- ctree(medv~.,data=Boston.train) #회귀나무 적용
#~.,의 의미는 모든 변수를 사용하라는 의미
# fit.tr <- ctree(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,data=Boston.train)
# 같은의미

fit.tr 
plot(fit.tr) #시각화
yhat.tr <- predict(fit.tr,newdata=Boston.test) #예측

#예측 시각화
tr_data <- data.frame(y = Boston.test$medv, pred_tr = yhat.tr)
tr_data %>%
ggplot(aes(x=Boston.test$medv,y=yhat.tr)) + geom_point()

#############################
#############################
# 랜덤포레스트

fit.rf = randomForest(medv~., data=Boston.train, ntree=500) #, mtry=5) 
fit.rf
# ntree 는 생성할 나무 갯수
# myry 는 사용할 변수

varImpPlot(fit.rf) #중요도 시각화

yhat.rf=predict(fit.rf,newdata=Boston.test) #예측


#시각화
rf_data <- data.frame(y = Boston.test$medv, pred_tr = yhat.rf)
rf_data %>%
  ggplot(aes(x=Boston.test$medv,y=yhat.rf)) + geom_point()


# 최종결과
#rmse (루트 제곱의 평균)
sqrt(mean((Boston.test$medv-yhat.tr)^2))
sqrt(mean((Boston.test$medv-yhat.rf)^2))
