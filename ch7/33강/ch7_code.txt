# 데이터 분석가 _ Kevin          \
#                                \
# 스크립트 실행(Run a script)    \
##  : Windows : 'Ctrl + Enter'   \
##  : MAC     : 'Command + Enter'\
#---------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)

#시간 다루는 피키지
install.packages("lubridate")
library(lubridate)


train <- read.csv("bike.csv")
head(train)


# libridate 패키지란?
# 날짜를 쉽게 변환하는 패키지
# ymd : 연월일
# ymd_hms : 연월일_시간

ymd("2020-01-01")
ymd("20200101")

ymd_hms("2020-01-01-03-25-30")
ymd_hms("20200101032530")

# 1. 날짜형으로 변형
train$datetime <-ymd_hms(train$datetime)


# year,month,day 그리고 weekday, hour을 추출후 범주형으로 변환
train$year <- year(train$datetime)   #년
train$month <- month(train$datetime) #월
train$day <- day(train$datetime)     #일
train$hour <- hour(train$datetime)   #시간
train$weekday <- weekdays(train$datetime) #요일


## 데이터 변형해주기
str(train)

# 위에서 만든 변수는 범주화
train$year= as.factor(train$year)
train$month= as.factor(train$month)
train$day <- as.factor(train$day)
train$hour <- as.factor(train$hour)
train$weekday <- as.factor(train$weekday)

# 기존에 있던 자료 범주화
train$season= as.factor(train$season)
train$weather<- as.factor(train$weather)
train$holiday<- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)

## 범주형, 연속형을 구분해주는것은 중요!

## 데이터 분석 시작

# 1. 시각적 접근
## workingday마다 시간에 따른 자전거 수요 시각화하기
## (온도에 대해서 색으로 구분하기!)

train %>%
  filter(workingday == 1) %>%
  ggplot(aes(hour, count, color = temp)) +
  geom_point() +
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69")

# position_jitter 은 퍼뜨리는 역할
train %>%
  filter(workingday == 1) %>%
  ggplot(aes(hour, count, color = temp)) +
  geom_point(position = position_jitter()) +
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69")

#휴일
train %>%
  filter(workingday == 0) %>%
  ggplot(aes(hour, count)) +
  geom_point(position = position_jitter(), aes(color = temp)) +
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69")


################################
### facet_grid로 한번에 그려보기
################################

# train %>%
#   ggplot(aes(hour, count)) +
#   geom_point(position = position_jitter(), aes(color = temp)) +
#   scale_color_gradient(low = "#88d8b0", high = "#ff6f69") +
#   facet_grid(~workingday)

################################



## holiday마다 시간에 따른 자전거 수요 시각화하기
## (온도에 대해서 색으로 구분하기!)

train %>%
  ggplot(aes(hour, count)) +
  geom_point(position = position_jitter(), aes(color = temp)) +
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") +
  facet_grid(~holiday)



######################
# (데이터에 대한 의심)

# 계절별 시간에 따른 대여 연황 확인
train %>%
  group_by(season,hour) %>%
  summarise(mean = mean(count)) %>%
  ggplot(aes(x=hour,y=mean,group=season,color=season)) + geom_line()

# 그림에서 이상한 점을 발견할 수 있을 것이다.
# 봄보다 겨울에 자전거를 더 많이 ??

train %>%
  select(month,season) %>%
  unique()

###################
# 이상한 점을 확인하고, 수정하여 다시 그리시오!
###################

# train %>%
#   mutate(season_new = ifelse(month %in% c(3,4,5),1,
#                              ifelse(month %in% c(6,7,8),2,
#                                     ifelse(month %in% c(9,10,11),3,4)))) %>%
#   group_by(season_new,hour) %>%
#   summarise(mean = mean(count)) %>%
#   ggplot(aes(x=hour,y=mean,group=factor(season_new),color=factor(season_new))) + geom_line()


##############################


train %>%
  ggplot(aes(x=windspeed)) + geom_histogram(fill="blue")

summary(train$windspeed)
# 0을 전처리 하기!
# 중앙값으로 대체하기!

# train[train$windspeed==0,"windspeed"] <- median(train$windspeed)
# train %>%
#   ggplot(aes(x=windspeed)) + geom_histogram(fill="blue")


## 요일에 따른 데이터 확인
##########################

train %>%
  group_by(weekday,hour) %>%
  dplyr::summarise(mean = mean(count)) %>%
  ggplot(aes(x=hour,y=mean,color=weekday,group=weekday)) + geom_line()

# train$weekday <- factor(train$weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
train$weekday <- factor(train$weekday,levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))


train %>%
  group_by(weekday,hour) %>%
  dplyr::summarise(mean = mean(count)) %>%
  ggplot(aes(x=hour,y=mean,group=weekday,color=weekday)) + geom_line()



## 달에 따른 데이터 확인
##########################
train %>%
  group_by(month,hour) %>%
  dplyr::summarise(mean = mean(count)) %>%
  ggplot(aes(x=hour,y=mean,color=factor(month),group=month)) + geom_line()




## 달에 따른 데이터 확인 (히트맵)
##########################
train %>% 
  ggplot(aes(x=hour, y=weekday, fill=count)) +
  geom_tile() +
  scale_fill_distiller(palette='Blues')




######################################
###### 상관성 확인해보기 #############

# 자전거 대여에 가장 영향을 주는 요소는 어떤것일까??
# (상관계수는 연속 데이터에 대한 관계기 떄문에 범주 제외)
# (registered, casual)

str(train)
pairs.panels(train[,c("temp","atemp","humidity","windspeed","count")])



# 실제 이러한 EDA가 전략, 마케팅에 매우 큰 도움이 됨




###########################################
###########################################
### 머신러닝 적용해보기

# 데이터 나누기
set.seed(1234)
idx <- sample(1:nrow(train),round(nrow(train)*0.7))
train_data <- train[idx,]
test_data <- train[-idx,]

# 랜덤포레스트 적용해보기
#1. count 예측해보기!
library(randomForest)

# 예측에 사용할 변수
# datetime, casual, registered를 제외한 나머지변수 사용
# 왜? -> datetime은 구분자로써 사용 x
# -> casual, registered는 정답이므로 사용 x

train_data <- train_data[,-c(1,10,11)]
test_data <- test_data[,-c(1,10,11)]

rf <- randomForest(count~.,data=train_data,ntree=100)
varImpPlot(rf)

yhat <- predict(rf,newdata=test_data) #예측

#시각화
rf_data <- data.frame(y = test_data$count, pred_tr = yhat)
rf_data %>%
  ggplot(aes(x=y,y=yhat)) + geom_point()


# 최종결과
#rmse (루트 제곱의 평균)
sqrt(mean((test_data$count-yhat)^2)) #


############################
############################

# 만약 수량예측이 아닌 계절을 예측하는 문제였다면??
# (month변수 제외한 후 진행 -> month에 의한 season이기 때문에 100%예측)

rf_class <- randomForest(season~.,data=train_data[,-11],ntree=100) #11번쨰 열 month제외
varImpPlot(rf_class) #변수 중요도

library(caret)
yhat_class <- predict(rf_class,newdata=test_data) #예측
confusionMatrix(yhat_class, test_data$season) #혼돈행렬 그리기

