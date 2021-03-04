# 데이터 분석가 _ Kevin          \
#                                \
# 스크립트 실행(Run a script)    \
##  : Windows : 'Ctrl + Enter'   \
##  : MAC     : 'Command + Enter'\
#---------------------------------

##0 색상

# plot 함수로 색깔 점 찍기
plot(0,0, pch=16, cex=10, col='black')
plot(0,0, pch=16, cex=10, col='pink')
plot(0,0, pch=16, cex=10, col='dodgerblue')
## 일반적으로 "col=" 옵션으로 색상 변경 가능

## 색상이름은 아래 참고
## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# rgb( ) 함수와 "#RRGGBB" HEX코드 활용
rgb(  0/255,  70/255,  42/255)
## Ewha Green

plot(0,0, pch=16, cex=10, col='#00462A')

# RColorBrewer 패키지의 활용
install.packages('RColorBrewer')
library(RColorBrewer)

## http://colorbrewer2.org/


# 패키지 내 모든 색상조합 확인  
display.brewer.all()  
## 색상조합 이름 확인

brewer.pal(9, 'Set1')
brewer.pal(9, 'Blues')
brewer.pal(9, 'YlGnBu')
brewer.pal(9, 'Spectral')


##1 ggplot2 패키지를 활용한 시각화

# ggplot2 패키지 설치, 불러오기 
install.packages('ggplot2')
library(ggplot2)

# 데이터 요약/처리를 위한 패키지도 불러오기  
library(dplyr)
library(tidyr)

install.packages("gapminder")
library(gapminder)
data(gapminder)
data1 <- gapminder[gapminder$year=="2007",]

####################
## 1.  그릴 부분의 도와지를 그려본다. (aes(x = , y=))

ggplot(data1) +
  aes(x = gdpPercap) + #x축 지정
  aes(y = lifeExp) #y축 지정

# 이렇게 한번에 그릴 수 있다.
# ggplot(data1,aes(x=gdpPercap,y=lifeExp))

####################
## 2. 그림을 선택한다. +geom_point
ggplot(data1) +
  aes(x = gdpPercap) + #x축 지정
  aes(y = lifeExp) + #y축 지정
  geom_point() #나타낼 그림

####################
## 3. 그림을 꾸며준다

## 3-1 색을 지정한다 aes(color = )
ggplot(data1) +
  aes(x = gdpPercap) + #x축 지정
  aes(y = lifeExp) + #y축 지정
  geom_point() + #나타낼 그림
  aes(color = continent) #색 지정


#같은 표현
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, color=continent)) + geom_point()
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point(aes(color=continent))
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point(color = "red")

## 불가능
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, color = "red")) + geom_point()
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, fill = continent)) + geom_point()


####################
## 3-2 모양 지정한다 aes(shape = )

ggplot(data1) +
  aes(x = gdpPercap) + #x축 지정
  aes(y = lifeExp) + #y축 지정
  geom_point() + #나타낼 그림
  aes(color = continent) + #색 지정
  aes(shape = continent) #모양 지정

# 같은표현
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, color = continent, shape = continent)) + geom_point()
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, color = continent)) + geom_point(aes(shape = continent))

# 특정모양 지정
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, color = continent)) + geom_point(shape = 3)


####################
## 3-3 크기 지정한다 aes(size = )

ggplot(data1) +
  aes(x = gdpPercap) + #x축 지정
  aes(y = lifeExp) + #y축 지정
  geom_point() + #나타낼 그림
  aes(color = continent) + #색 지정
  aes(shape = continent) + #모양 지정
  aes(size = pop) #크기 지정

# 같은표현
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, color = continent, shape = continent, size = pop)) + geom_point()
# 특정 크기 지정
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, color = continent, shape = continent)) + geom_point(size = 3)


####################
## 3-4 투명도를 지정한다 aes(alpha = )

ggplot(data1) +
  aes(x = gdpPercap) + #x축 지정
  aes(y = lifeExp) + #y축 지정
  geom_point() + #나타낼 그림
  aes(color = continent) + #색 지정
  aes(shape = continent) + #모양 지정
  aes(size = pop) + #크기 지정
  aes(alpha = lifeExp) #투명도

# 같은표현
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, color = continent, shape = continent, size = pop, alpha = lifeExp)) + geom_point()
# 특정 크기 지정
data1 %>% ggplot(aes(x=gdpPercap, y=lifeExp, color = continent, shape = continent, size = pop)) + geom_point(alpha = 0.5)


###########################################
############### 연습해보기  ###############
###########################################

head(insurance)
#1. bmi에 따라서 charges가 어떻게 변하는지 점그래프를 그리시오
## region을 색으로 지정
## sex를 모양으로 지정
## 투명도는 0.7


#2. age에 따라서 charges가 어떻게 변하는지 점그래프를 그리시오
## bmi 색으로 지정
## smoker를 모양으로 지정


###########################################



## 막대그래프

######################### 막대그래프
#1. 도화지 그리기
ggplot(data1) +  
  aes(x = continent) # x축 지정


#2. 그림 그리기
ggplot(data1) +  
  aes(x = continent) + # x축 지정
  geom_bar() # 막대그래프 그리기


#3. 꾸미기  
ggplot(data1) +  
  aes(x = continent) + # x축 지정
  geom_bar() + # 막대그래프 그리기
  aes(fill = continent) # 전체색


ggplot(data1) +  
  aes(x = continent) + # x축 지정
  geom_bar() + # 막대그래프 그리기
  aes(fill = continent) + # 전체색
  scale_fill_brewer(palette='Set1') #팔레트 사용하기


#######
# 주의!
# 막대그래프는 color이 아닌 fill로 사용!
ggplot(data1) +  
  aes(x = continent) + # x축 지정
  geom_bar() + # 막대그래프 그리기
  aes(color = continent)  # 개별색
#######


##### x와 y를 모두 지정해주고 싶으면? stat = "identity"

ggplot(data1) +  
  aes(x = continent) + # x축 지정
  aes(y = lifeExp) + # y축 지정
  geom_bar(stat = "identity") + # 막대그래프 x,y축  
  aes(fill = continent) # 전체색

# 주의
ggplot(data1) +  
  aes(x = continent) + # x축 지정
  aes(y = lifeExp) + # y축 지정
  geom_bar(stat = "identity") + # 막대그래프 x,y축  
  aes(color = continent) # 전체색


##### 데이터 전처리와 막대 차트 그리기

# continent 마다 평균을 그리고 싶으면??

data1 %>%
  group_by(continent) %>%
  summarise(mean = mean(lifeExp))


data1 %>%
  group_by(continent) %>%
  dplyr::summarise(mean = mean(lifeExp)) %>%
  ggplot() +  
  aes(x = continent) +  
  aes(y = mean) +
  geom_bar(stat = "identity") +  
  aes(fill = continent) +
  aes(alpha = 0.7)


# 나눠서 그리는 방법!
gapminder %>%
  filter(year %in% c(2002,2007)) %>%
  group_by(continent,year) %>%
  dplyr::summarise(mean = mean(lifeExp)) %>%
  ggplot() +  
  aes(x = continent) +  
  aes(y = mean) +
  geom_bar(stat = "identity") +  
  aes(color = continent) +  
  aes(fill = continent) +
  facet_grid(~year) # 특정 변수로 구분해서 그리고 싶다면?


###########################################
############### 연습해보기  ###############
###########################################
head(insurance)

#1. insurance 데이터에서 region별 중앙값을 구한후 막대그래프를 그리시고
##  region을 색으로 지정
## 투명도는 0.7





#2. insurance 데이터에서 sex, smoker별 중앙값을 구한후 막대그래프를 그리시고
## x축은 smoker이며 sex를 색으로 구분
##  region을 색으로 지정
## 투명도는 0.7



######################### 박스 그래프 geom_boxplot()

gapminder %>%
  ggplot(aes(x=continent, y= lifeExp)) + 
  geom_boxplot()

gapminder %>%
  ggplot(aes(x=continent, y= lifeExp, fill= continent)) + 
  geom_boxplot()


gapminder %>%
  ggplot(aes(x=continent, y= lifeExp, fill= continent)) + 
  geom_boxplot(alpha = 0.5)


# 주의! 요약을 한 데이터를 사용하지 않는다!
gapminder %>% 
  group_by(continent) %>%
  dplyr::summarise(mean = mean(lifeExp)) %>%
  ggplot(aes(x=continent, y= mean, fill= continent)) + geom_boxplot()


######################### 히스토그램 geom_histogram()
gapminder %>%
  ggplot(aes(x=lifeExp)) + 
  geom_histogram()

gapminder %>%
  ggplot(aes(x=lifeExp)) + 
  geom_histogram() +
  facet_grid(~continent)


######################### 선 그래프

gapminder %>%
  group_by(year) %>%
  summarise(sum = sum(lifeExp))


gapminder %>%
  group_by(year) %>%
  dplyr::summarise(sum = sum(lifeExp)) %>%
  ggplot(aes(x=year,y=sum)) + geom_line()


#  여러 그룹을 그리고 싶을 경우

gapminder %>%
  group_by(year,continent) %>%
  summarise(mean = mean(lifeExp))

gapminder %>%
  group_by(year,continent) %>%
  dplyr::summarise(mean = mean(lifeExp)) %>%
  ggplot(aes(x=year, y=mean , group=continent ,color= continent)) + geom_line()


###########################################
############### 연습해보기  ###############
###########################################

#1 insurance데이터에서 children이 0보다 크면 1, 0이면 0인 변수 ch_data를 만드시오



#2 insurance데이터를 활용해서 상자그래프(boxplot)를 그리시오
## x축은 region y축은 charges이며 ch_data를 색으로 구분



#3 insurance데이터를 활용해서 히스토그램(histogram)을 그리시오
## x축은 charges ch_data를 색으로 구분
## region마다 4개의 그래프를 그리시오



#4 insurance데이터를 활용해서 막대그래프를 그리시오
## x축은 region y축은 charges이며 ch_data를 색으로 구분
## (누적 막대그래프와 ch_data별 비교 막대그래프)





### ggplot 추가

HR <- read.csv("HR_comma_sep.csv")
HR$left = as.factor(HR$left)
HR$salary = factor(HR$salary,levels = c("low","medium","high"))

# satisfaction_level : 직무 만족도
# last_evaluation : 마지막 평가점수
# number_project : 진행 프로젝트 수
# average_monthly_hours : 월평균 근무시간
# time_spend_company : 근속년수
# work_accident : 사건사고 여부(0: 없음, 1: 있음, 명목형)
# left : 이직 여부(0: 잔류, 1: 이직, 명목형)
# promotion_last_5years: 최근 5년간 승진여부(0: 승진 x, 1: 승진, 명목형)
# sales : 부서
# salary : 임금 수준


#####################
### 테마 변경하기 ###
#####################
install.packages("ggthemes")
library(ggthemes)

# Classic Theme
ggplot(HR,aes(x=salary)) +  
  geom_bar(aes(fill=salary)) +
  theme_classic()


# BW Theme
ggplot(HR,aes(x=salary)) +  
  geom_bar(aes(fill=salary)) +
  theme_bw()

Graph = ggplot(HR,aes(x=salary)) +  
  geom_bar(aes(fill=salary)) 


## 패키지를 통한 다양한 테마 변경

Graph + theme_bw() + ggtitle("Theme_bw") 
Graph + theme_classic() + ggtitle("Theme_classic") 
Graph + theme_dark() + ggtitle("Theme_dark") 
Graph + theme_light() + ggtitle("Theme_light")  

Graph + theme_linedraw() + ggtitle("Theme_linedraw") 
Graph + theme_minimal() + ggtitle("Theme_minimal") 
Graph + theme_test() + ggtitle("Theme_test") 
Graph + theme_void() + ggtitle("Theme_vold") 


#####################
### 범례제목 수정 ###
#####################
ggplot(HR,aes(x=salary)) +  
  geom_bar(aes(fill=salary)) +
  theme_bw() +
  labs(fill = "범례 제목 수정(fill)")

# 범례 테두리 설정
Graph + theme(legend.position = "top")
Graph + theme(legend.position = "bottom")
Graph + theme(legend.position = c(0.9,0.7))
Graph + theme(legend.position = 'none')


#####################
### 축 변경 ###
#####################

# 이산형 - deiscreate()
# 연속형 - continuous()

ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  scale_x_discrete(labels = c("하","중","상")) +
  scale_y_continuous(breaks = seq(0,8000,by = 1000))

ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  scale_x_discrete(labels = c("하","중","상")) +
  scale_y_continuous(breaks = seq(0,8000,by = 1000)) +
  scale_fill_discrete(labels = c("하","중","상"))


ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  ylim(0,5000)

ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  ylim(0,13000)


#####################
### 색 변경 ###
#####################

ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  scale_fill_manual(values = c('red','royalblue','tan')) 


ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary), alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(values = c('red','royalblue','tan')) 

#####################
### 글자크기,각도 수정 ###
#####################

# coord_flip() : 대칭 그래프
# theme_bw : 글자체 수정

ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary), alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(values = c('red','royalblue','tan')) +
  coord_flip()



ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  scale_fill_manual(values = c('red','royalblue','tan'))  +
  coord_flip() +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 15,angle = 90),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 15))


# 그래프에 평행선, 수직선, 대각선을 그릴 수 있는 명령어

ggplot(NULL) +
  geom_vline(xintercept = 10, 
             col = 'royalblue', size = 2) +
  geom_hline(yintercept = 10, linetype = 'dashed', 
             col = 'royalblue', size = 2) +
  geom_abline(intercept = 0, slope = 1, col = 'red',
              size = 2) +
  theme_bw()



#### 추가 유용한 그래프
###################열지도(heatmap)

# 데이터 요약

agg2 = insurance %>% 
  mutate(bmi_grp = cut(bmi, 
                       breaks=c(0,30,35,40,100), 
                       labels=c('G1','G2','G3','G4'))) %>% 
  group_by(region, bmi_grp) %>% 
  summarise(Q90 = quantile(charges, 0.9))

quantile(iris$Sepal.Width,0.5) #중위수
quantile(iris$Sepal.Width,0.7) #70% 

## quantile( , q) : q*100% 값 계산

agg2 %>% 
  ggplot(aes(x=region, y=bmi_grp, fill=Q90)) +
  geom_tile()


# 색상 지정  
agg2 %>% 
  ggplot(aes(x=region, y=bmi_grp, fill=Q90)) +
  geom_tile() +
  scale_fill_gradient(low='white', high='#FF6600')


agg2 %>% 
  ggplot(aes(x=region, y=bmi_grp, fill=Q90)) +
  geom_tile() +
  scale_fill_distiller(palette='YlGnBu')



###########################################
############### 연습해보기  ###############
###########################################

# (실습) NHIS에서 AGE_GROUP, DSBJT_CD별 EDEC_TRAMT 평균 계산 후 저장
#        저장된 데이터로 열지도 시각화

NHIS <- read.csv("NHIS.csv")



###########################################
# tidyr + dplyr + ggplot을 한번에

# 데이터 불러오기
## 역변호가 150인 서울역 데이터 
library(openxlsx)
subway_2017 = read.xlsx('subway_1701_1709.xlsx')
names(subway_2017)[6:25] <- paste0('H', substr(names(subway_2017)[6:25], 1, 2))

head(subway_2017)
# gather( ) 함수를 활용하여 H05부터 H24까지 변수를 모아
# '시간대'와 '승객수'으로 구분하는 데이터 subway2 만들기
subway2 = gather(subway_2017, 시간대, 승객수, H05:H24)

## 위에서 만든 subway2 데이터와 dplyr 패키지를 활용하여

# 역명/시간대별 전체 승객수 합계 계산 (승객수 합계의 내림차순으로 정렬)
subway2 %>% 
  group_by(역명, 시간대) %>% 
  summarise(SUM = sum(승객수)) %>% 
  arrange(desc(SUM))


### 이러한 tidyr을 통해서 데이터를 시각화하기
### 시간대별로 승객 합계 막대차트로 그려보기!



# options("scipen" = 100)

#############################################################


##########################
# plotly 맛보기
##########################

library(plotly)
library(gapminder)
library(dplyr)

##########################
# ggplot + plotly
# plotly 는 반응형 차트로 자바스크립트 라이브러리를 R로 가져온
# 인터랙티브 시각화 패키지를 의미한다

#데이터 호출
data(gapminder)
data1 <- gapminder[gapminder$year=="2007",]
head(data1)


# 막대그래프 1
# 요약을 안한 막대그래프 그대로 사용했을경우

bar_1 <- data1 %>%
  ggplot(aes(x=continent,y=lifeExp,fill=continent)) + geom_bar(stat="identity")

bar_1
ggplotly(bar_1) #plotly 적용하기

# 막대그래프 2
# 요약을 한 막대그래프 사용

bar_2 <- data1 %>%
  group_by(continent) %>%
  summarise(lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x=continent,y=lifeExp,fill=continent)) + geom_bar(stat="identity")

ggplotly(bar_2) #plotly 적용하기

# boxplot그래프

box <- data1 %>%
  ggplot(aes(x=continent,y=lifeExp,fill=continent)) + geom_boxplot()

ggplotly(box)  #plotly 적용하기


# 히스토 그램 그래프
hist <- data1 %>%
  ggplot(aes(x=lifeExp)) + geom_histogram(fill="blue",bins=25)

ggplotly(hist) #plotly 적용하기


# plotly 정규 함수 사용

data1 %>%
  plot_ly(x= ~lifeExp) %>%
  add_histogram(nbinsx = 25)
