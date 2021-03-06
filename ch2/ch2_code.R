# 데이터 분석가 _ Kevin          \
#                                \
# 스크립트 실행(Run a script)    \
##  : Windows : 'Ctrl + Enter'   \
##  : MAC     : 'Command + Enter'\
#---------------------------------

##### 복습

# matrix
mat <- matrix(c(1,2,3,4,5,6,7,8,9), ncol=3, byrow=T) #행 기준 3열의 행렬 생성
mat[1,]                                       #행렬 mat의 1행의 값
mat[,3]                                       #행렬 mat의 3열의 값

# data.frame
a<-1:10
b<-rep("a",10)
c<-data.frame(a,b)


# list
list1 <- list("A", 1:8)                         #list1 리스트 생성
list1[[3]] <- list(c(T, F))                      #세 번째 성분을 추가
list1[[2]][9] <- 9                             #두 번째 성분에 원소 추가
list1[3] <- NULL                             #세 번째 성분 삭제
list1[[2]] <- list1[[2]][-9]                   #두 번째 성분의 9번째 원소 삭제

rm(list=ls())


# 연습해보기!

# 1. a에 1부터 10까지 홀수를 3번씩 출력하고 다음과 같은 matrix 저장하시오
#     [,1] [,2] [,3] [,4] [,5]
#[1,]   1    3    5    7    9
#[2,]   1    3    5    7    9
#[3,]   1    3    5    7    9


a <- matrix(rep(seq(1,9,2),each =3), nrow=3)
# 2. b와 c로 데이터 프레임을 d에 저장하시오
b <- c(1,2,3)
c <- c("a","b","c")

d <- data.frame(b,c)

# 3. a와 d로 리스트를 e에 저장하고 2번째층에 저장된 데이터프레임 2행 2열을 출력하시오

e <- list(A=a,D=d)
e[[2]][2,2]
e$D[2,2]
### 데이터 저장하기 및 호출 하기

# --- read.csv( )를 활용한 csv파일 불러오기 ----

# 작업 폴더 확인하기 
getwd()

# 작업폴더 설정(Set Working Directory) : 'Ctrl+ Shift + h'
## RStudio Menu ; Session -> Set Working Directory -> Choose...

# 작업 폴더 지정하기
setwd()
## setwd('Your Working Directory Address')


# 데이터 불러오기
dt <- read.csv('test.csv')
## 작업 폴더에 있는 test.csv 불러와서 출력하기 


# 인코딩 지정
read.csv('pop_seoul_euckr.csv', fileEncoding='euc-kr')


## Windows 인코딩 : CP949/euc-kr
## mac/Linux 인코딩 : UTF-8

## 같은 운영체제에서는 생략 가능 



# '<-'을 활용해서 저장하기 
pop_seoul <- read.csv('pop_seoul_euckr.csv')
## 오른쪽 위 환경창에서 데이터 이름 클릭 


View(pop_seoul)
## 혹은 직접 View( )에 데이터를 넣고 실행



# 데이터 샘플 살펴보기
head(pop_seoul)
tail(pop_seoul, n=10)
## 첫 6개, 끝 10개 관측치만 콘솔창에서 보기



# 데이터 특성 확인하기 
str(pop_seoul)
## 데이터의 구조(Structure) 살펴보기
## 변수 형식 (뒤에 설명)


names(pop_seoul)
## 변수이름 확인하기


nrow(pop_seoul)
ncol(pop_seoul)
dim(pop_seoul)
## 행/관측치 수, 열/변수 수 확인



# 데이터 요약
summary(pop_seoul)




##1.1 read.table( )로 txt파일 불러오기
## 탭으로 구분된 데이터
temp = read.table('pop_seoul.txt',  
                  header=TRUE,
                  fileEncoding='UTF-8')
temp


write.csv(pop_seoul, file='aaa.csv', row.names = F)
## write.csv(저장할객체, file='경로/이름')  



##3 openxlsx 패키지를 활용한 엑셀파일 불러오기 

# openxlsx 패키지 설치
install.packages('openxlsx')


# library( )로 패키지 불러오기
library(openxlsx)
## 필요할 때마다 불러오기
## 원래는 없었던 read.xlsx( ) 함수 사용가능 


# 데이터 불러오기
SHEET1 = read.xlsx('test.xlsx', sheet=1)
## xlsx 파일 경로와 시트 번호를 지정
SHEET1


SHEET2 = read.xlsx('test.xlsx', sheet=2, startRow=3)
## startRow= 옵션으로 데이터 시작 행번호 지정 가능
SHEET2


SHEET3 = read.xlsx('test.xlsx', sheet=3, colNames=FALSE)
## 첫 행이 변수이름이 아니라 관측치일때, colNames=FALSE 옵션 사용
SHEET3

# 데이터 불러오기 실실

SHEET4 =read.xlsx("광역시도별_연령성별_인구수.xlsx", startRow=2)


######################### 1번 연습문제 #############################


##6 (실습) 다양한 데이터 불러오기
# 통계청 인구 데이터 
## 출처 : http://kosis.kr/statisticsList/statisticsListIndex.do?menuId=M_01_01&vwcd=MT_ZTITLE&parmTabId=M_01_01#SelectStatsBoxDiv

## 파일위치 :'data/'
## 파일이름 : '광역시도별_연령성별_인구수.xlsx'
## 데이터 시작 위치 : 2행



# 통계청 가구별 주택 거주 데이터
## 출처 : http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1JU1501&vw_cd=&list_id=&seqNo=&lang_mode=ko&language=kor&obj_var_id=&itm_id=&conn_path=I2

## 파일위치 :'data/'
## 파일이름 : '시도별_가구_주택종류.xlsx'
## 데이터 시작 위치 : 2행



# 국토교통부 아파트 실거래가 데이터(2018, 강남구)
## 출처 : http://rtdown.molit.go.kr/

## 파일위치 :'data/'
## 파일이름 : '아파트매매_2019_강남구.xlsx'
# 데이터 시작 위치 : 17행




#### week 사칙연산 ####
# 
# ▶ R에서 제공하는 연산에 대하여 알아본다.

a<-c(1,2)
b<-c(3,4)
a+b #벡터 변수의 덧셈
a-b #벡터 변수의 뺄셈
a*b #벡터 변수의 곱셈
a/b #벡터 변수의 나눗셈

a<-c(4,5)
b<-c(2,3)
a^b #벡터변수의 제곱(구성 요소들간의 제곱, 4^2, 5^3)

rm(list = ls()) #오브젝트 내의 모든 변수 삭제


#%/% : 나눗셈에서 몫만 출력함
#%% : 나눗셈에서 몫만 출력함

a<-c(7,2)
b<-c(3,4)
a%/%b #벡터 변수의 정수나눗셈
a%%b #벡터 변수의 나머지

#행렬의 곱
A<-matrix(c(5,10,2,1), ncol=2)
B<-matrix(c(3,4,5,6), ncol=2)
#(5*3) + (2*4) ; (5*5) + (2*6) ; (10*3) + (1*4) ; (10*5) + (1*6)
A;B
A%*%B

rm(list = ls()) #오브젝트 내의 모든 변수 삭제


#### 비교연산 ####
# 
# ▶ R에서 제공하는 연산에 대하여 알아본다.


# '==' 비교되는 두 항이 같은지를 비교함. 같을 경우 True, 다를 경우 False
x<-2
y<-3
x==y

# '!=' 비교되는 두 항이 다른지를 비교함. 같을 경우 False, 다를 경우 True
x<-2
y<-3
x!=y

# '<=' 왼쪽 항이 오른쪽 항보다 작거나 같음을 비교함. 작거나 같으면 True, 크면 False
x<-2
y<-2
x<=y

# '<' 왼쪽 항이 오른쪽 항보다 작음을 비교함. 작으면 True, 크면 False
1<2

# '>' 왼쪽 항이 오른쪽 항보다 큼을 비교함. 크면 True, 작으면 False
1>2

# '>=' 왼쪽 항이 오른쪽 항보다 크거나 같음을 비교함. 크거나 같으면 True, 작으면 False
1>=2


rm(list = ls()) #오브젝트 내의 모든 변수 삭제


#### 논리연산 ####
# 
# ▶ R에서 제공하는 연산에 대하여 알아본다.

# ! : not 연산자

# & : 벡터에서의 and 논리 연산자
2==2 & c( 3>4)
2==2 & c( 3<4)
# | : 벡터에서의 or 논리 연산자
2==2 | c( 3>4)
2!=2 | c( 3>4)



rm(list = ls()) #오브젝트 내의 모든 변수 삭제


#### week. 4.3  열 추가 ####
# ▶ 이번 절에서는 데이터의 열 추가에 대하여 알아본다.

tmp_df <- data.frame(AA = c(1:5), BB = c("A","A","B","B","B"))

head(tmp_df) # 데이터 확인 
#CC컬럼을 새로 생성하고 그 안에 값을 1로 채워 넣음 
tmp_df$CC <- 1

#컬럼 AA와 컬럼 CC의 값의 합한 값을 새로운 DD컬럼으로 생성
tmp_df$DD <- tmp_df$AA + tmp_df$CC
head(tmp_df) # 데이터 확인 


#### 열 제거 ####

# ▶ 열을 제거 하기 위해서는 열의 위치 번호에 (-)를 입력하여 제거 한다. 

tmp_df[, -1] #첫번째 위치의 컬럼 제거

tmp_df[, -"AA"]  #오류 발생함
# 다음에 오류가 있습니다-"AA" : 단항연산자에 유효한 인자가 아닙니다

tmp_df[, c("BB","CC","DD")]

rm(list = ls()) #오브젝트 내의 모든 변수 삭제


#### 데이터 추출(Select) ####

# 데이터 추출(Select)
# ▶ 이번 절에서는 데이터 추출하기에 대하여 알아본다. 

Sample.df <- data.frame(AA = rep(letters[1:5],10), BB = sample(60:70, 50, replace = T))
# sample (범위, 추출수, replace = 중복가능)
sample(1:6,10,replace = T) # 1부터 6을 10번 랜덤추출
#만약 랜덤을 고정하고 싶다면 set.seed(숫자)
set.seed(1234)
sample(1:6,10,replace=T)
set.seed(1234)
sample(1:6,10,replace=T)


head(Sample.df)
#AA컬럼의 값중에서 a인 값만 추출
#Type1
Sample.df[Sample.df$AA == "a",] ###

#Type2
# subset(데이터, 조건)
subset(Sample.df, AA == "a") ###
#AA컬럼의 값중에서 a 와 b의 값만 추출
#Type1
Sample.df[Sample.df$AA %in% c("a","b"),] ### a or b를 포함한 row
#Type2
subset(Sample.df, AA %in% c("a","b"))


# 필요한 컬럼 Select
Sample.df1 <- Sample.df
#Type1
Sample.df1[,c("AA","BB")]
#Type2
Sample.df1[,c(1,2)]
#Type3
Sample.df1[,c(-4,-5)]


rm(list = ls()) #오브젝트 내의 모든 변수 삭제

#### 데이터 정렬하기 ####

# ▶ 데이터 정렬하기 위해 사용되어지는 함수는 sort, order 이다.
tmp1 <- data.frame(AA = c("A","A","B","C","D"), BB = c(5,3,6,1,2))
head(tmp1)

tmp1[order(tmp1$BB),] #오름차순 정렬
#   AA BB
# 4  C  1
# 5  D  2
# 2  A  3
# 1  A  5
# 3  B  6
tmp1[order(tmp1$BB, decreasing = T), ] #내림차순 정렬
# AA BB
# 3  B  6
# 1  A  5
# 2  A  3
# 5  D  2
# 4  C  1
rm(list = ls()) #오브젝트 내의 모든 변수 삭제


######################## 2번 연습문제 ##########################
data(iris)
head(iris)

# 1. Sepal.Length 변수의 짝수행을 출력하시오 .

iris$Sepal.Length[seq(2,150,2)]
iris$Sepal.Length[1:150 %% 2 ==0]
iris[seq(2,150,2),"Sepal.Length"]

# 2. Subset을 사용해서 변수 Species 에서 setosa 인 데이터를 추출하시오

iris[iris$Species == "setosa",]
a1 <- subset(iris, Species == "setosa")

# 3. 2번에서 뽑은 데이터를 a1에 저장하고 Sepal.Length 가 5 보다 작은 Petal.Width 의 합을 구하시오

sum(a1[a1$Sepal.Length < 5, "Petal.Width" ])

# 내가 푼답 (이걸 모름)-> sum함수에 첫번째 인자는 column 조건, 두번째 연산 대상 column
b1 <- subset(a1, Sepal.Length < 5)
sum(b1$Petal.Width)


#############################################################



#### 데이터 합치기 ####

# ▶ 데이터를 합치기 위해서 사용되는 함수는 cbind, rbind, merge 가 가장 많이 사용되어진다.

# 예제 데이터 불러오기
## 국토교통부 아파트 실거래가 데이터
## 출처 : http://rtdown.molit.go.kr/

library(openxlsx)
GN = read.xlsx('아파트매매_2018_강남구.xlsx', sheet=1, startRow=17)
head(GN)
tail(GN)

GD = read.xlsx('아파트매매_2018_강동구.xlsx', sheet=1, startRow=17)
head(GD)

SC = read.xlsx('아파트매매_2018_서초구.xlsx', sheet=1, startRow=17)
SP = read.xlsx('아파트매매_2018_송파구.xlsx', sheet=1, startRow=17)


# 데이터 구성 확인
names(GN)
names(GD)

str(GN)
str(GD)

# rbind( )를 활용한 행/관측치 결합
GN4 = rbind(GN, GD, SC, SP)
head(GN4)
tail(GN4)


##2 열 결합

# 가상의 예제 데이터 확인
my_data = data.frame(id = 1:5,
                     gender = c('M','F','F','F','M'),
                     age = seq(15, 35, 5))

my_data


# 추가 변수를 포함한 데이터
another_data = data.frame(region = c('Seoul','Seoul','Seoul','Busan','Busan'),
                          amount = c(1,1,1,1,1))

another_data


# cbind( )로 열/변수 결합
cbind(my_data, another_data)

## 일반적으로 잘 활용하지 않음
## $를 활용한 변수 추가 혹은 key(id) 변수를 활용한 결합 활용

my_data$amount = 100
my_data
## 동일한 값 변수 추가

#cut 구간별 라벨로 column추가
my_data$age_grp = cut(my_data$age, 
                      breaks=c(10,20,30,40), 
                      include.lowest=TRUE, 
                      right=FALSE,
                      labels=c('10_19','20_29','30_39'))
my_data


## cut( )을 활용한 연령대 변수 추가
## breaks : 구간 경계값
## include.lowest : 첫 경계값 포함 여부
## right : 각 구간의 오른쪽 경계 포함 여부 
## labels : 각 구간의 이름


############ 조건에 맞는 데이터 합치기 (merge)
sales = read.csv('ex_sales.csv')
sales

prod  = read.csv('ex_prod.csv')
prod


# merge( )를 활용한 데이터 결합
merged = merge(sales, prod, by.x='PROD', by.y='PROD')
## merge(데이터1, 데이터2, by.x='첫번째데이터의 기준변수', by.y='두번째...')


merged = merge(sales, prod, by='PROD')
## 기준변수가 같을 때는 "by="으로 한번에 지정 가능

merged


# all 옵션의 활용
merge(sales, prod, by.x='PROD', by.y='PROD', all.x=TRUE)
## all.x=TRUE : 짝이 없는 첫번째 데이터의 관측치도 포함
## Excel의 VLOOKUP 느낌

merge(sales, prod, by.x='PROD', by.y='PROD', all.y=TRUE)
## all.y=TRUE : 짝이 없는 두번째 데이터의 관측치도 포함

merge(sales, prod, by.x='PROD', by.y='PROD', all=TRUE)
## all=TRUE : 짝이 없는 모든 관측치 포함

###########

# 1:1, 다:1은 문제가 없지만 다:다 결합은 조심!
prod2  = read.csv('ex_prod2.csv')
prod2
## 상품 B에 대한 정보가 중복

merge(sales, prod2, by.x='PROD', by.y='PROD')
## by로 지정된 변수값 기준 
## 모든 가능한 결합을 생성


################################ merge 연습문제 ###################################

a1 <-  data.frame(name=c("aa","bb","cc"),value=seq(10,20,length.out = 3))
a2 <-  data.frame(name=c("cc","dd","ee"),value=seq(30,50,length.out = 3))
a3 <-  data.frame(name=c("aa","dd","ee","ff"),value=seq(20,80,length.out = 4),any=seq(0,3,1))

#  1. a1 와 a2 를 행결합 하시오



# 2. a3 와 a1 를 행결합하시오



# 3. a3 와 a2 를 name 기준으로 결합 하시오 (a3 데이터는 모두출력)



# 4. 3번 데이터를 z 에 저장하고 value.y 기준으로 정렬하시오



####################################################################################


#### 조건문(if/ifelse) ####
# 
# ▶ R 프로그래밍을 작성할 때 가장 많이 사용하는 것 중에 if문이 있다.
#특정한 조건을 만족했을 경우에만 프로그램 코드를 수행하는 제어 구문. 항상 논리 연산이 수반 된다
#if(조건) 실행문
x <- c(1,2,3,4); y <- c(2,1,4,5)
if(sum(x) < sum(y)) print(x) #x의 합이 y의 합보다 작을 경우 실행

#if(조건) 조건이 참일 때 실행문 else 조건이 참이 아닐 때 실행문
#괄호안의 조건이 참이면 참일때의 실행문을 수행하고 거짓일 때는 참이 아닐때의 실행문을 수행하는 표현식
x <- c(1,2,3,4)
y <- c(2,1,4,5)
if(mean(x)>mean(y)) print("Mean(x)>Mean(y)") else print("Mean(x)<Mean(y)")

#중첩 조건문 : 조건문 안에 조건문이 있는 표현식
if(length(x)==5) {
  if(sum(x) == 10) print("length=5, sum=10")
} else {
  print("length=4, sum=10")
}

x <- c(1,2,3,4)
if(length(x)==5){
  pring(5) } else if(length(x)==4){
  print(4)} else {
  print("알수없음")}

#ifelse(조건, 조건이 참일때의 실행문, 조건이 참이 아닐때의 실행문)
x <- c(1,2,3,4)
y <- c(2,1,4,5)
ifelse(x<y, x, y)
ifelse(sum(x-y) > 0, "positive", ifelse(sum(x-y) < 0 , "negative", "zero"))


##################### iris 데이터 변형 연습문제 ###########################

# iris에 Sepal.Length 변수가 5.8보다 크거나 같으면 1 아니면 0을 iris$new변수에 저장하시오

data(iris)
head(iris)

#내가 푼 코딩: 정답
iris$new <- ifelse(iris$Sepal.Length >= 5.8, 1, 0)

##################################################################


#### 반복문(for) ####
#
# ▶ 이번 절에서는 유사한 패턴을 갖는 작업을 여러번 반복해서 수행하는 제어 구문 중 for 문에 대하여 진행한다.
# for(변수 in 반복횟수) 실행문 : 실행문을 반복횟수만큼 실행

#예제1
#변수 i가 1에서 5까지의 값을 갖을 때까지 print(rep(i,i))라는 실행문을 실행한다. 
#i=1이면 print(rep(1,1))을 실행하고 i=2이면 print(rep(2,2))을 실행한다. 이렇게 i=5일때까지 실행을 하게된다.
for(i in 1:5) {print(rep(i,i))}

#예제2 : 1부터 10까지 합 구하기
sum.x<-0

for(i in 1:10){
  sum.x<-sum.x + i}
sum.x

rm(list = ls()) #오브젝트 내의 모든 변수 삭제


#######################  for문 주사위 문제 ############################
# sample(1:6,1)
# set.seed(1234)

# 주사위를 10번 던진 값들의 합을 구하시오


set.seed(1234)
sum.y<-0
for(i in 1:10){
  sum.y<-sum.y + sample(1:6,1) 
}
sum.y


################################################################



################# 복습하기 ########################

a1 <-  data.frame(name=c("aa","bb","cc"),value=seq(10,20,length.out = 3))
a2 <-  data.frame(name=c("cc","dd","ee"),value=seq(30,50,length.out = 3))
a3 <-  data.frame(name=c("aa","dd","ee","ff"),value=seq(20,80,length.out = 4),any=seq(0,3,1))

#  1. a1 와 a2 를 행결합 하시오
rbind(a1,a2)

# 2. a3 와 a1 를 행결합하시오
install.packages("plyr")
library(plyr)
rbind.fill(a3,a1)

install.packages("plyr")
library(plyr)
rbind.fill(a3,a1)
# 3. a3 와 a2 를 name 기준으로 결합 하시오 (a3 데이터는 모두출력)
merge(a3,a2,by="name",all.x=T)
merge(a3,a2,by="name",all=T)
# 4. 3번 데이터를 z 에 저장하고 value.y 기준으로 정렬하시오
z<-merge(a3,a2,by="name",all.x=T)
z[order(z$value.y),]
z[order(z$value.y),]



#ifelse(조건, 조건이 참일때의 실행문, 조건이 참이 아닐때의 실행문)
x <- c(1,2,3,4)
y <- c(2,1,4,5)
ifelse(x<y, x, y)

ifelse(sum(x-y) > 0, "positive", 
       ifelse(sum(x-y) < 0 , "negative", "zero"))

############### 연습 문제 ##################
a<-c(4,3,3,5,"setosa") #순자가 문자로 변경
# 1. a를 iris 데이터 밑에 추가하고 iris에 저장하시오

iris <- rbind(iris,a) #iris 숫자 컬럼이 문자로 변경

str(iris)
str(a)
# 2. 저장한 iris로 Sepal.Width가 Sepal.Width의 평균보다 큰값은 1 아니면 0인 변수 z1을 만드시오
# 평균은 mean(iris$Sepal.Width)
ifelse()

iris$Sepal.Width <- as.integer(iris$Sepal.Width) ###
iris$z1 <- ifelse(iris$Sepal.Width > mean(iris$Sepal.Width),1,0)


############################################


###########################
###### 다양한 apply #######
###########################

# apply( )

# 배열 또는 행렬에 주어진 함수를 적용한 뒤 그 결과를 벡터, 배열 또는 리스트로 반환
# 배열 또는 행렬에 적용

apply(
  X,       # 배열 또는 행렬
  MARGIN,  # 함수를 적용하는 방향. 1은 행 방향, 2는 열 방향
  # c(1, 2)는 행과 열 방향 모두를 의미
  FUN      # 적용할 함수
)


# sum이라는 함수 적용에 대해서 진행
sum(1:10)
d <- matrix(1:9, ncol=3)
d
apply(d, 1, sum)
apply(d, 2, sum)

head(iris)
apply(iris[, 1:4], 2, sum)

# 이와같이 행, 열의 합 평균은 빈번하게 사용되므로 알면 좋은 함수들
rowSums(
  x,            # 배열 또는 숫자를 저장한 데이터 프레임
  na.rm=FALSE,  # NA를 제외할지 여부
)
#반환 값은 행 방향에 저장된 값의 합이다.

rowSums(iris[, 1:4])
colSums(iris[, 1:4])


#####

# lapply( )
# lapply : 벡터, 리스트, 표현식, 데이터 프레임 등에 함수를 적용하고 그 결과를 리스트로 반환한다.

lapply(
  X,    # 벡터, 리스트, 표현식 또는 데이터 프레임
  FUN,  # 적용할 함수
)
# 반환 값은 X와 같은 길이의 리스트다.

# 백터를 넣었을 경우
(result <- lapply(1:3, function(x) { x*2 })) ###() 실해값을 보여줌

# unlist : 리스트 구조를 벡터로 변환한다.
unlist(result)

# list를 넣었을 경우
(x <- list(a=1:3, b=4:6))
lapply(x, mean)
unlist(lapply(x, mean))

# data.frame을 넣었을 경우
lapply(iris[, 1:4], mean)
unlist(lapply(iris[, 1:4], mean))

sapply(iris[, 1:4], mean) ### 위에 두줄 이것과 동일함
########################

sapply( )
# lapply와 유사하지만 결과를 벡터, 행렬 또는 배열로 반환

sapply(
  X,    # 벡터, 리스트, 표현식 또는 데이터 프레임
  FUN,  # 적용할 함수
)
# 반환 값은 FUN의 결과가 길이 1인 벡터들이면 벡터, 길이가 1보다 큰 벡터들이면 행렬이다.

lapply(iris[, 1:4], mean)
sapply(iris[, 1:4], mean)


# sapply( )에 인자로 주어진 함수의 출력이 길이가 1보다 큰 벡터들이라면 sapply( )는 행렬을 반환
sapply(1:10, function(x){ x + 1})

sapply(iris[, 1:4], function(x) { x > 3 })
sapply(iris[, 1:4], function(x) { x + 1 })




###########################
###### 함수 만들기  #######
###########################

# 1. 함수 생성 및 실행하기
myfunction <- function(){
  print("Hi Hello")
}

myfunction()


# 2. 인수 값 전달 함수
make_sum <- function(x,y){
  x+y
}

make_sum(3,4)


# 3. 기본 값 지정하기
pp <- function(x,y=6){
  x^y
}

pp(2)
pp(4,2)


# 4. 함수에서 특정 값 반환 return

make_sum <- function(x,y){
  return(x+y)
}

make_sum(3,4)


dt <- function(x,y){
  add <- x+y
  mul <- x*y
  c(add = add, mul = mul)  #2개 값을 리턴할 결우 c로 묶어줌
}

dt(3,5)
