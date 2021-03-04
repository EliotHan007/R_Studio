setwd("~/R/cs_task/edl") # 작업하는 워킹디렉토리 셋팅을 합니다.

# 필요한 라이브러리를 불러옵니다. 패키지가 안깔렸다면 install.packages()를 이용해서 설치해 줍니다.
library(data.table)
library(R.utils)

# 파일 용량이 크므로 read.csv 등으로 읽지 않고 fread 함수를 이용해서 읽습니다.
# 한글이 깨질수 있으니 인코딩은 "UTF-8"로 해줍니다.
data <- fread(file="EXP_114112_20210115155334_06M1c-000000000000.csv.gz", encoding='UTF-8')

# 불러온 데이터의 구조와 어떤 컬럼과 내용이 있는지 str(), head() 함수를 통해 확인해 봅니다.
str(data)
head(data)
# 읽은 데이터가 데이터프레임이 아니므로 데이터프레임으로 변경해서 사용합니다.
data_df <- as.data.frame(data)

# 특정모델명의 데이터만 뽑아서 분석하고자 합니다. dplyr 패키지의 filter 함수를 이용합니다.
# %>%(체인오퍼레이터)를 이용해 좌측의 데이터를 우측의 함수로 넘겨서 사용하겠습니다.
library(dplyr)
data_model <- data_df %>% filter(MODEL_CODE %in% c('LGF500L', 'LGF500S','LGF500K', 'LGF600L','LGF600S','LGF600K', 
                                                   'LGF700L', 'LGF700S','LGF700K')) 

names(data_model)
str(data_model)

# 연속형 숫자인 컬럼 이름만 따로 뽑아봅니다. apply 계열의 함수인 sapply() 함수를 사용합니다.
ind <- sapply(data_model, is.numeric)
data_model_numeric <- data_model[,ind]
ncol(data_model_numeric) # 컬럼 수를 확인해 봅니다.

# 이산형인 character 인 컬럼 이름만 따로 뽑아봅니다. apply 계열의 함수인 sapply() 함수를 사용합니다.
ind1 <- sapply(data_model, is.character)
data_model_character <- data_model[,ind1]
ncol(data_model_character) # 컬럼 수를 확인해 봅니다.

# 연속형 데이터만 모인 데이터를 이용해 다양한 그래프를 그려 봅니다. ggplot 패키지를 사용하도록 하겠습니다.
library(ggplot2)
names(data_model_numeric) # 먼저 사용할 컬럼 이름을 확인합니다.
summary(data_model_numeric$PART_COST_USD_AMOUNT) # 사용할 컬럼에 값의 분포 및 NA갯수를 확인합니다.
ind3 <- is.na(data_model_numeric$PART_COST_USD_AMOUNT) # is.na() 함수를 통해 na가 있는 행을 찾습니다.
data_model_numeric_not_na <- data_model_numeric[!ind3,] # na를 제거한 데이터를 다시 만듭니다.
summary(data_model_numeric_not_na$PART_COST_USD_AMOUNT)
# ggplot 함수에 데이터는 na 제거한 데이터를 지정하고 x축의 컬럼을 PART_COST_USD_AMOUNT을 지정합니다.
# 먼저 x가 연속형이므로 히스토그램을 그려봅니다.
ggplot(data=data_model_numeric_not_na, aes(x=PART_COST_USD_AMOUNT)) +
  geom_histogram(bins = 50) # bins는 간격을 지정하는 것으로 값을 바꾸면 그래프의 모양이 바뀝니다.

# 확률 밀도함수를 그려보도록 하겠습니다.x의 컬럼은 다양하게 바꿔봐도 됩니다.
# geom_density() 함수를 이용합니다. 밀도함수이므로 아래쪽에 색을 입히려면 fill 옵션을 주면 됩니다.
ggplot(data=data_model_numeric_not_na, aes(x=PART_COST_USD_AMOUNT)) +
  geom_density()
ggplot(data=data_model_numeric_not_na, aes(x=PART_COST_USD_AMOUNT)) +
  geom_density(fill='green')

# 두개 함수를 합쳐서 그려 보도록 하겠습니다. aes에 변수를 지정하는데 y=..density..를 추가해 줍니다.
ggplot(data=data_model_numeric_not_na, aes(x=PART_COST_USD_AMOUNT, y=..density..)) +
  geom_histogram(bins = 50) +
  geom_density(fill='green')

# 박스플랏을 그려보도록 하겠습니다.
ggplot(data=data_model_numeric_not_na, aes(x=PART_COST_USD_AMOUNT)) +
  geom_boxplot()

# 이산형 변수와 함게 박스플랏을 그려보도록 하겠습니다. 연속형, 이산형이 함께 있는 전체 데이터를 사용하겠습니다.
names(data_model) # 전체데이터에서 어떤 컬럼을 선택할지 컬럼 이름을 확인합니다.
# ggplot 함수를 이용할때 na.rm=TRUE 옵션을 추가해서 na 데이터는 무시하게 설정 합니다.
ggplot(data=data_model, aes(x=MODEL_CODE, y=PART_COST_USD_AMOUNT, na.rm=TRUE)) +
  geom_boxplot(na.rm=TRUE)
# 박스플랏의 이상치에 대해 빨간색으로 표시를 해주고, 모양은 여러가지중에 2번을 사용해서 삼각형으로 표시합니다.
ggplot(data=data_model, aes(x=MODEL_CODE, y=PART_COST_USD_AMOUNT, na.rm=TRUE)) +
  geom_boxplot(na.rm=TRUE, outlier.color = 'red',outlier.shape = 2) 

# 이산형 데이터에 대한 그래프를 그려보도록 하겠습니다.
names(data_model_character)
# SECTION_CODE 별 갯수에 대해 그려보도록 하겠습니다. geom_bar() 함수를 이용합니다.
ggplot(data=data_model_character, aes(x=SECTION_CODE)) +
  geom_bar()
# fill 옵션을 이용해 SECTION_CODE내에서 MODEL_CODE 별로 그래프를 그려봅니다.
ggplot(data=data_model_character, aes(x=SECTION_CODE, fill=MODEL_CODE)) +
  geom_bar()
ggplot(data=data_model_character, aes(x=SECTION_CODE, fill=MODEL_CODE)) +
  geom_bar(position='dodge') # position을 줘서 별도의 막대그래프로 그려볼 수 있습니다.

# 파이차트를 그려보도록 하겠습니다.
# 파이차트를 그리기 위해 먼저 SECTION_CODE value별로 갯수를 카운트한 요약 데이터를 만듭니다.
# dplyr의 group_by()함수를 사용하고, 그 후에 summarise 함수를 이용해서 만듭니다.
data_model_summary <- data_model_character %>% group_by(SECTION_CODE) %>% summarise(count=n())
# 요약데이터를 ggplot에 넣어줍니다. y는 갯수 컬럼을 넣어주고, fill에 구분되는 변수를 넣어줍니다.
# 나머지 옵션은 실습사례 처럼 동일하게 넣어줍니다.
ggplot(data=data_model_summary, aes(x='', y=count, fill=SECTION_CODE)) +
  geom_bar(stat='identity', width=1) + 
  coord_polar('y', start=0)

# 하나의 변수가 아닌 변수간 그래프를 그려보도록 하겠습니다.
# 먼저 산점도를 그려보도록 하겠습니다. 연속형 데이터를 활용하겠습니다.
# 산점도는 x, y 모두 연속형일때 그리는 그래프입니다.
names(data_model_numeric)
# 연속현 변수의 컬럼인 REPAIR_TIME, LABOR_COST_LOC_AMOUNT을 x, y에 넣고 geom_point() 함수를 이용합니다.
ggplot(data=data_model_numeric, aes(x=REPAIR_TIME, y=LABOR_COST_LOC_AMOUNT, na.rm=TRUE)) +
  geom_point()
ggplot(data=data_model_numeric, aes(x=REPAIR_TIME, y=LABOR_COST_LOC_AMOUNT, na.rm=TRUE)) +
  geom_point(shape=15, size=3, colour='blue') # 점의 모양과 색을 지정할 수 있습니다.

data_model_numeric_sel <- data_model_numeric %>% filter(LABOR_COST_LOC_AMOUNT >0)
ggplot(data=data_model_numeric_sel, aes(x=REPAIR_TIME, y=LABOR_COST_LOC_AMOUNT, na.rm=TRUE)) +
  geom_point() + geom_line() # geom_line()함수를 이용해 선을 추가할 수있습니다.
ggplot(data=data_model_numeric_sel, aes(x=REPAIR_TIME, y=LABOR_COST_LOC_AMOUNT, na.rm=TRUE)) +
  geom_point() + geom_line() +
  stat_smooth(method=lm) # stat_smooth() 함수를 이용해 선형 회귀선을 추가할 수 있습니다.

# 시계열 그래프를 그려보도록 하겠습니다. 시계열 그래프는 산점도에서 x를 시간 변수로만 지정해주면 됩니다.
names(data_model_numeric_sel)
# 구매 년도별 LABOR_COST_LOC_AMOUNT를 합친 요약 데이터를 만들어 사용하도록 하겠습니다.
# 구매년월일 데이터에서 년월까지만 substr() 함수를 이용해 뽑아서 별도의 컬럼에 저장합니다.
data_model_numeric_sel$purchase_date <- substr(data_model_numeric_sel$PURCHASE_YYYYMMDD,1,6) 
# group_by() 함수를 이용해 LABOR_COST_LOC_AMOUNT를 구매년월 별로 합친 요약데이터를 만듭니다.
data_model_summary1 <- data_model_numeric_sel %>% group_by(purchase_date) %>% 
  summarise(sum_col=sum(LABOR_COST_LOC_AMOUNT, na.rm=TRUE))
# 요약데이터를 이용해 그래프를 그립니다.
# x축이 character면 에러가 발생해 group=1을 추가하여 사용하고, numeric이면 group=1이 필요없습니다.
# data_model_summary1$purchase_date <- as.numeric(data_model_summary1$purchase_date) 이렇게 먼저 처리하면 group=1은 없어도 됩니다.
ggplot(data=data_model_summary1, aes(x=purchase_date, y=sum_col, group=1)) +
  geom_point() + geom_line()
# X축의 년월이 너무 많아 겹치는걸 방지하기 위해 라벨을 90도 회전시킵니다.
ggplot(data=data_model_summary1, aes(x=purchase_date, y=sum_col, group=1)) +
  geom_point() + geom_line() +
  theme(axis.text.x=element_text(angle=90))

# 연속형 변수간 상관관계 그래프를 그려보도록 하겠습니다.
library(corrplot)
names(data_model_numeric)
# 연속형 변수중에 분석할 컬럼만 뽑아서 데이터를 다시 만듭니다.
data_cor <- data_model_numeric[,44:47]
# na가 포함된 행은 모두 삭제합니다.
data_cor <- na.omit(data_cor)
# cor 함수를 통해 상관관계 분석하고, 그 결과를 변수에 저장합니다.
x <- cor(data_cor)
# corrplot 함수에 상관관계 분석 결과를 저장한 변수를 넣어줍니다.
corrplot(x)
corrplot(x, method='number') # method에 number를 지정하면 숫자로 볼수 있습니다.
corrplot(x, type='upper') # type에 upper를 지정하면 위쪽 삼각형만 보여줍니다.
corrplot(x, type='upper', addCoef.col = "black") # 원에 r 값을 함께 보고 싶으면 addCoef.col을 넣어줍니다.

# 이산형 변수간에 히트맵을 그려보겠습니다.
names(data_model)
# 그래 안에 표시해줄 값을 요약데이터를 통해 만들어 보겠습니다.
# MODEL_CODE, SECTION_CODE별 LABOR_COST_LOC_AMOUNT의 평균을 구해서 사용해 보겠습니다.
data_model_summary2 <- data_model %>% group_by(MODEL_CODE, SECTION_CODE) %>% 
  summarise(mean_col=mean(LABOR_COST_LOC_AMOUNT, na.rm=TRUE))
# 요약한 데이터를 넣어주고, 변수에는 이산형 변수를 x, y에 넣고 fill에 값으로 쓸 변수를 넣어줍니다.
ggplot(data=data_model_summary2, aes(x=MODEL_CODE, y=SECTION_CODE, fill=mean_col)) +
  geom_tile() +
  theme(panel.grid = element_blank()) # 뒷 배경이 되는 선들은 theme() 함수를 이용해 없애 줍니다.

# 서비스 스크립트 내용을 가지고 워드클라우드 분석을 해보도록 하겠습니다.
# 워드클라우드는 단어별로 갯수를 카운트 해서 많은 수의 단어를 좀 더 크게 보여주는 그래프입니다.
# 아래 3가지의 패키지가 필요합니다. 본 과정에서는 설치에 대해서는 skip 하도록 하겠습니다.
# 필요하다면 구글에서 아래 패키지 설치 방법을 찾아 보십시요.
library(KoNLP)
library(Sejong)
library(wordcloud2)            
# 사전 사용을 위해 useSejongDic() 함수를 사용합니다.
useSejongDic()

# 먼저 SYMPTOM_DESC 컬럼의 내용을 하나의 글처럼 만들기 위해 한줄씩 paste() 함수를 이용해 이어 붙여줍니다.
SYMPTOM_DESC_total <- vector() # 이어붙인 글을 저장할 변수를 만듭니다.
for(i in 1:length(data_model$SYMPTOM_DESC)){ # 전체 행수를 length() 함수를 이용해 찾고 for문에 지정해 줍니다.
  # pate() 함수를 이용해 각각의 스크립트를 SYMPTOM_DESC_total에 한줄씩 붙여줍니다.
  # paste의 결과를 SYMPTOM_DESC_total에 다시 넣어주는건 계속 이어붙이기 위함입니다.
  SYMPTOM_DESC_total <- paste(SYMPTOM_DESC_total, data_model$SYMPTOM_DESC[i], sep=" ")
}

# 전체 이어붙인 문장에서 명사만 뽑아냅니다. 
data1 <- sapply(SYMPTOM_DESC_total, extractNoun, USE.NAMES = F) 
#한글자는 거의 의미가 없어 제외하기 위해 두글자 이상 필터링 합니다.
# Filter 함수안에 각각의 단어에 대해 글자수를 세는 nchar() 함수를 넣어주어 2글자 이상인 데이터만 남깁니다.
data3 <- Filter(function(x) {nchar(x) >= 2}, data1)

# 분석이 의미가 없는 단어를 빼는 작업 예시
# data3 <- gsub("\\d+", "", data3)              #숫자 제외, 정규식
# data3 <- gsub("\\(", "", data3)                 # (괄호 제외
# data3 <- gsub("\\)", "", data3)                 # )괄호 제외
# data3 <- gsub("\\조직", "", data3)
# data3 <- gsub("\\업무", "", data3)
# data3 <- gsub("\\이슈", "", data3)
# data3 <- gsub("\\필요", "", data3)
# data3 <- gsub("\\NA", "", data3)
# data3 <- gsub("\\경우", "", data3)
# data3 <- gsub("\\어려움", "", data3)
# data3 <- gsub("\\들이", "", data3)
# data3 <- gsub("\\생각", "", data3)
# data3 <- gsub("\\사항", "", data3)
# data3 <- gsub("\\진행", "", data3)
# data3 <- gsub("\\활동", "", data3)
# data3 <- gsub("\\관련", "", data3)
# data3 <- gsub("\\부분", "", data3)
# data3 <- gsub("\\경우", "", data3)
# data3 <- gsub("\\하기", "", data3)
# data3 <- gsub("\\Process", "프로세스", data3)
# data3 <- gsub("[A-Za-z]", "", data3)     # 영문 제외, 정규식

# 단어별로 숫자를 세서 새로운 요약데이터를 만듭니다.
wordcount <- table(data3)
# 요약데이터의 단어별 숫자에 대해 정렬을 하고, 상위 100개만 head() 함수를 통해 선택합니다.
wordcount <- head(sort(wordcount,decreasing=T), 100)
# wordcloud2() 함수에 요약데이터를 넣어주고, 그래프를 그립니다. 다양한 옵션을 스스로 학습해서 다양하게 그려보면 됩니다.
wordcloud2(data=wordcount)
# LG스마트체 Regular 폰트가 설치되어 있어 사용가능하다면 아래처럼 해볼 수도 있습니다.
wordcloud2(data=wordcount, fontFamily = 'LG스마트체 Regular', size=1, color="random-dark", backgroundColor="white", rotateRatio=0.75)
