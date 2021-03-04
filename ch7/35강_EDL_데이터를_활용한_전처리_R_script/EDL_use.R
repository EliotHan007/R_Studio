setwd("~/R/cs_task/edl") # 작업하는 워킹디렉토리 셋팅을 합니다.

# 필요한 라이브러리를 불러옵니다. 패키지가 안깔렸다면 install.packages()를 이용해서 설치해 줍니다.
library(data.table)
library(R.utils)
library(readr) # read_csv(), write_csv() 함수에 사용

# 파일 용량이 크므로 read.csv 등으로 읽지 않고 fread 함수를 이용해서 읽습니다.
# 한글이 깨질수 있으니 인코딩은 "UTF-8"로 해줍니다.
data <- fread(file="EXP_114112_20210115155334_06M1c-000000000000.csv.gz", encoding='UTF-8')
# 참고로 read_csv(file="EXP_114112_20210115155334_06M1c-000000000000.csv.gz", locale=locale('ko',encoding='UTF-8')) 해도 됩니다.

# 불러온 데이터의 구조와 어떤 컬럼과 내용이 있는지 str(), head() 함수를 통해 확인해 봅니다.
str(data)
head(data)
# 읽은 데이터가 데이터프레임이 아니므로 데이터프레임으로 변경해서 사용합니다.
data_df <- as.data.frame(data)

# 읽어온 데이터의 컬럼 이름 그냥 쓰기도 하지만 바꾸어 쓰는 경우에는 names() 함수를 이용해서 바꿉니다.
names(data_df) # 전체 컬럼 이름을 확인합니다.
names(data_df)[1] # 바꾸고 싶은 컬럼이 몇 번째인지 확인한 후 []에 넣어줍니다.
# 1번 컬럼 이름을 바꿔줍니다. P_PTT를 P_new 로 바꿔봅니다.
names(data_df)[1] <- 'P_new'
names(data_df)[1]
# 여러 컬럼 이름을 바꾸고 싶다면 여러개의 이름을 c() 함수에 넣어줘서 바꿉니다.
# 1, 3번 컬럼의 이름을 바꾸는 경우입니다.
names(data_df)[c(1, 3)] <- c('P_new', 'AFFILIATE_CODE_new')
names(data_df)

# 특정모델명의 데이터만 뽑아서 분석하고자 합니다. dplyr 패키지의 filter 함수를 이용합니다.
# %>%(체인오퍼레이터)를 이용해 좌측의 데이터를 우측의 함수로 넘겨서 사용하겠습니다.
library(dplyr)
data_model <- data_df %>% filter(MODEL_CODE %in% c('LGF500L', 'LGF500S','LGF500K', 'LGF600L','LGF600S','LGF600K', 
                                                   'LGF700L', 'LGF700S','LGF700K')) 
# filter(data_df, MODEL_CODE == "LGF500S")로 해도 됩니다.

# 환경에 따라 메모리가 부족한 경우 원하는 모델의 데이터만 뽑아 따로 변수에 넣었다면,
# 기존의 data는 rm() 함수를 이용해 메모리에서 삭제하면 됩니다.
rm(data)

# 모든 값이 NA인 컬럼은 의미가 없으므로 먼저 삭제합니다.
ind_not_na_col <- !apply(is.na(data_model), 2, all)
data_model_not_NA_col <- data_model[ ,ind_not_na_col]

# 행의 값이 NA가 포함된 행을 없애 보도록 하겠습니다. NA.omit 함수를 이용합니다.
# 행의 전체가 NA인 경우를 없애고 싶으면 complete.cases() 함수를 사용합니다.
data_model_not_NA_col_row <- na.omit(data_model_not_NA_col)


# 특정 컬럼의 값에서 일부를 뽑아서 새로운 컬럼을 만들어 보겠습니다.

# SUFFIX에서 바이어 이름을 뽑아서 새로운 컬럼에 생성하도록 하겠습니다.
# 먼저 SUFFIX_CODE를 확인 한 후 몇번째 문자를 뽑으면 되나 확인한 후 substr() 함수를 이용해서 뽑겠습니다.
# 2~4번째 문자를 뽑으면 바이어를 뽑을 수 있습니다.
data_model$BUYER_NEW <- substr(data_model$SUFFIX_CODE,2,4)
data_model[,c('SUFFIX_CODE','BUYER_NEW')]

# MODEL_CODE 컬럼의 값이 LSK가 붙어 있으므로 이를 제거해서 새로운 컬럼을 만들겠습니다.
# 다만 맨 끝에 문자만 제거하면 되지만, 이번에는 조건에 따라 만드는 방법으로 해보겠습니다.
# 먼저 LGF500 모델명 3개가 포함된 행 번호를 찾습니다. 이때 해당 컬럼과 조건을 %in%로 연결해서 행번호를 뽑습니다.
# 행번호는 ind에 저장되는데 행번호마다 true, false로 뽑아줍니다.
ind <- data_model$MODEL_CODE %in% c('LGF500L', 'LGF500S','LGF500K')
data_model[ind,'new_model'] <- 'LGF500' # true인 행에만 값을 넣어줍니다.
data_model[,c('MODEL_CODE', 'new_model')] # 정상적으로 바뀌었나 확인해 봅니다.

ind1 <- data_model$MODEL_CODE %in% c('LGF600L','LGF600S','LGF600K')
data_model[ind1,'new_model'] <- 'LGF600'

ind2 <- data_model$MODEL_CODE %in% c('LGF700L', 'LGF700S','LGF700K')
data_model[ind2,'new_model'] <- 'LGF700'

data_model[,c('MODEL_CODE', 'new_model')]

# 만약 여러 컬럼에 조건을 걸어서 뽑고 싶다면 &나 |로(and, or) 해줍니다.
ind3 <- data_model$MODEL_CODE %in% c('LGF600L','LGF600S','LGF600K') & data_model$CAUSE_CODE %in% c('MDA')
data_model[ind3,c('MODEL_CODE', 'CAUSE_CODE')]

# 생산 년월에 구매 날짜를 연결해서 새로운 날짜 컬럼을 만들어 보도록 하겠습니다.
# 먼저 생선월 컬럼과 구매월 컬럼을을 확인합니다.
data_model$PRODUCTION_YYYYMM
data_model$PURCHASE_YYYYMMDD
# 두개 컬럼에서 어떤 데이터를 뽑을 지 확인이 된 후 새로운 컬럼을 생성합니다.
# 각 컬럼에서 substr()을 이용해서 문자를 뽑고, paste0()를 이용해서 붙여줍니다.
data_model[,'new_date'] <- paste0(substr(data_model$PRODUCTION_YYYYMM,1,4), substr(data_model$PURCHASE_YYYYMMDD,5,6))
data_model$new_date

# 바이어별로 원인코드가 몇개인지 확인하는 새로운 데이터를 만들고자 할때는
# dplyr의 group_by() 함수를 이용해 원하는 컬럼끼리 조합을 만들어 내고,
# summarise() 함수를 이용해 조합별로 요약을 합니다. 행수를 카운트 하므로 여기는 n() 함수를 썻습니다.
# count=n()에서 count는 행수를 넣어줄 컬럼 이름입니다.
# 만약 조합에서 어떤 컬럼의 합을 구하고 싶다면 sum=sum(컬럼이름)을 쓰면 됩니다.
data_buyer_cause_combination <-  data_model %>% group_by(BUYER_NEW, CAUSE_CODE) %>% summarise(count=n())
# 새로 생성한 데이터를 저장해서 쓰기
write_csv(data_buyer_cause_combination, "new_data.csv")

# 환경에 따라 메모리가 부족한 경우 원하는 모델의 데이터만 뽑아 따로 변수에 넣었다면,
# 기존의 data는 rm() 함수를 이용해 메모리에서 삭제하면 됩니다.
rm(list=ls())

# 생산데이터 읽어오고, 편의상 MODEL_CODE, PRODUCTION_QTY 컬럼만 뽑아 쓰겠습니다.
data_prod <- fread(file="EXP_114112_20210115155257_G7Tut-000000000000.csv.gz", encoding="UTF-8")
data_prod_sel <- data_prod[,c('MODEL_CODE', 'PRODUCTION_QTY')]
head(data_prod_sel)

# 서비스데이터는 컬럼이 너무 많아서 편의상 MODEL_CODE, DIVISION_CODE 컬럼만 뽑아 쓰겠습니다.
data <- fread(file="EXP_114112_20210115155334_06M1c-000000000000.csv.gz", encoding='UTF-8')
data_svc_sel <- data[,c('MODEL_CODE', 'DIVISION_CODE')]
head(data_svc_sel)


# 환경에 따라 메모리가 부족한 경우 원하는 모델의 데이터만 뽑아 따로 변수에 넣었다면,
# 기존의 data는 rm() 함수를 이용해 메모리에서 삭제하면 됩니다.
rm(data_prod)
rm(data)


# merge() 함수를 이용해서 두개 데이터를 합칩니다. 이때 키가 되는 컬럼은 MODEL_CODE 입니다.

# 생산데이터는 MODEL_CODE와 PRODUCTION_QTY 컬럼만 뽑았으므로, MODEL_CODE별 생산수량이 여러개 나타납니다.
# 마찬가지로 서비스 데이터도 MODEL_CODE, DIVISION_CODE 별 그룹핑이 안되어 중복이 여러개 나타납니다.
# 그러므로 서비스데이터, 생산데이터는 다시 한번 그룹핑해서 진행을 먼저해야 합니다.
data_svc_sel_gr <- data_svc_sel %>% group_by(MODEL_CODE, DIVISION_CODE) %>% summarise(n())
data_prod_sel_gr <- data_prod_sel %>% group_by(MODEL_CODE) %>% summarise(PRODUCTION_sum=sum(PRODUCTION_QTY))

# all.x=TRUE 옵션을 통해 먼저 적은 변수인 서비스 데이터에 그 다음 적은 생산데이터를
# 엑셀의 vlookup 하는 형태로 진행합니다. 
merge_data <- merge(data_svc_sel_gr, data_prod_sel_gr, by='MODEL_CODE', all.x=TRUE)
tail(merge_data)

# 제대로 동작하는 지 확인하기 위해, 서비스 데이터를 100개만 뽑아서 실행한 결과를 확인해 보도록 하겠습니다.
merge_data1 <- merge(data_svc_sel_gr[1:100,], data_prod_sel_gr, by='MODEL_CODE', all.x=TRUE)
print(merge_data1)
