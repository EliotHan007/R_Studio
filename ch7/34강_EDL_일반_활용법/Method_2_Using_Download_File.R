# install.packages('data.table')  # Package가 설치 안 되어 있는 경우에만 실행


library('data.table')

tmp   <- 'EXP_272630_20210121154732_gL6g4-000000000000.csv.gz'

sc_dt <- fread(file=tmp, encoding="UTF-8")

head(sc_dt)
