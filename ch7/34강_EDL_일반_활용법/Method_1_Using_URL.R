# install.packages('data.table')  # Package가 설치 안 되어 있는 경우에만 실행


library('data.table')

download_url <- 'http://e-datalake-dl.lge.com/exp/272630/20210121154750_saq39/EXP_272630_20210121154732_gL6g4-000000000000.csv.gz?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=svcac-edl-prd-bigquery%40pj-lge-edl.iam.gserviceaccount.com%2F20210121%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210121T064826Z&X-Goog-Expires=604800&X-Goog-SignedHeaders=host%3Bx-lge-edl-dl-signature&X-Goog-Signature=69f4d04fd5fceef31f35bec263149e15bb85a7ac3bccf8c2bf26e2e7dd1c65a050263f3c1e266b9d8288b0ebb66f2e630fbbb7afecbe352500f4105c1c98ea038fbc63f543f94e5be3f8620b8ab0506ee0a43a3cd8d2d20ac1444939bfc7c46312da7a68173f919da3c186250ecf09871cc1993dd24c9b2da665b7c7addf961f5f30fc0e29c049ea783bd15f73312356d4306121df804a53ed3b0c7e8cdaf418fbbd21d4b19a8c2ca6458a3bd3dec683c80100789435b795178928cb5e1ed17311512ba01706ed44a05afb4ebd36677060450b302f1c2f88747e945cc65fc5802fe0228d685e33196e734b8d8937dc4208e55792eec8e433e733e54843d5ea0a'

tmp   <- tempfile(fileext=".gz")

download.file(download_url, tmp, mode="wb")

sc_dt <- fread(file=tmp, encoding="UTF-8")

head(sc_dt)
