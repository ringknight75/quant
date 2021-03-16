#===========================================================================
# 작성목적 : 적절한 달러 매매 시기 확보 
# 작 성 자 : 반지기사 
# 작성일시 : 2021년 3월 16일 ~ 
# 특이사항 : 인베스팅 닷컴 사이트에서 직접 엑셀파일 다운로드 하여 이용함  
# 단    점 :  
#===========================================================================
library(tidyverse)
library(tidyr)
library(lubridate)


getwd()

#===========================================================================
# 1) 인베스팅 닷컴에서 "USD/KRW - 미국 달러 원" 엑셀 다운로드하기 
#    - 위치 : https://kr.investing.com/currencies/usd-krw-historical-data
#             > 과거데이터 > 2020년 1월 1일 부터 ~ 
#    - data 폴더에 위치 시킴 
#===========================================================================
won_dolloar_dt <- read_csv("data/USD_KRW 내역.csv") %>% 
  rename(dates = 날짜, price = 종가, var_dollar = `변동 %`) %>% 
  select(dates, price, var_dollar) %>% 
  mutate(year = substring(dates,1,4), month = substring(dates, 7, 8), day = substring(dates, 11, 12),
         dates1 = str_c(year, month, day),
         date = as_date(dates1)) 

won_dolloar_2_dt <- won_dolloar_dt %>% 
  select(date, price, var_dollar)

head(won_dolloar_2_dt) 

#===========================================================================
# 2) 인베스팅 닷컴에서 "미국 달러 지수" 엑셀 다운로드하기 
#    - 위치 : https://kr.investing.com/currencies/us-dollar-index-historical-data
#             > 과거데이터 > 2020년 1월 1일 부터 ~ 
#    - data 폴더에 위치 시킴 
#===========================================================================
dallor_idx_dt <- read_csv("data/미국 달러 지수 선물 내역.csv") %>% 
  rename(dates = 날짜, index = 종가, var_index = `변동 %`) %>% 
  select(dates, index, var_index) %>% 
  mutate(year = substring(dates,1,4), month = substring(dates, 7, 8), day = substring(dates, 11, 12),
       dates1 = str_c(year, month, day),
       date = as_date(dates1)) 

dallor_idx_2_dt <- dallor_idx_dt %>% 
  select(date, index, var_index)


head(dallor_idx_2_dt)


#===========================================================================
# 3) dataframe JOIN (full join)
#===========================================================================
join_full_dt <-  full_join(dallor_idx_2_dt, won_dolloar_2_dt, by = "date")

head(join_full_dt)




