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
join_full_dt <-  full_join(dallor_idx_2_dt, won_dolloar_2_dt, by = "date") %>% 
  filter(!is.na(index))

head(join_full_dt)




#===========================================================================
# 4) 통계
#===========================================================================
join_full_dt %>% 
  summarise(max_price = max(price),
            min_price = min(price),
            mean_price = mean(price))
# max_price min_price mean_price
#     <dbl>     <dbl>      <dbl>
#1     1141.     1085.      1111.

join_full_dt %>% 
  summarise(max_index = max(index),
            min_index = min(index),
            mean_index = mean(index))
# max_index min_index mean_index
#     <dbl>     <dbl>      <dbl>
#1      92.3      89.4       90.7


#===========================================================================
# 5) 달러 갭 비율 계산하기 
#    달러지수 / 원 달러 환율 * 100 
#===========================================================================

join_cal_1_dt <- join_full_dt %>% 
  mutate(dollar_gap = index/price * 100)

# head(join_cal_1_dt)



#===========================================================================
# 6) 52주 평균 달러 갭 비율 계산하기 
#    현재의 달러지수 / 52주 평균 달러 갭 비율 * 100 
#===========================================================================
join_cal_2_dt <- join_cal_1_dt
join_cal_2_dt$mean_52week_gap <- 0


for(i in 1:nrow(join_cal_2_dt)) {
  join_cal_2_dt[i,7] <- join_cal_2_dt %>% 
    filter(date <=  as.Date(unlist(join_cal_2_dt[i,1])),  date >= as.Date(unlist(join_cal_2_dt[i,1] - 365))) %>% 
    select(dollar_gap) %>% 
    summarise(mean_52week_gap = mean(dollar_gap)) %>% unlist()
}

# head(join_cal_2_dt, 50)
# tail(join_cal_2_dt)


#===========================================================================
# 7) 52주 평균 달러 계산하기 
#===========================================================================
join_cal_2_dt$mean_price <- 0

for(i in 1:nrow(join_cal_2_dt)) {
  join_cal_2_dt[i,8] <- join_cal_2_dt %>% 
    filter(date <=  as.Date(unlist(join_cal_2_dt[i,1])),  date >= as.Date(unlist(join_cal_2_dt[i,1] - 365))) %>% 
    select(price) %>% 
    summarise(mean_price = mean(price)) %>% unlist()
}

#===========================================================================
# 8) 52주 평균 달러 지수 계산하기 
#===========================================================================
join_cal_2_dt$mean_index <- 0

for(i in 1:nrow(join_cal_2_dt)) {
  join_cal_2_dt[i,9] <- join_cal_2_dt %>% 
    filter(date <=  as.Date(unlist(join_cal_2_dt[i,1])),  date >= as.Date(unlist(join_cal_2_dt[i,1] - 365))) %>% 
    select(index) %>% 
    summarise(mean_index = mean(index)) %>% unlist()
}



#===========================================================================
# 9) adjusted price
#===========================================================================
join_cal_2_dt <- join_cal_2_dt %>% 
  mutate(adjust_price = price / mean_52week_gap * 100)




#===========================================================================
# 7) 달러 최적 매수 타이밍 판단하기  
#===========================================================================
#
#    7-1) 원달러 환율 하락 (각각 또는 두가지 상황이 발생될 수 있음)
#         - 달러 가치의 하락 -> 달러지수 값이 하락 
#         - 원화 가치의 상승 -> 달러지수는 동일하나 원화가치가 상승 할 경우 
#
#    7-2) 달러지수 (달러의 절대가치): dollar_idx 
#         - 1993년 3월 기준 100을 설정 (6개국 기준)
#
#    7-3) 매수 타이밍 
#         조건 1) 현재 환율이 52주 평균 환율 보다 낮을 때 
#         조건 2) 현재 달러지수가 52주 평균 달러 지수 보다 낮을 때 
#         조건 3) 현재 달러 Gap비율이 52주 평균 달러 Gap비율 보다 높을 때  
#         조건 4) 현재 환율이 적정 활율 보다 낮을 때 
#                 적정환율 = 현재 달러지수 / 52주 평균달러 Gap비율 * 100 
# 
#===========================================================================

join_cal_2_dt$cnd_1 <- FALSE
join_cal_2_dt$cnd_2 <- FALSE
join_cal_2_dt$cnd_3 <- FALSE
join_cal_2_dt$cnd_4 <- FALSE

# 조건1 : 현재 환율이 52주 평균 환율 보다 낮을 때 
join_cal_2_dt$cnd_1 <- ifelse(join_cal_2_dt$price  <  join_cal_2_dt$mean_price, TRUE, FALSE)


# 조건 2) 현재 달러지수가 52주 평균 달러 지수 보다 낮을 때 
join_cal_2_dt$cnd_2 <- ifelse(join_cal_2_dt$index  <  join_cal_2_dt$mean_index, TRUE, FALSE)


#조건 3) 현재 달러 Gap비율이 52주 평균 달러 Gap비율 보다 높을 때
join_cal_2_dt$cnd_3 <- ifelse(join_cal_2_dt$dollar_gap  >  join_cal_2_dt$mean_52week_gap, TRUE, FALSE)


#조건 4) 현재 환율이 적정 활율 보다 낮을 때 
join_cal_2_dt$cnd_4 <- ifelse(join_cal_2_dt$price < join_cal_2_dt$index / join_cal_2_dt$mean_52week_gap *100, TRUE, FALSE)

head(join_cal_2_dt)




