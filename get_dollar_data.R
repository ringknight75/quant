#===========================================================================
# 작성목적 : 적절한 달러 매매 시기 확보 
# 작 성 자 : 반지기사 
# 작성일시 : 2021년 3월 16일 ~ 
# 특이사항 : getSymbols를 활용하여 데이터 Crawling 
# 단    점 : 당일 데이터를 가져올수 없음 
#===========================================================================
library(quantmod)
library(tidyverse)
library(lubridate)

(to_day   <- Sys.Date() - 1)
(from_day <- Sys.Date() - 367)

#===========================================================================
# 1) 원달러 환율 
#===========================================================================
won_dolloar_xts <- getSymbols("KRW=X", src = "yahoo", auto.assign = FALSE)
#won_dolloar_xts <- getSymbols("DEXKOUS", src = "FRED", auto.assign = FALSE)  # 4일전 데이터 적재됨 
# head(won_dolloar_xts)
tail(won_dolloar_xts)



#===========================================================================
# 2) 달러 지수 
#===========================================================================
dallor_idx_xts <- getSymbols("DX-Y.NYB",  src = "yahoo", auto.assign = FALSE)
# head(dallor_idx_xts)
tail(dallor_idx_xts)


#===========================================================================
# 3) xts -> dataframe 변경 
#===========================================================================
#한꺼번에 Adjusted만 남겨놓고 dataframe형식에 date를 넣어보자 

won_dolloar_dt <- data.frame(dates = index(won_dolloar_xts), 
                             adjusted = won_dolloar_xts$`KRW=X.Adjusted`, 
                             row.names = NULL) %>% 
  as_tibble() %>% 
  complete(dates = seq.Date(from = min(dates),
                            to = max(dates),
                            by = "day"))
# tail(won_dolloar_dt, 20)

dallor_idx_dt <- data.frame(dates = index(dallor_idx_xts), 
                            adjusted = dallor_idx_xts$`DX-Y.NYB.Adjusted`, 
                            row.names = NULL) %>%
  as_tibble() %>% 
  complete(dates = seq.Date(from = min(dates),
                            to = max(dates),
                            by = "day"))


# tail(dallor_idx_dt, 20)


#===========================================================================
# 4) dataframe JOIN (full join)
#===========================================================================

join_full_dt <-  full_join(dallor_idx_dt, won_dolloar_dt, by = "dates") %>% 
  mutate(week     = wday(dates),
         day_week = week(dates),
         before_year = dates-365) %>% 
  rename(dollar_idx = DX.Y.NYB.Adjusted,
         won_dolloar_price = KRW.X.Adjusted) %>% 
  filter(week %in% 2:6) %>%   #1:일요일, 7:토요일 제외
  filter(!is.na(dollar_idx), !is.na(won_dolloar_price)) %>%  # NA값 제외 
  arrange(desc(dates))

# head(join_full_dt, 100)



#===========================================================================
# 5) 통계
#===========================================================================
join_full_dt %>% 
  summarise(max_won_dolloar_price = max(won_dolloar_price),
            min_won_dolloar_price = min(won_dolloar_price),
            mean_won_dolloar_price = mean(won_dolloar_price))
# max_won_dolloar_price min_won_dolloar_price mean_won_dolloar_price
#                 <dbl>                 <dbl>                  <dbl>
#1                 1571.                  890.                  1120.

join_full_dt %>% 
  summarise(max_dollar_idx = max(dollar_idx),
            min_dollar_idx = min(dollar_idx),
            mean_dollar_idx = mean(dollar_idx))
# max_dollar_idx min_dollar_idx mean_dollar_idx
#          <dbl>          <dbl>           <dbl>
#1           103.           71.3            87.0



#===========================================================================
# 6) 달러 갭 비율 계산하기 
#    달러지수 / 원 달러 환율 * 100 
#===========================================================================

join_cal_1_dt <- join_full_dt %>% 
  mutate(dollar_gap = dollar_idx/won_dolloar_price * 100)

# head(join_cal_1_dt)



#===========================================================================
# 7) 52주 평균 달러 갭 비율 계산하기 
#    현재의 달러지수 / 52주 평균 달러 갭 비율 * 100 
#===========================================================================
join_cal_2_dt <- join_cal_1_dt
join_cal_2_dt$mean_52week_gap <- 0


join_cal_2_dt %>% 
  filter(dates <= as.Date('2021-03-12'),  dates >=  as.Date('2020-03-12')) %>% 
  select(dollar_gap) %>% 
  summarise(mean_52week_gap = mean(dollar_gap))


for(i in 1:nrow(join_cal_2_dt)) {
  join_cal_2_dt[i,8] <- join_cal_1_dt %>% 
    filter(dates <=  as.Date(unlist(join_cal_2_dt[i,1])),  dates >= as.Date(unlist(join_cal_2_dt[i,1] - 365))) %>% 
    select(dollar_gap) %>% 
    summarise(mean_52week_gap = mean(dollar_gap)) %>% unlist()
}

# as.Date(unlist(join_cal_2_dt[7,1]))
# as.Date(unlist(join_cal_2_dt[7,1]-365))
# head(join_cal_2_dt, 50)
# tail(join_cal_2_dt)


#===========================================================================
# 8) 차이 계산하기 
#===========================================================================
join_cal_3_dt <- join_cal_2_dt %>% mutate(before_dollar_idx = lead(dollar_idx, n=1),
                         before_won_dolloar_price = lead(won_dolloar_price, n=1)) %>% 
  mutate(diff_dollar_idx = dollar_idx - before_dollar_idx,
         diff_won_dolloar_price = won_dolloar_price - before_won_dolloar_price) %>% 
  select(dates, dollar_idx, diff_dollar_idx, won_dolloar_price, diff_won_dolloar_price, dollar_gap, mean_52week_gap)



#===========================================================================
# 8) 시각화 하기  
#===========================================================================

join_cal_3_dt %>% select(dates, dollar_idx, won_dolloar_price, mean_52week_gap) %>% 
  mutate(adjust_won_dolloar_price = won_dolloar_price * 0.1,
         adjust_mean_52week_gap = mean_52week_gap * 10) %>% 
  select(dates, dollar_idx, adjust_won_dolloar_price, adjust_mean_52week_gap) %>% 
  gather(dollar_idx:adjust_mean_52week_gap, key = 'key', value = 'value') %>% 
  filter(dates >= '2021-01-01') %>% 
  ggplot(aes(x = dates, y = value)) +
  geom_line(aes(color = key)) +
  scale_y_log10()


# join_cal_3_dt 

#===========================================================================
# 9) 달러 최적 매수 타이밍 판단하기  
#===========================================================================
#
#    9-1) 원달러 환율 하락 (각각 또는 두가지 상황이 발생될 수 있음)
#         - 달러 가치의 하락 -> 달러지수 값이 하락 
#         - 원화 가치의 상승 -> 달러지수는 동일하나 원화가치가 상승 할 경우 
#
#    9-2) 달러지수 (달러의 절대가치): dollar_idx 
#         - 1993년 3월 기준 100을 설정 (6개국 기준)
#
#    9-3) 매수 타이밍 
#         조건 1) 현재 환율이 52주 평균 환율 보다 낮을 때 
#         조건 2) 현재 달러지수가 52주 평균 달러 지수 보다 낮을 때 
#         조건 3) 현재 달러 Gap비율이 52주 평균 달러 Gap비율 보다 높을 때  
#         조건 4) 현재 환율이 적정 활율 보다 낮을 때 
#                 적정환율 = 현재 달러지수 / 52주 평균달러 Gap비율 * 100 
# 
#===========================================================================

# 현재 max기준 52주 평균 환율 
max_date <- join_cal_3_dt %>% summarise(max_date = max(dates)) %>% unlist()

mean_won_dolloar_price <- join_cal_3_dt %>% 
  filter(dates <= max_date, dates >= max_date - 365) %>% 
  summarise(mean_won_dolloar_price = mean(won_dolloar_price)) %>% unlist()

mean_dollar_idx <- join_cal_3_dt %>% 
  filter(dates <= max_date, dates >= max_date - 365) %>% 
  summarise(mean_dollar_idx = mean(dollar_idx)) %>% unlist()


cat("================================================================================ ")
head(join_cal_3_dt, 1)


# 조건1 : 현재 환율이 52주 평균 환율 보다 낮을 때 
cat("현재 달러 가격 : ", unlist(join_cal_3_dt[1, 4]))
cat("52주 평균 환율 : ", mean_won_dolloar_price)
if(unlist(join_cal_3_dt[1, 4])  <  mean_won_dolloar_price) print("Condition 1 Yes")  else ("Condition 1 No") 

# 조건 2) 현재 달러지수가 52주 평균 달러 지수 보다 낮을 때 
cat("현재 달러 지수 : ", unlist(join_cal_3_dt[1, 2]))
cat("52주 평균 달러 지수 : ", mean_dollar_idx )
if(unlist(join_cal_3_dt[1, 2])  <  mean_dollar_idx) print("Condition 2 Yes")  else ("Condition 2 No") 

#조건 3) 현재 달러 Gap비율이 52주 평균 달러 Gap비율 보다 높을 때
cat("현재 달러 갭 비율  : ", unlist(join_cal_3_dt[1, 6]))
cat("52주 평균 달러 갭 비율  : ", unlist(join_cal_3_dt[1, 7]))
if(unlist(join_cal_3_dt[1, 6])  >  unlist(join_cal_3_dt[1, 7])) print("Condition 3 Yes")  else ("Condition 3 No") 

#조건 4) 현재 환율이 적정 활율 보다 낮을 때 
cat("현재 달러 가격 : ", unlist(join_cal_3_dt[1, 4]))
cat("적정 환율  : ", unlist(join_cal_3_dt[1, 2]) / unlist(join_cal_3_dt[1, 7]) * 100)
if(unlist(join_cal_3_dt[1, 4])  <  (unlist(join_cal_3_dt[1, 2]) / unlist(join_cal_3_dt[1, 7]) * 100)) print("Condition 4 Yes")  else ("Condition 4 No") 




