#===========================================================================
# 작성목적 : 적절한 달러 매매 적정 매수 타이밍 확인
# 작 성 자 : 반지기사 
# 작성일시 : 2021년 4월 28일 ~ 
# 특이사항 : getSymbols를 활용하여 데이터 Crawling 
# 단    점 : 당일 데이터를 가져올수 없음 
#===========================================================================
library(quantmod)
library(tidyverse)
library(lubridate)
(to_day   <- Sys.Date() - 1)
(from_day <- Sys.Date() - 367)
#===========================================================================
# 1) 분석 할 대상 지정 
#===========================================================================
# 배당주 대상 : T, MO, XOM, SPHD, SDIV, DIV, KO, ABBV, MMM, OHI, PG, QYLD, JNJ

# 5월 10일 기준 매도 가능 종목 : MO

target_stock <- "KO"





#===========================================================================
# 2) 데이터 crawling하기 
#===========================================================================
# 2-1) 배당 데이터 받기 
div_xts <- getDividends(target_stock, from = "1970-01-01", to = Sys.Date(),  src = "yahoo",  auto.assign = FALSE)
div_tb  <- data.frame(
  div_date = index(div_xts),
  div_mony = div_xts[,1]) %>%   
  as_tibble()  

colnames(div_tb) <- c("div_date", "div_mony")
# tail(div_tb)
# head(div_tb)

# 2-2 전체 주식 데이터 받기 
# ?getSymbols
target_stock_all_price <- getSymbols(Symbols = target_stock, src = "yahoo", auto.assign = FALSE)
# 
# tail(target_stock_all_price)
# index(target_stock_all_price)

# 2-3 최근 1년간 주식 데이터 받기 
target_stock_df <- data.frame(
  stock_date = index(target_stock_all_price),
  stock_mony = target_stock_all_price[,6]) %>%   
  as_tibble()  

colnames(target_stock_df) <- c("stock_date", "stock_mony")
# glimpse(target_stock_df)
# head(target_stock_df)

#===========================================================================
# 2) 배당 동향 
#===========================================================================

# 년 기준 
div_year_tb <- div_tb %>%
  mutate(year = year(div_date)) %>% 
  filter(year < 2021) %>% 
  group_by(year) %>% 
  summarise(sum_div_mony = sum(div_mony)) %>% 
  ungroup()


#===========================================================================
# 3) 주식 동향 
#===========================================================================

target_stock_1year_df <- target_stock_df %>% 
  filter(stock_date > from_day)


target_stock_range_df  <- target_stock_1year_df %>% 
  mutate(rank = min_rank(stock_mony)) %>% 
  filter(rank %in% range(rank))


gap_stock <- target_stock_range_df[2, 2] - target_stock_range_df[1, 2]

# 고점과 저점대비 하락 계산 
target_stock_range2_df <- target_stock_range_df %>% 
  slice(2) %>% 
  mutate(price_low_10 = round(gap_stock*0.9, 4) + target_stock_range_df[1, 2], 
         price_low_20 = round(gap_stock*0.8, 4) + target_stock_range_df[1, 2], 
         price_low_30 = round(gap_stock*0.7, 4) + target_stock_range_df[1, 2], 
         price_low_50 = round(gap_stock*0.5, 4) + target_stock_range_df[1, 2])
# target_stock_range2_df 
  


lo <- loess(stock_mony ~ as.double(stock_date) , data = target_stock_1year_df)
# summary(lo)
lo_result <- predict(lo, as.double(target_stock_1year_df$stock_date))
# length(lo_result)           #253
# nrow(target_stock_1year_df) #253

target_stock2_1year_df <- bind_cols(target_stock_1year_df, predict_price = lo_result) %>% 
  mutate(gap_price = stock_mony - predict_price, gap_tag = gap_price>0)









#===========================================================================
# 4) Plot 그리기 
#===========================================================================
# 배당 지급 기준 
div_tb %>% 
  filter(div_mony < 20) %>% 
  ggplot(aes(x = div_date, y = div_mony)) +
  geom_line() +
  stat_smooth(method = "loess", se = FALSE) +
  labs(x = "date", y = "dividend($)", title = paste0(target_stock, " [Chart Dividend]")) 


div_year_tb %>%
  ggplot(aes(x = year, y = sum_div_mony)) +
  geom_bar(stat = "identity") +
  stat_smooth(method = "loess", se = FALSE) + 
  #stat_smooth(method = "lm", se = FALSE)  
  labs(x = "date", y = "total dividend($) per year", title = paste0(target_stock, " [Chart Dividend by year]")) 

chart_Series(Ad(target_stock_all_price))

ggplot(target_stock_1year_df, aes(x = stock_date,  y = stock_mony)) +
  geom_line() +
  geom_hline(aes(yintercept = unlist(target_stock_range_df[1,2])), color = "red", linetype = 3) +
  annotate(geom = "text", x = as_date(unlist(target_stock_range_df[1,1])), y = unlist(target_stock_range_df[1,2])-0.3, 
           label = unlist(target_stock_range_df[1,2]), color = "red") +
  geom_hline(aes(yintercept = unlist(target_stock_range_df[2,2])),  color = "red", linetype = 3) +
  annotate(geom = "text", x = as_date(unlist(target_stock_range_df[2,1])), y = unlist(target_stock_range_df[2,2])+0.3, 
           label = unlist(target_stock_range_df[2,2]), color = "red") +
  
  geom_hline(aes(yintercept = unlist(target_stock_range2_df[,4])),  color = "blue", linetype = 3) +
  annotate(geom = "text", x = from_day + 30, y = unlist(target_stock_range2_df[,4])+0.4, 
           label = paste0("   (10%) ",unlist(unlist(target_stock_range2_df[,4]))), color = "blue") +
  
  geom_hline(aes(yintercept = unlist(target_stock_range2_df[,5])),  color = "blue", linetype = 3) +
  annotate(geom = "text", x = from_day + 30, y = unlist(target_stock_range2_df[,5])+0.4, 
           label = paste0("   (20%) ",unlist(unlist(target_stock_range2_df[,5]))), color = "blue") +
  
  geom_hline(aes(yintercept = unlist(target_stock_range2_df[,6])),  color = "blue", linetype = 3) +
  annotate(geom = "text", x = from_day + 30, y = unlist(target_stock_range2_df[,6])+0.4, 
           label = paste0("   (30%) ",unlist(unlist(target_stock_range2_df[,6]))), color = "blue") +  
  
  geom_hline(aes(yintercept = unlist(target_stock_range2_df[,7])),  color = "blue", linetype = 3) +
  annotate(geom = "text", x = from_day + 30, y = unlist(target_stock_range2_df[,7])+0.4, 
           label = paste0("   (50%) ",unlist(unlist(target_stock_range2_df[,7]))), color = "blue") +  
  stat_smooth(method = "loess", se = FALSE) +  #span = 0.75
  labs(title = paste0(target_stock, " [for 1 year Chart]")) 


ggplot(target_stock2_1year_df, aes(x = stock_date, gap_price)) +
  geom_bar(stat = "identity", aes(color = gap_tag)) +
  labs(title = paste0(target_stock, " [Stock Bubble Indoex Chart]")) 

