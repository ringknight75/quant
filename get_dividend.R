#===========================================================================
# 작성목적 : 적절한 달러 매매 시기 확보 
# 작 성 자 : 반지기사 
# 작성일시 : 2021년 4월 28일 ~ 
# 특이사항 : getSymbols를 활용하여 데이터 Crawling 
# 단    점 : 당일 데이터를 가져올수 없음 
#===========================================================================
library(quantmod)
library(tidyverse)
library(lubridate)
library(stats)

(to_day   <- Sys.Date() - 1)
(from_day <- Sys.Date() - 367)

#===========================================================================
# 1) 원달러 환율 
#===========================================================================
won_dolloar_xts <- getSymbols("KRW=X",src = "yahoo", auto.assign = FALSE)
class(won_dolloar_xts)
typeof(won_dolloar_xts)

tail(won_dolloar_xts)
nrow(won_dolloar_xts)
won_dolloar_xts[nrow(won_dolloar_xts), 1]   #가장 최근 원달러 환율 구하기 


#===========================================================================
# 2) 배당 데이터 가져오기 후 그래프 그리기 
#===========================================================================


disp_div_history <- function(symbol){
  div_xts <- getDividends(symbol, from = "1970-01-01", to = Sys.Date(),  src = "yahoo",  auto.assign = FALSE)
  div_tb  <- data.frame(
    div_date = index(div_xts),
    div_mony = div_xts[,1]) %>%   
    as_tibble()  

  colnames(div_tb) <- c("div_date", "div_mony")
  div_tb %>% filter(div_mony < 20) %>% 
    ggplot(aes(x = div_date, y = div_mony)) +
      geom_line() +
      stat_smooth(method = "loess", se = FALSE)    
}


disp_div_year_history <- function(symbol){
  div_xts <- getDividends(symbol, from = "1970-01-01", to = Sys.Date(),  src = "yahoo",  auto.assign = FALSE)
  div_tb  <- data.frame(
    div_date = index(div_xts),
    div_mony = div_xts[,1]) %>%   
    as_tibble()  
  
  colnames(div_tb) <- c("div_date", "div_mony")
  div_year_tb <- div_tb %>%
    mutate(year = year(div_date)) %>% 
    filter(year < 2021) %>% 
    group_by(year) %>% 
    summarise(sum_div_mony = sum(div_mony)) %>% 
    ungroup()
  
  lo <- loess(sum_div_mony ~ year, data = div_year_tb)
  # summary(lo)
  print(predict(lo, seq(from = 2021, to = 2040)))
  
  div_year_tb %>%
    ggplot(aes(x = year, y = sum_div_mony)) +
    geom_bar(stat = "identity") +
    # stat_smooth(method = "loess", se = FALSE)  
    stat_smooth(method = "lm", se = FALSE)  
}





disp_div_history("O")
disp_div_year_history("O")

disp_div_history("SPHD")
disp_div_year_history("SPHD")

disp_div_history("SDIV")
disp_div_year_history("SDIV")

disp_div_history("T")
disp_div_year_history("T")

disp_div_history("MO")
disp_div_year_history("MO")

disp_div_history("QYLD")
disp_div_year_history("QYLD")

disp_div_history("XOM")
disp_div_year_history("XOM")

disp_div_history("ABBV")
disp_div_year_history("ABBV")

disp_div_history("JNJ")
disp_div_year_history("JNJ")








div_o_xts <- getDividends("T", from = "1970-01-01", to = Sys.Date(),  src = "yahoo",  auto.assign = FALSE)

div_o_tb <- data.frame(
    div_date = index(div_o_xts),
    div_mony = div_o_xts[,1]) %>%   
  as_tibble() 

tail(div_o_tb)
colnames(div_o_tb) <- c("div_date", "div_mony")

div_o_tb <- div_o_tb %>% mutate(year = year(div_date)) %>% 
  filter(year < 2021) %>% 
  group_by(year) %>% 
  summarise(sum_div_mony = sum(div_mony)) 

glimpse(div_o_tb)

?loess
lo <- loess(sum_div_mony ~ year, div_o_tb)
summary(lo)

next_year <- data.frame(year = seq(from = 2021, to = 2040, by = 1))
next_year <- seq(from = 1984, to = 2040, by = 1)
predict(lo, next_year)





