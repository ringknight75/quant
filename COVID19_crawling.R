
#===================================================================================================================
# 1) 작성일자 : 2021년 2월 20일
# 2) 작 성 자 : ben
# 3) 개발목적 : 코로나 바이러스 관련 데이터 시각화 
# 4) 수정사항 
# 5) 참조주소 
#    - 데이터 설명 주소 : https://github.com/owid/covid-19-data/tree/master/public/data
#    - 컬럼설명 주소 : https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-codebook.csv
#    - 데이터 다운로드 주소 : https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv
#===================================================================================================================
rm(list=ls())
library(tidyverse)
library(lubridate)


#===================================================================================================================
# 1) 데이터 수집 
#===================================================================================================================
url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'

data_raw_covid <- read.csv(url)
data_mal_covid <- data_raw_covid
str(data_mal_covid)
head(data_mal_covid)


data_mal_covid %>% select(location) %>% 
  distinct() %>% 
  filter(location=='South Korea')

data_mal_covid %>% 
  filter(location=='South Korea')



# date가 factor로 인식되어서 date형태로 변경이 필요함 
data_mal_covid$date <- as_date(data_mal_covid$date)
str(data_mal_covid)

data_mal_covid %>%
  select(continent, date) %>% 
  group_by(continent) %>% 
  summarise(n = n(), start_date = min(date), end_date = max(date))



#===================================================================================================================
# 2) 데이터 시각화 
#===================================================================================================================




