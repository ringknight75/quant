

# 페이지 51 코드 작성 
# 네이버금융 속보 제목 크롤링 하기 
# 주소 : https://finance.naver.com/news/news_list.nhn?mode=LSS2D&section_id=101&section_id2=258


library(rvest)
library(httr)
library(tidyverse)

url_naver <- paste0('https://finance.naver.com/news/news_list.nhn?',
              'mode=LSS2D&section_id=101&section_id2=258')

data <- GET(url_naver) # {httr}
print(data)

data_titles <- data %>% read_html(encoding = 'EUC-KR') %>% 
  html_nodes('dl') %>%                                        # {rvest}
  html_nodes('.articleSubject') %>% 
  html_nodes('a') %>% 
  html_attr('title')  

print(data_titles)  
