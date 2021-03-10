

# 페이지 58 코드 작성 
# 네이버금융 코스피 리스트 출력하기 
# 주소 : https://finance.naver.com/sise/sise_market_sum.nhn?sosok=0&page=1


library(httr)
library(rvest)

i <-  0       #코스피 : 0 
ticker <- list()
url <- paste0('https://finance.naver.com/sise/',
              'sise_market_sum.nhn?sosok=',i,'&page=1')
down_table <- GET(url)


#=====================================================================
# 1) 맨 마지막 페이지 잦기 
#=====================================================================
navi.final <- read_html(down_table, encoding = 'EUC-KR') %>% 
  html_nodes(., '.pgRR') %>% 
  html_nodes(., 'a') %>% 
  html_attr(., 'href')


print(navi.final) #"/sise/sise_market_sum.nhn?sosok=0&page=32"


navi.final <- navi.final %>% 
  strsplit(., '=') %>% 
  unlist() %>% 
  tail(., 1) %>% 
  as.numeric()


navi.final # 32




#=====================================================================
# 2) 테이터 가져오기 
#=====================================================================

i <-0   #코스피 : 0 
j <- 1  # 페이지 1
url <- paste0('https://finance.naver.com/sise/',
              'sise_market_sum.nhn?sosok=',i,'&page=',j)
down_table <- GET(url)

tables <- read_html(down_table, encoding = 'EUC-KR') %>% 
  html_table(fill = TRUE)
print(tables)

table <- tables[[2]]
print(head(table))


# 마직막 열인 토론실은 필요 없는 열이며, 첫번째 행과 같은 아무런 정보가 없는 행도 있습니다. 
table[, ncol(table)] <- NULL
table <- na.omit(table)
print(head(table))


# 티커 정보 가져오기 
symbol <- read_html(down_table, encoding = 'EUC-KR') %>% 
  html_nodes(., 'tbody') %>% 
  html_nodes(., 'td') %>% 
  html_nodes(., 'a') %>%   
  html_attr(., 'href')
print(head(symbol))

symbol <- sapply(symbol, function(x){
  substr(x, nchar(x) - 5, nchar(x))
})
print(head(symbol))

symbol <- unique(symbol)
print(head(symbol))

table$N <- symbol
colnames(table)[1] <- "종목코드"

rownames(table) <- NULL


