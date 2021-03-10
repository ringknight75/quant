

# 페이지 514코드 작성 
# 한국 거래소 기업공시채널 
# 주소 : https://kind.krx.co.kr/disclosure/todaydisclosure.do?method=searchTodayDisclosureMain


library(httr)
library(rvest)

Sys.setlocale("LC_ALL", "English")

url <- "https://kind.krx.co.kr/disclosure/todaydisclosure.do"

data <- POST(url, body = 
            list(
               method = 'searchTodayDisclosureSub',
               currentPageSize = '15',
               pageIndex = '1',
               orderMode = '0',
               orderStat = 'D',
               forward = 'todaydisclosure_sub',
               chose = 'S',
               todayFlag ='Y',
               selDate = '2021-02-02')
)

data <- read_html(data) %>% 
  html_table(fill = TRUE) %>% .[[1]]

Sys.setlocale("LC_ALL", "Korean")

print(head(data))

