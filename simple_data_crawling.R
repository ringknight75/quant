

# 페이지 35 코드 작성 
# 단순 데이터 

# https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?api_key=xw3NU3xLUZ7vZgrz5QnG
# 실제 해당주소를 크롬에서 실행하면 CSV가 다운로드가 된다. 


url <- "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?api_key=xw3NU3xLUZ7vZgrz5QnG"
data_appl <- read.csv(url)

head(data_appl)
tail(data_appl)


