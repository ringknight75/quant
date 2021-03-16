#===========================================================================
# 적절한 달러 매매 시기 확보 
# 
# 
#===========================================================================
library(quantmod)
library(tidyverse)

(to_day   <- Sys.Date() - 1)
(from_day <- Sys.Date() - 367)

#===========================================================================
# 1) 원달러 환율 
#===========================================================================
won_dolloar_xts <- getSymbols("KRW=X", src = "yahoo", auto.assign = FALSE)
#won_dolloar_xts <- getSymbols("DEXKOUS", src = "FRED", auto.assign = FALSE)
head(won_dolloar_xts)
tail(won_dolloar_xts)



#===========================================================================
# 2) 달러 지수 
#===========================================================================
dallor_idx_xts <- getSymbols("DX-Y.NYB",  src = "yahoo", auto.assign = FALSE)
tail(dallor_idx_xts)
head(dallor_idx_xts)


#===========================================================================
# 3) xts -> dataframe 변경 
#===========================================================================
#한꺼번에 Adjusted만 남겨놓고 dataframe형식에 date를 넣어보자 

won_dolloar_dt <- data.frame(date = index(won_dolloar_xts), 
                             adjusted = won_dolloar_xts$`KRW=X.Adjusted`, 
                             row.names = NULL) %>% 
  as_tibble() %>% 
  complete(date, fill = list(KRW.X.Adjusted = NA))
  
tail(won_dolloar_dt, 20)

dallor_idx_dt <- data.frame(date = index(dallor_idx_xts), 
                            adjusted = dallor_idx_xts$`DX-Y.NYB.Adjusted`, 
                            row.names = NULL) %>%
  as_tibble() %>% 
  complete(date, fill = list(DX.Y.NYB.Adjusted = NA))

tail(dallor_idx_dt, 20)


#===========================================================================
# 4) dataframe JOIN (full join)
#===========================================================================

join_full_dt <-  full_join(dallor_idx_dt, won_dolloar_dt, by = "date")
head(join_full_dt)
tail(join_full_dt)






