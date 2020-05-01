pkgs <- c('dplyr','stringr','rvest','RSelenium',
          'httr','tidyr','DBI','RMySQL','rlist')
sapply(pkgs,require,character.only = TRUE)

# java -jar selenium-server-standalone-3.141.59.jar
# lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
# data crawl
urll <- 'http://race.kra.co.kr/chulmainfo/ChulmaDetailInfoList.do?Act=02&Sub=1&meet=1'
a <- POST(urll)

b <- read_html(a) %>% html_table()

# data wrangling
df <- b[[1]][,2:10]
colnames(df) <- c('run_date','run_ord','grade','distance',
                  'num_expect','num_run','run_name','start_time','etc')

df[,c(4,5,6)] <- apply(df[,c(4,5,6)],2,function(x)str_remove_all(x,"[^0-9.-]"))

df$run_date <- str_remove_all(df$run_date,'\\n|\\t') %>% str_replace_all('/','-')
tmp <- df %>% separate(run_date,c('date','day'),'\\(')
tmp$day <- tmp$day %>% str_remove_all('[^[:alpha:]]') %>% str_trim()
tmp$date <- str_trim(tmp$date)
tmp$etc <- ifelse(tmp$etc == '','NA',tmp$etc)

eCaps <- list(
  chromeOptions = list(
    args = c('--headless', '--disable-gpu')))

remdr <- remoteDriver(port=4444,browser='chrome',
                      extraCapabilities = eCaps)
remdr$open()

result_bb <- list()

con <- dbConnect(MySQL(),user='simon',password='Simon1304!',
                 host='175.119.87.54',dbname='horse',port=9560)
dbGetQuery(con,'set names utf8')

for(l in 1:nrow(tmp)){
  remdr$navigate(urll)
  
  css_tb <- paste0('#contents > form > div.tableType2 > table > tbody > tr:nth-child(',l,') > td:nth-child(3) > a')
  ab <- remdr$findElement('css selector',css_tb)
  ab$clickElement()
  
  df_info_html <- remdr$getPageSource()[[1]] %>% read_html()
  a <- df_info_html %>% html_table()
  
  df_info <- a[[1]] %>% data.frame()
  
  # 경기종류, 거리, 별정 정보
  df_info_1 <- df_info %>% select(1)
  colnames(df_info_1) = 'a'
  df_info_1 <- df_info_1 %>% separate(a,sep="[:space:]{1,}",into=c('run_name','distance','weight_type'))
  
  # 날짜 경주번호 등.
  df_info_2 <- colnames(df_info)[1] %>% str_replace_all('\\.{2,}','\\.') %>%
    str_remove_all('X') %>% data.frame()
  colnames(df_info_2) <- 'ab'
  df_info_2 <- df_info_2 %>% separate(col = 'ab',sep = '\\.',fill='right',
                         into = c('date','day','run_ord','region','year_run')) %>%
                         select(1:4)
  df_info_final <- cbind(df_info_2,df_info_1)
  
  df_info_final$date <- df_info_final$date %>% str_replace_all('년|월','-') %>%
                                               str_remove_all('일')
  df_info_final[,c('run_ord','distance')] <- lapply(df_info_final[,c('run_ord','distance')],function(x)
                                                    str_remove_all(x,'[^[:digit:]]'))
  df_info_final$region <- df_info_final$region %>% str_replace_all('서울','seoul')
  
  # 출전정보 데이터 전처리
  tmpp <- a[[3]]
  tmpp <- tmpp[,2:ncol(tmpp)]
  names(tmpp) <- c('horse_name','born','sex','age','rating','weight','weight_diff',
                   'jockey','trainer','owner','train_count','run_term','kit','etc')
  tmpp$rating <- na.replace(tmpp$rating,'999(999)')
  tmpp <- tmpp %>% separate(rating,sep = '\\(',into = c('rating','rating_grade'),fill='right')
  tmpp$rating_grade <- tmpp$rating_grade %>% str_remove_all('\\)') %>% na.replace('999')
  tmpp <- cbind(tmpp[,1:7],gain_weight = str_detect(tmpp$weight,'\\*') %>% as.numeric(),tmpp[,8:ncol(tmpp)])
  tmpp$weight <- str_remove_all(tmpp$weight,'\\*')
  tmpp[,c('jockey','trainer','owner')] <- lapply(tmpp[,c('jockey','trainer','owner')],function(x)
    str_remove_all(x,'\\(+[[:digit:]|-[:digit:]]+\\)|♠'))
  tmpp$run_term <- str_replace_all(tmpp$run_term,'^$','0주') %>% str_remove_all('주')
  tmpp[,c('kit','etc')] <- lapply(tmpp[,c('kit','etc')],function(x)
    str_replace_all(x,'^$','unknown') %>% na.replace('unknown'))
  
  result_df <- cbind(df_info_final,tmpp)
  
  for(j in 1:nrow(result_df)){
    sqll <- paste(result_df[j,],collapse = "','") %>% str_remove_all('\\\\')
    sqll <- paste0("'",sqll,"'")
    sqll_ua <- paste0(colnames(result_df))
    sqll_ub <- paste0("'",result_df[j,],"'")
    sqll_ud <- paste0(sqll_ua,'=',sqll_ub,collapse = ',')
    sqll <- paste0('insert ignore into chulmainfo_detail_seoul values (',sqll,')')
    dbGetQuery(con,sqll)
  }
  
}
remdr$closeall()
dbDisconnect(con)
