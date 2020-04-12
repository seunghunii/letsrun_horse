# chulmainfo 넣을때 중간에 weight_type(핸디캡, 별정A,B,C) 넣을려고 만든 코드.
# 출전정보에는 가장 최근것만 있어서 경주성적에서 뽑아서 넣었음.

pkgs <- c('dplyr','stringr','rvest','RSelenium','pbapply',
          'httr','tidyr','DBI','RMySQL','gtools')
sapply(pkgs,require,character.only = TRUE)

urll = 'http://race.kra.co.kr/raceScore/ScoretableScoreList.do?Act=04&Sub=1&meet=3'

# get race_info(num)
tb_pgsource <-  read_html(urll) %>% html_table() %>% data.frame()

sapply(1:nrow(tb_pgsource),function(k)
  tb_pgsource$경주[k] <<- str_remove_all(tb_pgsource$경주[k],'\n') %>%
    str_remove_all('[^[:digit:]]  ') %>% 
    strsplit('  ') %>% unlist() %>% as.numeric() %>% max())

tb_pgsource$경주일자 <- str_remove_all(tb_pgsource$경주일자,'\n|\t| ')
tb_pgsource <- tb_pgsource %>% separate(경주일자,into = c('day','date'),
                                            sep = '\\(')
tb_pgsource$date <- tb_pgsource$date %>% str_remove_all('\\)')
tb_pgsource$day  <- tb_pgsource$day  %>% str_replace_all('/','-')
colnames(tb_pgsource) <- c('order','day','date','race')

tb_pgsource$race <- as.numeric(tb_pgsource$race)

eCaps <- list(
          chromeOptions = list(
          args = c('--headless', '--disable-gpu')))

remdr <- remoteDriver(port=4444,browser='chrome',
                      extraCapabilities = eCaps)
remdr$open()

con <- dbConnect(MySQL(),user='simon',password='Simon1304!',
                 host='175.119.87.54',dbname='horse',port=9560)
dbGetQuery(con,'set names utf8')

urll <- 'http://race.kra.co.kr/raceScore/ScoretableScoreList.do?Act=04&Sub=1&meet=3'

for(k in 3:7){
  
  remdr$navigate(urll)
  race_num <- tb_pgsource[k,4]
  
  for(i in 1:race_num){
    remdr$navigate(urll)
    xppath <- paste0("#contents > div.tableType2 > table > tbody > tr:nth-child(",k,") > td.alignL > p > a:nth-child(",i,")")
    
    aa <- remdr$findElement('css selector',xppath)
    aa$clickElement()
    
    httml <- remdr$getPageSource()[[1]] %>% read_html() %>% html_node('#contents > div:nth-child(6) > table')
    html_df <- httml %>% html_table(fill=TRUE) %>% data.frame()
    
    run_tt <- html_df[,3]
    
    sqll <- paste0('update chulmainfo_busan set weight_type="',run_tt,'"where date="',tb_pgsource[k,2],'" and run_ord="',i,'"')
    dbGetQuery(con,sqll)
    Sys.sleep(1)
  }
}
