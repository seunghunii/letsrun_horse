pkgs <- c('dplyr','stringr','rvest',
          'httr','tidyr','DBI','RSelenium')
sapply(pkgs,require,character.only = TRUE)

# java -jar selenium-server-standalone-3.141.59.jar 

urll = 'http://race.kra.co.kr/raceScore/ScoretableScoreList.do?Act=04&Sub=1&meet=1'

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

# selenium
eCaps <- list(
  chromeOptions = list(
    args = c('--headless', '--disable-gpu')))

remdr <- remoteDriver(port=4444L,browser='chrome',
                      extraCapabilities = eCaps)
remdr$open()

for(k in 2:8){

  xppath <- paste0('/html/body/div[1]/div[2]/div[1]/div[2]/table/tbody/tr[',k,']/td[3]/p/a[',1:tb_pgsource$race[k],']')
  
  con <- dbConnect(MySQL(),user='race',password='koreafirst',
                   host='49.50.165.83',dbname='horse',port=7325)
  dbGetQuery(con,'set names utf8mb4')
  dbGetQuery(con,'set charset utf8mb4')
  
  for(i in 1:length(xppath)){
    remdr$navigate(urll)
    aa <- remdr$findElement('xpath',xppath[i])
    aa$clickElement()
    
    df_info <- remdr$getPageSource()[[1]] %>% read_html() %>% html_nodes(css='#contents > div:nth-child(6) > table')
    a <- df_info[[1]] %>% html_table(header=FALSE) %>% select(1)
    
    run_info <- data.frame(bb = paste0(a[,1],collapse = ' '))
    run_info <- run_info %>% separate(bb,into = c('y','m','d','day','run_ord','region','grade'),sep = ' ')
    run_info[,c(1,2,3,5)] <- apply(run_info[,c(1,2,3,5)],2,
                                   function(x) str_remove_all(x,'[^[:digit:]]'))
    run_info[,4] <- str_remove_all(run_info[,4],'[:punct:]')
    run_info <- run_info %>% unite(date,y,m,d,sep='-')
    
    df_dataa <- remdr$getPageSource()[[1]] %>% 
                  read_html() %>%
                  html_nodes(css='#contents > div:nth-child(9) > table')
    
    df_dataa <- df_dataa %>%
                  html_table(fill=TRUE) %>%
                  data.frame()
    
    colnames(df_dataa) <- c('rank','horse_num','horse_name','born','sex','age',
                            'weight','rating','jockey','trainer','owner',
                            'arrive_diff', 'horse_weight','single_win','contin_win','kit')
    
    df_dataa <- cbind(run_info[,-4],df_dataa)
    
    df_dataa$age <- str_remove_all(df_dataa$age,'[^[:digit:]]')
    df_dataa <- df_dataa %>% separate(horse_weight,sep='\\(',
                                      into = c('horse_weight','weight_diff'))
    df_dataa$weight_diff <- str_remove_all(df_dataa$weight_diff,'\\)')
    df_dataa$arrive_diff[df_dataa$arrive_diff == ''] <- 'winner'
    df_dataa$kit[df_dataa$kit == ''] <- 'NA'
    df_dataa$rating <- na.replace(df_dataa$rating,'0')
    df_dataa$gain_weight <- str_extract_all(df_dataa$weight,'\\*')
    df_dataa$gain_weight <- ifelse(df_dataa$gain_weight == '*',1,0)
    df_dataa$weight <- str_remove_all(df_dataa$weight,'\\*')
    df_dataa$rank <- na.replace(df_dataa$rank,999)
    df_dataa[,c('single_win','contin_win')] <- apply(df_dataa[,c('single_win','contin_win')],2,
                                                     function(x) str_replace_all(x,'----','0'))
    df_dataa$jockey <- df_dataa$jockey %>% str_remove_all('\\(+[[:digit:]|-[:digit:]]+\\)')
    df_dataa$owner <- df_dataa$owner %>% str_remove_all('♠')
    
    df_record <- remdr$getPageSource()[[1]] %>% 
      read_html() %>%
      html_nodes(css='#contents > div:nth-child(10) > table')
    
    df_record <- df_record %>%
      html_table(fill=TRUE) %>%
      data.frame()
    
    df_record <- df_record[2:nrow(df_record),] # busan에는 필요없는 처리
    # 서울은 경기기록에서 시간 나오는 테이블 열이 두개임. 부산은 하나.
    record <- df_record$경주기록
    
    df_dataa <- cbind(df_dataa[,c(1:11,22,12:15)],record,df_dataa[,16:21])
    df_dataa$record <- as.character(df_dataa$record)
    df_dataa$record <- ifelse(df_dataa$record=='','0:0',df_dataa$record)
    df_dataa$record <- ms(df_dataa$record) %>% seconds %>% str_remove_all('S')
    
    options(digits=5)
    df_jockey <- dbGetQuery(con,'select * from jockey_info')
    df_dataa <- left_join(df_dataa,df_jockey[,c(1,9:14)],by = 'jockey')
    
    df_trainer <- dbGetQuery(con,'select * from trainer_info')
    df_dataa <- left_join(df_dataa,df_trainer[,c(1,8:13)],by = 'trainer')
    
    for(j in 1:nrow(df_dataa)){
      sqll <- paste(df_dataa[j,],collapse = "','") %>% str_remove_all('\\\\')
      sqll <- paste0("'",sqll,"'")

      sqll_ua <- paste0(colnames(df_dataa))
      sqll_ub <- paste0("'",df_dataa[j,],"'")
      sqll_ud <- paste0(sqll_ua,'=',sqll_ub,collapse = ',')
      sqll <- paste0('insert ignore into race_result_seoul values (',sqll,')')
      dbGetQuery(con,sqll)
    }
  }
  dbDisconnect(con)
  Sys.sleep(3)
}
remdr$closeall()