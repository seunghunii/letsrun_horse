print('------')
print(paste('code start',as.character(Sys.time())))
sttime <- Sys.time()

pkgs <- c('dplyr','stringr','rvest','RSelenium','pbapply',
          'httr','tidyr','DBI','RMySQL','gtools','lubridate')
sapply(pkgs,require,character.only = TRUE,quietly = TRUE)
# java -jar selenium-server-standalone-3.141.59.jar
# lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
# data crawl
urll = 'http://race.kra.co.kr/racehorse/ProfileHorsenameKinds.do?Act=08&Sub=1&meet=1'

xppath <- c(
  '/html/body/div[1]/div[2]/div/div[1]/table/tbody/tr[1]/td[4]/a',
  '/html/body/div[1]/div[2]/div/div[1]/table/tbody/tr[2]/td[4]/a',
  '/html/body/div[1]/div[2]/div/div[1]/table/tbody/tr[3]/td[4]/a',
  '/html/body/div[1]/div[2]/div/div[1]/table/tbody/tr[4]/td[4]/a',
  '/html/body/div[1]/div[2]/div/div[1]/table/tbody/tr[5]/td[4]/a',
  '/html/body/div[1]/div[2]/div/div[1]/table/tbody/tr[6]/td[4]/a',
  '/html/body/div[1]/div[2]/div/div[1]/table/tbody/tr[7]/td[4]/a')

eCaps <- list(
  chromeOptions = list(
    args = c('--headless', '--disable-gpu')))

remdr <- remoteDriver(port=4444,browser='chrome',
                      extraCapabilities = eCaps)
remdr$open(silent = TRUE)

for(k in 1:length(xppath)){
  #sapply(1:length(xppath),function(k){
  remdr$navigate(urll)
  
  ab <- remdr$findElement(using = 'xpath',xppath[k])
  ab$clickElement()
  
  doc_name <- remdr$getPageSource()[[1]] %>% 
    read_html() %>% html_table(fill=TRUE) %>% data.frame()
  
  doc_name <- doc_name[2:nrow(doc_name),'마명']
  
  for(l in 1:length(doc_name)){
    
    remdr$navigate(urll)
    
    ab <- remdr$findElement(using = 'xpath',xppath[k])
    ab$clickElement()
    
    con <- dbConnect(MySQL(),user='race',password='koreafirst',
                     host='49.50.165.83',dbname='horse',port=7325)
    dbGetQuery(con,'set names utf8')
    
    Sys.sleep(0.5)
    
    xppath2 <- paste0(
      '/html/body/div[1]/div[2]/form/div/div[2]/table/tbody/tr[',l,']/td[2]/a')
    ab <- remdr$findElement(using = 'xpath',xppath2)
    ab$clickElement()
    
    Sys.sleep(0.5)
    
    # 경주성적 클릭
    js_record <- '/html/body/div[1]/div[2]/form/div/ul/li[3]/a'
    ab <- remdr$findElement(using = 'xpath',js_record)
    ab$clickElement()
    
    Sys.sleep(0.5)
    
    # 경주마 프로필의 
    js_tb <- '/html/body/div[1]/div[2]/form[1]/div/div[2]/table'
    
    doc_html <- remdr$getPageSource()[[1]] %>% 
      read_html()
    
    df_record <- doc_html %>% 
      html_nodes(css = '#contents > div.tableType2 > table') %>%
      html_table() %>% data.frame()
    
    df_record <- df_record[,-1]
    
    if(df_record[1,1] == '자료가 없습니다.'){
      dbDisconnect(con)
      next
    } else {
      
      colnames(df_record) <- c('date','horse_num','run_type','distance','grade',
                               'rank','jockey','weight','rating','record',
                               'horse_weight','runway_status')
      
      df_record <- df_record %>% separate(date,sep='-',
                                          into=c('date','run_ord'))
      df_record$date <- str_replace_all(df_record$date,'/','-')
      df_record$run_ord <- str_remove_all(df_record$run_ord,'R')
      df_record$distance <- na.replace(df_record$distance,0)
      df_record$grade <- str_replace_all(df_record$grade,'^$','unknown')
      df_record <- df_record %>% separate(rank,sep = '/',
                                          into = c('rank','total_run'))
      
      df_record$horse_num <- na.replace(df_record$horse_num,0)
      df_record$jockey <- na.replace(df_record$jockey,'unknown')
      df_record$jockey <- df_record$jockey %>% str_replace_all('^$','unknown')
      df_record$weight <- na.replace(df_record$weight,0)
      df_record$horse_weight <- df_record$horse_weight %>% str_replace_all('^$','0(0)')
      df_record <- df_record %>% separate(horse_weight,sep = '\\(',
                                          into = c('horse_weight','weight_diff'))
      df_record$weight_diff <- str_remove_all(df_record$weight_diff,'\\)')
      df_record <- cbind(horse_name = doc_name[l],df_record)
      df_record$horse_name <- as.character(df_record$horse_name)
      df_record$rating <- na.replace(df_record$rating,0)
      df_record$rank <- str_replace_all(df_record$rank,'^$','999')
      df_record$rank <- sapply(df_record$rank,as.integer)
      df_record <- df_record %>% separate(date,sep='\\]',
                                          into = c('region','date'),
                                          fill = 'left')
      df_record$region <- na.replace(df_record$region,'seoul')
      df_record$region <- str_replace_all(df_record$region,'\\[부','busan')
      df_record$region <- str_replace_all(df_record$region,'\\[해외','foreign')
      df_record$horse_weight <- str_replace_all(df_record$horse_weight,
                                                '^$','0')
      df_record$weight_diff <- na.replace(df_record$weight_diff,99)
      df_record$grade <- na.replace(df_record$grade,'unknown')
      df_record[,c('jockey','runway_status')] <- lapply(df_record[,c('jockey','runway_status')],
                                                        function(x) str_remove_all(x,"\\(+[:graph:]+\\)"))
      df_record$runway_status <- df_record$runway_status %>% str_replace_all('^$','unknown')
      
      record_unknown <- c('','실격','주행중지','출전제외','출전취소','^$')
      df_record$record <- ifelse(df_record$record %in% record_unknown,
                                 '0:0',df_record$record)
      df_record$record <- lubridate::ms(df_record$record) %>% seconds %>% str_remove_all('S')
      
      for(j in 1:nrow(df_record)){
        #sapply(1:nrow(df_record),function(j){
        
        sqll <- paste(df_record[j,],collapse = "','") %>% str_remove_all('\\\\')
        sqll <- paste0("'",sqll,"'")
        
        sqll_ua <- paste0(colnames(df_record))
        sqll_ub <- paste0("'",df_record[j,],"'")
        sqll_ud <- paste0(sqll_ua,'=',sqll_ub,collapse = ',')
        
        sqll <- paste0('insert into horseinfo_record_seoul values (',sqll,
                       ') on duplicate key update ',sqll_ud)
        dbGetQuery(con,sqll)
      }#)
    } 
    dbDisconnect(con)
    Sys.sleep(0.5)
  }
  Sys.sleep(2)
}#)
remdr$closeall()

timediff <- difftime(Sys.time(),sttime,units = 'mins') %>% as.numeric() %>% round(5)
print(paste('code end',as.character(Sys.time()),',time spent',timediff,'mins'))
