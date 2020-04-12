pkgs <- c('dplyr','stringr','rvest','RSelenium','pbapply',
          'httr','tidyr','DBI','RMySQL','gtools')
sapply(pkgs,require,character.only = TRUE)
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
remdr$open()
#for(k in 1:length(xppath)){
pbsapply(1:length(xppath),function(k){
  remdr$navigate(urll)
  
  ab <- remdr$findElement(using = 'xpath',xppath[k])
  ab$clickElement()
  
  doc_name <- remdr$getPageSource()[[1]] %>% 
    read_html() %>% html_table(fill=TRUE) %>% data.frame()
  
  doc_name <- doc_name[2:nrow(doc_name),'마명']
  
 # for(l in 1:length(doc_name)){
  sapply(1:length(doc_name),function(l){
    
    remdr$navigate(urll)
    
    ab <- remdr$findElement(using = 'xpath',xppath[k])
    ab$clickElement()
    
    con <- dbConnect(MySQL(),user='simon',password='Simon1304!',
                     host='175.119.87.54',dbname='horse',port=9560)
    dbGetQuery(con,'set names utf8')
    
    xppath2 <- paste0(
      '/html/body/div[1]/div[2]/form/div/div[2]/table/tbody/tr[',l,']/td[2]/a')
    ab <- remdr$findElement(using = 'xpath',xppath2)
    ab$clickElement()
    
    # 경주성적 클릭
    js_record <- '/html/body/div[1]/div[2]/form/div/ul/li[3]/a'
    ab <- remdr$findElement(using = 'xpath',js_record)
    ab$clickElement()
    
    # 경주마 프로필의 
    js_tb <- '/html/body/div[1]/div[2]/form[1]/div/div[2]/table'
    
    doc_html <- remdr$getPageSource()[[1]] %>% 
      read_html()
    
    df_record <- doc_html %>% 
      html_nodes(css = '#contents > div.tableType2 > table') %>%
      html_table() %>% data.frame()
    
    df_record <- df_record[,-1]
   
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
    df_record$region <- na.replace(df_record$region,'서울')
    df_record$region <- str_replace_all(df_record$region,'\\[부','부산')
    df_record$region <- str_replace_all(df_record$region,'\\[해외','해외')
    df_record$horse_weight <- str_replace_all(df_record$horse_weight,
                                              '^$','0')
    df_record$weight_diff <- na.replace(df_record$weight_diff,99)
    df_record$runway_status <- str_replace_all(df_record$runway_status,
                                               '^$','no_status')
    
    #for(j in 1:nrow(df_record)){
    sapply(1:nrow(df_record),function(j){
      
      sqll <- paste(df_record[j,],collapse = "','") %>% str_remove_all('\\\\')
      sqll <- paste0("'",sqll,"'")
       
      sqll_ua <- paste0(colnames(df_record))
      sqll_ub <- paste0("'",df_record[j,],"'")
      sqll_ud <- paste0(sqll_ua,'=',sqll_ub,collapse = ',')
       
      sqll <- paste0('insert into horseinfo_result_seoul values (',sqll,
                      ') on duplicate key update ',sqll_ud)
      dbGetQuery(con,sqll)
      })
    dbDisconnect(con)
    Sys.sleep(0.5)
  })
  Sys.sleep(3)
})
remdr$closeall()
