pkgs <- c('dplyr','stringr','rvest','RSelenium',
          'httr','tidyr','DBI','RMySQL','gtools')
sapply(pkgs,require,character.only = TRUE)
# java -jar selenium-server-standalone-3.141.59.jar
# data crawl

urll = 'http://race.kra.co.kr/racehorse/ProfileHorsenameKinds.do?Act=08&Sub=1&meet=2'

xppath <- c(
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[2]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[3]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[4]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[5]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[7]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[8]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[9]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[10]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[11]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[12]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[14]/a',
  '/html/body/div[1]/div[2]/div/table/tbody/tr/td[16]/a'
  )

eCaps <- list(
  chromeOptions = list(
    args = c('--headless', '--disable-gpu')))

remdr <- remoteDriver(port=4444,browser='chrome',
                      extraCapabilities = eCaps)
remdr$open()

sapply(1:length(xppath),function(k){
  remdr$navigate(urll)

  ab <- remdr$findElement(using = 'xpath',xppath[k])
  ab$clickElement()

  doc <- remdr$getPageSource()[[1]] %>% 
    read_html() %>% html_table(fill=TRUE) %>% data.frame()

  doc <- doc[2:nrow(doc),c(2:14)]
  names(doc) <- c('horse_name','rating1','rating2','rating3','rating4','groups','grade','born','sex','age','ddd','recent','etc')
  doc$ddd <- ifelse(doc$ddd == '0(0/0)','0(0/0/0)',doc$ddd)
  doc <- doc %>% separate(ddd,sep = '\\(',
                          into = c('recent_total','aaa'))
  doc$aaa <- doc$aaa %>% str_remove_all('\\)')
  doc <- doc %>% separate(aaa,'[/]',
                          into = c('1st','2nd','3rd'))
  doc$recent <- na.replace(doc$recent,'9999/12/31-9999R')
  doc$recent <- ifelse(doc$recent == '','9999/12/31-9999R',doc$recent)
  doc <- doc %>% separate(recent,'-',
                          into = c('recent_run','recent_round'))
  doc$recent_run <- str_replace_all(doc$recent_run,'[/]','-')
  doc$recent_round <- str_remove_all(doc$recent_round,'[^[:digit:]]')
  doc$recent_round <- na.replace(doc$recent_round,'9999')
  doc$etc <- ifelse(doc$etc == '','empty',doc$etc)
  doc$etc <- na.replace(doc$etc,'empty')
  doc$groups <- str_remove_all(doc$groups,'[^[:digit:]]')
  doc$region <- 'jeju'
  
  # fill na values
  doc[,c(2,3,4,5,12,13,14)] <- apply(doc[,c(2,3,4,5,12,13,14)],2,
                            function(x) na.replace(x,0))
  
  con <- dbConnect(MySQL(),user='race',password='koreafirst',
                   host='49.50.165.83',dbname='horse',port=7325)
  
  sapply(1:nrow(doc),function(i){
    sqll <- paste(collapse(doc[i,]),collapse = "','")
    sqll <- paste("'",sqll,"'") %>% str_remove_all(' ')
    
    sqll_ua <- paste(colnames(doc[1,2:ncol(doc)]))
    sqll_ub <- paste("'",doc[i,2:ncol(doc)],"'")
    sqll_ub <- str_remove_all(sqll_ub,' ')
    sqll_ud <- paste(sqll_ua,'=',sqll_ub,collapse = ',')
    
    qq <- paste(colnames(doc),collapse = ",")
    
    sqll_final <- paste0('insert into horseinfo(',qq,') values (',sqll,")",
                         ' on duplicate key update ',sqll_ud)
    dbGetQuery(con,'set names utf8')
    dbGetQuery(con,sqll_final)
  })
  dbDisconnect(con)
  Sys.sleep(3)
})
remdr$closeall()