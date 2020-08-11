print('------')
print(paste('code start',as.character(Sys.time())))
sttime <- Sys.time()

pkgs <- c('dplyr','stringr','rvest','RSelenium','lubridate',
          'httr','tidyr','DBI','RMySQL')
sapply(pkgs,require,character.only = TRUE,quietly = TRUE)

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
tmp <- df %>% separate(run_date,c('date','day'),'\\(',extra='drop',fill = 'right')
tmp$day <- tmp$day %>% str_remove_all('[^[:alpha:]]') %>% str_trim()
tmp$date <- str_trim(tmp$date)
tmp$etc <- ifelse(tmp$etc == '','NA',tmp$etc)

eCaps <- list(
  chromeOptions = list(
    args = c('--headless', '--disable-gpu')))

remdr <- remoteDriver(port=4444,browser='chrome',
                      extraCapabilities = eCaps)

remdr$open(silent = TRUE)
remdr$navigate(urll)
toto <- list()
sapply(1:nrow(tmp),function(l){
  remdr$navigate(urll)
  
  css_tb <- paste0('#contents > form > div.tableType2 > table > tbody > tr:nth-child(',l,') > td:nth-child(3) > a')
  ab <- remdr$findElement('css selector',css_tb)
  ab$clickElement()
  
  css_recent <- '#contents > form > ul > li:nth-child(9) > a'
  ab <- remdr$findElement('css selector',css_recent)
  ab$clickElement()
  
  df_html <- remdr$getPageSource()[[1]] %>% read_html()
  a <- df_html %>% html_table()
  
  sapply(3:length(a),function(m){
    tmp_sep <- a[[m]] %>% data.frame()
    
    coll <- colnames(tmp_sep)[1] %>% str_remove_all('\n') %>%
      str_replace_all('[:space:]{2,}|\\.{2,}',' ')
    coll <- data.frame(coll) %>% separate(coll,sep=' ',extra='drop',fill = 'right',
                                          into = paste0(letters[1:10],1))
    
    horse_name <- coll[1,3]
    
    colnames(tmp_sep) <- tmp_sep[1,]
    tmp_sep <- tmp_sep[2:nrow(tmp_sep),-17]
    
    colnames(tmp_sep) <- c('date','horse_num','run_type','grade','distance','moisture',
                           'quickness','rank','jockey','weight','S1F','G3F','G1F','record',
                           'horse_weight','rating')
    
    tmp_sep <- tmp_sep %>% separate(date,sep='-',extra='drop',fill = 'right',
                                    into=c('date','run_ord'))
    tmp_sep$date <- str_replace_all(tmp_sep$date,'/','-')
    tmp_sep$run_ord <- str_remove_all(tmp_sep$run_ord,'R')
    tmp_sep <- tmp_sep %>% separate(rank,sep='/',extra = 'drop',fill = 'right',
                                    into=c('rank','total_run'))
    
    tmp_sep[,c('S1F','G3F','G1F','record')] <- lapply(tmp_sep[,c('S1F','G3F','G1F','record')],
                                                      function(x) str_replace_all(x,'^$','0:0.0'))
    tmp_sep[,c('S1F','G3F','G1F','record')] <- lapply(tmp_sep[,c('S1F','G3F','G1F','record')],
                                                      function(x) lubridate::ms(x) %>% seconds %>% str_remove_all('S'))
    tmp_sep <- tmp_sep %>% separate(horse_weight,sep='\\(',extra = 'drop',fill = 'right',
                                    into = c('horse_weight','weight_diff'))
    tmp_sep$weight_diff <- tmp_sep$weight_diff %>% str_remove_all('\\)')
    
    df_dataa <- cbind(horse_name,tmp_sep)
    df_dataa$horse_name <- as.character(df_dataa$horse_name)
    df_dataa$update_time <- Sys.time() %>% str_remove_all(' KST')
    df_dataa$rating <- df_dataa$rating %>% str_replace_all('^$','0')
    df_dataa$grade <- df_dataa$grade %>% str_replace_all('^$','no grade')
    df_dataa$quickness <- df_dataa$quickness %>% str_replace_all('^$','0')
    df_dataa[,c('S1F','G3F','G1F')] <- sapply(df_dataa[,c('S1F','G3F','G1F')],
                                              function(v)na.replace(v,0))
  
    con <- dbConnect(MySQL(),user='race',password='koreafirst',
                     host='49.50.165.83',dbname='horse',port=7325)
    dbGetQuery(con,'set names utf8')
    
    df_jockey <- dbGetQuery(con,'select * from jockey_info')
    df_dataa <- cbind(left_join(df_dataa[,1:12],df_jockey[,c(1,9:14)],by = 'jockey'),
                      df_dataa[,13:ncol(df_dataa)])
    
    sapply(1:nrow(df_dataa),function(j){
      sqll <- paste(df_dataa[j,],collapse = "','") %>% str_remove_all('\\\\')
      sqll <- paste0("'",sqll,"'")
      sqll_ua <- paste0(colnames(df_dataa))
      sqll_ub <- paste0("'",df_dataa[j,],"'")
      sqll_ud <- paste0(sqll_ua,'=',sqll_ub,collapse = ',')
      sqll <- paste0('insert ignore into record_chulmainfo_seoul values (',sqll,')')
      dbGetQuery(con,sqll)
    })
    dbDisconnect(con)
    Sys.sleep(0.5)
  })
  Sys.sleep(3)
})
remdr$closeall()

timediff <- difftime(Sys.time(),sttime,units = 'mins') %>% as.numeric() %>% round(5)
print(paste('code end',as.character(Sys.time()),',time spent',timediff,'mins'))