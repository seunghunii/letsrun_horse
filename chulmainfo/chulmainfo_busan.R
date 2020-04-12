pkgs <- c('dplyr','stringr','rvest',
          'httr','tidyr','DBI','RMySQL')
sapply(pkgs,require,character.only = TRUE)

# data crawl
urll <- 'http://race.kra.co.kr/chulmainfo/ChulmaDetailInfoList.do?Act=02&Sub=1&meet=3'
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

weight_type_list <- list()
for(l in 1:nrow(tmp)){
  
  remdr$navigate(urll)
  
  css_tb <- paste0('#contents > form > div.tableType2 > table > tbody > tr:nth-child(',l,') > td:nth-child(3) > a')
  ab <- remdr$findElement('css selector',css_tb)
  ab$clickElement()
  
  df_info_html <- remdr$getPageSource()[[1]] %>% read_html()
  a <- df_info_html %>% html_table()
  
  df_info <- a[[1]] %>% data.frame() %>% select(1)
  names(df_info) = 'a'
  df_info <- df_info %>% separate(a,sep="[:space:]{1,}",into=c('a1','b1','c1'))
  weight_type_a <- df_info[,3]
  weight_type_list[l] <- weight_type_a
}
remdr$closeall()

tmp <- cbind(tmp[,1:4],
             weight_type=unlist(weight_type_list),tmp[,5:ncol(tmp)])
tmp$weight_type <- as.character(tmp$weight_type)

# automatic function
chulmainfo_insert <- function(x){
  # connect db, set utf8
  con <- dbConnect(MySQL(),user='simon',password='Simon1304!',
                   host='175.119.87.54',dbname='horse',port=9560)
  dbGetQuery(con,'set names utf8')
  for(k in 1:nrow(x)){
    sqlll <- paste(x[k,],collapse="','") %>% str_remove_all('\\\\')
    dataa <- paste0(c("'",sqlll,"'"),collapse='')
    
    sqll_ua <- paste0(colnames(tmp))
    sqll_ub <- paste0("'",tmp[k,],"'")
    sqll_ud <- paste0(sqll_ua,'=',sqll_ub,collapse = ',')
    
    sqll  <- paste0('insert into chulmainfo_busan values(',dataa,
                    ') on duplicate key update ',sqll_ud) %>% str_remove_all('\\\\')
    
    dbSendQuery(con,sqll)
  }
  dbDisconnect(con)
  Sys.sleep(0.5)
}
chulmainfo_insert(tmp)

