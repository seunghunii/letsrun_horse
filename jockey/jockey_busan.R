pkgs <- c('dplyr','stringr','rvest','RSelenium','pbapply',
          'httr','tidyr','DBI','RMySQL','gtools')
sapply(pkgs,require,character.only = TRUE)

# java -jar selenium-server-standalone-3.141.59.jar
# lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

eCaps <- list(
  chromeOptions = list(
    args = c('--headless', '--disable-gpu')))

remdr <- remoteDriver(port=4444,browser='chrome',
                      extraCapabilities = eCaps)
remdr$open()

###jockey information###
url_jockey <- 'http://race.kra.co.kr/jockey/ProfileJockeyListActive.do?Act=09&Sub=1&meet=3'
a <- url_jockey %>% read_html() %>% html_table()
jockey_list <- a[[1]][,2:7]

names(jockey_list) <- c('jockey','gg','debut_date','year_record','total_record','available_weight')
jockey_list <- separate(jockey_list,sep = '\\(',fill = 'right',
                        col = 'gg',into = c('groups','trainer'))
jockey_list$trainer <- jockey_list$trainer %>% str_remove_all('\\)')
jockey_list$jockey <- jockey_list$jockey %>% str_remove_all('\\(+[[:digit:]|-[:digit:]]+\\)')
jockey_list$trainer <- jockey_list$trainer %>% na.replace('unknown')
jockey_list$debut_date <- jockey_list$debut_date %>% str_replace_all('\\/','-')
jockey_list[,c(paste0('jockey_total_win',1:3),paste0('jockey_year_win',1:3))] <- 0

for(j in 1:nrow(jockey_list)){
  remdr$navigate(url_jockey)
  xppath <- paste0('#contents > div.tableType2.normal_title > table > tbody > tr:nth-child(',j,') > td:nth-child(2) > div > a')
  aa <- remdr$findElement('css selector',xppath)
  aa$clickElement()
  
  df_jockey <- remdr$getPageSource()[[1]] %>% read_html() %>% html_table()
  
  df_jockey <- df_jockey[[1]][,3:5]
  df_jockey <- cbind(df_jockey[1,],df_jockey[2,])
  
  jockey_regex <- paste0(paste0(c('복승률 ','승률 ','연승률 '),': ',collapse = '|'),'| %')
  df_jockey[,1:ncol(df_jockey)] <- df_jockey[,1:ncol(df_jockey)] %>% str_remove_all(jockey_regex)
  
  jockey_list[j,8:13] <- df_jockey[,1:6]
}

con <- dbConnect(MySQL(),user='simon',password='Simon1304!',
                 host='175.119.87.54',dbname='horse',port=9560)
dbGetQuery(con,'set names utf8')
dbGetQuery(con,'delete from jockey_info where region="busan"')

region <- 'busan'
jockey_list <- cbind(jockey = jockey_list[,1],region,jockey_list[,2:ncol(jockey_list)])
jockey_list$jockey <- as.character(jockey_list$jockey)
jockey_list$region <- as.character(jockey_list$region)
jockey_list[,c(1,3,4)] <- lapply(jockey_list[,c(1,3,4)],
                                 function(x) str_remove_all(x,'\n|\t'))

for(k in 1:nrow(jockey_list)){
  sqll <- paste(jockey_list[k,],collapse = "','") %>% str_remove_all('\\\\')
  sqll <- paste0("'",sqll,"'")
  sqll_ua <- paste0(colnames(jockey_list))
  sqll_ub <- paste0("'",jockey_list[k,],"'")
  sqll_ud <- paste0(sqll_ua,'=',sqll_ub,collapse = ',')
  
  sqll_final <- paste0('insert into jockey_info values(',sqll,') on duplicate key update ',sqll_ud)
  dbGetQuery(con,sqll_final)
}
dbDisconnect(con)
remdr$closeall()
