print('------')
print(paste('code start',as.character(Sys.time())))
sttime <- Sys.time()

pkgs <- c('dplyr','stringr','rvest','RSelenium','pbapply',
          'httr','tidyr','DBI','RMySQL','gtools')
sapply(pkgs,require,character.only = TRUE,quietly = TRUE)

eCaps <- list(
  chromeOptions = list(
    args = c('--headless', '--disable-gpu')))

remdr <- remoteDriver(port=4444,browser='chrome',
                      extraCapabilities = eCaps)
remdr$open(silent = TRUE)

###trainer information###
url_trainer <- 'http://race.kra.co.kr/trainer/profileTrainerList.do?Act=10&Sub=1&meet=3'
a <- url_trainer %>% read_html() %>% html_table()
trainer_list <- a[[1]][,2:7]

names(trainer_list) <- c('trainer','groups','debut_date','year_record','total_record','horse_care')

trainer_list$trainer <- trainer_list$trainer %>% na.replace('unknown')
trainer_list$debut_date <- trainer_list$debut_date %>% str_replace_all('\\/','-')
trainer_list[,c(paste0('trainer_total_win',1:3),paste0('trainer_year_win',1:3))] <- 0

for(j in 1:nrow(trainer_list)){
  remdr$navigate(url_trainer)
  xppath <- paste0('#contents > div.tableType2.normal_title > table > tbody > tr:nth-child(',j,') > td:nth-child(2) > a')
  aa <- remdr$findElement('css selector',xppath)
  aa$clickElement()
  
  df_trainer <- remdr$getPageSource()[[1]] %>% read_html() %>% html_table()
  
  df_trainer <- df_trainer[[1]][1:2,3:5]
  df_trainer <- cbind(df_trainer[1,],df_trainer[2,])
  
  trainer_regex <- paste0(paste0(c('복승률 ','승률 ','연승률 '),': ',collapse = '|'),'|%')
  df_trainer[,1:ncol(df_trainer)] <- df_trainer[,1:ncol(df_trainer)] %>% str_remove_all(trainer_regex)
  
  trainer_list[j,7:12] <- df_trainer[,1:6]
}

con <- dbConnect(MySQL(),user=mysql_id,password=mysql_pwd,
                 host=mysql_ip,dbname=mysql_db,port=mysql_port)
dbGetQuery(con,'set names utf8')
dbGetQuery(con,'delete from trainer_info where region="busan"')

region <- 'busan'
trainer_list <- cbind(trainer = trainer_list[,1],region,trainer_list[,2:ncol(trainer_list)])
trainer_list$trainer <- as.character(trainer_list$trainer)
trainer_list$region <- as.character(trainer_list$region)

for(k in 1:nrow(trainer_list)){
  sqll <- paste(trainer_list[k,],collapse = "','") %>% str_remove_all('\\\\')
  sqll <- paste0("'",sqll,"'")
  
  sqll_final <- paste0('insert into trainer_info values(',sqll,')')
  dbGetQuery(con,sqll_final)
}
dbDisconnect(con)
remdr$closeall()

#timediff <- difftime(Sys.time(),sttime,units = 'mins') %>% as.numeric() %>% round(5)
#print(paste('code end',as.character(Sys.time()),',time spent',timediff,'mins'))
