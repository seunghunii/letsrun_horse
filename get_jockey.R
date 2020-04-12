pkgs <- c('dplyr','stringr','rvest','RSelenium','pbapply',
          'httr','tidyr','DBI','RMySQL','gtools','XML')
sapply(pkgs,require,character.only = TRUE)

# java -jar selenium-server-standalone-3.141.59.jar
# lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
eCaps <- list(
  chromeOptions = list(
    args = c('--headless', '--disable-gpu')))

remdr <- remoteDriver(port=4444,browser='chrome',
                      extraCapabilities = eCaps)
remdr$open()

urll_jockey <- 'http://race.kra.co.kr/jockey/ProfileJockeyListActive.do'

httml <- read_html(urll_jockey) %>%
  html_node('#contents > div.tableType2.normal_title > table') %>% 
  html_table()
httml[,2] <- str_remove_all(httml[,2],'[(]-2[)]')

result_df <- matrix(nrow=nrow(httml),
                      ncol=4) %>% data.frame()

for(k in 1:nrow(httml)){
  remdr$navigate(urll_jockey)
  
  css_sel <- paste0('#contents > div.tableType2.normal_title > table > tbody > tr:nth-child(',k,') > td:nth-child(2) > div > a')
  aa <- remdr$findElement('css selector',css_sel)
  aa$clickElement()
  
  httml_sel <- remdr$getPageSource()
  aa <- httml_sel[[1]] %>% 
    read_html() %>% 
    html_nodes('#contents > div.tableType1 > table')
  info <- aa[[1]] %>% html_table()
  info <- info[2,2:5]
  
  info[,1] <- str_remove_all(info[,1],'[(](.*?)[)]|전')
  info[,c(2:4)] <- sapply(2:4,function(x)
    str_remove_all(info[,x],'%|승률|복승률|연승률|:') %>% str_trim())
  
  result_df[k,1:4] <- info[1,1:4]}

result_df <- cbind(httml[,2],result_df)
colnames(result_df) <- c('jockey','recent_runcount','recent_win1','recent_win2','recent_win3')

result_df[,c(3:5)] <- apply(result_df[,c(3:5)],2,
                            function(x) str_replace_all(x,'^$','0.0'))

result_df[,1] <- str_replace_all(result_df[,1],'^$','0')
remdr$closeall()
