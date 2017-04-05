setwd("E:/d/Columbia/QMSS/CDS/Shannon Database Search")

library(rvest)
library(tidyverse)
library(stringr)


##########################
# Preparation & Settings #
##########################


## search terms / area
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- "abstract"

## maxsize
maxsize_sj <- 100
maxsize_sd <- 200


scrape <- function(keywordsA, keywordsB, area, maxsize, databasename){

  # if databasename is a vector, maxsize should be a vector as well
  # as for different database the max page size can be different
  require(rvest)
  require(tidyverse)
  require(stringr)
  require(qdap)
  
  
  ## check number of database
  if (length(maxsize) != length(databasename)){
    stop("Number of maxsize and number of database should be equal")
  }
  
  num_database <- length(databasename)
  
  ## check database name
  databaseidx <- c()
  for (i in 1:length(databasename)){
    if (tolower(str_replace_all(databasename[i]," ","")) == "sagejournal"){
      databaseidx[i] <- 1
    }
    if (tolower(str_replace_all(databasename[i]," ","")) == "pubmed"){
      databaseidx[i] <- 2
    }
  }
  
  geturl <- function(databaseidx_cur,maxsize_db,area,keywordA,keywordB,page){
    if (databaseidx_cur == 1){
      pageidx <- ifelse((page-1)==0,"",page-1)
      return(paste("http://journals.sagepub.com/action/doSearch?pageSize=",
                   maxsize_db,
                   "&field1=",
                   area,
                   "&text1=",
                   keywordA,
                   "&field2=",
                   area,
                   "&text2=",
                   keywordB,
                   "&ContentItemType=research-article&startPage=",
                   pageidx, sep=""))
    }else if (databaseidx_cur == 2){
      return()
    }
    
  }
  
  getinfo_sj <- function(page,n){
    title <- c()
    author <- c()
    link <- c()
    abstract <- c()
    for (i in 1:n){
      title_cur <- html_nodes(page,xpath=paste('//*[@id="frmSearchResults"]/ol/li[',i,']/article',sep='')) %>% html_attrs() %>% unlist()
      names(title_cur) <- c() # clean the tag name
      title_cur <- str_trim(rm_white(title_cur[2])) # the second element is data-title
      
      ## loop to catch all authors
      j = 1
      while (j > 0){
        author_loop <- try(str_trim(html_nodes(page,xpath = paste('//*[@id="frmSearchResults"]/ol/li[',i,']/article/div[2]/div/span/span[',j,']/a/text()',sep="")) %>% html_text()),'both')
        if (length(author_loop) > 0){
          if (j == 1) {
            author_cur <- author_loop
          }else{
            author_cur <- paste(author_cur,author_loop,sep=", ") 
          }
          j = j + 1
          
        } else{
          break
        }
      }
      

      link_cur <- html_nodes(page,xpath = paste('//*[@id="frmSearchResults"]/ol/li[',i,']/article/div[4]/a[1]',sep='')) %>% html_attrs() %>% unlist() # from 'show abstract'
      if (length(link_cur)==0){
        link_cur <- html_nodes(page,xpath = paste('//*[@id="frmSearchResults"]/ol/li[',i,']/article/div[3]/a[2]',sep='')) %>% html_attrs() %>% unlist() # from icon 'abstract'
        if (length(link_cur)==0 || str_detect(link_cur,'pdf')){
          link_cur <- html_nodes(page,xpath = paste('//*[@id="frmSearchResults"]/ol/li[',i,']/article/div[3]/a[3]',sep='')) %>% html_attrs() %>% unlist() # from icon 'translated abstract' for articles in foreign language
        }
      }
      link_cur <- link_cur[which(names(link_cur)=='href')] # extract the link
      names(link_cur) <- c() # clean the tag name
      link_cur <- paste('http://journals.sagepub.com',str_trim(rm_white(link_cur)),sep='')
      
      page_full <- read_html(link_cur)
      
      abstract_cur <- html_nodes(page_full, xpath = '//*[@id="25e8d4fe-c5d2-4686-bdef-3b125e01db4b"]/div/div[2]/div/div/div[1]/article/div[1]/div[2]') %>% html_text()
      if (length(abstract_cur)==0){
        abstract_cur <- html_nodes(page_full, xpath='//*[@id="25e8d4fe-c5d2-4686-bdef-3b125e01db4b"]/div/div[2]/div/div/div[1]/article/div[1]/div[2]') %>% html_text()
        if (length(abstract_cur)==0){
          abstract_cur <- 'No abstract'
        }
      }
      
      title <- c(title, title_cur)
      author <- c(author, author_cur)
      link <- c(link, link_cur)
      abstract <- c(abstract, abstract_cur)
      
      print(paste("Done: ",i,"/",n,sep=''))
    }
    return(data.frame(title = title, author = author, abstract = abstract, link = link))
  }
  getinfo_sd <- function(page,n){
    n <- page %>% html_nodes('strong') %>% html_text() # Search results: 5,859
    n <- str_split_fixed(n,' ',3)
    n <- as.numeric(str_replace(n[1,3],',','')) # the 3rd element is number of result; remove ','
    
    title <- c()
    author <- c()
    link <- c()
    abstract <- c()
    
    if (n <= 25){
      title <- page %>% html_nodes('.S_C_artTitle') %>% html_text()
      author <- page %>% html_nodes('.authorTxt') %>% html_text()
      link <- page %>% html_nodes('#title_S1572308917302231')
    }
    
  }
  
  
  num_stage <- length(keywordsA) * length(keywordsB) * num_database
  num_stage_cur <- 0
  data_total <- data.frame()
  for (db in 1:num_database){
    databaseidx_cur <- databaseidx[i]
    
    for (keywordA in keywordsA){
      for (keywordB in keywordsB){
        
        keywordA_original <- keywordA
        keywordB_original <- keywordB
        
        maxsize_db <- maxsize[db]
        num_stage_cur <- num_stage_cur + 1
        
        if (databaseidx[i] == 1){ ## if databse is sage journal
          keywordA <- str_replace_all(str_trim(keywordA),' ', '+')
          keywordB <- str_replace_all(str_trim(keywordB),' ', '+')
          
          url_sage <- geturl(databaseidx_cur,maxsize_db,area,keywordA,keywordB,1)
          page <- read_html(url_sage)
          closeAllConnections()
          
          n <- page %>%
            html_nodes(xpath = '//*[@id="searchResultContainer"]/div[1]/ul/li/a/span/text()') %>% 
            html_text() %>%
            str_sub(2,-2) %>% # delete parenthese
            as.numeric()
          
          if (length(n) == 0){
            print(paste('Start current stage (', num_stage_cur, '/', num_stage, '): ', sep=''))
            print(paste('KeywordA: ',keywordA,'; KeywordB: ',keywordB, '; Database: ', databasename[db]))
            print(paste('Total results: 0'))
          }else{
            print(paste('Start current stage (', num_stage_cur, '/', num_stage, '): ', sep=''))
            print(paste('KeywordA: ',keywordA,'; KeywordB: ',keywordB, '; Database: ', databasename[db]))
            print(paste('Total results: ', n, sep=''))
            if (n <= maxsize_db){
              print(paste('Total pages: 1'))
              print(paste('Current page: 1/1'))
              data <- getinfo_sj(page,n) %>% 
                mutate(searchtermA = keywordA, searchtermB = keywordB, database = databasename[db])
            }else{
              num_page = n %/% maxsize_db + 1
              print(paste('Total pages: ', num_page, sep=''))
              for (k in 1:num_page){
                print(paste('Current page: ',k,'/', num_page, sep=''))
                if (k == 1){
                  data <- getinfo_sj(page,maxsize_db)
                }else {
                  url_sage_cur <- geturl(databaseidx_cur,maxsize_db,area,keywordA_original,keywordB_original,k)
                  page_cur <- read_html(url_sage_cur)
                  closeAllConnections()
                  
                  if (k < num_page){
                    data_cur <- getinfo_sj(page_cur,maxsize_db)
                    data <- rbind(data,data_cur)
                  }else if (k == num_page){
                    num_cur <- n %% maxsize_db
                    data_cur <- getinfo_sj(page_cur,num_cur)
                    data <- rbind(data,data_cur) %>% 
                      mutate(searchtermA = keywordA_original, searchtermB = keywordB_original, database = databasename[db])
                  }
                }
              }
            }
          }
          
         
        }else if (databaseidx == 2){
          session <- html_session('http://www.sciencedirect.com/science/search')
          form <- html_form(session)[[4]]
          
          searchvalue <- form %>% 
            set_values(SearchText = keywordA,addSearchText = keywordB)
          
          
          url <- paste(submit_form(session,searchvalue)$url)
          page <- read_html(url)
          data <- getinfo_sd(page)
          
          session_result <- submit_form(session,searchvalue)
          session2 <- html_form(session_result)[[4]] %>%
             set_values(resultsPerPage = '5') %>%
            submit_form(session=session_result,form=.,submit = 'bottomNext')
          
          page <- read_html(session2$url)
          page %>% html_nodes('.authorTxt') %>% html_text()
          
          page <- read_html('http://www.sciencedirect.com/science?_ob=ArticleListURL&_method=tag&searchtype=a&refSource=search&_st=4&sort=r&filterType=&_chunk=0&NEXT_LIST=1&view=c&md5=8961aff8b7b164190c4bccd3ee3d42fc&_ArticleListID=-1177584790&chunkSize=25&sisr_search=&navSrc=280203&navCon=jrl&art=1-s2.0-S1572308917302231&bottomPaginationBoxChanged=&bottomNext=Next%20%3E%3E&displayPerPageFlag=f&resultsPerPage=200')
          page %>% html_nodes('.authorTxt') %>% html_text()
          
          title <- page %>% html_nodes('.authorTxt') %>% html_text()
        }
        
        data_total <- rbind(data_total,data)
      }
    }
  }
  return(data_total)
}

# Sage Journals
# http://journals.sagepub.com/action/doSearch?content=articlesChapters&countTerms=true&pageSize=100&target=default&field1=Abstract&text1=Default+AND+Decisions&field2=AllField&text2=&Ppub=&Ppub=&AfterYear=&BeforeYear=&access=
# http://journals.sagepub.com/action/doSearch?field1=Abstract&text1=default+effect&field2=Abstract&text2=decision-making&Ppub=&Ppub=&AfterYear=&BeforeYear=&access=



keywordsA <- c('defaults','default effect')
keywordsB <- c('decisions','psychology')
area <- 'abstract'
maxsize <- 100
databasename <- 'sagejournal'

data_test <- scrape(keywordsA,keywordsB,area,maxsize,databasename)

data_test <- data_test[!duplicated(data_test[,c('title','author','abstract','link')]),]




keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- "abstract"
maxsize <- 100
databasename <- 'sage journal'
data_test <- scrape(keywordsA,keywordsB,area,maxsize,databasename)
data_test_clean <- data_test[!duplicated(data_test[,c('title','author','abstract','link')]),]


library(dplyr)
library(tidytext)
library(reshape2)


keywords_uni <- c(keywordsA,keywordsB) %>%
  as_tibble() %>%
  unnest_tokens(word,value)

data(stop_words)
stop_words <- stop_words %>%
  anti_join(keywords_uni,by='word')
words <- data_test %>%
  select(abstract) %>%
  mutate(document = row_number(),
         abstract = as.character(abstract)) %>%
  as_tibble() %>%
  unnest_tokens(word,abstract) %>%
  anti_join(stop_words,by="word") %>% # remove stop words
  count(document, word, sort = TRUE) %>%
  ungroup() 

total_words <- words %>% 
  group_by(document)%>%
  summarise(total = sum(n)) %>%
  ungroup()

words <- words %>%
  left_join(total_words,by='document') %>%
  mutate(std_n = n / total) %>%
  bind_tf_idf(word, document, std_n) %>%
  arrange(desc(tf_idf)) 

tf_idf <- left_join(keywords_uni,words,by='word') %>%
  acast(document ~ word, value.var = "tf_idf", fill = 0) %>%
  as.data.frame()

tf_idf$score <- rowSums(tf_idf)
tf_idf$document <- as.numeric(rownames(tf_idf))
data_test_tfidf <- data_test %>%
  mutate(document = row_number()) %>%
  inner_join(tf_idf,by='document') %>%
  arrange(desc(score))

if (length(keywordsA)>0 & length(keywordsB)>0){
  
}







session <- html_session('http://www.sciencedirect.com/science/search')
form <- html_form(session)[[4]]

searchvalue <- form %>% 
  set_values(
    SearchText = 'defaults',
    addSearchText = 'decisions'
  )

result <- submit_form(session,searchvalue)
url <- result$url
page <- read_html(url)





united <- html_session("http://www.united.com/")

login_form <- united %>%
  html_node("form[name=LoginForm]") %>%
  html_form() 
login_form



library(httr)
uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
session <- html_session("https://www.linkedin.com/job/")
form <- html_form(session)[[1]]
form <- set_values(form, keywords = "Data Science", location="New York")











query = "data science"
loc = "New York"
session <- html_session("http://www.indeed.com")
form <- html_form(session)[[1]]
form <- set_values(form, q = query, l = loc)
url <- submit_form(session,form)$url
page <- read_html(url)
page %>% html_nodes(".jobtitle") %>% html_text()
write_html(session,'test.html')

submit_form2 <- function(session, form){
  library(XML)
  url <- XML::getRelativeURL(form$url, session$url)
  url <- paste(url,'?',sep='')
  values <- as.vector(rvest:::submit_request(form)$values)
  att <- names(values)
  if (tail(att, n=1) == "NULL"){
    values <- values[1:length(values)-1]
    att <- att[1:length(att)-1]
  }
  q <- paste(att,values,sep='=')
  q <- paste(q, collapse = '&')
  q <- gsub(" ", "+", q)
  url <- paste(url, q, sep = '')
  html_session(url)
}

session2 <- submit_form2(session, form)
session2 <- follow_link(session1, css = "#more_9 li:nth-child(3) a")

session_sd <- html_session('http://www.sciencedirect.com/science/search')
form <- html_form(session_sd)[[4]]









united <- html_session("http://www.united.com/")

united <- html_session('https://www.united.com/ual/en/us/')

login_form <- united %>%
  html_nodes('//*[@id="bookTravelNav"]') %>%
  html_form()

login_form <- united %>%
  html_node("form[name=LoginForm]") %>%
  html_form() 
login_form
login <- login_form %>%
  set_values(
    MpNumber = "GY797363",
    Password = password
    
  )
logged_in <- united %>% submit_form(login)
logged_in %>%
  follow_link("View account") %>%
  html_node("#ctl00_ContentInfo_AccountSummary_spanEliteMilesNew") %>%
  html_text() %>%
  readr::parse_number()




