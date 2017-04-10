setwd("E:/d/Columbia/QMSS/CDS/Shannon Database Search")

library(rvest)
library(tidyverse)
library(stringr)


#########################
# function
##################

# set_values <- function(form, ...) {
#   new_values <- list(...)
#   
#   # check for valid names
#   no_match <- setdiff(names(new_values), names(form$fields))
#   if (length(no_match) > 0) {
#     stop("Unknown field names: ", paste(no_match, collapse = ", "),
#          call. = FALSE)
#   }
#   
#   for (field in unique(names(new_values))) {
#     type <- form$fields[[field]]$type %||% "non-input"
#     if (type == "hidden") {
#       warning("Setting value of hidden field '", field, "'.", call. = FALSE)
#     } else if (type == "submit") {
#       stop("Can't change value of submit input '", field, "'.", call. = FALSE)
#     }
#     
#     if (type == "checkbox") {
#       # there could be multiple check boxes with same 'name's and different 'value's
#       idx <- unlist(names(new_values) == field)
#       form <- set_checkbox(form, new_values[idx])
#     } else if (type == "radio") {
#       # there could be multiple radio buttons with same 'name's and different 'value's
#       idx <- unlist(names(new_values) == field)
#       form <- set_radio(form, new_values[idx])
#     } else {
#       form$fields[[field]]$value <- new_values[[field]]
#     }
#   }
#   
#   form
#   
# }
# 
# set_checkbox <- function(form, values) {
#   idx <- which(unlist(lapply(form$fields, function(x) { x$name %in% values })))
#   
#   for (i in unname(idx)) {
#     if (!is.null(form$fields[[i]]$value) && (form$fields[[i]]$value %in% values))
#       form$fields[[i]]$checked <- "checked"
#   }
#   return(form)
# }
# 
# set_radio <- function(form, values) {
#   idx <- which(unlist(lapply(form$fields, function(x) { x$name %in% names(values) })))
#   
#   for (i in unname(idx)) {
#     if (!is.null(form$fields[[i]]$value)) {
#       if (form$fields[[i]]$value %in% values)
#         form$fields[[i]]$checked <- "true"
#       else
#         form$fields[[i]]$checked <- "false"
#     }
#   }
#   return(form)
# }
# 
# submit_form <- function(session, form, submit = NULL, ...) {
#   request <- submit_request(form, submit)
#   url <- xml2::url_absolute(form$url, session$url)
#   
#   # Make request
#   if (request$method == "GET") {
#     request_GET(session, url = url, query = request$values, ...)
#   } else if (request$method == "POST") {
#     request_POST(session, url = url, body = request$values,
#                  encode = request$encode, ...)
#   } else {
#     stop("Unknown method: ", request$method, call. = FALSE)
#   }
# }
# 
# submit_request <- function(form, submit = NULL) {
#   submits <- Filter(function(x) {
#     identical(tolower(x$type), "submit") | identical(tolower(x$type), "image")
#   }, form$fields)
#   if (is.null(submit)) {
#     submit <- names(submits)[[1]]
#     message("Submitting with '", submit, "'")
#   }
#   if (!(submit %in% names(submits))) {
#     stop(
#       "Unknown submission name '", submit, "'.\n",
#       "Possible values: ", paste0(names(submits), collapse = ", "),
#       call. = FALSE
#     )
#   }
#   other_submits <- setdiff(names(submits), submit)
#   
#   # Parameters needed for http request -----------------------------------------
#   method <- form$method
#   if (!(method %in% c("POST", "GET"))) {
#     warning("Invalid method (", method, "), defaulting to GET", call. = FALSE)
#     method <- "GET"
#   }
#   
#   url <- form$url
#   
#   fields <- form$fields
#   fields <- Filter(function(x) length(x$value) > 0, fields)
#   fields <- Filter(function(x) is.null(x$type) || ((x$type != "radio") && (x$type != "checkbox")) || (!is.null(x$type) && (x$type %in% c("checkbox", "radio")) && !is.null(x$checked) && (x$checked == "true" || x$checked == "checked")), fields)
#   fields <- fields[setdiff(names(fields), other_submits)]
#   
#   values <- pluck(fields, "value")
#   names(values) <- names(fields)
#   
#   list(
#     method = method,
#     encode = form$enctype,
#     url = url,
#     values = values
#   )
# }


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
    }else if (tolower(str_replace_all(databasename[i]," ","")) == "sciencedirect"){
      databaseidx[i] <- 2
    }else if (tolower(str_replace_all(databasename[i]," ","")) == "pubmed"){
      databaseidx[i] <- 3
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
  getinfo_sd <- function(session,page){
    maxsize_db <- 25
    
    n <- page %>% html_nodes('strong') %>% html_text() # Search results: 5,859
    
    title <- c()
    author <- c()
    link <- c()
    abstract <- c()
    
    if (length(n) == 0){
      print(paste('Total results: 0'))
    }else{    
      n <- str_split_fixed(n,' ',3)
      n <- as.numeric(str_replace(n[1,3],',','')) # the 3rd element is number of result; remove ','
      
      if (n <= maxsize_db){
        print(paste('Total results: ', n, sep=''))
        
        title <- c(title,page %>% html_nodes('.S_C_artTitle') %>% html_text())
        author <- c(author,page %>% html_nodes('.authorTxt') %>% html_text())
        link <- c(link,page %>% html_nodes('.S_C_artTitle') %>% html_attr('href'))
        
        abstract_check <- str_detect(tolower(page %>% html_nodes('ul.extLinkBlock ') %>% html_text()),'abstract')
        link_abs_raw <- page %>% html_nodes('ul.extLinkBlock a[data-type="abstract"]') %>% html_attr('data-url')
        link_abs <- c()
        for (i in 1:n){
          if (abstract_check[i]==TRUE){
            link_abs[i] <- link_abs_raw[1]
            link_abs_raw <- link_abs_raw[-1]
          }else{
            link_abs[i] <- ''
          }
        }
        
        for (i in 1:n){
          if (link_abs[i]!=''){
            page_cur <- read_html(link_abs[i])
            abstract <- c(abstract,paste(page_cur %>% html_nodes('.paraText') %>% html_text(),collapse = ' '))
          }else{
            abstract <- c(abstract,'No abstract')
          }
          print(paste("Done: ", i,'/',n,sep=''))
        }
      }else{
        num_page <- n %/% maxsize_db + 1
        
        print(paste('Total results: ', n, sep=''))
        print(paste('Total pages: ', num_page, sep=''))
        for (k in 1:num_page){
          print(paste('Current page: ',k,'/', num_page, sep=''))
          if (k > 1){
            session <- html_form(session)[[4]] %>%
              submit_form(session=session,form=.,submit = 'bottomNext')
            page <- read_html(session$url)
          }
          title <- c(title,page %>% html_nodes('.S_C_artTitle') %>% html_text())
          author <- c(author,page %>% html_nodes('.authorTxt') %>% html_text())
          link <- c(link,page %>% html_nodes('.S_C_artTitle') %>% html_attr('href'))
          
          abstract_check <- str_detect(tolower(page %>% html_nodes('ul.extLinkBlock ') %>% html_text()),'abstract')
          link_abs_raw <- page %>% html_nodes('ul.extLinkBlock a[data-type="abstract"]') %>% html_attr('data-url')
          link_abs <- c()
          for (i in 1:n){
            if (abstract_check[i]==TRUE){
              link_abs[i] <- link_abs_raw[1]
              link_abs_raw <- link_abs_raw[-1]
            }else{
              link_abs[i] <- ''
            }
          }
          
          for (i in 1:n){
            if (link_abs[i]!=''){
              page_cur <- read_html(link_abs[i])
              abstract <- c(abstract,paste(page_cur %>% html_nodes('.paraText') %>% html_text(),collapse = ' '))
            }else{
              abstract <- c(abstract,'No abstract')
            }
          }
        }
      }
    }
    closeAllConnections()
    return(data.frame(title = title, author = author, abstract = abstract, link = link))
  }
  
  
  getinfo_pm <- function(page,url_pm){
    maxsize_db <- 20
    
    n <- page %>% html_nodes('.result_count') %>% html_text() # Search results: 5,859
    
    title <- c()
    author <- c()
    link <- c()
    abstract <- c()
    
    if (length(n) == 0){
      print(paste('Total results: 0'))
    }else{    
      n <- str_split_fixed(n,' ',6)
      n <- as.numeric(str_replace(n[1,6],',','')) # the 3rd element is number of result; remove ','
      
      if (n <= maxsize_db){
        print(paste('Total results: ', n, sep=''))
        
        title <- c(title,page %>% html_nodes('.title') %>% html_text())
        author <- c(author,page %>% html_nodes('.desc') %>% html_text())
        link_abs <- paste('https://www.ncbi.nlm.nih.gov',page %>% html_nodes('.title a') %>% html_attr('href'),sep='')
        link <- c(link,link_abs)
        
        for (i in 1:n){
          page_cur <- read_html(link_abs[i])
          abstract <- c(abstract,paste(page_cur %>% html_nodes('abstracttext') %>% html_text(),collapse = ' '))
          print(paste("Done: ", i,'/',n,sep=''))
        }
      }else{
        num_page <- n %/% maxsize_db + 1
        
        print(paste('Total results: ', n, sep=''))
        print(paste('Total pages: ', num_page, sep=''))
        for (k in 1:num_page){
          print(paste('Current page: ',k,'/', num_page, sep=''))
          if (k > 1){
            page %>% html_nodes('.next')
            session <- html_form(html_session(url_pm))[[4]] %>%
              submit_form(session=session,form=.,submit = 'bottomNext')
            page <- read_html(session$url)
          }
          title <- c(title,page %>% html_nodes('.title') %>% html_text())
          author <- c(author,page %>% html_nodes('.desc') %>% html_text())
          link_abs <- paste('https://www.ncbi.nlm.nih.gov',page %>% html_nodes('.title a') %>% html_attr('href'),sep='')
          link <- c(link,link_abs)
          for (i in 1:maxsize_db){
            page_cur <- read_html(link_abs[i])
            abstract <- c(abstract,paste(page_cur %>% html_nodes('abstracttext') %>% html_text(),collapse = ' '))
            print(paste("Done: ",i,"/",maxsize_db,sep=''))
          }
        }
      }
    }
    closeAllConnections()
    return(data.frame(title = title, author = author, abstract = abstract, link = link))
  }
  
  
  
  num_stage <- length(keywordsA) * length(keywordsB) * num_database
  num_stage_cur <- 0
  data_total <- data.frame()
  for (db in 1:num_database){
    databaseidx_cur <- databaseidx[db]
    
    for (keywordA in keywordsA){
      for (keywordB in keywordsB){
        closeAllConnections()
        
        keywordA_original <- keywordA
        keywordB_original <- keywordB
        
        maxsize_db <- maxsize[db]
        num_stage_cur <- num_stage_cur + 1
        
        print(paste('Start current stage (', num_stage_cur, '/', num_stage, '): ', sep=''))
        print(paste('KeywordA: ',keywordA,'; KeywordB: ',keywordB, '; Database: ', databasename[db]))
        
        if (databaseidx[db] == 1){ ## if databse is sage journal
          keywordA <- str_replace_all(str_trim(keywordA),' ', '+')
          keywordB <- str_replace_all(str_trim(keywordB),' ', '+')
          
          url_sage <- geturl(databaseidx_cur,maxsize_db,area,keywordA,keywordB,1)
          download.file(url_sage, destfile = "scrapedpage.html", quiet=TRUE)
          page <- read_html("scrapedpage.html")
          closeAllConnections()
          
          n <- page %>%
            html_nodes(xpath = '//*[@id="searchResultContainer"]/div[1]/ul/li/a/span/text()') %>% 
            html_text() %>%
            str_sub(2,-2) %>% # delete parenthese
            as.numeric()
          
          if (length(n) == 0){
            print(paste('Total results: 0'))
          }else{
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
                  url_sage_cur <- geturl(databaseidx_cur,maxsize_db,area,keywordA,keywordB,k)
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
          
          
        }else if (databaseidx[db] == 2){
          session <- html_session('http://www.sciencedirect.com/science/search')
          form <- html_form(session)[[4]]
          
          # set search keyword --> works
          searchvalue <- form %>% 
            set_values(SearchText = keywordA,
                       addSearchText = keywordB) 
          
          # set range for keyword --> works
          searchvalue$fields$keywordOpt$value <- form$fields$keywordOpt$options[2]
          searchvalue$fields$addkeywordOpt$value <- form$fields$addkeywordOpt$options[2]
          
          # set checkbox (Subscribed publications: value = '1')
          searchvalue$fields[[16]]$checked <- searchvalue$fields[[12]]$checked
          
          session_result <- submit_form(session,searchvalue)
          page <- read_html(session_result$url)
          page %>% html_nodes('strong') %>% html_text() # Search results: 5,859
          
          data <- getinfo_sd(session_result,page) %>%
            mutate(searchtermA = keywordA_original, searchtermB = keywordB_original, database = databasename[db])
        }else if (databaseidx[db] == 3){
          
          keywordA <- str_replace_all(str_trim(keywordA),' ', '%20')
          keywordB <- str_replace_all(str_trim(keywordB),' ', '%20')
          
          url_pm <- paste('https://www.ncbi.nlm.nih.gov/pubmed?term=(',
                          keywordA,'%5BTitle%2FAbstract%5D)%20AND%20',
                          keywordB,'%5BTitle%2FAbstract%5D', sep='')
          
          url_pm <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',
                          keywordA,'[title/abstract]',
                          '+AND+',
                          keywordB,'[title/abstract]',
                          '&retmax=10000',sep='')
          page <- read_html(url_pm)
          write_html(page,'test.html')
          page %>% html_nodes(xpath='//*[@id="collapsible1"]/div[1]/div[2]/div[1]/span[2]') %>% html_text()
          # https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="science"[journal]+AND+breast+cancer+AND+2008[pdat]

          # https://www.ncbi.nlm.nih.gov/pubmed?term=(defaults%5BTitle%2FAbstract%5D)%20AND%20decisions%5BTitle%2FAbstract%5D
          # https://www.ncbi.nlm.nih.gov/pubmed?term=(consumer%20behavior%5BTitle%2FAbstract%5D)%20AND%20decision-making%5BTitle%2FAbstract%5D
          
          page <- read_html(url_pm)
          
          data <- getinfo_pm(page,url_pm) %>%
            mutate(searchtermA = keywordA_original, searchtermB = keywordB_original, database = databasename[db])
        }
        
        data_total <- rbind(data_total,data)
      }
    }
  }
  return(data_total)
}




#####################
# test
####################

# sage journal with a few keywords
keywordsA <- c('defaults','default effect')
keywordsB <- c('decisions','psychology')
area <- 'abstract'
maxsize <- 100
databasename <- 'sagejournal'

data_test <- scrape(keywordsA,keywordsB,area,maxsize,databasename)

data_test <- data_test[!duplicated(data_test[,c('title','author','abstract','link')]),]



# sage journal with full keywords
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- "abstract"
maxsize <- 100
databasename <- 'sage journal'
data_test <- scrape(keywordsA,keywordsB,area,maxsize,databasename)
data_test_clean <- data_test[!duplicated(data_test[,c('title','author','abstract','link')]),]


# sage journal and science direct with a few keywords

t <- proc.time()

keywordsA <- c('defaults','default effect')
keywordsB <- c('decisions','psychology')
area <- 'abstract'
maxsize <- c(100,0)
databasename <- c('sage journal','science direct')

data_test <- scrape(keywordsA,keywordsB,area,maxsize,databasename)

proc.time() - t




# sage journal and science direct with full keywords

t <- proc.time()

keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- 'abstract'
maxsize <- c(100,0)
databasename <- c('sage journal','science direct')

data_test <- scrape(keywordsA,keywordsB,area,maxsize,databasename)

proc.time() - t





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




