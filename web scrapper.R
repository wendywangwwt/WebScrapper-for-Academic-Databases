setwd("E:/d/Columbia/QMSS/CDS/Shannon Database Search")

library(rvest)
library(tidyverse)
library(stringr)


#########################
# function
##################


##########################
# Main Function     
##########################


scrape <- function(keywordsA, keywordsB, area, databasename, sdkey="", filterduplication=T){
  
  # if databasename is a vector, maxsize should be a vector as well
  # as for different database the max page size can be different
  require(rvest)
  require(tidyverse)
  require(stringr)
  require(qdap)
  
  
  
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
  
  readkey <- function(){ 
    key <- readline(prompt="Please enter your API key for Science Direct (if you don't have one, get it from here: https://dev.elsevier.com/user/login): ")
    while (nchar(str_trim(key))<25){
      print("The length of key seems wrong")
      key <- readline(prompt="Please enter your API key for Science Direct (if you don't have one, get it from here: https://dev.elsevier.com/user/login): ")
    }
    return(key)
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
    maxsize_db <- 200
    
    if (n <= maxsize_db){
      title <- page %>% html_nodes('title') %>% html_text()
      link <- page %>% html_nodes('entry link[ref="scidir"]') %>% html_attr('href')
      
      article_nodes <- page %>% html_nodes('entry')
      author <- c()
      abstract <- c()
      for (article_node in article_nodes){
        author <- c(author, paste(article_node %>% html_nodes('given-name') %>% html_text(),article_node %>% html_nodes('surname') %>% html_text(),collapse = ", "))
        abstract_now <- article_node %>% html_nodes('description') %>% html_text()
        abstract <- if(length(abstract_now)>0) c(abstract,abstract_now) else c(abstract,'No abstract')
      }
      print(paste("Done: ", n,'/',n,sep=''))
      
    }else{
      num_page <- n %/% maxsize_db + 1
      
      print(paste('Total results: ', n, sep=''))
      print(paste('Total pages: ', num_page, sep=''))
      
      title <- c()
      author <- c()
      abstract <- c()
      link <- c()
      count_done <- 0
      
      page_cur <- page
      for (k in 1:num_page){
        print(paste('Current page: ',k,'/', num_page, sep=''))
        
        if(k>1){
          url_cur <- page_cur %>% html_nodes('link[ref="next"]') %>% html_attr('href')
          page_cur <- read_html(url_cur)
        }
        title <- c(title, page_cur %>% html_nodes('title') %>% html_text())
        link <- c(link, page_cur %>% html_nodes('entry link[ref="scidir"]') %>% html_attr('href'))
        
        article_nodes_cur <- page_cur %>% html_nodes('entry')
        author_cur <- c()
        abstract_cur <- c()
        for (article_node_cur in article_nodes_cur){
          author_cur <- c(author_cur, paste(article_node_cur %>% html_nodes('given-name') %>% html_text(),article_node_cur %>% html_nodes('surname') %>% html_text(),collapse = ", "))
          abstract_cur_now <- article_node_cur %>% html_nodes('description') %>% html_text()
          abstract_cur <- if(length(abstract_cur_now)>0) c(abstract_cur,abstract_cur_now) else c(abstract_cur,'No abstract')  
          count_done <- count_done+1
        }
        author <- c(author,author_cur)
        abstract <- c(abstract,abstract_cur)
        print(paste("Done: ", count_done,'/',n,sep=''))
      }
    }
    
    
    #### get information using session (advanced search form) ####
    # title <- c()
    # author <- c()
    # link <- c()
    # abstract <- c()
    # 
    # if (length(n) == 0){
    #   print(paste('Total results: 0'))
    # }else{    
    #   n <- str_split_fixed(n,' ',3)
    #   n <- as.numeric(str_replace(n[1,3],',','')) # the 3rd element is number of result; remove ','
    #   
    #   if (n <= maxsize_db){
    #     print(paste('Total results: ', n, sep=''))
    #     
    #     title <- c(title,page %>% html_nodes('.S_C_artTitle') %>% html_text())
    #     author <- c(author,page %>% html_nodes('.authorTxt') %>% html_text())
    #     link <- c(link,page %>% html_nodes('.S_C_artTitle') %>% html_attr('href'))
    #     
    #     abstract_check <- str_detect(tolower(page %>% html_nodes('ul.extLinkBlock ') %>% html_text()),'abstract')
    #     link_abs_raw <- page %>% html_nodes('ul.extLinkBlock a[data-type="abstract"]') %>% html_attr('data-url')
    #     link_abs <- c()
    #     for (i in 1:n){
    #       if (abstract_check[i]==TRUE){
    #         link_abs[i] <- link_abs_raw[1]
    #         link_abs_raw <- link_abs_raw[-1]
    #       }else{
    #         link_abs[i] <- ''
    #       }
    #     }
    #     
    #     for (i in 1:n){
    #       if (link_abs[i]!=''){
    #         page_cur <- read_html(link_abs[i])
    #         abstract <- c(abstract,paste(page_cur %>% html_nodes('.paraText') %>% html_text(),collapse = ' '))
    #       }else{
    #         abstract <- c(abstract,'No abstract')
    #       }
    #       print(paste("Done: ", i,'/',n,sep=''))
    #     }
    #   }else{
    #     num_page <- n %/% maxsize_db + 1
    #     
    #     print(paste('Total results: ', n, sep=''))
    #     print(paste('Total pages: ', num_page, sep=''))
    #     for (k in 1:num_page){
    #       print(paste('Current page: ',k,'/', num_page, sep=''))
    #       if (k > 1){
    #         session <- html_form(session)[[4]] %>%
    #           submit_form(session=session,form=.,submit = 'bottomNext')
    #         page <- read_html(session$url)
    #       }
    #       title <- c(title,page %>% html_nodes('.S_C_artTitle') %>% html_text())
    #       author <- c(author,page %>% html_nodes('.authorTxt') %>% html_text())
    #       link <- c(link,page %>% html_nodes('.S_C_artTitle') %>% html_attr('href'))
    #       
    #       abstract_check <- str_detect(tolower(page %>% html_nodes('ul.extLinkBlock ') %>% html_text()),'abstract')
    #       link_abs_raw <- page %>% html_nodes('ul.extLinkBlock a[data-type="abstract"]') %>% html_attr('data-url')
    #       link_abs <- c()
    #       for (i in 1:n){
    #         if (abstract_check[i]==TRUE){
    #           link_abs[i] <- link_abs_raw[1]
    #           link_abs_raw <- link_abs_raw[-1]
    #         }else{
    #           link_abs[i] <- ''
    #         }
    #       }
    #       
    #       for (i in 1:n){
    #         if (link_abs[i]!=''){
    #           page_cur <- read_html(link_abs[i])
    #           abstract <- c(abstract,paste(page_cur %>% html_nodes('.paraText') %>% html_text(),collapse = ' '))
    #         }else{
    #           abstract <- c(abstract,'No abstract')
    #         }
    #       }
    #     }
    #   }
    # }
    # closeAllConnections()
    return(data.frame(title = title, author = author, abstract = abstract, link = link))
  }
  getinfo_pm <- function(page,n){
    maxsize_db <- 200
    
    ids <- page %>% html_nodes('id') %>% html_text()
    
    link <- paste('https://www.ncbi.nlm.nih.gov/pubmed/',ids,sep='')
    
    if (n <= maxsize_db){
      print(paste('Total results: ', n, sep=''))
      url_summary <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=',
                           paste(ids,collapse=','),
                           '&rettype=text',sep='')
      page_summary <- read_html(url_summary)
      title <- page_summary %>% html_nodes('articletitle') %>% html_text()
      article_nodes <- page_summary %>% html_nodes('article')
      author <- c()
      abstract <- c()
      for (article_node in article_nodes){
        author <- c(author,paste(article_node %>% html_nodes('forename') %>% html_text(),article_node %>% html_nodes('lastname') %>% html_text(),collapse = ', '))
        abstract_now <- article_node %>% html_nodes('abstract') %>% html_text()
        abstract <- if(length(abstract_now)>0) c(abstract,abstract_now) else c(abstract,'No abstract')
      }
      
      print(paste("Done: ", n,'/',n,sep=''))
      
    }else{
      num_page <- n %/% maxsize_db + 1
      
      print(paste('Total results: ', n, sep=''))
      print(paste('Total pages: ', num_page, sep=''))
      
      title <- c()
      author <- c()
      abstract <- c()
      count_done <- 0
      for (k in 1:num_page){
        print(paste('Current page: ',k,'/', num_page, sep=''))
        
        ids_cur <- if (k < num_page) ids[seq((1+maxsize_db*(k-1)),maxsize_db*k)] else ids[seq((1+maxsize_db*(k-1)),length(ids))]
        url_summary_cur <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=',
                                 paste(ids_cur,collapse=','),
                                 '&rettype=text',sep='')
        page_summary_cur <- read_html(url_summary_cur)
        title_cur <- page_summary_cur %>% html_nodes('articletitle') %>% html_text()
        article_nodes_cur <- page_summary_cur %>% html_nodes('pubmedarticle')
        author_cur <- c()
        abstract_cur <- c()
        for (article_node_cur in article_nodes_cur){
          count_done <- count_done + 1
          author_cur <- c(author_cur,paste(article_node_cur %>% html_nodes('forename') %>% html_text(),article_node_cur %>% html_nodes('lastname') %>% html_text(),collapse = ', '))
          abstract_cur_now <- article_node_cur %>% html_nodes('abstract') %>% html_text()
          abstract_cur <- if(length(abstract_cur_now)>0) c(abstract_cur,abstract_cur_now) else c(abstract_cur,'No abstract')
        }
        
        title <- c(title,title_cur)
        author <- c(author,author_cur)
        abstract <- c(abstract,abstract_cur)
        print(paste("Done: ", count_done,'/',n,sep=''))
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
        
        num_stage_cur <- num_stage_cur + 1
        
        print(paste('Start current stage (', num_stage_cur, '/', num_stage, '): ', sep=''))
        print(paste('KeywordA: ',keywordA,'; KeywordB: ',keywordB, '; Database: ', databasename[db]))
        
        if (databaseidx[db] == 1){ ## if databse is sage journal
          keywordA <- str_replace_all(str_trim(keywordA),' ', '+')
          keywordB <- str_replace_all(str_trim(keywordB),' ', '+')
          
          maxsize_db <- 100
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
                  download.file(url_sage_cur, destfile = "scrapedpage_cur.html", quiet=TRUE)
                  page_cur <- read_html("scrapedpage_cur.html")
                  
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
          maxsize_db <- 200
          
          if (sdkey==""){
            sdkey <- readkey()
          }
          
          keywordA_fix <- str_replace_all(str_trim(keywordA),' ', '%20')
          keywordB_fix <- str_replace_all(str_trim(keywordB),' ', '%20')
          
          url_sd <- paste('http://api.elsevier.com/content/search/scidir?apiKey=',
                          sdkey,
                          '&query=tak(',
                          keywordA_fix,
                          ')%20AND%20tak(',
                          keywordB_fix,
                          ')&subscribed=true&oa=true&content=journals&count=',
                          maxsize_db,
                          '&httpaccept=application/xml&view=complete',sep="")
          page <- read_html(url_sd)
          n <- page %>% html_nodes('totalresults') %>% html_text() %>% as.numeric()
          
          if (n > 0){
            data <- getinfo_sd(page,n) %>%
              mutate(searchtermA = keywordA_original, searchtermB = keywordB_original, database = databasename[db])
          }else{
            print(paste('Total results: 0'))
            next
          }
          
          
          #           
          #           
          #           session <- html_session('http://www.sciencedirect.com/science/search')
          #           form <- html_form(session)[[4]]
          #           
          #           # set search keyword --> works
          #           
          #           searchvalue <- form %>% 
          #             set_values(SearchText = keywordA,
          #                        addSearchText = keywordB) 
          #           
          #           # set range for keyword --> works
          #           searchvalue$fields$keywordOpt$value <- form$fields$keywordOpt$options[2]
          #           searchvalue$fields$addkeywordOpt$value <- form$fields$addkeywordOpt$options[2]
          #           
          #           # set checkbox (Subscribed publications: value = '1')
          #           searchvalue$fields[[16]]$checked <- searchvalue$fields[[12]]$checked
          #           
          #           session_result <- submit_form(session,searchvalue)
          #           page <- read_html(session_result$url)
          #           page %>% html_nodes('strong') %>% html_text() # Search results: 5,859
          #           
          #           data <- getinfo_sd(session_result,page) %>%
          #             mutate(searchtermA = keywordA_original, searchtermB = keywordB_original, database = databasename[db])
        }else if (databaseidx[db] == 3){
          
          keywordA_fix <- str_replace_all(str_trim(keywordA),' ', '%20')
          keywordB_fix <- str_replace_all(str_trim(keywordB),' ', '%20')
          
          url_pm <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=[journal]+',
                          keywordA_fix,'[title/abstract]',
                          '+AND+',
                          keywordB_fix,'[title/abstract]',
                          '&retmax=10000',sep='')
          page <- read_html(url_pm)
          
          n <- page %>% html_nodes('count') %>% html_text() %>% as.numeric()
          n <- n[1]
          
          if (n > 0){
            data <- getinfo_pm(page,n) %>%
              mutate(searchtermA = keywordA_original, searchtermB = keywordB_original, database = databasename[db])
          }else{
            print(paste('Total results: 0'))
            next
          }
          
          
        }
        
        data_total <- rbind(data_total,data)
      }
    }
  }
  
  
  if (filterduplication){
    title_tmp <- tolower(gsub( "[^[:alnum:]]", "", data_total$title))
    data_total <- data_total[!duplicated(title_tmp),]
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
databasename <- 'sagejournal'

data_test <- scrape(keywordsA,keywordsB,area,databasename)



# sage journal with full keywords
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- "abstract"
databasename <- 'sage journal'

data_test <- scrape(keywordsA,keywordsB,area,maxsize,databasename)




# science direct with full keywords

t <- proc.time()

keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- "abstract"
databasename <- 'science direct'

data_test <- scrape(keywordsA,keywordsB,area,databasename,sdkey=sdkey)

proc.time() - t






# pubmed with full keywords

t <- proc.time()

keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- 'abstract'
databasename <- c('pubmed')

data_test <- scrape(keywordsA,keywordsB,area,databasename,filterduplication = T)

proc.time() - t




# sage journal, science direct, and pubmed with full keywords

t <- proc.time()

keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- 'abstract'
databasename <- c('sage journal','science direct','pubmed')
data_test <- scrape(keywordsA,keywordsB,area,databasename,filterduplication = T,sdkey=sdkey)

proc.time() - t
