setwd("E:/d/Columbia/QMSS/CDS/Shannon Database Search")

library(rvest)
library(tidyverse)
library(stringr)


##########################
# Preparation & Settings #
##########################

## data storage initilize
title <- c()
author <- c()
abstract <- c()
link <- c()
searchwordA <- c()
searchwordB <- c()
searcharea <- c()
database <- c()

## search terms / area
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- "abstract"

## maxsize
maxsize_sj <- 100



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
  if (tolower(str_replace_all(databasename," ","")) == "sagejournal"){
    databse = 1
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
      
      link_cur <- html_nodes(page,xpath = paste('//*[@id="frmSearchResults"]/ol/li[',i,']/article/div[4]/a[1]',sep='')) %>% html_attrs() %>% unlist()
      names(link_cur) <- c() # clean the tag name
      link_cur <- paste('http://journals.sagepub.com',str_trim(rm_white(link_cur[2])),sep='') # the second element is href
      
      page_full <- read_html(link_cur)
      
      abstract_cur <- html_nodes(page_full, xpath = '//*[@id="25e8d4fe-c5d2-4686-bdef-3b125e01db4b"]/div/div[2]/div/div/div[1]/article/div[1]/div[2]') %>% html_text()
      if (length(abstract_cur)==0){
          <- html_nodes(page_full,xpath = '//*[@id="25e8d4fe-c5d2-4686-bdef-3b125e01db4b"]/div/div[2]/div/div/div[1]/article/div[1]/div[2]/p') %>% html_text()
      }
      
      title <- c(title, title_cur)
      author <- c(author, author_cur)
      link <- c(link, link_cur)
      abstract <- c(abstract, abstract_cur)
      
      print(paste("Done: ",i,"/",n,sep=''))
    }
    return(data.frame(title = title, author = author, abstract = abstract, link = link))
  }
  
  for (db in 1:num_database){
    for (keywordA in keywordsA){
      for (keywordB in keywordsB){
        maxsize_db <- maxsize[db]
        
        if (database == 1){ ## if databse is sage journal
          url_sage <- paste("http://journals.sagepub.com/action/doSearch?pageSize=",
                            maxsize_db,
                            "&field1=",
                            area,
                            "&text1=",
                            keywordA,
                            "&field2=",
                            area,
                            "&text2=",
                            keywordB,
                            "&ContentItemType=research-article", sep="")
          
          # http://journals.sagepub.com/action/doSearch?pageSize=100&field1=abstract&text1=default&field2=abstract&text2=decisions&startPage=&ContentItemType=research-article
          
          page <- read_html(url_sage)
          
          n <- page %>%
            html_nodes(xpath = '//*[@id="searchResultContainer"]/div[1]/ul/li/a/span/text()') %>% 
            html_text() %>%
            str_sub(2,-2) %>% # delete parenthese
            as.numeric()
          
          if (n <= maxsize_db){
            data <- getinfo_sj(page,n) %>% 
              mutate(searchtermA = keywordA, searchtermB = keywordB, database = databasename[db])
          }else{
            num_page = n %/% maxsize_db + 1
            for (k in 1:num_page){
              if (k == 1){
                data <- getinfo_sj(page,n)
              }else {
                url_sage_cur <- paste("http://journals.sagepub.com/action/doSearch?pageSize=",
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
                                      k-1, sep="")
                page_cur <- read_html(url_sage_cur)
                
                if (k < num_page){
                  data_cur <- getinfo_sj(page_cur,maxsize_db)
                  data <- rbind(data,data_cur)
                }else if (k == num_page){
                  num_cur <- n %% maxsize_db
                  data_cur <- getinfo_sj(page_cur,num_cur)
                  data <- rbind(data,data_cur) %>% 
                    mutate(searchtermA = keywordA, searchtermB = keywordB, database = databasename[db])
                }
              }
            }
          }
        }
      }
      
  }
    }
}

# Sage Journals
# http://journals.sagepub.com/action/doSearch?content=articlesChapters&countTerms=true&pageSize=100&target=default&field1=Abstract&text1=Default+AND+Decisions&field2=AllField&text2=&Ppub=&Ppub=&AfterYear=&BeforeYear=&access=
# http://journals.sagepub.com/action/doSearch?field1=Abstract&text1=default+effect&field2=Abstract&text2=decision-making&Ppub=&Ppub=&AfterYear=&BeforeYear=&access=

data <- data.frame(title = title,
                   author = author,
                   abstract = abstract,
                   link = link)

#############################
#  Database: Sage Journal   #
#############################

## settings

maxsize <- maxsize_sj

# Sage Journals
# "http://journals.sagepub.com/action/doSearch?content=articlesChapters&countTerms=true&pageSize=100&target=default&field1=Abstract&text1=Default+AND+Decisions&field2=AllField&text2=&Ppub=&Ppub=&AfterYear=&BeforeYear=&access="



for (keywordA in keywordsA){
  for (keywordB in keywordsB){
    url_sage <- paste("http://journals.sagepub.com/action/doSearch?content=articlesChapters&countTerms=true&pageSize=",
                      maxsize,
                      "&target=default&field1=",
                      area,
                      "&text1=",
                      keywordA,
                      "+AND+",
                      keywordB,
                      "&field2=AllField&text2=&Ppub=&Ppub=&AfterYear=&BeforeYear=&access=", sep="")
    page <- read_html(url_sage)
    
    n <- page %>%
      html_nodes(xpath = '//*[@id="searchResultContainer"]/div[1]/ul/li/a/span/text()') %>% 
      html_text() %>%
      str_sub(2,-2) %>% # delete parenthese
      as.numeric()
    
    if (n <= maxsize){
      for (i in 1:n){
        
      }
    }
    test <- page %>%
      html_nodes(".all , .header .entryAuthor , .hlFld-Title .nowrap") %>%
      html_text()
    
     <- page %>%
      paste(html_nodes(page,xpath = '//*[@id="frmSearchResults"]/ol/li[1]/article/div[1]/a/text()') %>% html_text(), 
            html_nodes(page,xpath ='//*[@id="frmSearchResults"]/ol/li[1]/article/div[1]/a/span/text()') %>% html_text(), sep="")
  }
}

DV <- rnorm(100)
IV1 <- sample(1:16, 100, replace=T)
IV2 <- sample(1:2, 100 ,replace = T)
IV3 <- sample(1:2, 100, replace = T)

ggplot() + 
  geom_point(aes(x=IV1,y=DV,color=as.factor(IV2),shape=as.factor(IV3))) +
  geom_smooth(aes(x=IV1,y=DV,color=as.factor(IV2),shape=as.factor(IV3)),se=F)