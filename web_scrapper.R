library(rvest)
library(tidyverse)
library(tidytext)
library(stringr)
library(glue)





l_field <- list('sage_journal'=list('abstract'='abstract',
                                    'all'='allfield'),
                'science_direct'=list('abstract'='tak',
                                      'all'='all'),
                'pubmed'=list('abstract'='[title/abstract]',
                              'all'=''),
                'proquest'=list('abstract'='abstract%3D',
                                'all'='')
                
)


subdb_pq <- c('politicalscience',
              'publichealth',
              'psychology',
              'sociology',
              'socscijournals',
              'marketresearch',
              'medline')




##########################
# util function          #
##########################

readkey <- function(){ 
  key <- readline(prompt="Please enter your API key for Science Direct (if you don't have one, get it from here: https://dev.elsevier.com/user/login): ")
  while (nchar(str_trim(key))<25){
    print("The length of key seems wrong")
    key <- readline(prompt="Please enter your API key for Science Direct (if you don't have one, get it from here: https://dev.elsevier.com/user/login): ")
  }
  return(key)
}

#' Encode keywords. For keywords that have special character such as space or hyphen.
#' @param keyword A string of one keyword, or a vector of keywords. When a vector
#'   of multiple keywords are provided, this is a relationship="and" case.
encode_keyword <- function(keyword,db){
  res <- c()
  
    for (kw in keyword){
      if (db == 'sage_journal'){
        res <- c(res, str_replace_all(str_trim(kw),' ', '+'))
      }else if (db == 'science_direct'){
        res <- c(res, URLencode(str_trim(kw)))
      }else if (db == 'pubmed'){
        res <- c(res, URLencode(str_trim(kw)))
      }else if (db=='proquest'){
        res <- c(res, URLencode(str_trim(kw)))
      }
    }
  
  return(res)
}

#' @param keyword A string of one keyword, or a vector of keywords. When a vector
#'   of multiple keywords are provided, this is a relationship="and" case, and
#'   the request asks for the result where ALL keywords appear in the same article.
#' @param api_key A parameter only used by science direct.
#' @param subdb A parameter only used by proquest.
get_url <- function(keyword,field,db,api_key=NULL,subdb=NULL,start_record=NULL,verbose=0){
  field_name <- l_field[[db]][[field]]
  
  if (db == 'sage_journal'){
    
    res <- glue("http://journals.sagepub.com/action/doSearch?pageSize=100")
    
    for (i in 1:length(keyword)){
      res <- glue("{res}&field{i}={field_name}&text{i}={keyword[i]}")
    }
    
    res <- glue("{res}&ContentItemType=research-article&startPage=1")
    
  }else if (db == 'science_direct'){
    query <- glue('{field_name}({keyword})')
    res <- glue("http://api.elsevier.com/content/search/scidir?apiKey={api_key}&query={paste(query,collapse='%20AND%20')}&subscribed=true&oa=true&content=journals&count={maxsize_db}&httpaccept=application/xml&view=complete")
  }else if (db == 'pubmed'){
    query <- glue('{keyword}{field_name}')
    res <- glue("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=[journal]+{paste(query,collapse='+AND+')}&retmax=10000&sort=relevance")
  }else if (db == 'proquest'){
    query <- paste(keyword,collapse='%20AND%20')
    if (is.null(start_record)){
      res <- glue('http://fedsearch.proquest.com/search/sru/{subdb}?operation=searchRetrieve&version=2&maximumRecords=10000&query={field_name}"{query}"')
    }else{
      res <- glue('http://fedsearch.proquest.com/search/sru/{subdb}?operation=searchRetrieve&version=1.2&maximumRecords=10000&startRecord={start_record}&query={field_name}"{query}"')
    }
  }
  
  if (verbose > 0){
    print(res)
  }
  return(res)
}



get_info_sj <- function(url,page,n,limit_per_search){
  maxsize_db <- 100
  
  n <- ifelse(!is.null(limit_per_search) & (limit_per_search<n),limit_per_search,n)
  
  print(paste('Total results: ', n, sep=''))
  
  availability <- c()
  
  if (n <= maxsize_db){
    print(paste('Total pages: 1'))
    print(paste('Current page: 1/1'))
    title <- page %>% html_nodes('li article.searchResultItem') %>% html_attr('data-title')
    link <- paste('http://journals.sagepub.com',page %>% html_nodes('.hlFld-Title .nowrap ') %>% html_attr('href'),sep="")
    year <- page %>% html_nodes('.maintextleft ') %>% html_text()
    year <- str_sub(year[str_detect(year,'First Published')],-5,-2) %>% as.numeric()
    
    article_nodes <- page %>% html_nodes('li article.searchResultItem')
    
    author <- c()
    abstract <- c()
    count_done <- 0
    
    for (article_node in article_nodes){
      author <- c(author,paste(article_node %>% html_nodes('div.authorLayer  div.header a') %>% html_text(),collapse = ","))
      url_abs <- paste('http://journals.sagepub.com',article_node %>% html_nodes('.nowrap.abstract') %>% html_attr('href'),sep="")
      availability_cur <- article_node %>% html_nodes('[class="ref nowrap pdf"]')
      availability <- c(availability,ifelse(length(availability_cur)>0,"Y","N"))
      if (nchar(url_abs) > nchar("http://journals.sagepub.com")){
        page_abs <- read_html(url_abs)
        abstract_cur <- page_abs %>% html_nodes('.abstractSection') %>% html_text()
        if (length(abstract_cur)>0){
          abstract <- c(abstract,abstract_cur)
        }else{
          abstract <- c(abstract,"No abstract")
        }
      }else{
        url_abs <- paste('http://journals.sagepub.com',article_node %>% html_nodes('.nowrap.abs') %>% html_attr('href'),sep="")
        if (nchar(url_abs)>nchar("http://journals.sagepub.com")){
          page_abs <- read_html(url_abs)
          abstract_cur <- page_abs %>% html_nodes('.abstractSection') %>% html_text()
          if (length(abstract_cur)>0){
            abstract <- c(abstract,abstract_cur)
          }else{
            abstract <- c(abstract,"No abstract")
          }
        }else{
          abstract <- c(abstract,"No abstract")
        }
      }
      count_done = count_done + 1
      print(paste("Done: ",count_done,"/",n,sep=''))
    }
  }else{
    num_page = ifelse(n %% maxsize_db == 0, n %/% maxsize_db,n %/% maxsize_db + 1)
    print(paste('Total pages: ', num_page, sep=''))
    
    title <- c()
    link <- c()
    author <- c()
    abstract <- c()
    year <- c()
    
    count_done = 0
    
    page_cur <- page
    for (k in 1:num_page){
      print(paste('Current page: ',k,'/', num_page, sep=''))
      
      if (k > 1){
        url_cur <- paste(url,k-1,sep="")
        page_cur <- read_html(url_cur)
      }
      
      n_left <- n - (k-1)*maxsize_db
      
      title_cur <- page_cur %>% html_nodes('li article.searchResultItem') %>% html_attr('data-title')
      link_cur <- paste('http://journals.sagepub.com',page_cur %>% html_nodes('.hlFld-Title .nowrap ') %>% html_attr('href'),sep="")
      year_cur <- page_cur %>% html_nodes('.maintextleft ') %>% html_text()
      year_cur <- str_sub(year_cur[str_detect(year_cur,'First Published')],-5,-2) %>% as.numeric()
      article_nodes_cur <- page_cur %>% html_nodes('li article.searchResultItem')
      
      if ((n_left < maxsize_db) & !is.null(limit_per_search) & (n == limit_per_search)){
        title <- c(title,title_cur[1:n_left])
        link <- c(link,link_cur[1:n_left])
        year <- c(year,year_cur[1:n_left])
        article_nodes_cur <- article_nodes_cur[1:n_left]
      }else{
        title <- c(title,title_cur)
        link <- c(link,title_cur)
        year <- c(year,year_cur)
      }
      
      author_cur <- c()
      abstract_cur <- c()
      
      for (article_node_cur in article_nodes_cur){
        author_cur <- c(author_cur,paste(article_node_cur %>% html_nodes('div.authorLayer  div.header a') %>% html_text(),collapse = ","))
        url_abs_cur <- paste('http://journals.sagepub.com',article_node_cur %>% html_nodes('.nowrap.abstract') %>% html_attr('href'),sep="")
        availability_cur <- article_node_cur %>% html_nodes('[class="ref nowrap pdf"]')
        availability <- c(availability,ifelse(length(availability_cur)>0,"Y","N"))
        if (nchar(url_abs_cur) > nchar("http://journals.sagepub.com")){
          page_abs_cur <- read_html(url_abs_cur)
          abstract_cur_now <- paste(page_abs_cur %>% html_nodes('.abstractSection') %>% html_text(),collapse = " ")
          if (length(abstract_cur_now)>0){
            abstract_cur <- c(abstract_cur,abstract_cur_now)
          }else{
            abstract_cur <- c(abstract_cur,"No abstract")
          }
        }else{
          url_abs_cur <- paste('http://journals.sagepub.com',article_node_cur %>% html_nodes('.nowrap.abs') %>% html_attr('href'),sep="")
          if (nchar(url_abs_cur)>nchar("http://journals.sagepub.com")){
            page_abs_cur <- read_html(url_abs_cur)
            abstract_cur_now <- paste(page_abs_cur %>% html_nodes('.abstractSection') %>% html_text(),collapse = " ")
            if (length(abstract_cur_now)>0){
              abstract_cur <- c(abstract_cur,abstract_cur_now)
            }else{
              abstract_cur <- c(abstract_cur,"No abstract")
            }
          }else{
            abstract_cur <- c(abstract_cur,"No abstract")
          }
        }
        
        count_done = count_done + 1
        print(paste("Done: ",count_done,"/",n,sep=''))
      }
      
      author <- c(author,author_cur)
      abstract <- c(abstract,abstract_cur)
    }
  }
  return(data.frame(title = title, year = year, author = author, abstract = abstract, link = link, availability = availability,stringsAsFactors = F))
}
get_info_sd <- function(page,n,limit_per_search){
  maxsize_db <- 200
  
  n <- ifelse(!is.null(limit_per_search) & (limit_per_search<n),limit_per_search,n)
  
  print(paste('Total results: ', n, sep=''))
  
  
  if (n <= maxsize_db){
    title <- page %>% html_nodes('title') %>% html_text()
    link <- page %>% html_nodes('entry link[ref="scidir"]') %>% html_attr('href')
    year <- page %>% html_nodes('coverdate') %>% html_text() %>% str_sub(1,4) %>% as.numeric()
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
    num_page = ifelse(n %% maxsize_db == 0, n %/% maxsize_db,n %/% maxsize_db + 1)
    
    print(paste('Total pages: ', num_page, sep=''))
    
    title <- c()
    author <- c()
    abstract <- c()
    link <- c()
    year <- c()
    count_done <- 0
    
    page_cur <- page
    for (k in 1:num_page){
      print(paste('Current page: ',k,'/', num_page, sep=''))
      
      n_left <- n - (k-1)*maxsize_db
      
      if(k>1){
        url_cur <- page_cur %>% html_nodes('link[ref="next"]') %>% html_attr('href')
        page_cur <- read_html(url_cur)
      }
      
      title_cur <- page_cur %>% html_nodes('title') %>% html_text()
      link_cur <- page_cur %>% html_nodes('entry link[ref="scidir"]') %>% html_attr('href')
      year_cur <- page_cur %>% html_nodes('coverdate') %>% html_text() %>% str_sub(1,4) %>% as.numeric()
      article_nodes_cur <- page_cur %>% html_nodes('entry')
      
      if ((n_left < maxsize_db) & !is.null(limit_per_search) & (n == limit_per_search)){
        title <- c(title, title_cur[1:n_left])
        link <- c(link, link_cur[1:n_left])
        year <- c(year, year_cur[1:n_left])
        article_nodes_cur <- article_nodes_cur[1:n_left]
      }else{
        title <- c(title, title_cur)
        link <- c(link, link_cur)
        year <- c(year, year_cur)
      }
      
      
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
  
  return(data.frame(title = title, author = author,year=year, abstract = abstract, link = link, availability = rep("Y",length(title)),stringsAsFactors = F))
}
get_info_pm <- function(page,n,limit_per_search){
  maxsize_db <- 200
  
  n <- ifelse(!is.null(limit_per_search) & (limit_per_search<n),limit_per_search,n)
  
  ids <- page %>% html_nodes('id') %>% html_text()
  ids <- ids[1:n]
  
  link <- paste('https://www.ncbi.nlm.nih.gov/pubmed/',ids,sep='')
  
  if (n <= maxsize_db){
    print(paste('Total results: ', n, sep=''))
    url_summary <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=',
                         paste(ids,collapse=','),
                         '&rettype=text',sep='')
    page_summary <- read_html(url_summary)
    title <- page_summary %>% html_nodes('articletitle') %>% html_text()
    year <- page_summary %>% html_nodes('pubmedpubdate[pubstatus="pubmed"] year') %>% html_text() %>% as.numeric()
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
    num_page = ifelse(n %% maxsize_db == 0, n %/% maxsize_db,n %/% maxsize_db + 1)
    
    print(paste('Total results: ', n, sep=''))
    print(paste('Total pages: ', num_page, sep=''))
    
    title <- c()
    author <- c()
    abstract <- c()
    year <- c()
    count_done <- 0
    time_query <- Sys.time()
    
    for (k in 1:num_page){
      print(paste('Current page: ',k,'/', num_page, sep=''))
      
      # rate limit control; for non-apikey access, rate limit is 3 queries per sec
      while (Sys.time()-time_query < 0.5){
        Sys.sleep(0.1)
      }
      
      time_query <- Sys.time()
      ids_cur <- if (k < num_page) ids[seq((1+maxsize_db*(k-1)),maxsize_db*k)] else ids[seq((1+maxsize_db*(k-1)),length(ids))]
      url_summary_cur <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=',
                               paste(ids_cur,collapse=','),
                               '&rettype=text',sep='')
      page_summary_cur <- read_html(url_summary_cur)
      title_cur <- page_summary_cur %>% html_nodes('articletitle') %>% html_text()
      year_cur <- page_summary_cur %>% html_nodes('pubmedpubdate[pubstatus="pubmed"] year') %>% html_text() %>% as.numeric()
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
      year <- c(year,year_cur)
      print(paste("Done: ", count_done,'/',n,sep=''))
      
      

    }
  }
  closeAllConnections()
  return(data.frame(title = title, author = author, year = year, abstract = abstract, link = link, availability = rep(NA,length(title)),stringsAsFactors = F))
}
get_info_pq <- function(page,n,limit_per_search,type){    
  n <- ifelse(!is.null(limit_per_search) & (limit_per_search<n),limit_per_search,n)
  
  print(paste('Total results: ', n, sep=''))
  
  title <- page %>% html_nodes('[tag="245"]') %>% html_text()
  title <- title[type][1:n]
  year <- page %>% html_nodes('[tag="260"] [code="c"]') %>% html_text()
  year <- str_sub(year[type][1:n],-4,-1)
  abstract <- page %>% html_nodes('[tag="520"]') %>% html_text() %>% str_trim()
  abstract <- str_replace(abstract[type][1:n],"\n","")
  link <- page %>% html_nodes('[ind2="1"] [code="u"]') %>% html_text()
  link <- link[type][1:n]
  
  author <- c()
  availability <- c()
  count_done <- 0
  page_cur <- page %>% html_nodes('recorddata')
  
  for (i in 1:length(type)){
    if (count_done < limit_per_search){
      if (type[i]){
        author_fullname <- page_cur[i] %>% html_nodes('[tag="100"]') %>% html_text() %>% str_split_fixed(",",2)
        author_cur <- paste(author_fullname[2],author_fullname[1])
        
        author_other <- page_cur[i] %>% html_nodes('[tag="700"]')
        if (length(author_other)>0){
          for (j in 1:length(author_other)){
            author_fullname <- author_other[j]  %>% html_text() %>% str_split_fixed(",",2)
            author_cur <- paste(author_cur,paste(author_fullname[2],author_fullname[1]),sep=",")
          }
        }
        author <- c(author, str_trim(author_cur))
        
        availability_cur <- page_cur[i] %>% html_nodes('[tag="856"]') %>% html_text()
        availability <- c(availability,ifelse(str_detect(paste(availability_cur,collapse = " "),"Full Text"),"Y","N"))
        count_done <- count_done + 1
        print(paste("Done: ", count_done,'/',n,sep=''))
      }
    }
    
  }
  
  closeAllConnections()
  return(data.frame(title = title, author = author, year = year, abstract = abstract, link = link, availability = availability,stringsAsFactors = F))
}


fetch_and_clean_results <- function(keyword,field,db,sdkey,no_duplicate,limit_per_search){

  closeAllConnections()
  
  keyword_original <- paste(keyword,collapse = ', ') # concatenate keywords if multiple
  keyword_encoded <- encode_keyword(keyword,db)
  
  
  if (db == 'sage_journal'){
    maxsize_db <- 100
    
    url_sj <- get_url(keyword_encoded,field,db)
    download.file(url=url_sj, destfile = "scrapedpage.html", quiet=TRUE)
    page <- read_html("scrapedpage.html")
    closeAllConnections()
    
    n <- page %>%
      html_nodes(xpath = '//*[@id="searchResultContainer"]/div[1]/ul/li/a/span/text()') %>% 
      html_text() %>%
      str_sub(2,-2) %>% # delete parenthese
      as.numeric()
    
    if (length(n) == 0){
      print(paste('Total results: 0'))
      next
    }else{
      df_db <- get_info_sj(url_sj,page,n,limit_per_search) %>% 
        mutate(search_term = keyword_original, database = db)
      if (no_duplicate){
        title_tmp <- tolower(gsub( "[^[:alnum:]]", "", df_db$title))
        df_db <- df_db[!duplicated(title_tmp),]
      }
    }
    
    
  }else if (db == 'science_direct'){
    maxsize_db <- 200
    
    if (sdkey==""){
      sdkey <- readkey()
    }
    
    url_sd <- get_url(keyword_encoded,field,db,api_key=sdkey)
    
    page <- read_html(url_sd)
    n <- page %>% html_nodes('totalresults') %>% html_text() %>% as.numeric()
    
    if (n > 0){
      df_db <- get_info_sd(page,n,limit_per_search) %>%
        mutate(search_term = keyword_original, database = db)
      if (no_duplicate){
        title_tmp <- tolower(gsub( "[^[:alnum:]]", "", df_db$title))
        df_db <- df_db[!duplicated(title_tmp),]
      }
    }else{
      print(paste('Total results: 0'))
      next
    }
    
  }else if (db == 'pubmed'){
    
    url_pm <- get_url(keyword_encoded,field,db)
    
    page <- read_html(url_pm)
    
    n <- page %>% html_nodes('count') %>% html_text() %>% as.numeric()
    n <- n[1]
    
    if (n > 0){
      df_db <- get_info_pm(page,n,limit_per_search) %>%
        mutate(search_term = keyword_original, database = db)
      if (no_duplicate){
        title_tmp <- tolower(gsub( "[^[:alnum:]]", "", df_db$title))
        df_db <- df_db[!duplicated(title_tmp),]
      }
    }else{
      print(paste('Total results: 0'))
      next
    }
    
    
  }else if (db == 'proquest'){
    
    df_db <- data.frame(stringsAsFactors = F)
    count_subdb <- 0
    for (subdb in subdb_pq){
      count_subdb <- count_subdb + 1
      print(paste("Current subdatabase: ",subdb," ",count_subdb,"/",length(subdb_pq),sep=""))
      
      url_pq <- get_url(keyword,field,db)
      print(url_pq)
      page_pq <- read_html(url_pq)
      n <- page_pq %>% html_nodes('numberofrecords') %>% html_text() %>% as.numeric()
      
      type <- page_pq %>% html_nodes('[tag="513"]') %>% html_text() %>% tolower()
      
      count_done <- 0
      if (n > 0 & n > length(type)){ # in some examples the server only returns 1000 records per request
        num_page <- ceiling(n / length(type))
        print(paste0('Total pages: ', num_page))
        df_subdb <- data.frame()
        for (i in 1:num_page){
          print(paste0('Current page: ', i,'/',num_page))
          
          url_pq_cur <- get_url(keyword,field,db,start_record=length(type)*(i-1)+1)
          
          page_pq_cur <- read_html(url_pq_cur)
          type_cur <- page_pq_cur %>% html_nodes('[tag="513"]') %>% html_text() %>% tolower()
          type_cur <- str_detect(type_cur,"journal|feature|article|case|study|periodical|literature|statistics")
          
          if (count_done + sum(type_cur) <= limit_per_search){
            count_done <- count_done + sum(type_cur)
            df_subdb_cur <- get_info_pq(page_pq_cur,sum(type_cur),limit_per_search,type_cur) %>%
              mutate(search_term = keyword_original, database = db)
            df_subdb <- rbind(df_subdb,df_subdb_cur)
          }else{
            df_subdb_cur <- get_info_pq(page_pq_cur,sum(type_cur),limit_per_search - count_done,type_cur) %>%
              mutate(search_term = keyword_original, database = db)
            df_subdb <- rbind(df_subdb,df_subdb_cur)
            print('Finish collecting required amount of records.')
            break
          }
        }
      }else{
        type <- str_detect(type,"journal|feature|article|case|study|periodical|literature|statistics")
        
        n <- length(which(type))
        
        if (n > 0){
          df_subdb <- get_info_pq(page_pq,n,limit_per_search,type) %>%
            mutate(search_term = keyword_original, database = db)
        }else{
          print('Total results: 0')
          next
        }           
      }
      
      df_db <- rbind(df_db,df_subdb) 
    }
    
    if (no_duplicate){
      title_tmp <- tolower(gsub( "[^[:alnum:]]", "", df_db$title))
      df_db <- df_db[!duplicated(title_tmp),]
    }
  }
  
  return(df_db)
  
}


##########################
# main function          #
##########################




#' Search Specified Databases
#' 
#' This is the main function to be used. This function searches the specified
#' databases using the keywords provided.
#' 
#' @param keywords Required. A string of one keyword or a vector of multiple keywords.
#' @param relationship Relationship between the keywords. Can be either `or` or 
#'   `and`. Default to `or`. When using `or`, the function runs a search for each 
#'   and every keyword independently. When using `and`, the function runs one search
#'   that combines all the keywords.
#' @param database_name A string of one database name or a vector of multiple
#'   database names. Default to `sage_journal`. Currently support:
#'   `sage_journal`, `science_direct`, `pubmed`, `proquest`
#'   If both correct and wrong database names are specified, only the correct ones
#'   will be queried.
#' @param field The field to search for the keywords. Currently support:
#'   `abstract`, `all`
#'   Some databases may not support some field to be used in the search.
#' @param sdkey API key for science direct.
#' @param no_duplicate Whether to remove duplicated results (TRUE) or not (FALSE).
#'   One article could be returned by multiple databases, if you specified multiple
#'   databases to search in `database_name`.
#' @param limit_per_search The maximum number of results collected from each
#'   database.
#' @param availability_pubmed Whether query to see if the full text of an article
#'   is available to you (TRUE) or not (FALSE). Default to FALSE. Because it has
#'   to check each article one by one, it can be time consuming and can hit the
#'   3 queries / sec limit.
search_databases <- function(keywords,
                             relationship='or',
                             field = 'abstract',
                             database_name=c("sage_journal","science_direct","pubmed","proquest"), 
                             sdkey="", 
                             no_duplicate=T,limit_per_search=NULL,availability_pubmed=F){
  
  vals_relationship <- c('or','and')
  vals_field <- c('abstract','all')
  vals_database_name <- c("sage_journal","science_direct","pubmed","proquest")
  
  relationship <- match.arg(relationship,vals_relationship)
  field <- match.arg(field,vals_field)
  database_name <- match.arg(database_name,vals_database_name,several.ok=T)
  
  num_database <- length(database_name)
  num_keyword <- length(keywords)
  
  if (relationship=='and'){
    num_stage <- 1 * num_database
  }else{
    num_stage <- num_keyword * num_database
  }
  
  num_stage_cur <- 0
  df_total <- data.frame(stringsAsFactors = F)
  
  for (db in database_name){
    
    if (relationship=='and'){
      num_stage_cur <- num_stage_cur + 1
      
      print(glue('Start current stage ({num_stage_cur}/{num_stage}):'))
      print(glue('Keyword: {paste(keywords,collapse=", ")}; Database: {db}; Field: {field}'))
      
      df_db <- fetch_and_clean_results(keywords,field,db,sdkey,no_duplicate,limit_per_search)
    }else{
      df_db <- data.frame(stringsAsFactors = F)
      
      for (keyword in keywords){
        num_stage_cur <- num_stage_cur + 1
        
        print(glue('Start current stage ({num_stage_cur}/{num_stage}):'))
        print(glue('Keyword: {keyword}; Database: {db}; Field: {field}'))
        
        df_db_kw <- fetch_and_clean_results(keyword,field,db,sdkey,no_duplicate,limit_per_search)
        df_db <- rbind(df_db,df_db_kw)
      }
    }
    
    df_total <- rbind(df_total,df_db)
  }
  
  
  if (no_duplicate){
    df_total <- df_total[order(df_total$availability, decreasing = T),]
    title_tmp <- tolower(gsub( "[^[:alnum:]]", "", df_total$title))
    df_total <- df_total[!duplicated(title_tmp),]
  }
  
  if ('pubmed' %in% database_name){
    if (availability_pubmed){
      if (length(is.na(df_total$availability))>0){
        pubmed_idx <- which(df_total$database=='pubmed')
        count_done <- 0
        print("Collect availability info for the remaining PubMed records")
        for (idx in pubmed_idx){
          page_cur <- read_html(as.character(df_total$link[idx]))
          availability_cur <- page_cur %>% html_nodes('[class="icons portlet"]')
          df_total$availability[idx] <- ifelse(length(availability_cur)>0,"Y","N")
          count_done <- count_done + 1
          print(glue("Done: {count_done}/{length(pubmed_idx)}"))
        }
        closeAllConnections()
      }
    }
  }
  
  return(df_total)
}


