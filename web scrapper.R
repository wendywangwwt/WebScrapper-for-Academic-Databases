

##########################
# main function          #
##########################

scrape <- function(keywordsA, keywordsB, databasename=c("sage journal","science direct","pubmed","proquest"), sdkey="", filterduplication=T,limitpersearch=""){
  
  require(rvest)
  require(tidyverse)
  require(tidytext)
  require(stringr)
  
  subdb_pq <- c('politicalscience',
                'publichealth',
                'psychology',
                'sociology',
                'socscijournals',
                'marketresearch',
                'medline')
  
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
    }else if (tolower(str_replace_all(databasename[i]," ","")) == "proquest"){
      databaseidx[i] <- 4
    }else{
      print(paste(databasename[i],"is not an eligible name, please check it again"))
      return()
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
  
  geturl <- function(databaseidx_cur=1,maxsize_db,keywordA,keywordB,page){
    if (databaseidx_cur == 1){
      pageidx <- ifelse((page-1)==0,"",page-1)
      return(paste("http://journals.sagepub.com/action/doSearch?pageSize=",
                   maxsize_db,
                   "&field1=abstract",
                   "&text1=",
                   keywordA,
                   "&field2=abstract",
                   "&text2=",
                   keywordB,
                   "&ContentItemType=research-article&startPage=",
                   pageidx, sep=""))
    }else if (databaseidx_cur == 2){
      return()
    }
    
  }
  
  getinfo_sj <- function(url,page,n,limitsize){
    maxsize_db <- 100
    
    n <- ifelse(!is.na(limitsize) & (limitsize<n),limitsize,n)
    
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
        
        if ((n_left < maxsize_db) & !is.na(limitsize) & (n == limitsize)){
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
  getinfo_sd <- function(page,n,limitsize){
    maxsize_db <- 200
    
    n <- ifelse(!is.na(limitsize) & (limitsize<n),limitsize,n)
    
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
        
        if ((n_left < maxsize_db) & !is.na(limitsize) & (n == limitsize)){
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
  getinfo_pm <- function(page,n,limitsize){
    maxsize_db <- 200
    
    n <- ifelse(!is.na(limitsize) & (limitsize<n),limitsize,n)
    
    
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
      year <- page_summary %>% html_nodes('datecreated year') %>% html_text() %>% as.numeric()
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
      for (k in 1:num_page){
        print(paste('Current page: ',k,'/', num_page, sep=''))
        
        ids_cur <- if (k < num_page) ids[seq((1+maxsize_db*(k-1)),maxsize_db*k)] else ids[seq((1+maxsize_db*(k-1)),length(ids))]
        url_summary_cur <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=',
                                 paste(ids_cur,collapse=','),
                                 '&rettype=text',sep='')
        page_summary_cur <- read_html(url_summary_cur)
        title_cur <- page_summary_cur %>% html_nodes('articletitle') %>% html_text()
        year_cur <- page_summary_cur %>% html_nodes('datecreated year') %>% html_text() %>% as.numeric()
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
  getinfo_pq <- function(page,n,limitsize,type){    
    n <- ifelse(!is.na(limitsize) & (limitsize<n),limitsize,n)
    
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
    
    closeAllConnections()
    return(data.frame(title = title, author = author, year = year, abstract = abstract, link = link, availability = availability,stringsAsFactors = F))
  }
  
  limitsize <- if (limitpersearch=="") NA else as.numeric(limitpersearch)
  
  num_stage <- length(keywordsA) * length(keywordsB) * num_database
  num_stage_cur <- 0
  data_total <- data.frame(stringsAsFactors = F)
  
  for (db in 1:num_database){
    databaseidx_cur <- databaseidx[db]
    
    for (keywordA in keywordsA){
      for (keywordB in keywordsB){
        closeAllConnections()
        
        keywordA_original <- keywordA
        keywordB_original <- keywordB
        
        num_stage_cur <- num_stage_cur + 1
        
        print(paste('Start current stage (', num_stage_cur, '/', num_stage, '): ', sep=''))
        print(paste0('KeywordA: ',keywordA,'; KeywordB: ',keywordB, '; Database: ', databasename[db]))
        
        if (databaseidx[db] == 1){ ## if databse is sage journal
          keywordA_fix <- str_replace_all(str_trim(keywordA),' ', '+')
          keywordB_fix <- str_replace_all(str_trim(keywordB),' ', '+')
          
          maxsize_db <- 100
          url_sage <- geturl(databaseidx_cur,maxsize_db,keywordA_fix,keywordB_fix,1)
          download.file(url=url_sage, destfile = "scrapedpage.html", quiet=TRUE)
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
            data <- getinfo_sj(url_sage,page,n,limitsize) %>% 
              mutate(searchtermA = keywordA, searchtermB = keywordB, database = databasename[db])
            if (filterduplication){
              title_tmp <- tolower(gsub( "[^[:alnum:]]", "", data$title))
              data <- data[!duplicated(title_tmp),]
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
            data <- getinfo_sd(page,n,limitsize) %>%
              mutate(searchtermA = keywordA_original, searchtermB = keywordB_original, database = databasename[db])
            if (filterduplication){
              title_tmp <- tolower(gsub( "[^[:alnum:]]", "", data$title))
              data <- data[!duplicated(title_tmp),]
            }
          }else{
            print(paste('Total results: 0'))
            next
          }
          
        }else if (databaseidx[db] == 3){
          
          keywordA_fix <- str_replace_all(str_trim(keywordA),' ', '%20')
          keywordB_fix <- str_replace_all(str_trim(keywordB),' ', '%20')
          
          url_pm <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=[journal]+',
                          keywordA_fix,'[title/abstract]',
                          '+AND+',
                          keywordB_fix,'[title/abstract]',
                          '&retmax=10000&sort=relevance',sep='')
          page <- read_html(url_pm)
          
          n <- page %>% html_nodes('count') %>% html_text() %>% as.numeric()
          n <- n[1]
          
          if (n > 0){
            data <- getinfo_pm(page,n,limitsize) %>%
              mutate(searchtermA = keywordA_original, searchtermB = keywordB_original, database = databasename[db])
            if (filterduplication){
              title_tmp <- tolower(gsub( "[^[:alnum:]]", "", data$title))
              data <- data[!duplicated(title_tmp),]
            }
          }else{
            print(paste('Total results: 0'))
            next
          }
          
          
        }else if (databaseidx[db] == 4){
          keywordA_fix <- str_replace_all(str_trim(keywordA),' ', '%20')
          keywordB_fix <- str_replace_all(str_trim(keywordB),' ', '%20')
          
          data <- data.frame(stringsAsFactors = F)
          count_subdb <- 0
          for (subdb in subdb_pq){
            count_subdb <- count_subdb + 1
            print(paste("Current subdatabase: ",subdb," ",count_subdb,"/",length(subdb_pq),sep=""))
            url_pq <- paste0('http://fedsearch.proquest.com/search/sru/',
                             subdb,
                             '?operation=searchRetrieve&version=1.2&maximumRecords=10000&query=abstract%3D"',
                             keywordA_fix,'%20AND%20',keywordB_fix,'"')
            page_pq <- read_html(url_pq)
            n <- page_pq %>% html_nodes('numberofrecords') %>% html_text() %>% as.numeric()
            
            type <- page_pq %>% html_nodes('[tag="513"]') %>% html_text() %>% tolower()
            type <- str_detect(type,"journal|feature|article|case|study|periodical|literature")
            
            n <- length(which(type))
            
            if (n > 0){
              data_subdb <- getinfo_pq(page_pq,n,limitsize,type) %>%
                mutate(searchtermA = keywordA, searchtermB = keywordB, database = databasename[db])
            }else{
              print(paste('Total results: 0'))
              next
            }            
            
            data <- rbind(data,data_subdb) 
          }
          
          if (filterduplication){
            title_tmp <- tolower(gsub( "[^[:alnum:]]", "", data$title))
            data <- data[!duplicated(title_tmp),]
          }
        }
        
        data_total <- rbind(data_total,data)
      }
    }
  }
  
  
  if (filterduplication){
    data_total <- data_total[order(data_total$availability, decreasing = T),]
    title_tmp <- tolower(gsub( "[^[:alnum:]]", "", data_total$title))
    data_total <- data_total[!duplicated(title_tmp),]
  }
  
  if (length(is.na(data_total$availability))>0){
    pubmed_idx <- which(data_total$database==databasename[which(databaseidx==3)])
    count_done <- 0
    print("Collect availability info for the remaining PubMed records")
    for (idx in pubmed_idx){
      page_cur <- read_html(as.character(data_total$link[idx]))
      availability_cur <- page_cur %>% html_nodes('[class="icons portlet"]')
      data_total$availability[idx] <- ifelse(length(availability_cur)>0,"Y","N")
      count_done <- count_done + 1
      print(paste0("Done: ",count_done,"/",length(pubmed_idx)))
    }
    closeAllConnections()
  }

  

  return(data_total)
}



#####################
# test              #
#####################

# sage journal with a few keywords
keywordsA <- c("opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
databasename <- 'sage journal'
data_test <- scrape(keywordsA,keywordsB,databasename,limitpersearch = 300)



# sage journal with full keywords
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
databasename <- 'sage journal'
data_test <- scrape(keywordsA,keywordsB,databasename,limitpersearch = 300)


# sage journal and science direct with a few keywords

t <- proc.time()

keywordsA <- c('defaults','default effect')
keywordsB <- c('decisions','psychology')
databasename <- c('sage journal','science direct')

data_test <- scrape(keywordsA,keywordsB,databasename)

proc.time() - t


# science direct with full keywords

t <- proc.time()

keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
databasename <- 'science direct'

data_test <- scrape(keywordsA,keywordsB,databasename)

proc.time() - t




# sage journal and science direct with full keywords

t <- proc.time()

keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
databasename <- c('sage journal','science direct')

data_test <- scrape(keywordsA,keywordsB,databasename)

proc.time() - t


# pubmed with full keywords

t <- proc.time()

keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
databasename <- c('pubmed')

data_test <- scrape(keywordsA,keywordsB,databasename,filterduplication = T,sdkey=sdkey)

# data_test <- data_test[!duplicated(data_test$title),]
proc.time() - t


# sage journal, science direct, and pubmed with full keywords

t <- proc.time()

keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
databasename <- c('sage journal','science direct','pubmed')

data_test <- scrape(keywordsA,keywordsB,databasename,filterduplication = T,sdkey=sdkey,limitpersearch = "")

proc.time() - t



# proquest, pubmed with full keywords

t <- proc.time()

keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
databasename <- c('proquest','pubmed')

data_test <- scrape(keywordsA,keywordsB,databasename,filterduplication = T,sdkey=sdkey,limitpersearch = 500)

proc.time() - t


