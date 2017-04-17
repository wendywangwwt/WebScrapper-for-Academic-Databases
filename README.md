# WebScrapper-for-Academic-Databases

## Intention
Getting papers for meta analysis is always annoying: you need to try various combination of different keywords in various of databases, and it's very likely for you to come across with some paper you have met with in a previous database. Manually doing this is so inefficient.

## What I try to do
I'm starting to create a web scrapper for academic databases in R in the form of a (set of) function.

## Tool Summary

- Scrape databases

  - Sage Journal
    - approach to get the page: merge needed arguments into a href link (having an argument to change page)
    - approach to filter out info: xpath
    - 4 types of info are collected: title, author, abstract, link
    - no bug till now, can handle paper without abstract listed
    - the only problem is it runs slow (searching for the 4 keywords in group A and 3 keywords in group B (12 pairs in total, as Shannon listed) takes about 40 or 50 minutes).. currently I have no plan to optimize the code since you can run the function and put it aside for hours.

  - Science Direct
    - approach to get the page: merge needed arguments into a href link (get feedback from API, max number of article per page has a limit of 200)
    - approach to filter out info: css selector
    - 4 types of info are collected: title, author, abstract, link
    - no bug till now, can handle paper without abstract listed
    - runs very fast as searching and fetching through API links (12 pairs of test keywords take about 70 seconds)

  - PubMed
    - approach to get the page: merge needed arguments into a href link (get feedback from API, max number of article per page is set to be 10000)
    - approach to filter out info: css selector
    - 4 types of info are collected: title, author, abstract, link
    - no bug till now, can handle paper without abstract listed
    - runs very fast as searching and fetching through API links (12 pairs of test keywords take about 60 seconds)


- Rank or filter the result

  - tf-idf
    - tf-idf was tried on the result of Sage Journal (using the 2 groups of keywords listed by Shannon) to rank or score the relatedness or keyword importance of the articles using their abstract. It's calculated on unigram (single word) while some of our keywords are bigram (e.g., consumer behavior), I'm considering to add some constraints on the score and rank them again (e.g., the socre of 'consumer' and that of 'behavior' is meaningful only when both of them occur in the abstract).

  - Latent Dirichlet Allocation
    - I'm also considering to use this common topic modeling method to model the topic of the abstracts and see if it can help.

  - Doc2Vec neural network
    - a novel way to cluster documents. I may try it in the summer:p
    

## Reference
### API
- Sage Journal
  - [Argument](http://api.elsevier.com/documentation/search/SCIDIRSearchTips.htm)
   
- PubMed
  - [Walk Through](https://dataguide.nlm.nih.gov/eutilities/how_eutilities_works.html)
  - [Argument](https://dataguide.nlm.nih.gov/eutilities/utilities.html#esearch)
   
    
    
## Update Details
April 17:
- Science Direct
  - changed the approach, now api is used to search and fetch data
  - 4 types of info are collected: title, author, abstract , link
  - no bug found
  - speed up a lot by using api
  - examples tested:
```
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- "abstract"
databasename <- 'science direct'

data_test <- scrape(keywordsA,keywordsB,area,databasename)
```

April 13:
- PubMed
  - can search and fetch data
  - 4 types of info are collected: title, author, abstract , link
  - no bug found
  - examples tested:
```
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- 'abstract'
databasename <- c('pubmed')

data_test <- scrape(keywordsA,keywordsB,area,databasename)
```

- Overall
  - deleted function argument "maxsize"; max number of article per page now is set as default (sage journal 100, science direct 200, pubmed 10000 (because of API) )

April 10:
- Science Direct
  - fixed no abstract situation
  - in addtion to closeallconnections(), download a html file first then read and scrape it, in order to avoid 'time reachout' error

- PubMed
  - built skeleton to search and fetch data


April 6:
- Science Direct (submit search form)
  - 4 types of info are collected: title, author, abstract , link

- Overall
  - can search and collect data from multiple databases
  - examples tested:
 ```
keywordsA <- c('defaults','default effect')
keywordsB <- c('decisions','psychology')
area <- 'abstract'
maxsize <- c(100,0)
databasename <- c('sage journal','science direct')

data_test <- scrape(keywordsA,keywordsB,area,maxsize,databasename)
```

- Bug
  - when using form submission, it's not clear how to mark checkboxes (form$fields$checkbox$checked <- 'checked' doesn't work). As the function scrapes Science Direct by submitting the search form, scraping the data on the page, and clicking the 'next' button, there is some problem with the result: the result from Science Direct only contains the search result which has 'open access' marked (the default), while the full result should also contain search result that has 'subscribed journals' marked.


April 5:
- Sage Journal (create customized link)
  - can scrape search results with multiple pages
  - can search keywords with space or hyphen
  - fixed abstract link extraction; now it detects abstract link from the following options in order:
    - 'show abstract'
    - the first icon (usually icon 'abstract' but sometimes will be icon 'pdf')
    - the second icon (when there is no abstract and the first icon is not abstract, the second icon is possibly 'translated abstracts available' <-- although it may not direct you to a traslated abstract lol)
  - fixed info extraction situiation when the number of search result is 0
  - added printed process info
  - close connections after scrapping a webpage
  - created simple tf_idf score to rank the articles but haven't tested and validated yet
  - examples tested:
```
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- "abstract"
area <- 'abstract'
maxsize <- 100
databasename <- 'sage journal'
data_test <- scrape(keywordsA,keywordsB,area,maxsize,databasename)
```
- Science Direct (submit search form)
  - as the url is got from the feedback of search form submission, it can naturally handle keywords with space or hyphen
  - can scrape only the first page (maxsize = 200) <-- not sure if it can be adapted to multiple pages
  - can scrape multiple pages using the default maxsize (25), which involves creating new session by submitting 'Next >>'
  - 2 types info are recorded: title, author

April 3:
- turned the script into a function
- 4 types of info are recorded: title, author, abstract, link
- examples tested
  - sage journal: keywordA = "default" and keywordB = "decisions"
- next steps:
  - adapt to search term with space or hyphen
  - adapt to search result with multiple pages
  - set criteria to automatically filter out irrelated paper based on the abstract
  - adapt to various databases
