# WebScrapper-for-Academic-Databases

## Intention
Getting papers for meta analysis is always annoying: you need to try various combination of different keywords in various of databases, and it's very likely for you to come across with some paper you have met with in a previous database. Manually doing this is so inefficient.

## What I try to do
I'm starting to create a web scrapper for academic databases in R in the form of a (set of) function.

## Summary

- Function Arguments
  - keywordsA: an array of the first group of keywords
  - keywordsB: an array of the second group of keywords
  - area: currently only supports "abstract", which corresponds to abstract in Sage Journal, title/keywords/abstract in Science Direct, and title/abstract in PubMed
  - databasename: an array of database names to be searched; currently supports Sage Journal, Science Direct, and PubMed
  - filterduplication: whether to remove duplicated records in terms of title
  - sdkey: need to be a string (your api key) if Science Direct is in the databasename
  - limitpersearch: can put a number indicating the max number of result to be collected for each pair of keywords; currently must be larger than 200 and smaller than 10000 if is used

- Scrape databases

  - Sage Journal
    - approach to get the page: merge needed arguments into a href link (having an argument to change page)
    - approach to filter out info: css selectors
    - search field: abstract
    - 5 types of info are collected: title, author, abstract, link, year
    - no bug till now, can handle paper without abstract listed
    - the only problem is it runs slow (searching for the 4 keywords in group A and 3 keywords in group B (12 pairs in total, as Shannon listed) takes about 40 or 50 minutes).. currently I have no plan to optimize the code since you can run the function and put it aside for hours.

  - Science Direct
    - approach to get the page: merge needed arguments into a href link (get feedback from API, max number of article per page has a limit of 200)
    - approach to filter out info: css selectors
    - search field: title, keywords, abstract
    - 5 types of info are collected: title, author, abstract, link, year
    - no bug till now, can handle paper without abstract listed
    - runs very fast as searching and fetching through API links (12 pairs of test keywords take about 70 seconds)

  - PubMed
    - approach to get the page: merge needed arguments into a href link (get feedback from API, max number of article per page is set to be 10000)
    - approach to filter out info: css selectors
    - search field: title, abstract
    - 5 types of info are collected: title, author, abstract, link, year
    - no bug till now, can handle paper without abstract listed
    - runs very fast as searching and fetching through API links (12 pairs of test keywords take about 60 seconds)

  - ProQuest
    - API, using links like http://fedsearch.proquest.com/search/sru/sciencejournals?operation=searchRetrieve&version=1.2&maximumRecords=10000&query=abstract%3D%22defaults%20AND%20decisions%22
    - searchable databases in CDS office: 
```
Academic databases - abidateline abiglobal abitrade accountingtaxbanking advancedtechaerospace afi africannews agricolamodule altpresswatch americanperiodicals annualreports anz anznews artbibliographies artshumanities asfaaquaculture asfaaquaticpollution asfabiological asfamarine asfaocean asianeuropeanbusiness asianews avery barrons bhi biologyjournals blacknews ble britishperiodicals canadiannews career cbcacomplete cecilpapers chicagotribune computing conteurope copper criminaljusticeperiodicals csp daai dnsa_46 dnsa_af dnsa_ar dnsa_bc dnsa_cc dnsa_cd dnsa_ch dnsa_ci dnsa_cl dnsa_cm dnsa_co dnsa_ct dnsa_cu dnsa_el dnsa_ep dnsa_es dnsa_fj dnsa_gu dnsa_hn dnsa_ic dnsa_ig dnsa_in dnsa_ip dnsa_ir dnsa_ja dnsa_jt dnsa_ju dnsa_ka dnsa_kc dnsa_ko dnsa_kr dnsa_kt dnsa_md dnsa_ms dnsa_nh dnsa_ni dnsa_np dnsa_pd dnsa_pe dnsa_ph dnsa_pr dnsa_sa dnsa_se dnsa_su dnsa_te dnsa_vi dnsa_vw dnsa_wm eastcentraleurope eastsouthasia ebrary education eiuarchive eric ethnicnewswatch europeannews familyhealth fiaf fii gannettnews genderwatch georefinprocess georefmodule globalwires healthcompleteshell healthmanagement hispanicnews hnpamericanhebrew hnpamericanisraelite hnpatlantaconstitution hnpatlantadailyworld hnpaustinamericanstatesman hnpbaltimoreafricanamerican hnpbaltimoresun hnpchicagodefender hnpchicagotribune hnpchinesecollection hnpchristiansciencemonitor hnpclevelandcallpost hnpdetroitfreepress hnpguardianobserver hnphartfordcourant hnpirishtimes hnplasentinel hnplatimes hnplouisvillecourierjournal hnpnashvilletennessean hnpnewamsterdamnews hnpnewsday hnpnewyorkbostonglobe hnpnewyorktimeswindex hnpnewyorktribune hnpnewyorktribunefull hnpnorfolkjournalguide hnpphiladelphiatribune hnppittsburghcourier hnpscotsman hnpsfchronicle hnpstlouispostdispatch hnptimesofindia hnpwallstreetjournal hnpwashingtonpost hooverscompany iba ibss iimpft iipaft indexislamicus indianjournals jpmorgan latimes latinamericaiberian latinamericanews latinamericanews1 libraryscience linguistics marketresearch medline mgamodule middleeastafrica middleeastnews midwestnews1 military mlaib nahs nationalnewspremier northcentralnews northeastnews1 nytimes oceanic pais pao pilots pio politicalscience polymer pqdtglobal pqdtlocal1005860 pqdtuk pqrl psychology publichealth religion sciencejournals socabs socialservices sociology socscijournals southcentralnews southeastnews telecomms toxline trenchjournals turkey ukireland vogue wallstreetjournal washingtonpost westnews wma wpsa;
```

- Rank or filter the result
  - Duplication Filter
    - currently filter by title
    - check if there is duplicated title after removing all the spaces and punctuation and turning into lower case
    
  - tf-idf
    - use tidytext::bind_tf_idf()
    - tf-idf was tried on the result of Sage Journal (using the 2 groups of keywords listed by Shannon) to rank or score the relatedness or keyword importance of the articles using their abstract. It's calculated on unigram (single word) while some of our keywords are bigram (e.g., consumer behavior), I'm considering to add some constraints on the score and rank them again (e.g., the socre of 'consumer' and that of 'behavior' is meaningful only when both of them occur in the abstract).

  - Latent Dirichlet Allocation
    - use topicmodels::LDA()
    - first turn the array of abstracts into tidy form (word by document) then use tidy::cast_dtm() to cast it into document-term-matrix as the input for LDA function
    - Problem: the problem of unsupervised learning. The number of topic (k) is a tuning parameter and will largely influence the filtering/ranking/scoring (any word you want to call it) result. It will be a little bit tricky especially for specialized database like PubMed. For example, if a user only searches these keywords on PubMed and another only searches these on a psychology database (not sure if it exists but just an example), the clustering will be totally different and hard to compare.
    - Potential Solution: 
      - make use of keywords and tf-idf, which is a general solution
      - make use of labeled articles like here we have a couple of papers on hand, which is not a general solution but the quality could be really good depending on the number of labeled articles
    
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
April 20:
- Sage Journal
  - rewrote the subfunction to get info, removed unnecessary loops, changed the logic to be similar to subfunctions for Science Direct and PubMed.. hopefully will reduce runtime

- Overall
  - added info collected: now collect published year for all 3 databases
  - added new argument limitpersearch: the max number of result to be collected for each pair of keywords, or each search, must be larger than 200 and smaller than 10000
  - examples tested:
```
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- 'abstract'
databasename <- c('sage journal','science direct','pubmed')
data_test <- scrape(keywordsA,keywordsB,area,databasename,filterduplication = T,sdkey=sdkey,limitpersearch = 300)
```


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
- Overall
  - three databases are incoporated in the function without error till now
  - examples tested:
```
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- 'abstract'
databasename <- c('sage journal','science direct','pubmed')
data_test <- scrape(keywordsA,keywordsB,area,databasename,filterduplication = T,sdkey=sdkey)
```
  - example performance (separate): Sage Journal ~ 2800 seconds, Science Direct ~ 70 seconds, PubMed ~ 60 seconds

- LDA
  - found a package (topicmodels) to do Latent Dirichlet Allocation 
  - successfully performed LDA on abstracts collected
  - need to figure out a way to use the result

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
