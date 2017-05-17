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

- Info Returned by Function
  - title
  - author
  - published year
  - abstract (if there is no abstract the value will be "No abstract")
  - link
  - availability, or whether the full text is accessible

- Scrape databases

  - Sage Journal
    - approach to get the page: merge needed arguments into a href link (having an argument to change page)
    - search field: abstract
    - no bug till now, can handle paper without abstract listed
    - the only problem is it runs slow (searching for the 4 keywords in group A and 3 keywords in group B (12 pairs in total, as Shannon listed) takes about 40 or 50 minutes).. currently I have no plan to optimize the code since you can run the function and put it aside for hours
    - because the page of every article needs to be visted to scrape the abstract, 

  - Science Direct
    - approach to get the page: merge needed arguments into a href link (get feedback from API, max number of article per page has a limit of 200)
    - search field: title, keywords, abstract
    - no bug till now, can handle paper without abstract listed
    - runs very fast as searching and fetching through API links (12 pairs of test keywords take about 70 seconds)
    - alghouth the API itself doesn't give information on availability, I randomly picked over 10 articles and all of them could be downloaded; I just assume every article on Science Direct is accessible at Columbia University.

  - PubMed
    - approach to get the page: merge needed arguments into a href link (get feedback from API, max number of article per page is set to be 10000)
    - search field: title, abstract
    - no bug till now, can handle paper without abstract listed
    - runs very fast as searching and fetching through API links (12 pairs of test keywords take about 60 seconds) when availability information is not collected; becomes much slower when availability info is collected because th API itself doesn't give information on whether the database has the full text accessible, so the page of every article needs to be visited to know this info. 

  - ProQuest
    - approach to get the page: merge needed arguments into a href link (get feedback from API, max number of article per page is set to be 10000)
    - search field: abstract
    - document type: having at least one of the keywords (journal, article, feature, periodical, literature)
    - subdatabases searched: politicalscience, publichealth, psychology, sociology, socscijournals, marketresearch, medline
    - no bug till now, haven't come across with paper without abstract listed yet
    - all earchable subdatabases at Columbia University: 
```
Academic databases - abidateline abiglobal abitrade accountingtaxbanking advancedtechaerospace afi africannews agricolamodule altpresswatch americanperiodicals annualreports anz anznews artbibliographies artshumanities asfaaquaculture asfaaquaticpollution asfabiological asfamarine asfaocean asianeuropeanbusiness asianews avery barrons bhi biologyjournals blacknews ble britishperiodicals canadiannews career cbcacomplete cecilpapers chicagotribune computing conteurope copper criminaljusticeperiodicals csp daai dnsa_46 dnsa_af dnsa_ar dnsa_bc dnsa_cc dnsa_cd dnsa_ch dnsa_ci dnsa_cl dnsa_cm dnsa_co dnsa_ct dnsa_cu dnsa_el dnsa_ep dnsa_es dnsa_fj dnsa_gu dnsa_hn dnsa_ic dnsa_ig dnsa_in dnsa_ip dnsa_ir dnsa_ja dnsa_jt dnsa_ju dnsa_ka dnsa_kc dnsa_ko dnsa_kr dnsa_kt dnsa_md dnsa_ms dnsa_nh dnsa_ni dnsa_np dnsa_pd dnsa_pe dnsa_ph dnsa_pr dnsa_sa dnsa_se dnsa_su dnsa_te dnsa_vi dnsa_vw dnsa_wm eastcentraleurope eastsouthasia ebrary education eiuarchive eric ethnicnewswatch europeannews familyhealth fiaf fii gannettnews genderwatch georefinprocess georefmodule globalwires healthcompleteshell healthmanagement hispanicnews hnpamericanhebrew hnpamericanisraelite hnpatlantaconstitution hnpatlantadailyworld hnpaustinamericanstatesman hnpbaltimoreafricanamerican hnpbaltimoresun hnpchicagodefender hnpchicagotribune hnpchinesecollection hnpchristiansciencemonitor hnpclevelandcallpost hnpdetroitfreepress hnpguardianobserver hnphartfordcourant hnpirishtimes hnplasentinel hnplatimes hnplouisvillecourierjournal hnpnashvilletennessean hnpnewamsterdamnews hnpnewsday hnpnewyorkbostonglobe hnpnewyorktimeswindex hnpnewyorktribune hnpnewyorktribunefull hnpnorfolkjournalguide hnpphiladelphiatribune hnppittsburghcourier hnpscotsman hnpsfchronicle hnpstlouispostdispatch hnptimesofindia hnpwallstreetjournal hnpwashingtonpost hooverscompany iba ibss iimpft iipaft indexislamicus indianjournals jpmorgan latimes latinamericaiberian latinamericanews latinamericanews1 libraryscience linguistics marketresearch medline mgamodule middleeastafrica middleeastnews midwestnews1 military mlaib nahs nationalnewspremier northcentralnews northeastnews1 nytimes oceanic pais pao pilots pio politicalscience polymer pqdtglobal pqdtlocal1005860 pqdtuk pqrl psychology publichealth religion sciencejournals socabs socialservices sociology socscijournals southcentralnews southeastnews telecomms toxline trenchjournals turkey ukireland vogue wallstreetjournal washingtonpost westnews wma wpsa;
```
  - Ebsco
    - seems to have user-friendly api and examples but an account is a must
  - Web of Science
    - theoretically can be scraped using api because the api admits ip as one of the two ways for authentication
    - no html link for request submission but xml. I tried but haven't successed yet.

- Rank or filter the result
  - Duplication Filter
    - currently filter by title
    - 1. check if there is duplicated title after removing all the spaces and punctuation and turning into lower case within the data collected from a particular database.
    - 2. for the full dataset merged across databases, first order the dataset by column availability ("Y", "N", NA, where NA all come from PubMed records), then remove duplicated records based on title. In R, dataset[!duplicated(dataset),] will keep the record occurs at the first time and remove other duplicated records following, so moving PubMed records to the last will make records from PubMed database be removed once there is duplication. This is quite often and many PubMed records are expected to be removed to reduce the workload in step 3.
    - 3. visit the page of each remaining record from PubMed and check if full text is provided. Because the number of remaining records is smaller than the number of original records, the time needed to collect availability info for PubMed records is reduced.
    
    
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
   
- ProQuest
  - lack of official documents by ProQuest
  - [Unofficial Tutorial](https://bibwild.wordpress.com/2014/02/17/a-proquest-platform-api/)
  - [Some Arguments](http://www.loc.gov/standards/sru/sru-1-2.html)
  - document type examples
    - [newspaper](http://search.proquest.com/docview/324350195/fulltext?source=fedsrch&accountid=10226)
    - [news](http://search.proquest.com/docview/884136678/fulltext?source=fedsrch&accountid=10226)
    - [report](http://search.proquest.com/docview/926949141/fulltext?source=fedsrch&accountid=10226)
    - [country report](http://search.proquest.com/docview/1648087126/fulltext?source=fedsrch&accountid=10226)
    - [commentary](http://search.proquest.com/docview/274307816/fulltextPDF?source=fedsrch&accountid=10226)
    - [feature](http://search.proquest.com/docview/992946798/abstract?source=fedsrch&accountid=10226)
    - [commentary case report](http://search.proquest.com/docview/211345387/abstract?source=fedsrch&accountid=10226)
    - [comparative study](http://search.proquest.com/docview/220505679/abstract?source=fedsrch&accountid=10226)
    - [expanded reporting](expanded reporting http://search.proquest.com/docview/909432350/fulltext/91BF795E101C43FAPQ/1?accountid=10226)
    - [general information](http://search.proquest.com/docview/223952084/abstract?source=fedsrch&accountid=10226)
    - [editorial](http://search.proquest.com/docview/211326411/abstract?source=fedsrch&accountid=10226)
    - [literature review](http://search.proquest.com/docview/215867804/abstract?source=fedsrch&accountid=10226)
  - useful tags in the xml returned
```
datafield tag="245"
subfield code="a" # title

datafield tag="513"
subfield code="a" # document type

datafield tag="520"
subfield code="a" # abstract

datafield tag="260"
subfield code="c" # year

datafield tag="100"
subfield code="a" # first author

datafield tag="700"
subfield code="a" # other authors if available

datafield tag="856"
subfield code="u" # links
```


  
- Ebsco
  - [Overview](https://support.ebsco.com/eit/api.php)

- Web of Science
  - [Overview](http://ipscience-help.thomsonreuters.com/wosWebServicesLite/WebServicesLiteOverviewGroup/Introduction.html)
  - [Tutorial Video in English](https://www.youtube.com/watch?v=Xzatmo4He5k)
    
    
## Update Details
April 24 - May 12:
- ProQuest
  - now proquest database can be scraped
  - this api is not given by ProQuest but can be used to search ProQuest database, and it has fairly poor documentation on ProQuest related information
  - example tested:
```
keywordsA <- c("defaults","default effect","advance directives","opt-out")
keywordsB <- c("decisions","decision-making","consumer behavior")
area <- 'abstract'
databasename <- c('proquest','pubmed')

data_test <- scrape(keywordsA,keywordsB,area,databasename,filterduplication = T,limitpersearch = 500)
```
  
- Overall
  - added info collected: now collect availability for all 4 databases
  - adjusted the way to remove duplicated records: first remove duplications within each database, then order the merged dataset by availability ("Y", "N", NA), so that (1) when two records having same title, the one with the link to full text will be kept (2) to reducde the numebr of records from PubMed that need to be visited page by page to collect availability info

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
