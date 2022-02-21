## Notes


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
    - document type: having at least one of the keywords (journal, article, feature, periodical, literature, statistics)
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
    - [expanded reporting](http://search.proquest.com/docview/909432350/fulltext/91BF795E101C43FAPQ/1?accountid=10226)
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
    