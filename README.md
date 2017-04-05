# WebScrapper-for-Academic-Databases

## Intention
Getting papers for meta analysis is always annoying: you need to try various combination of different keywords in various of databases, and it's very likely for you to come across with some paper you have met with in a previous database. Manually doing this is so inefficient.

## What I try to do
I'm starting to create a web scrapper for academic databases in R in the form of a (set of) function.

## Update
April 5:
- Sage Journal
  - can scrape search results with multiple pages
  - can search keywords with space or hyphen
  - fixed abstract link extraction; now it detects abstract link from the following options in order:
    - 'show abstract'
    - the first icon (usually icon 'abstract' but sometimes will be icon 'pdf')
    - the second icon (when there is no abstract and the first icon is not abstract, the second icon is possibly 'translated abstracts available' <-- although it may not direct you to a traslated abstract lol)
  - fixed info extraction situiation when the number of search result is 0
  - added printed process info
  - close connections after scrapping a webpage
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
