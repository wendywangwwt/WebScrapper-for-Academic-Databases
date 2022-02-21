# WebScrapper-for-Academic-Databases
A web scrapper for academic databases in R/Python in the form of a (set of) function. Initially, this was built for accelerating meta-data analysis where one intends to try various combination of different keywords in a handful of databases. Manually typing the keywords in different databases and eye-balling the resulted articles can be extremely inefficient and painful, especially when an article already seen from a previous database came up again in a new database.

To address that, this web scrapper automatically
- generates query based on the provided keywords
- parses the result page (xml in PubMed for example)
- extracts the a group of information, including
  - title
  - author(s)
  - published year
  - abstract
  - link to the article
  - availability (may not be available to all databases)
  - search term
  - database
- returns the results as a data frame

Supported databases as of 2022-02-20:
- PubMed (`pubmed`)

Databases that needs code update as of 2022-02-20:
- Sage Journal (`sage_journal`)
- Science Direct (`science_direct`)
- ProQuest (`proquest`)

## How to use
#### param: keywords
A string of keyword, or a vector of multiple keywords. Here is an example of multiple keywords:
```
keywords <- c("decisions","decision-making","consumer behavior")
```

#### param: database_name
A string of database name, or a vector of multiple database names. Currently only `pubmed` is functional, so practically you can't use it on multiple databases :(
```
database_name <- 'pubmed'
```

#### param: limit_per_search
Put a limit to avoid collecting tens of thousands of results, unless that's what you want.
```
df_data <- search_databases(keywords,database_name=database_name,limit_per_search=300)
```

#### param: relationship
The relationship between keywords, if multiple are provided. Default to `or`. So the above example is equivalent to:
```
df_data <- search_databases(keywords,relationship='or',database_name=database_name,limit_per_search=300)
```

If you want to concatenate your keywords with an AND relationship for the search, change the value to `and`.
```
df_data <- search_databases(keywords,relationship='and',database_name=database_name,limit_per_search=300)
```

#### param: field
Which field to search. Default to `abstract` (depending on database, this usually includes article title & keywords as well). Optionally, you can switch to `all`, to search full article.
```
df_data <- search_databases(keywords,relationship='and',field='all',database_name=database_name,limit_per_search=300)
```

#### param: no_duplicate
Whether to drop duplicated results or not. Default to TRUE. Duplicated results come from searches across keywords (relationship='or'), and/or searches across databases. You may want to turn it off to better understand which database + search term combinations provide a duplicate. Higher number of duplicates could indicate higher relevancy of the article to the topic you intend to look into.
```
df_data <- search_databases(keywords,relationship='or',database_name=database_name,no_duplicate=F,limit_per_search=300)
```


## Caution
To existing users, if there is any:  
The code is re-factored and simplified to only do the job of database searching and data collection given ONE set of keywords (previously it was created with the assumption that TWO sets of keywords are needed, though you could skip one). Now if you have multiple sets of keywords, it will be up to you to loop through all combinations and call the function one at a time.





    
## Change log
2022-02-20:
- re-factored the code (not completely)
- only pubmed passed the tests, need to check and update the interaction with other databases later

