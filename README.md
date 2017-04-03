# WebScrapper-for-Academic-Databases

## Intention
Getting papers for meta analysis is always annoying: you need to try various combination of different keywords in various of databases, and it's very likely for you to come across with some paper you have met with in a previous database. Manually doing this is so inefficient.

## What I try to do
I'm starting to create a web scrapper for academic databases in R in the form of a (set of) function.

## Update
April 3:
- turned the script into a function
- the function now works for sage journal with keywordA = "default" and keywordB = "decisions"
- 4 types of infor are recorded: title, author, abstract, link
- next steps:
  - adapt to search term with space or hyphen
  - adapt to search result with multiple pages
  - set criteria to automatically filter out irrelated paper based on the abstract
  - adapt to various databases
