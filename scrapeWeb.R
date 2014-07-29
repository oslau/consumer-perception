##PROJECT: 
##Analyze public views of companies based on press release information.
##Include information such as company size, location, industry, sector, etc.

##Companies:
##Facebook, Twitter, Pandora, Apple, Google, Zynga, Tesla, Yahoo

#Scrape website for company size, industry, sector, etc. Yahoo! Finance has this info
#install.packages('scrapeR')
#library(scrapeR)
#load necessary libraries
library(XML)

#list of interesting companies
companies = c('Facebook', 'Twitter', 'Pandora', 'Apple', 'Google', 'Zynga', 'Tesla', 'Yahoo')

url = unlist(lapply(companies, function(x) paste("http://finance.yahoo.com/q/pr?s=", x, sep="")))