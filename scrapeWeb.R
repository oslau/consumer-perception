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
library(twitteR)

#list of interesting companies
companies = c('Facebook', 'Twitter', 'Pandora', 'Apple', 'Google', 'Zynga', 'Tesla', 'Yahoo')
#list of URLs for each company name
url = unlist(lapply(companies, function(x) paste("http://finance.yahoo.com/q/pr?s=", x, sep="")))
#get company details, specific to Yahoo! finance formatting
all.tables = lapply(url, function(x) readHTMLTable(x, header=TRUE))
#this webpage has a lot of empty table areas for formatting
actual.interested.values = lapply(
								lapply(all.tables, '[[', 5), 	#5th table
										'[', 2)				#2nd column values
#this is wrong...need a do.call cbind
table.info = actual.interested.table[6:8,] #sector, industry, employees
