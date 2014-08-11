##PROJECT: 
##Analyze public views of companies based on news information.
##Include information such as company size, location, industry, sector, etc.
##Companies:
##Facebook, Twitter, Pandora, Apple, Google, Zynga, Tesla, Yahoo
##Scrape website for company size, industry, sector, etc. Yahoo! Finance has this info

#set working directory
setwd('./Documents/Project/Consumer Perception/')

#load necessary libraries
library(XML)
library(rpart)

source("htmlToText.R")
source("twitterOAuth.R")
source("getHeadlines.R")


#list of interesting companies
companies = c('Facebook', 'Twitter', 'Pandora', 'Apple', 'Google', 'Zynga', 'Tesla', 'Yahoo')

#list of URLs for each company name
url = unlist(lapply(companies, function(x) paste("http://finance.yahoo.com/q/pr?s=", x, sep="")))
#get company details, specific to Yahoo! finance formatting
all.tables = lapply(url, function(x) readHTMLTable(x, header=TRUE))
#this webpage has a lot of empty table areas for formatting
table.info = lapply(
								lapply(all.tables, '[[', 5), 	#5th table
										'[', 2)				#2nd column values
#get info of interest: sector, industry, employees - respectively
actual.interested.values = data.frame(lapply(table.info, function(x) x[6:8,]))
colnames(actual.interested.values) <- companies

##Scrape websites for recent headline information
company.headlines = lapply(companies, getHeadlines)

#Manually entering key words of headlines
#Ideally, use a linguistic database for "positive" and "negative" words
#and machine learning techniques for contextual reference
#try: Calais
key.words = c("Krill Oil", "Outage", "Experiment", "Service", "Crash", "Recover", "login", 
		"log in", "disrupt", "stock", "sue", "privacy", "data")
##not finished here...

##Who is talking about these companies?
at.fb = searchTwitter("@facebook", n=500)
hashtag.fb = searchTwitter("#facebook", n=500)
talking.about.fb = unlist(c(at.fb, hashtag.fb))
about.fb = data.frame()
	##there's a better way to do this.
n.tweets = length(talking.about.fb)
	if(n.tweets > 1){
		for(i in 1:(n.tweets)){
			temp = as.data.frame(talking.about.fb[[i]])
			about.fb = rbind(about.fb, temp)
		}
	}
lookup.user = lookupUsers(about.fb$screenName)
users.info = data.frame()
	##there's a better way to do this.
n.user = length(lookup.user)
	if(n.user > 1){
		for(i in 1:(n.user)){
			temp = as.data.frame(lookup.user[[i]])
			users.info = rbind(users.info, temp)
		}
	}
	
#merge tweet info with user info
full.data.fb = merge(about.fb, users.info, by="screenName")
full.data.fb$talking.headlines = grepl(
	"Krill Oil|Outage|Experiment|Service|Crash|Recover|login|log in|disrupt|stock|sue|privacy|data", full.data.fb$text, ignore.case = TRUE
	)
#dropping singular information
#eventually will want to keep tweet text for sentiment analysis here
#later need to do text analysis for this.
full.data.fb$statusSource = strapply(full.data.fb$statusSource, ">(.*)<", simplify = TRUE)
full.data.fb$localSource = grepl("Twitter (.*)",full.data.fb$statusSource)
full.data.fb = full.data.fb[,(colnames(full.data.fb) %in% c("talking.headlines","favorited", "favoriteCount", "truncated", "localSource", "isRetweet", "retweeted", "statusCount", "followersCount", "favoritesCount", "friendsCount", "verified"))]

user.train = sample(users.info$screenName, 400) ##btw, some users tweeted multiple times
												##so number of resulting tweets is uneven
												##let's choose slightly less than half of users
train = full.data.fb[(about.fb$screenName %in% user.train), ]
test = full.data.fb[!(about.fb$screenName %in% user.train), !(colnames(full.data.fb) == "isRetweet")]
test.actual = test[,(colnames(full.data.fb) == "isRetweet")]
myrpart = rpart(isRetweet ~ ., method = "class", data = train)
#summary(myrpart)
plot(myrpart)text(myrpart)title(main = "Classification Tree Based on Training Data")
summary(residuals(myrpart))
plot(predict(myrpart),residuals(myrpart))
predictions = predict(myrpart, test, type = "class")
table(test.actual, predictions)

#playing with pruning
pfit<- prune(myrpart, cp=myrpart$cptable[which.min(myrpart$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE, main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
prune.predictions = predict(myrpart, test, type = "class")
table(test.actual, prune.predictions)