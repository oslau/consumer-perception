##PROJECT: 
##Analyze public views of companies based on news information.
##Include information such as company size, location, industry, sector, etc.

##Companies:
##Facebook, Twitter, Pandora, Apple, Google, Zynga, Tesla, Yahoo

#Scrape website for company size, industry, sector, etc. Yahoo! Finance has this info
#install.packages('scrapeR')
#library(scrapeR)
#load necessary libraries
library(XML)
library(twitteR)
library(rpart)

##MAKE SURE THESE ARE https://, not http://
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
#private
apikey
apisecret

twitCred <- OAuthFactory$new(
  consumerKey = apikey, 
  consumerSecret = apisecret,
  requestURL = reqURL,
  accessURL = accessURL, 
  authURL = authURL
  )
  twitCred$handshake(cainfo="cacert.pem")
  registerTwitterOAuth(twitCred)
  

##htmlToText function adapted from github.com/tonybreyal
htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
	      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)

  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  #return(text.vector) #this returns one large block
  return(text.list) #this returns a list
}


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
#Let's explore where the headlines are...
x = readLines("http://finance.yahoo.com/q/h?s=TSLA")
grep('August 1', x)
x[183]
###Hmm, this line is an entire block of all headlines for the day.
#use HTML to text function (above)
x = unlist(htmlToText("http://finance.yahoo.com/q/h?s=TSLA"))
#now: still one large chunk, but text only.
#edited: htmlToText function to spit out each HTML element as its own list item
headlines = x[263:328]
#but not all companies have the same number of headlines?
today = format(Sys.Date(), "%A, %B%d, %Y")
begin.select.row = grep(today, x)
end.select.row = grep("Older Headlines", x)
headlines = x[(begin.select.row + 1):(end.select.row - 1)]

#lets make a function of it:
get.headlines = function(company){
	#for debugging
	print(company)
	url = paste("http://finance.yahoo.com/q/h?s=", company, sep = "")
	page.text = unlist(htmlToText(url))
	
	headline.dates.row = grep("(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday), [A-Za-z]* [0-9]{1,2}, [0-9]{4}", page.text)
	end.select.row = grep("Older Headlines", page.text)
	
	headlines = list()
	##there's a better way to do this.
	n.dates = length(headline.dates.row)
	if(n.dates > 1){
		for(i in 1:(n.dates - 1)){
			temp = page.text[(headline.dates.row[i]+1):(headline.dates.row[i+1]-1)]
			headlines = unlist(c(headlines, temp))
		}
	}
	last_date = page.text[(headline.dates.row[n.dates] + 1):(end.select.row - 1)]
	headlines = unlist(c(headlines, last_date))
	headline.data = matrix(headlines, nrow= 3, ncol = length(headlines)/3)
	headline.data = data.frame(t(headline.data))
	colnames(headline.data) <- c("Headline", "Source", "Date")
	return(headline.data)
}

company.headlines = lapply(companies, get.headlines)

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