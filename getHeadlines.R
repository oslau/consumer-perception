getHeadlines = function(company){
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