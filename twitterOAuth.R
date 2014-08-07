library(twitteR)

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