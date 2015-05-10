#Question 1
library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "299d0fde8afc2abd3388",
                   secret = "d109f80d203524388430d24a592f400d023c5fff")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
GitJleek <- content(req)

library(XML)
names(GitJleek[[1]])
which(GitJleek[[]][["full_name"]] == "jtleek/datasharing")
dataSharing <- numeric(length = 1)
for(i in 1:30){
    if(GitJleek[[i]][["full_name"]] == "jtleek/datasharing"){
        dataSharing <- i
        break
    }
}
names(GitJleek[[dataSharing]])
GitJleek[[dataSharing]][["created_at"]]