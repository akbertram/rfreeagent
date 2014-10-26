

# See the very helpful, multi-part, blog post:
# FreeAgent, OAuth & HTTP
# http://baddotrobot.com/blog/2012/08/11/oauth-and-http-part-i/

#' Get the FreeAgent access token from the 
#' user's home directory
GetFreeAgentAccessToken <- function() {
  credentials.path <- Sys.getenv("FREEAGENT_CREDENTIALS",
    unset = path.expand("~/.freeagent.credentials"))
  credentials <- read.dcf(credentials.path, all = TRUE)
  if(is.null(.GlobalEnv$.freeagent.access.token)) {
    RefreshFreeAgentAccessToken(credentials)
  }
  .GlobalEnv$.freeagent.access.token
}

#' Retrieves a new access token using the stored
#' refresh token
#' 
#' @importFrom RCurl postForm
#' @importFrom rjson fromJSON
RefreshFreeAgentAccessToken <- function(credentials) {
  
  json <-
    postForm("https://api.freeagent.com/v2/token_endpoint",
           grant_type = "refresh_token",
           refresh_token = credentials$RefreshToken,
           .opts = list(
             userpwd = paste(
              credentials$ConsumerKey, 
              credentials$ConsumerSecret, sep=":"),
             httpauth = 1L,
             httpheader = c(  'User-Agent' = credentials$UserAgent,
                              'Accept' = 'application/json' )))
  
  results <- fromJSON(json)
  
  .GlobalEnv$.freeagent.access.token <- results[["access_token"]]
} 
