

#' PutOrPostFreeAgentResource
#' 
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
GetFreeAgentResource <- function(url) {
  
  cat(sprintf("Fetching %s\n", url))
  
  fromJSON(getURL(url, httpheader = c(  'User-Agent' = 'rfreeagent',
                                        'Accept' = 'application/json',
                                        'Authorization' = sprintf("Bearer %s", GetFreeAgentAccessToken()))))
}

#' PutOrPostFreeAgentResource
#' 
#' @importFrom RCurl curlPerform
PutOrPostFreeAgentResource <- function(url, json, method = "POST") {
  
  content <- charToRaw(json)  
  response <- curlPerform(url = url,         
                          .opts = list(
                            infilesize = length(content), 
                            readfunction = content, 
                            upload = TRUE, 
                            customrequest = method,
                            httpheader = c(  'User-Agent' = 'rfreeagent',
                                             'Content-Type' = 'application/json',
                                             'Accept' = 'application/json',
                                             'Authorization' = sprintf("Bearer %s", GetFreeAgentAccessToken()))
                          ))
  response
}

#' Fetches a list of FreeAgent resources, optionally requesting additional
#' pages until the list is complete
#' 
#' @importFrom rjson fromJSON
GetFreeAgentResources <- function(resource.name, wrapper.name = resource.name, fetch.all=TRUE, ...) {
  query.params <- list(...)
  
  page.index <- 1
  page.size <- 100
  
  resources <- NULL
  has.more <- TRUE
  
  while(has.more) {
    
    query.params$per.page <- page.size
    query.params$page <- page.index
    
    url <- paste("https://api.freeagent.com/v2/", 
                 resource.name,
                 FormatQueryString(query.params),
                 sep = "")
    
    cat(sprintf("Fetching %s\n", url))
    response <- GetFreeAgentResource(url)
    
    if(!is.null(response$error)) {
      stop(toJSON(response))
    }
    
    resources.page <- if(is.null(wrapper.name)) response else response[[wrapper.name]]
    resources <- c(resources, resources.page)
    
    cat(sprintf("Retrieved %d items...\n", length(resources.page)))
    
    has.more <- fetch.all && length(resources.page) == page.size
    page.index <- page.index + 1
  }
  resources
}


#' Deletes the resource at the given URL
#' 
#' @export
#' @importFrom RCurl curlPerform
DeleteFreeAgentResource <- function(url) {
  
  response <- curlPerform(url = url,         
                          .opts = list(
                            customrequest = "DELETE",
                            httpheader = c(  'User-Agent' = 'rfreeagent',
                                             'Accept' = 'application/json',
                                             'Authorization' = sprintf("Bearer %s", GetFreeAgentAccessToken()))
                          ))
}

FormatQueryString <- function(params) {
  if(length(params) > 0) {
    names(params) <- chartr(x = names(params), ".", "_")
    paste("?", 
          paste(names(params), as.character(params), sep="=", collapse="&"),
          sep="")
  } else {
    ""
  }
}



#' DataFrameFromJson
#' 
#' Converts a list of JSON objects to a data.frame
#'
#' @param json.objects a list of json objects from fromJSON
#' @param column.classes a named list of columns and converter 
DataFrameFromJson <- function(json.objects, column.classes) {
  stopifnot(is.list(column.classes))
  stopifnot(all(!is.na(names(column.classes))))
  
  # flatten objects
  rows <- lapply(json.objects, function(json.object) {
    list <- unlist(json.object, recursive=TRUE)
    names(list) <- chartr("_", ".", names(list))
    list
  })
  
  # create a vector for each column
  vectors <- lapply(names(column.classes), function(column) {
    
    # the json property uses _ instead of .
    converter <- column.classes[[column]]
    
    accessor <- function(row) row[column]
    converter(sapply(rows, accessor))
  })
  names(vectors) <- names(column.classes)
  as.data.frame(vectors, stringsAsFactors=FALSE)
}