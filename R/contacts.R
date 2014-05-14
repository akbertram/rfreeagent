

#' Retrieves the list of FreeAgent contacts
#'
#' @export
GetFreeAgentContacts <- function() 
  DataFrameFromJson(
    GetFreeAgentResources("contacts"), 
    column.classes = c(
      url =               as.character,
      first.name =        as.character,
      last.name =         as.character,
      organisation.name = as.character,
      country =           as.character,
      locale =            as.character,
      account.balance =   as.numeric
    ))


#' Retrieves the list of FreeAgent users
#'
#' @export
GetFreeAgentUsers <- function() 
  DataFrameFromJson(
    GetFreeAgentResources("users"), 
    column.classes = c(
      url =               as.character,
      first.name =        as.character,
      last.name =         as.character,
      email =             as.character,
      role =           as.character
    ))