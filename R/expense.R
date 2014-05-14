
#' Creates a new FreeAgent expense
#' 
#' @export
CreateFreeAgentExpense <- function(expense) {
  
  names(expense) <- chartr(names(expense), old=".", new="_")
  update <- list(expense = expense)
  PutOrPostFreeAgentResource("https://api.freeagent.com/v2/expenses", toJSON(update), method="POST")
  
}

#' Updates an existing FreeAgent expense
#'
#' @export
UpdateFreeAgentExpense <- function(expense) {
  
  names(expense) <- chartr(names(expense), old=".", new="_")
  update <- list(expense = expense)
  PutOrPostFreeAgentResource(expense$url, toJSON(update), method="PUT")
}


#' Fetches all FreeAgent expenses
#' 
#' @export
GetFreeAgentExpenses <- function(...)
  DataFrameFromJson(
    GetFreeAgentResources("expenses", ...),
    column.classes = c(
      url =                as.character,
      user =               as.character,
      category =           as.character,
      dated.on =           as.Date,
      description =        as.character,
      gross.value =        as.numeric,
      sales.tax.rate =     as.numeric,
      currency =           as.character,
      attachment.url =           as.character,
      attachment.content.src =   as.character,
      attachment.content.type =  as.character,
      attachment.file.name =     as.character,
      attachment.file.size =     as.integer))