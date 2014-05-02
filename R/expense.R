
#' Creates a new FreeAgent expense
#' 
#' @export
CreateFreeAgentExpense <- function(expense) {
  
  names(expense) <- chartr(names(expense), old=".", new="_")
  update <- list(expense = expense)
  PutOrPostFreeAgentResource("https://api.freeagent.com/v2/expenses", toJSON(update), method="POST")
  
}
