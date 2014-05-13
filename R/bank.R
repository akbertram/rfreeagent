

#' Retrieves the list of FreeAgent bank accounts
#'
#' @export
GetFreeAgentBankAccounts <- function() 
  DataFrameFromJson(
    GetFreeAgentResources("bank_accounts"), 
    column.classes = c(
      url =              as.character,
      opening.balance =  as.numeric,
      bank.name =        as.character,
      due.on =           as.Date,
      type =             as.character,
      is.personal =      as.logical,
      is.primary =       as.logical,
      current.balance =  as.numeric,
      email =            as.character,
      updated.at =       as.Date,
      created.at =       as.Date))
    

#' Retrieves the list of transactions stored in FreeAgent for a given bank account
#'
#' @export
GetFreeAgentBankTransactions <- function(bank.account, ...)
  DataFrameFromJson(
    GetFreeAgentResources("bank_transactions", bank.account = bank.account, ...),
    column.classes = c(
      url =                as.character,
      amount =             as.numeric,
      bank.account =       as.character,
      dated.on =           as.Date,
      description =        as.character,
      uploaded.at =        as.Date,
      unexplained.amount = as.numeric,
      is.manual =          as.logical))
  
#' Retrieves the list of explanations for bank account transactions
#' 
#' @export
GetFreeAgentBankExplanations <- function(bank.account, ...)
  DataFrameFromJson(
    GetFreeAgentResources("bank_transaction_explanations", bank.account = bank.account, ...),
    column.classes = c(
      url =                as.character,
      bank.transaction =   as.character,
      bank.account =       as.character,
      category =           as.character,
      dated.on =           as.Date,
      description =        as.character,
      gross.value =        as.numeric,
      sales.tax.rate =     as.numeric,
      paid.user =          as.character,
      paid.invoice =       as.character,
      rebill.type =        as.character,
      rebill.to.project =  as.character,
      paid.bill =          as.character,
      linked.transfer.explanation = as.character,
      attachment.url =           as.character,
      attachment.content.src =   as.character,
      attachment.content.type =  as.character,
      attachment.file.name =     as.character,
      attachment.file.size =     as.integer))
    
#' Creates a new explanation
#' 
#' @export
#' @importFrom rjson toJSON
CreateFreeAgentExplanation <- function(explanation) {
  
  url <- "https://api.freeagent.com/v2/bank_transaction_explanations"
  json <- toJSON(list(bank_transaction_explanation = explanation))
  PutOrPostFreeAgentResource(url, json, method="POST")
}


#' Formats a list of transactions as a CSV
#' file ready to import into FreeAgent
#' 
#' @export 
#' @importFrom RCurl postForm fileUpload
UploadFreeAgentStatement <- function(account, date, amount, description) {
  
  desc <- description
  
  # Remove all non-ascii characters
  Encoding(desc) <- "latin1"  # (just to make sure)
  desc <- iconv(desc, "latin1", "ASCII", sub="")
  
  desc <- gsub(x = desc, pattern = "(,|\\s+)", " ")
  desc <- gsub(x = desc, pattern = "\\s+$", "")
  
  desc <- gsub("\\s+", " ", x = desc)

  csv <- paste(format(date, "%d/%m/%Y"), amount, desc, collapse="\n", sep=",")
  
  postForm(sprintf("https://api.freeagent.com/v2/bank_transactions/statement?bank_account=%s", account),
           statement = fileUpload(filename = "stmt.csv", contents = csv, contentType = "text/plain"),
           .opts = list(
             httpheader = c(  'User-Agent' = 'rfreeagent',
                              'Accept' = 'application/json',
                              'Authorization' = sprintf("Bearer %s", GetFreeAgentAccessToken()))))
}
