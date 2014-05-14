


#' Retrieves the list of FreeAgent bills
#'
#' @export
GetFreeAgentBankBills <- function() 
  DataFrameFromJson(
    GetFreeAgentResources("bills"), 
    column.classes = c(
      url =              as.character,
      contact =          as.character,
      category =         as.character,
      reference =        as.character,
      dated.on =         as.Date,
      due.on =           as.Date,
      total.value =      as.numeric,
      paid.value =       as.numeric,
      due.value =        as.numeric,
      sales.tax.value =  as.numeric,
      sales.tax.rate =   as.numeric,
      status =           as.character,
      attachment.url =           as.character,
      attachment.content.src =   as.character,
      attachment.content.type =  as.character,
      attachment.file.name =     as.character,
      attachment.file.size =     as.integer))
