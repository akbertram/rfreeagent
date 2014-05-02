

#' Retrieves invoices from FreeAgent
#'
#' @param view Can be recent_open_or_overdue, open_or_overdue, draft,
#'          scheduled_to_email, thank_you_emails, reminder_emails, last_N_months
#' @export
GetFreeAgentInvoices <- function(...) 
  invoices <- DataFrameFromJson(
    GetFreeAgentResources("invoices", ...), 
    column.classes = c(
      url =              as.character,
      contact =          as.character,
      dated.on =         as.Date,
      due.on =           as.Date,
      reference =        as.character,
      currency =         factor,
      exchange.rate =    as.numeric,
      net.value =        as.numeric,
      sales.tax.value =  as.numeric,
      total.value =      as.numeric,
      paid.value =       as.numeric,
      due.value =        as.numeric,
      status =           factor,
      paid.on =          as.Date,
      project =          as.character,
      written.off.date = as.Date,
      payment.url =      as.character,
      po.reference =     as.character))
  