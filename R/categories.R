


#' Retrieves FreeAgent's list of categories
#'
#' @export
GetFreeAgentCategories <- function(...) {
  
  types <- GetFreeAgentResources("categories", wrapper.name=NULL, ...)
  
  categories <- lapply(names(types), function(type) {
    df <- DataFrameFromJson(types[[type]],
                            column.classes = c(
                              url =              as.character,
                              description =      as.character,
                              nominal.code =     as.numeric))  
    df$type <- type
    df
  })
  do.call("rbind", categories)
}