

#' Retrieves FreeAgent's list of projects
#'
#' @export
GetFreeAgentProjects <- function(...) 
  DataFrameFromJson(
    GetFreeAgentUrl("project", ...),
    column.classes = c(
      url =                as.character,
      name =               as.character,
      contact =            as.character,
      budget =             as.numeric,
      status =             as.character,
      budget.units=        as.character))