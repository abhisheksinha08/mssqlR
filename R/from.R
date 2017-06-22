#' Generates from part for the query
#'
#' This function creates a from part for the query
#' @param query Existing query
#' @param data Table or View to query from
#' @keywords from
#' @importFrom magrittr %>%
#' @export
#' @examples
#' from_tb <- from("","tableName")

from <- function(query=NULL, data){
  if(is.null(query))
  {
    query ="";
  }

  if(query=="")
  {
    return(paste("from", data, sep = " "))
  }
  else
  {
    if(grepl("select", query) == FALSE)
    {
      return(paste("from", data, query, sep = " "))
    }
    else
    {
      return(paste(query, "from", data, sep = " "))
    }
  }
}
