#' Generates from part fo the query
#'
#' This function creates a from part fo the query
#' @param data Table or View to query from
#' @keywords from
#' @export
#' @examples
#' from_tb <- from(dbHandle$tabl_dbo.table)

from <- function(data){
  return(paste("from", data, sep = " "))
}
