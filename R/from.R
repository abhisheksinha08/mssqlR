#' Connect to Database Server and Generate Entity Details Function
#'
#' This function connects to database and creates entity details
#' @param data Table or View to query from
#' @keywords from
#' @export
#' @examples
#' from_tb <- from(dbHandle$tabl_dbo.table)

from <- function(data){
  return(paste("from", data, sep = " "))
}
