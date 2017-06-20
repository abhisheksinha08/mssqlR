#' Connect Function
#'
#' This function allows you to connect to a MS SqlServer Database
#' @param server Database Server Url/IP Address
#' @param db Database Name
#' @param uid User id
#' @param pwd Password
#' @keywords Connect
#' @import RODBC magrittr
#' @export
#' @examples
#' dbHandle <- connect_db("server.com","db_test","admin","password")

connect_db <- function(server, db, uid, pwd){
  dbhandle <- tryCatch({
    odbcDriverConnect(paste('driver={SQL Server Native Client 11.0};server=', server, ';database=',db, ';uid=', uid, ';pwd=',pwd, sep = ""))
  },
  error = function(e){
    print(paste("Error!", e))
  }
  )
  return(dbhandle)
}
