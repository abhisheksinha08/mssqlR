#' Executes the query
#'
#' This function executes the query
#' @param query full query for execution
#' @param dbHandle Database Handle to use for query
#' @keywords exec
#' @export
#' @examples
#' df <- from(dbHandle$tabl_dbo.table) %>% exec(dbHandle)

exec <- function(query, dbHandle){
  df <- tryCatch({
    if(grepl("Select", query) == FALSE && grepl("select", query) == FALSE)
    {
      query <- paste("Select *", query, sep = " ") #Add Select * if Select part is missing
    }
    sqlQuery(dbHandle, query)
  },
  error = function(e){
    cat(paste("Error!\n", e))
    return(NULL)
  }
  )
}
