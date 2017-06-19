#' Generates where part of a query
#'
#' This function generates "where" part of a query
#' @param query Existing query
#' @param  ... where list
#' @keywords where
#' @export
#' @examples
#' from_tb <- where("", dbHandle$col_tabl_dbo.col1, "=", "test1",dbHandle$col_tabl_dbo.col1, >, val1)

where <- function(query=NULL,  ...){
  if(is.null(query))
  {
    query ="";
  }

  c1 <- c(...);

  if(length(c1)==0 || length(c1)%%3>0)
  {
    return(query);
  }

  where_clause <- "";

  for(i in seq(1,length(c1),3))
  {
    if(i==1)
    {
      where_clause <- paste("where", c1(i), c1(i+1),c1(i+2), sep = " ");
    }
    else
    {
      where_clause <- paste(where_clause, paste(c1(i), c1(i+1),c1(i+2), sep = " "), sep=",");
    }
  }

  return(trim(paste(query, where_clause, sep = " ")));
}
