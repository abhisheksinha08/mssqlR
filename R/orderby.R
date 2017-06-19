#' Generates order by part of a query
#'
#' This function generates "order by" part of a query
#' @param query Existing query
#' @param  ... order by list
#' @keywords order by
#' @export
#' @examples
#' order_by_cols <- orderby("", dbHandle$col_tabl_dbo.col1, "ASC",dbHandle$col_tabl_dbo.col1, "DESC")

orderby <- function(query=NULL,  ...){

  if(is.null(query))
  {
    query ="";
  }

  c1 <- c(...);

  if(length(c1)==0 || (length(c1)) %%2>0)
  {
    return(query);
  }

  for(i in seq(0,length(c1),2))
  {
    if(i==0)
    {
      order_by_clause <- paste("ORDER BY", c1[i], c1[i+1], sep = " ");
    }
    else
    {
      order_by_clause <- paste(order_by_clause, paste(c1[i], c1[i], sep = " "), sep=",");
    }
  }

  return(trim(paste(query, order_by_clause, sep = " ")));
}
