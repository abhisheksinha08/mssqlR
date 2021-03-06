#' Generates order by part of a query
#'
#' This function generates "order by" part of a query
#' @param query Existing query
#' @param  ... order by list
#' @keywords order by
#' @importFrom magrittr %>%
#' @export
#' @examples
#' order_by_cols <- orderby("", "col1", "ASC","col2", "DESC")

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

  for(i in seq(1,length(c1),2))
  {
    orderbydir <- toupper(c1[i+1]);
    if(orderbydir!='ASC' && orderbydir!='DESC')
    {
      orderbydir <- 'ASC';
    }
    if(i==1)
    {
      order_by_clause <- paste("ORDER BY", c1[i], orderbydir, sep = " ");
    }
    else
    {
      order_by_clause <- paste(order_by_clause, paste(c1[i], orderbydir, sep = " "), sep=",");
    }
  }

  return(trim(paste(query, order_by_clause, sep = " ")));
}
