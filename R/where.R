#' Generates where part of a query
#'
#' This function generates "where" part of a query
#' @param query Existing query
#' @param  ... where list
#' @keywords where
#' @export
#' @examples
#' where_cond <- where("", "col1", "=", "test1", "AND", "col2", ">", 25)

where <- function(query=NULL,  ...){

  if(is.null(query))
  {
    query ="";
  }

  c1 <- c(...);

  if(length(c1)==0 || (length(c1) +1) %%4>0)
  {
    return(query);
  }

  where_clause <- "";

  for(i in seq(0,length(c1),4))
  {
    where_clause_right_side <- c1[i+3];
    if(suppressWarnings(is.na(as.numeric(c1[i+3]))))
    {
      where_clause_right_side <- paste("'", c1[i+3], "'", sep = "");
    }

    if(i==0)
    {
      where_clause <- paste("where", c1[i+1], c1[i+2],where_clause_right_side, sep = " ");
    }
    else
    {
      if(toupper(c1[i])=="AND")
      {
        #AND Condition
        where_clause <- paste(where_clause, paste("AND",c1[i+1], c1[i+2],where_clause_right_side, sep = " "), sep=" ");
      }
      else
      {
        #OR Condition
        where_clause <- paste(where_clause, paste("OR",c1[i+1], c1[i+2],where_clause_right_side, sep = " "), sep=" ");
      }
    }
  }

  return(trim(paste(query, where_clause, sep = " ")));
}
