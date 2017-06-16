#' Generates Select part fo the query
#'
#' This function creates "Select" part fo the query
#' @param query Existing query
#' @param ... Fields to query
#' @param top Top n rows of the data
#' @keywords from
#' @export
#' @examples
#' from_tb <- select(from("Table1"),"COl1","COl2",Col3", top=100)

select <- function(query, ..., top=NULL){

  top_provided<-TRUE
  if(is.null(top))
  {
    top_provided <- FALSE # Top Parameter not provided
  }
  else
  {
    if(is.numeric(top)==FALSE)
    {
      top_provided <- FALSE # Top Parameter is a string
    }
    else
    {
      if(top%%1!=0)
      {
        top_provided <-FALSE #Top parameter is not an integer
      }
      else
      {
        if(top<=0)
        {
          top_provided<-FALSE #TOP parameter is not a positive number
        }
      }
    }
  }

  # no columns name are provided
  if(length(c(...))==0)
  {
    if(top_provided==TRUE)
    {
      return(paste("Select TOP", top , "*",query,sep = " "))
    }
    return(paste("Select *",query,sep = " "))
  }

  # when columns name are provided
  if(top_provided==TRUE)
  {
    return(trim(paste("Select TOP", top, paste(...,sep = ","), query, sep = " ")))
  }
  return(trim(paste("Select",paste(...,sep = ","), query, sep = " ")))

}
