#' Generates Select part fo the query
#'
#' This function creates "Select" part fo the query
#' @param query Existing query
#' @param ... Fields to query
#' @param top Top n rows of the data
#' @param distinct True or FALSE
#' @keywords from
#' @export
#' @examples
#'
#' #Returns Top 100 rows
#' from_tb <- select("","COl1","COl2","Col3", top=100)
#'
#' #Returns distinct rows
#' from_tb <- select("","COl1","COl2","Col3", distinct=TRUE)

select <- function(query=NULL, ..., top=NULL, distinct=NULL){

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

  if(is.null(query))
  {
    query ="";
  }

  if(is.null(distinct))
  {
    distinct = FALSE;
  }


  # no columns name are provided
  if(length(c(...))==0)
  {
    if(top_provided==TRUE)
    {
      if(distinct==TRUE)
      {
        return(paste("Select DISTINCT TOP", top , "*",query,sep = " "))
      }
      return(paste("Select TOP", top , "*",query,sep = " "))
    }
    if(distinct==TRUE)
    {
      return(paste("Select DISTINCT *",query,sep = " "))
    }
    return(paste("Select *",query,sep = " "))
  }

  # when columns name are provided
  if(top_provided==TRUE)
  {
    if(distinct==TRUE)
    {
      return(trim(paste("Select DISTINCT TOP", top, paste(...,sep = ","), query, sep = " ")))
    }
    return(trim(paste("Select TOP", top, paste(...,sep = ","), query, sep = " ")))
  }
  if(distinct==TRUE)
  {
    return(trim(paste("Select DISTINCT",paste(...,sep = ","), query, sep = " ")))
  }
  return(trim(paste("Select",paste(...,sep = ","), query, sep = " ")))

}
