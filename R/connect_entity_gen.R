#' Connect to Database Server and Generate Entity Details Function
#'
#' This function connects to database and creates entity details
#' @param server Database Server Url/IP Address
#' @param db Database Name
#' @param uid User id
#' @param password Password
#' @keywords Connect Entity Generation
#' @export
#' @examples
#' dbHandle <- connect_entity_gen("server.com","db_test","admin","password")

connect_entity_gen <- function(server, db, uid, pwd){

  #Connect to SQL Server
  cat("mssqlR v 0.0.0.9102: Connecting to Database...")
  dbhandle <- tryCatch({
    odbcDriverConnect(paste('driver={SQL Server Native Client 11.0};server=', server, ';database=',db, ';uid=', uid, ';pwd=',pwd, sep = ""))
  },
  error = function(e){
    cat(paste("Error!\n", e))
    return(NULL)
  }
  )


  cat("Connected!\n")
  cat("Generating Entities...")
  #Get Table Names
  tables_sys <- sqlQuery(dbhandle, 'Select SS.name + \'.\' + ST.name from sys.tables ST LEFT JOIN sys.schemas SS ON ST.schema_id=SS.schema_id WHERE ST.type_desc = \'USER_TABLE\' ORDER BY ST.Name')
  tables_sys<-data.frame(tables_sys, stringsAsFactors = FALSE)
  tables_sys$Var.1 <- levels(tables_sys$Var.1)

  dbDetails<-list(dbhandle = dbhandle)

  i<-1
  for(i in 1:nrow(tables_sys)){
    dbDetails[[paste("tbl",tables_sys[i,1], sep = "_")]] <-  as.character(tables_sys[i,1])
    i<-i+1
  }
  rm( list = c("tables_sys","i"))

  #Get View Names
  tables_sys <- sqlQuery(dbhandle, 'Select SS.name + \'.\' + SV.name from sys.views SV LEFT JOIN sys.schemas SS ON SV.schema_id=SS.schema_id ORDER BY SV.Name')
  tables_sys<-data.frame(tables_sys, stringsAsFactors = FALSE)
  tables_sys$Var.1 <- levels(tables_sys$Var.1)

  i<-1
  for(i in 1:nrow(tables_sys)){
    dbDetails[[paste("view",tables_sys[i,1], sep = "_")]] <-  as.character(tables_sys[i,1])
    i<-i+1
  }
  rm( list = c("tables_sys","i"))


  #Get Field Names

  tables_sys <- as.list(sqlQuery(dbhandle, 'Select SS.name, ST.name from sys.tables ST LEFT JOIN sys.schemas SS ON ST.schema_id=SS.schema_id WHERE ST.type_desc = \'USER_TABLE\' ORDER BY ST.Name'))
  i<-1
  for(i in 1:length(tables_sys$name.1)){
    cat(".")
    schema_name <- tables_sys$name[i]
    table_name <- tables_sys$name.1[i]
    tableName <- paste(schema_name, ".", table_name, sep = "")
    cols <- sqlQuery(dbhandle, paste("Select COLUMN_NAME from INFORMATION_SCHEMA.columns where TABLE_NAME=\'", table_name, "\' and TABLE_SCHEMA=\'", schema_name, "\'", sep = ""))
    cols<-data.frame(cols, stringsAsFactors = FALSE)
    cols$Var.1 <- levels(cols$Var.1)
    j<-1
    for(j in 1:nrow(cols)){
      dbDetails[[paste("col",tableName, cols[j,1], sep = "_")]] <-  as.character(cols[j,1])
      j<-j+1
    }


    i<-i+1
  }
  rm( list = c("tables_sys","cols","i","j","schema_name","table_name","tableName"))

  #Get View Field Names

  view_sys <- as.list(sqlQuery(dbhandle, 'Select SS.name, ST.name from sys.views ST LEFT JOIN sys.schemas SS ON ST.schema_id=SS.schema_id ORDER BY ST.Name'))
  i<-1
  for(i in 1:length(view_sys$name.1)){
    cat(".")
    schema_name <- view_sys$name[i]
    view_name <- view_sys$name.1[i]
    viewName <- paste(schema_name, ".", view_name, sep = "")
    cols <- sqlQuery(dbhandle, paste("Select COLUMN_NAME from INFORMATION_SCHEMA.columns where TABLE_NAME=\'", view_name, "\' and TABLE_SCHEMA=\'", schema_name, "\'", sep = ""))
    cols<-data.frame(cols, stringsAsFactors = FALSE)
    cols$Var.1 <- levels(cols$Var.1)
    j<-1
    for(j in 1:nrow(cols)){
      dbDetails[[paste("vcol",viewName, cols[j,1], sep = "_")]] <-  as.character(cols[j,1])
      j<-j+1
    }


    i<-i+1
  }
  rm( list = c("view_sys","cols","i","j","schema_name","view_name","viewName"))

  cat("Done\n")

  return(dbDetails)
}
