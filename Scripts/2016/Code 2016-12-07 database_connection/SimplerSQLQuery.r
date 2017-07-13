#_______________________________________________________________________________
# This package creates a simplified SQL interface for complex data structures.
# Tested are
#   data.frame, data.table and tbl (dplyr)
#   SQLite, PostgreSQL, MySQL
# usage examples can be found below.
# Requirements (most are optional):
#   tcltk, R6, DBI, dplyr, data.table
# 
# Questions? Falk.Mielke@hu-berlin.de


#_______________________________________________________________________________
# Password Prompt
PasswordPrompt <- function(  title = "Password"
                           , label = "please enter password:"
                           ){
    require(tcltk)
    prompt <- tktoplevel()
    tktitle(prompt) <- title

    pwd <- tclVar("")

    tkgrid(tklabel(prompt, text = label))
    tkgrid(pwdbox <- tkentry(prompt, textvariable=pwd, show="*"))

    tkbind(pwdbox,"<Return>",function() tkdestroy(prompt))
    tkgrid(tkbutton(prompt,text="OK",command=function() tkdestroy(prompt)))

    tkwait.window(prompt)

    return(tclvalue(pwd)) 
}  

#_______________________________________________________________________________
# a more convenient SQL query

require(R6)
require(DBI)
SQL_DF <- R6Class(
        "SQL_DF"
      , public = list(
          # the constructor
            initialize = function(  drv
                                  , dbname
                                  , user = NULL
                                  , host = NULL
                                  , port = NULL
                                  ) {
                # drivers:
                #       RSQLite::SQLite()
                #       RMySQL::MySQL()
                #       RPostgreSQL::PostgreSQL()
                
                if (identical(drv, RSQLite::SQLite())) {
                    private$db <- dbConnect(drv, dbname)
                } else {
                    private$db <- dbConnect(  
                                drv
                              , user = user
                              , dbname = dbname
                              , host = host
                              , port = port
                              , password = PasswordPrompt(
                                                title = "Database Query"
                                              , label = sprintf('please enter password for \n %s @ %s'
                                                                , user, host)
                                                          )
                              )
                }
                private$alive <- T
            } # end: initialize

          # run a query and return converted result
          , Run = function(query_text) {
                if (!private$alive) {
                    return()
                }
                rs <- dbSendQuery(private$db, query_text)
                if (dbGetRowsAffected(rs) == 0) {
                    dbClearResult(rs)
                    return (NULL)
                } else {
                    result <- fetch(
                                            rs
                                          , n = dbGetRowsAffected(rs)
                                        )
                    dbClearResult(rs)
                    return  ( private$Convert(result) )
                }
            }
          # upload data to a table
          , Upload = function(dat, table_name, ...) {
                if (!private$alive) {
                    return()
                }
                dbWriteTable(private$db, table_name, as.data.frame(dat), ...)  
                return()
            }
          # download a whole table
          , LoadTable = function(table_name, ...) {
                if (!private$alive) {
                    return()
                }
                result <- dbReadTable(private$db, table_name, ...)  
                return( private$Convert(result) )
            }
          
          # upload data to a table
          , Close = function() {
                if (!private$alive) {
                    return()
                }
                dbDisconnect(private$db)
                private$alive <- F
              # rm(list=ls()[!grepl('^QQQ$',ls())]) 
            }
        )
      , active = list(
            tables = function(){
                return(dbListTables(private$db))
            }
          , connected = function(){
                return(private$alive)
            }
        )
      , private = list(
            db = NULL
          , alive = F
          , Convert = function(result) {
                # require(data.table)
                return (as.data.frame(result))
            }
        )
    )

# analogous, returning a TBL
require(dplyr)
SQL_TBL <- R6Class(
        "SQL_TBL"
      , inherit = SQL_DF
      , private = list(
            Convert = function(result) {
                return (as.tbl(result))
            }
        )
    )

# analogous, returning a data.table
require(data.table)
SQL_DT <- R6Class(
        "SQL_DT"
      , inherit = SQL_DF
      , private = list(
            Convert = function(result) {
                return (as.data.table(result))
            }
        )
    )


### Usage example 1:
# sql_connection <- SQL_DF$new(drv = RPostgreSQL::PostgreSQL()
#                          , user = "monkey"
#                          , dbname = "sandbox"
#                          , host = "localhost"
#                          , port = 5432
#                          )
# 
# a <- sql_connection$Run("SELECT * FROM hflights")
# b <- sql_connection$LoadTable("hflights")
# 

### Usage example 2:
sql_to_dt <- SQL_DT$new(drv = RSQLite::SQLite(), dbname = ":memory:")
print (sql_to_dt$connected)
sql_to_dt$Upload(as.data.table(mtcars), 'MTCars', overwrite = T, append = F)
dt <- sql_to_dt$Run('SELECT * FROM MTCars')
summary(dt)
sql_to_dt$Run('DROP TABLE MTCars')
sql_to_dt$Close()
print (sql_to_dt$connected)
# 



#_______________________________________________________________________________
# Optionally, pack a library.
# if (F) {
#     package.skeleton( 
#         name = 'SimplerSQLQuery'
#       , list = c('PasswordPrompt', 'SQL_DF', 'SQL_DT', 'SQL_TBL')
#       , path = '/grove/R/CEB'
#       , force = T
#       )
# }

#_______________________________________________________________________________
# eof. thanks for reading!