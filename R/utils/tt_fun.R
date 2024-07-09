#### FUNCTION TO GRAB TRAVEL TIME MATRICES #####################################

ttm <- function(mode = "foot", under_x_minutes = 15) {
  
  # Connect to the database
  conn <- cc.data::db_connect()
  
  # Laval DBs
  DBs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DB")
  ids <- paste0(paste0("'", DBs$GeoUID, "'"), collapse = ", ")
  
  # Grab the matrix
  travel_seconds <- under_x_minutes * 60
  matrix <- DBI::dbGetQuery(conn, 
                            sprintf("SELECT * FROM ttm_%s_DB WHERE `from` IN (%s) AND travel_seconds < %s", 
                                    mode, ids, travel_seconds))
  
  DBI::dbDisconnect(conn)
  
  return(matrix)
  
}

ttm_DA <- function(mode = "car") {
  # Connect to the database
  conn <- cc.data::db_connect()
  DAs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DA")
  
  matrix <- purrr::map_dfr(DAs$GeoUID, \(ID) {
    DBI::dbGetQuery(conn, sprintf("SELECT * FROM ttm_car_%s", ID))
  })
  
  DBI::dbDisconnect(conn) 
  
  return(matrix)
}
