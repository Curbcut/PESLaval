#### FUNCTION TO GRAB TRAVEL TIME MATRICES #####################################

ttm <- function(mode = "foot", under_x_minutes = 15) {
  
  # File name
  file_name <- sprintf("data/ttm_%s_%s.qs", mode, under_x_minutes)
  
  # If it already exists, just grab it
  if (file.exists(file_name)) return(qs::qread(file_name))
  
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
  # Add self
  self <- tibble::tibble(from = matrix$from,
                         to = matrix$from,
                         travel_seconds = 0)
  matrix <- rbind(matrix, self)
  
  # Disconnect
  DBI::dbDisconnect(conn)
  
  # Save it, similar as a cache
  qs::qsave(matrix, file_name)
  
  return(matrix)
  
}

ttm_DA <- function(mode = "car") {
  # Connect to the database
  conn <- cc.data::db_connect()
  DAs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DA")
  
  matrix <- sapply(DAs$GeoUID, \(ID) {
    DBI::dbGetQuery(conn, sprintf("SELECT * FROM ttm_car_%s", ID))
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  matrix <- mapply(\(n, df) {
    df[1] <- n
    df <- tibble::as_tibble(df)
    names(df) <- c("from", "to", "travel_seconds")
    df
  }, names(matrix), matrix, SIMPLIFY = FALSE)
  matrix <- Reduce(rbind, matrix)
  
  DBI::dbDisconnect(conn) 
  
  return(matrix)
}
