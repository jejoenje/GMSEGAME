connect_game_dbase = function(databaseName = Sys.getenv("db_gmsegame"), 
                              databaseHost = "127.0.0.1", 
                              databasePort = 3306,
                              databaseUser = Sys.getenv("db_u"), 
                              databasePassword = Sys.getenv("db_p")) {
  db = dbConnect(MySQL(), 
                 dbname = databaseName, 
                 host = databaseHost, 
                 port = databasePort, 
                 user = databaseUser, 
                 password = databasePassword)
  return(db)
}