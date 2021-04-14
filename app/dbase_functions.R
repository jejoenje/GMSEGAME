connect_game_dbase = function(databaseName = "testing", 
                              databaseHost = "127.0.0.1", 
                              databasePort = 3306,
                              databaseUser = "jejoenje", 
                              databasePassword = "$Craigie17_st") {
  db = dbConnect(MySQL(), 
            dbname = databaseName, 
            host = databaseHost, 
            port = databasePort, 
            user = databaseUser, 
            password = databasePassword)
  return(db)
}

newRunRecord = function(session, player, startTime, extinct) {
  
  db = connect_game_dbase()
  
  q = sprintf("INSERT INTO run (session, player, startTime, extinct) VALUES ('%s', '%s', '%s', %d)", session, player, startTime, extinct)
  dbGetQuery(db, q)
  newID = dbGetQuery(db, "SELECT max(id) FROM run")[1,]
  dbDisconnect(db)
  return(newID)
}

updateRunRecord = function(runID, endTime, extinct) {
  db = connect_game_dbase()
  q1 = sprintf("UPDATE run SET endTime = '%s' WHERE id = %d", endTime, runID)
  dbGetQuery(db, q1)
  q2 = sprintf("UPDATE run SET extinct = %d WHERE id = %d", extinct, runID)
  dbGetQuery(db, q2)
  dbDisconnect(db)
}
