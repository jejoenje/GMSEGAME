library(RMySQL)
source("connect_db.R")

db = connect_game_dbase()

run = dbGetQuery(db, "SELECT * FROM run")
run_par = dbGetQuery(db, "SELECT * FROM run_par")
gdata = dbGetQuery(db, "SELECT * FROM gdata")
scores = dbGetQuery(db, "SELECT * FROM scores") 

write.csv(run, "~/af_run.csv")
write.csv(run_par, "~/af_run_par.csv")
write.csv(gdata, "~/af_run_gdata.csv")
write.csv(scores, "~/af_run_scores.csv")
