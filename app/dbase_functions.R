

newRunRecord = function(session, player, startTime, extinct) {
  
  db = connect_game_dbase()
  
  q = sprintf("INSERT INTO run (session, player, startTime, extinct) VALUES ('%s', '%s', '%s', %d)", session, player, startTime, extinct)
  dbGetQuery(db, q)
  newID = dbGetQuery(db, "SELECT max(id) FROM run")[1,]
  if(is.null(newID)) newID = 1
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

addRunPar = function(runID, paras) {
  db = connect_game_dbase()
  q = list()
  q[[1]] = sprintf("INSERT INTO run_par (id) VALUES (%d)", runID)
  q[[2]] = sprintf("UPDATE run_par SET K = %d WHERE ID = %d", paras$K, runID)
  q[[3]] = sprintf("UPDATE run_par SET land_ownership = %d WHERE ID = %d", as.numeric(paras$LAND_OWNERSHIP), runID )
  q[[4]] = sprintf("UPDATE run_par SET stakeholders = %d WHERE ID = %d", paras$STAKEHOLDERS, runID )
  q[[5]] = sprintf("UPDATE run_par SET manager_budget = %d WHERE ID = %d", paras$MANAGER_BUDGET, runID )
  q[[6]] = sprintf("UPDATE run_par SET manage_target = %d WHERE ID = %d", paras$MANAGE_TARGET, runID )
  q[[7]] = sprintf("UPDATE run_par SET observe_type = %d WHERE ID = %d", paras$OBSERVE_TYPE, runID )
  q[[8]] = sprintf("UPDATE run_par SET res_move_obs = %d WHERE ID = %d", paras$RES_MOVE_OBS, runID )
  q[[9]] = sprintf("UPDATE run_par SET res_death_k = %d WHERE ID = %d", paras$RES_DEATH_K, runID )
  q[[10]] = sprintf("UPDATE run_par SET lambda = %f WHERE ID = %d", paras$LAMBDA, runID )
  q[[11]] = sprintf("UPDATE run_par SET res_death_type = %d WHERE ID = %d", paras$RES_DEATH_TYPE, runID )
  q[[12]] = sprintf("UPDATE run_par SET remove_pr = %f WHERE ID = %d", paras$REMOVE_PR, runID )
  q[[13]] = sprintf("UPDATE run_par SET user_budget = %d WHERE ID = %d", paras$USER_BUDGET, runID )
  q[[14]] = sprintf("UPDATE run_par SET culling = %d WHERE ID = %d", as.numeric(paras$CULLING), runID )
  q[[14]] = sprintf("UPDATE run_par SET scaring = %d WHERE ID = %d", as.numeric(paras$SCARING), runID )
  q[[15]] = sprintf("UPDATE run_par SET tend_crops = %d WHERE ID = %d", as.numeric(paras$TEND_CROPS), runID )
  q[[16]] = sprintf("UPDATE run_par SET land_dim_1 = %d WHERE ID = %d", as.numeric(paras$LAND_DIM_1), runID )
  q[[17]] = sprintf("UPDATE run_par SET land_dim_2 = %d WHERE ID = %d", as.numeric(paras$LAND_DIM_2), runID )
  q[[18]] = sprintf("UPDATE run_par SET resource_ini = %d WHERE ID = %d", paras$RESOURCE_INI, runID )
  q[[19]] = sprintf("UPDATE run_par SET tend_crop_yld = %f WHERE ID = %d", paras$TEND_CROP_YLD, runID )
  q[[20]] = sprintf("UPDATE run_par SET max_years = %d WHERE ID = %d", paras$TIME_MAX, runID)
  
  lapply(q, function(x) dbGetQuery(db, x))
  dbDisconnect(db)
}

addAllRunPar = function(runID,t,paras) {
  db = connect_game_dbase()
  q = sprintf("INSERT INTO run_par_all (id, t, K, land_ownership, 
                                    stakeholders, manager_budget, manage_target, observe_type, 
                                    res_move_obs, res_death_K, lambda, res_death_type, remove_pr, 
                                    user_budget, culling, scaring, tend_crops, land_dim_1, 
                                    land_dim_2, resource_ini, tend_crop_yld) 
                            VALUES (%d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %f, %d, %f, 
                                    %d, %d, %d, %d, %d, %d, %d, %f)", 
              runID,t,paras$K,paras$LAND_OWNERSHIP,paras$STAKEHOLDERS,paras$MANAGER_BUDGET,paras$MANAGE_TARGET,
              paras$OBSERVE_TYPE, paras$RES_MOVE_OBS, paras$RES_DEATH_K, paras$LAMBDA, paras$RES_DEATH_TYPE, paras$REMOVE_PR,
              paras$USER_BUDGET, paras$CULLING, paras$SCARING, paras$TEND_CROPS, paras$LAND_DIM_1, paras$LAND_DIM_2, paras$RESOURCE_INI, 
              paras$TEND_CROP_YLD)
  dbGetQuery(db, q)
  dbDisconnect(db)
}

### Called only on start of a new session. Inserts all K time steps into gdata dbase table.
addInitGdata = function(runID, gd) {
  db = connect_game_dbase()
  t = 1:nrow(gd)
  gd = cbind(t, gd)
  lastline = gd[nrow(gd),]
  gd = gd[1:(nrow(gd)-1),]
  
  ### Add fully complete previous time steps:
  apply(gd, 1, function(x) {
    dbGetQuery(db, sprintf("INSERT INTO 
                           gdata (id, t, res, obs, culls, scares, tend_crops, cull_cost, scare_cost, yield) 
                           VALUES (%d, %d, %d, %f, %d, %d, %d, %d, %d, %f)",
                           runID, x[['t']], x[['res']], x[['obs']], x[['culls']], 
                           x[['scares']], x[['tend_crops']], x[['cull_cost']], x[['scare_cost']], x[['yield']]))
  })

  ### Add available data for last time step:
  lastline = lastline[!is.na(lastline)]
  valnames = paste("id", paste(names(lastline),collapse = ","),sep=",")
  vals = paste(runID, paste(round(lastline,3),collapse = ","), sep=",")
  dbGetQuery(db, sprintf("INSERT INTO gdata (%s) VALUES (%s)", valnames, vals))
  
  dbDisconnect(db)
  
}

addInitYieldData = function(runID, yields) {
  db = connect_game_dbase()
  
  row.names(yields) = 1:nrow(yields)
  yields = melt(yields)
  names(yields) = c("t","user","pyield")
  yields = yields[order(yields$t, yields$user),]
  
  yields = cbind(data.frame(id = rep(runID, nrow(yields))), yields)
  
  yields_names = names(yields)
  yields_names =  paste(yields_names,collapse = ",")
  
  for(i in 1:nrow(yields)) {
    dbGetQuery(db, sprintf("INSERT INTO yield (id, t, user, pyield) VALUES (%d,%d,%d,%f)", 
                           yields[i,"id"], yields[i,"t"], yields[i,"user"], yields[i,"pyield"]))
  }
  
  dbDisconnect(db)
  
}

addYields = function(runID, yields) {
  db = connect_game_dbase()
  
  new_t = nrow(yields)
  new_yields = yields[new_t,]
    
  db_last_t = dbGetQuery(db, sprintf("SELECT max(t) FROM yield WHERE id = %d", runID))[[1]]
  # Sanity safety catch:
  if(db_last_t+1 != new_t) stop("addYields: LAST t IN NEW DATA DOES NOT MATCH WITH NEXT ONE FOR DBASE. QUITTING.")

  db_users = dbGetQuery(db, sprintf("SELECT user FROM yield WHERE id = %d AND t = %d",runID,db_last_t))
  db_users = db_users$user
  if(length(db_users) != length(new_yields)) stop ("addYields: UNEXPECTED NUMBER OF USERS IN NEW DATA!")
  
  new_yields = melt(new_yields)
  names(new_yields) = "pyield"
  new_yields$id = runID
  new_yields$t = new_t
  new_yields$user = db_users
  
  for(i in 1:nrow(new_yields)) {
    dbGetQuery(db, sprintf("INSERT INTO yield (id, t, user, pyield) VALUES (%d,%d,%d,%f)", 
                           new_yields[i,"id"], new_yields[i,"t"], new_yields[i,"user"], new_yields[i,"pyield"]))
  }
  
  dbDisconnect(db)
  
}

### Adds new line to gdata file when nextStep is pressed.
### Note this needs a few steps as it needs to both add a new line (with population count and observation but missing the actions,
###  for the next time step) as well as updating the previous line with actions from previous step (as recorded in GDATA, but not the 
###  inputs currently displayed!).
addNewData = function(runID, gd) {
  db = connect_game_dbase()
  
  prev = gd[nrow(gd)-1,]
  new = gd[nrow(gd),]
  
  # Find last t for given runID and add this to the "previous" data
  last_t = dbGetQuery(db, sprintf("SELECT max(t) FROM gdata WHERE id = %d", runID))[[1]]
  #prev = cbind(t = last_t, prev)
  # Get the last of data for the given runID
  last_dat = dbGetQuery(db, sprintf("SELECT * FROM gdata WHERE id = %d AND t = %s", runID, last_t))
  # Find which values from the last line are missing and add these from the new run:
  last_nulls = names(last_dat)[which(is.na(last_dat))]
  last_nulls_vals = as.vector(prev[last_nulls])
  
  # Ensure we don't try to enter blanks into previous line:
  last_nulls = last_nulls[!is.na(last_nulls_vals)]
  last_nulls_vals = last_nulls_vals[!is.na(last_nulls_vals)]
  for(i in 1:length(last_nulls)) {
    dbGetQuery(db, sprintf("UPDATE gdata SET %s = %d WHERE id = %d AND t = %d", last_nulls[i],last_nulls_vals[[i]],runID,last_t))
  }
  
  # Add the new data line:
  new_not_null = new[!is.na(new)]
  new_names = names(new)[!is.na(new)]
  new_vals = paste(round(as.vector(new_not_null),3),collapse = ",")
  new_vals = sprintf("%d,%d,%s",runID,last_t+1,new_vals)
  new_names = c("id", "t",new_names)
  new_names = paste(new_names,collapse=",")
  
  dbGetQuery(db, sprintf("INSERT INTO gdata (%s) VALUES (%s)", new_names, new_vals))
  
  dbDisconnect(db)
  
}

### Ensures that the last input cull and scare costs are entered when the population went extinct.
addLastCostsOnExtinction = function(runID, cull_cost, scare_cost) {
  
  db = connect_game_dbase()
  
  last_t = dbGetQuery(db, sprintf("SELECT max(t) FROM gdata WHERE id = %d", runID))[[1]]
  dbGetQuery(db, sprintf("UPDATE gdata SET cull_cost = %d WHERE id = %d AND t = %d", cull_cost, runID, last_t))
  dbGetQuery(db, sprintf("UPDATE gdata SET scare_cost = %d WHERE id = %d AND t = %d", scare_cost, runID, last_t))
  
  dbDisconnect(db)
  
}

updateLiveScores = function(gd) {
  
  res = as.vector(gd$summary[,"res"])
  res = res[6:length(res)]
  res_score = round(mean(res/res[1])*100)
  
  yld = gd$yields
  yld = yld[6:nrow(yld),]
  yld_score = round(mean(yld)*100)
  
  return(list(res = round(res_score), yld = round(yld_score) ))
  
}

addScores = function(runID, gd) {
  
  ### Flag to keep a record of what scoring calculaton is used. Where this is NA in the database, the score was calculated using a the
  ### initial version of this function, i.e. commit 0f501c2877caa1ab00f73044da7186287fed514f and older.
  SCORE_VERSION_ID = 1
  
  db = connect_game_dbase()
  
  res = gd$summary[,"res"]
  res = res[6:length(res)]
  res_score = round(mean(res/res[1])*100)
  
  yld = gd$yields
  yld = yld[6:nrow(yld),]
  yld_score = round(mean(yld)*100)
  
  steps = nrow(gd$summary)
  
  total_score = res_score + yld_score
  
  dbGetQuery(db, sprintf("INSERT INTO scores (id, steps, mean_res, mean_yield, total, score_version) VALUES (%d,%d,%d,%d,%d,%d)",
                         runID,steps,res_score,yld_score,total_score,SCORE_VERSION_ID))
  
  dbDisconnect(db)
  
}

getScores = function() {
  db = connect_game_dbase()
  
  # Get top 10 and match player name:
  scores = dbGetQuery(db, "SELECT scores.*, run.player FROM scores INNER JOIN run ON scores.id = run.id ORDER BY scores.total DESC LIMIT 10")
  
  dbDisconnect(db)
  return(scores)
}

getCurrentRunScore = function(runID) {
  db = connect_game_dbase()
  scores = dbGetQuery(db, sprintf("SELECT * FROM scores WHERE id = '%d'", runID))
  player = dbGetQuery(db, sprintf("SELECT player FROM run WHERE id = '%d'", runID))[[1]]
  scores$player = player
  dbDisconnect(db)
  return(scores)
}

# CREATE TABLE run (
#   id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
#   session VARCHAR(32) NOT NULL,
#   player VARCHAR(64),
#   startTime VARCHAR(23) NOT NULL,
#   endTime VARCHAR(23),
#   extinct BOOLEAN NOT NULL
# );

# CREATE TABLE gdata (
#   id INT NOT NULL,
#   t INT,
#   res INT,
#   obs FLOAT,
#   culls INT,
#   scares INT,
#   tend_crops INT,
#   cull_cost INT,
#   scare_cost INT,
#   yield FLOAT
# );

# CREATE TABLE run_par (
#   id INT NOT NULL PRIMARY KEY,
#   K INT,
#   land_ownership BOOLEAN,
#   stakeholders INT,
#   manager_budget INT,
#   manage_target INT,
#   observe_type INT,
#   res_move_obs INT,
#   res_death_k INT,
#   lambda FLOAT,
#   res_death_type INT,
#   remove_pr FLOAT,
#   user_budget INT,
#   culling BOOLEAN,
#   scaring BOOLEAN,
#   tend_crops BOOLEAN,
#   land_dim_1 INT,
#   land_dim_2 INT,
#   resource_ini INT,
#   tend_crop_yld FLOAT,
#   max_years INT
# );

# CREATE TABLE scores (
#   id INT NOT NULL PRIMARY KEY,
#   steps INT,
#   mean_res INT,
#   mean_yield INT,
#   total INT
# );

# CREATE TABLE yield (
#   id INT NOT NULL,
#   t INT NOT NULL,
#   user INT NOT NULL,
#   pyield FLOAT NOT NULL
# );

# CREATE TABLE run_par_all (
#   id INT NOT NULL,
#   t INT NOT NULL,
#   K INT,
#   land_ownership BOOLEAN,
#   stakeholders INT,
#   manager_budget INT,
#   manage_target INT,
#   observe_type INT,
#   res_move_obs INT,
#   res_death_k INT,
#   lambda FLOAT,
#   res_death_type INT,
#   remove_pr FLOAT,
#   user_budget INT,
#   culling BOOLEAN,
#   scaring BOOLEAN,
#   tend_crops BOOLEAN,
#   land_dim_1 INT,
#   land_dim_2 INT,
#   resource_ini INT,
#   tend_crop_yld FLOAT
# )