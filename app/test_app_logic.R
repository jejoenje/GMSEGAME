rm(list=ls())

source("app/app_helpers.R")
source("app/infoDialogs.R")
source("app/connect_db.R")
source("app/dbase_functions.R")

INIT_SCARING_BUDGET = 100
INIT_CULLING_BUDGET = 100

NEWSESSION = TRUE

initGame = function() {
  gdata = list()
  
  GMSE_PARAS = list(
    K = 5,
    LAND_OWNERSHIP = TRUE,
    STAKEHOLDERS  = round(runif(1, 4, 12)),
    MANAGER_BUDGET  = 1000,
    MANAGE_TARGET  = 2000,
    OBSERVE_TYPE  = 0,
    RES_MOVE_OBS  = TRUE,
    #RES_DEATH_K  = round(runif(1, 1000, 6000)),
    RES_DEATH_K  = 5000,
    #LAMBDA = runif(1, 0.2, 0.4),
    LAMBDA = 0.3,
    RES_DEATH_TYPE  = 3,
    REMOVE_PR = runif(1, 0.05, 0.2),
    #REMOVE_PR = 0.25,
    USER_BUDGET = 1500,
    CULLING = TRUE,
    SCARING = TRUE,
    TEND_CROPS = TRUE,
    #TEND_CROP_YLD = runif(1, 0.1, 0.4),
    TEND_CROP_YLD = 0.3,
    LAND_DIM_1 = 100,
    LAND_DIM_2 = 100,
    RESOURCE_INI = 1000,
    TIME_MAX = 20,
    PUBLIC_LAND = 0.2,
    OWNERSHIP_VAR = 0.5,
    USR_BUDGET_RNG = 500
  )
  
  initdata = init_man_control(K = GMSE_PARAS$K, gmse_paras = GMSE_PARAS)
  gdata$summary = initdata$summary
  gdata$laststep = initdata$gmse_list[[length(initdata$gmse_list)]]
  gdata$observed_suggested = initdata$observed_suggested
  gdata$yields = initdata$prev_yield
  gdata$paras = GMSE_PARAS
  
  gdata$land_colors = sample(grey.colors(GMSE_PARAS$STAKEHOLDERS))
  
  return(gdata)
}

initBudget = function(paras) {
  budget = list(
    total = paras$MANAGER_BUDGET,
    culling = INIT_CULLING_BUDGET,
    scaring = INIT_SCARING_BUDGET,
    remaining = paras$MANAGER_BUDGET-(INIT_CULLING_BUDGET+INIT_SCARING_BUDGET)
  )
  return(budget)
}

getLastParas = function(laststep, K) {
  last_paras = list(
    K = K,
    LAND_OWNERSHIP = laststep$land_ownership,
    STAKEHOLDERS  = laststep$stakeholders,
    MANAGER_BUDGET  = laststep$manager_budget,
    MANAGE_TARGET  = laststep$manage_target,
    OBSERVE_TYPE  = laststep$observe_type,
    RES_MOVE_OBS  = laststep$res_move_obs,
    RES_DEATH_K  = laststep$res_death_K,
    LAMBDA = laststep$lambda,
    RES_DEATH_TYPE  = laststep$res_death_type,
    REMOVE_PR = laststep$remove_pr,
    USER_BUDGET = laststep$user_budget,
    CULLING = laststep$culling,
    SCARING = laststep$scaring,
    TEND_CROPS = laststep$tend_crops,
    TEND_CROP_YLD = laststep$tend_crop_yld,
    LAND_DIM_1 = laststep$land_dim_1,
    LAND_DIM_2 = laststep$land_dim_2,
    RESOURCE_INI = laststep$RESOURCE_ini
  )
  return(last_paras)
}

updateCurrentBudget = function(budget, manager_budget, culling_budget, scaring_budget) {
  budget$total = manager_budget
  budget$culling = culling_budget
  budget$scaring = scaring_budget
  budget$leftover = budget$total-(budget$culling+budget$scaring)
  return(budget)
}

budgetToCost = function(budgetAllocated, minimum_cost) {
  return(floor(budgetAllocated/10 + minimum_cost))
}

RUN = reactiveValues(id = NULL)

GDATA = list(summary = NULL,
                       laststep = NULL,
                       observed_suggested = NULL,
                       yields = NULL,
                       year = NULL,
                       MaxYear = NULL,
                       achievedMaxYear = NULL,
                       extinction = NULL,
                       PLAYER_NAME = NULL,
                       live_scores = NULL)
CURRENT_BUDGET = list(total = NULL,
                                culling = NULL,
                                scaring = NULL,
                                leftover = NULL)
USER_DISPLAY_SELECT = list(user = NULL, act_plot_dat = NULL)

### Run initial game steps and initialise budgets:
gdata = initGame()
budget = initBudget(paras = gdata$paras)
### Add initial game data to reactiveValues:
GDATA$summary = gdata$summary
GDATA$laststep = gdata$laststep
GDATA$observed_suggested = gdata$observed_suggested
GDATA$yields = gdata$yields
GDATA$year = 0
GDATA$MaxYear = gdata$paras$TIME_MAX
GDATA$achievedMaxYear = FALSE
GDATA$extinction = FALSE
GDATA$land_colors = gdata$land_colors
GDATA$paras = gdata$paras
GDATA$live_scores = list(res = 100, yld = 100)
### Initialise budget reactiveValues:
CURRENT_BUDGET$total = budget$total
CURRENT_BUDGET$culling = budget$culling
CURRENT_BUDGET$scaring = budget$scaring
CURRENT_BUDGET$leftover = budget$remaining

### User input
culling_cost = budgetToCost(450, minimum_cost = 10)
scaring_cost = budgetToCost(450, minimum_cost = 10)

costs_as_input = list(culling = culling_cost, scaring = scaring_cost)
prev = GDATA$laststep
prev = set_man_costs(prev, newcost = costs_as_input)

### Run next time step:
nxt = try({gmse_apply_UROM(get_res = "Full", old_list = prev)}, silent = TRUE)

if(class(nxt)!="try-error") {
  
  # Add appropriate outputs.
  GDATA$summary = append_UROM_output(dat = nxt, costs = costs_as_input, old_output = GDATA$summary)
  GDATA$yields = rbind(GDATA$yields, tapply(nxt$LAND[,,2],nxt$LAND[,,3],mean)) # Store per-user yield (before reset)
  
    # Reset time step for GMSE, includes landscape reset.
  nxt$LAND[,,2] = 1
  GDATA$laststep = nxt
  
  # Update live scores.
  GDATA$live_scores = updateLiveScores(GDATA)
  
  # Increment year counter and check if we're inside MaxYear:
  GDATA$year = GDATA$year+1
  if(GDATA$year > (GDATA$MaxYear-1) ) GDATA$achievedMaxYear = TRUE
  
} else {
  
  GDATA$extinction = TRUE
  
}

plot_land_res(land = GDATA$laststep$LAND, 
              resources = GDATA$laststep$RESOURCES, 
              cols = GDATA$land_colors,
              extinction_message = GDATA$extinction,
              show_labels = TRUE,
              selected_user = USER_DISPLAY_SELECT$user
)
