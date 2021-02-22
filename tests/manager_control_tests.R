# library(devtools)
# install_github("ConFooBio/gmse", ref = "man_control")
library(GMSE) 
rm(list=ls())

call_bogus_for_debug = function(res_mod  = resource, 
                                obs_mod  = observation, 
                                man_mod  = manager, 
                                use_mod  = user,
                                get_res  = "basic",
                                old_list = NULL,
                                ...) {
    return(sys.call())
}

source("app_helpers.R")

LAND_OWNERSHIP = TRUE
TEND_CROPS = TRUE
SCARING = TRUE
CULLING = TRUE
OBSERVE_TYPE = 0
RES_MOVE_OBS = TRUE
RES_DEATH_K = 3000
LAMBDA = 0.3
MANAGE_TARGET = 1500
STAKEHOLDERS = 4
USER_BUDGET = 1500
MANAGER_BUDGET = 1000
RES_DEATH_TYPE = 3
REMOVE_PR = 0.05

### Initial time steps:
init_steps = init_man_control(K = 5)
# Appends output:
output = init_steps$summary 
# Extracts last time step (last `old_list`)
prev = init_steps$gmse_list[[length(init_steps$gmse_list)]]
# Print output:
init_steps$summary
init_steps$observed_suggested

YIELD = get_init_yields(init_steps$gmse_list)

#par(mfrow = c(1,2))
plot_pop(output, yield_dat = YIELD, track_range = FALSE)
plot_land_res(prev$LAND, prev$RESOURCES)
output

### User input
costs_as_input = list(culling = 100, scaring = 10)
prev = set_man_costs(prev, newcost = costs_as_input)

### Run next time step:
nxt = try({gmse_apply_UROM(get_res = "Full", old_list = prev)}, silent = TRUE)

if(class(nxt)!="try-error") {
    # Add appropriate outputs.
    output = append_UROM_output(dat = nxt, costs = costs_as_input, old_output = output)
    #YIELD = rbind(YIELD, tapply(nxt$LAND[,,2],nxt$LAND[,,3],mean))
    YIELD = add_yields(nxt, YIELD)
    # Reset time step
    prev = nxt
    
    ### Output new results:
    observed_suggested(prev)
    output
    par(mfrow = c(1,2))
    plot_pop(output, yield_dat = YIELD, track_range = FALSE)
    plot_land_res(prev$LAND, prev$RESOURCES)
    output
} else {
    print("STOP - POPULATION WIPED OUT")
    output
}
    
