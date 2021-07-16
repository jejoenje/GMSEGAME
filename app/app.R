rm(list=ls())
library(shiny)
library(shinyjs)
library(shinyBS)
library(waiter)
library(GMSE) # CURRENTLY NEEDS devtools::install_github("ConFooBio/GMSE", ref = "man_control")
library(RMySQL)
library(reshape2)
library(DT)
library(scales)

source("app_helpers.R")
source("infoDialogs.R")
source("connect_db.R")
source("dbase_functions.R")

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
        PUBLIC_LAND = 0,
        OWNERSHIP_VAR = 0.5,
        USR_BUDGET_RNG = 0
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
        RESOURCE_INI = laststep$RESOURCE_ini,
        PUBLIC_LAND = laststep$public_land,
        OWNERSHIP_VAR = laststep$ownership_var,
        USR_BUDGET_RNG = laststep$usr_budget_rng
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


reset_waiting_screen <- tagList(
    spin_flower(),
    h4("Resetting game - running intial time steps...")
) 

init_waiting_screen <- tagList(
    spin_flower(),
    h4("Starting game...")
) 

ui <- fixedPage(
    tagList(tags$head(
        tags$style(".butt{font-family: Courier New;}"),
        HTML("
            <!-- Matomo -->
            <script type='text/javascript'>
              var _paq = window._paq = window._paq || [];
              _paq.push(['trackPageView']);
              _paq.push(['enableLinkTracking']);
              (function() {
                var u='https://jejoenje.matomo.cloud/';
                _paq.push(['setTrackerUrl', u+'matomo.php']);
                _paq.push(['setSiteId', '2']);
                var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
                g.type='text/javascript'; g.async=true; g.src='//cdn.matomo.cloud/jejoenje.matomo.cloud/matomo.js'; s.parentNode.insertBefore(g,s);
              })();
            </script>
            <!-- End Matomo Code -->
             ")
    )),
    
    shinyjs::useShinyjs(),
    use_waiter(),

    fixedRow(
        column(1),
        column(5, align = "center",
               div(id = "pop_plot_div", style = "padding: 1em",
                   plotOutput("pop_plot", height="auto")
                  ),
               bsTooltip(id = "pop_plot_div", "Animal population trend (black line and points) and agricultural yield (green lines). Dotted horizontal green line represents expected maximum yield without damage from animals or investment in crops.", placement = "left", trigger = "hover", options = NULL)
        ),
        column(5, align = "center",
            fixedRow(
                div(id = "budget_report_header", style = "padding-top:4em; padding-left:1em; padding-right:1em, padding-bottom:1em",
                    column(4,
                           div(span("Budget remaining", 
                                    style = "color:#D35E60; font-family: Courier New; font-size:125%; font-weight: bold;vertical-align: middle;"))
                    ),
                    column(4,
                           div(span("Animal Score", 
                                    style = "color:#000000; font-family: Courier New; font-size:125%; font-weight: bold;vertical-align: middle;"))
                    ),
                    column(4,
                           div(span("Yield Score", 
                                    style = "color:#000000; font-family: Courier New; font-size:125%; font-weight: bold;vertical-align: middle;"), 
                               style = "padding-left: 1em;padding-right: 1em;")
                    )    
                )
                
            ),
            
            fixedRow(
                div(id = "budget_report", style = "padding-top:1em; padding-left:1em; padding-right:1em, padding-bottom:1em",
                    # column(4, align = "right",
                    #        div(style="text-align: center; vertical-align: middle; padding-top: 3em, padding-left: 3em",
                    #            span("Budget available", style="color:#D35E60; font-family: Courier New; font-size:125%;"),br(),
                    #            span("(to set costs)", style="color:#D35E60; font-family: Courier New; font-size:100%;"),
                    #        )
                    # ),
                    column(4, align = "center", 
                           div(span(textOutput("budgetRemaining"), style="color:#D35E60; font-family: Courier New; font-size:325%; font-weight: bold;vertical-align: middle; padding-top: 0em, padding-left: 3em"))
                    ),
                    column(4,
                        div(span(textOutput("live_resScore"), 
                                 style="color:#000000; font-family: Courier New; font-size:325%; font-weight: bold;vertical-align: middle; padding-top: 0em, padding-left: 3em"))
                    ),
                    column(4,
                        div(span(textOutput("live_yldScore"),
                                 style="color:#000000; font-family: Courier New; font-size:325%; font-weight: bold;vertical-align: middle; padding-top: 0em, padding-left: 3em"))
                    )
                )
            ),
            hr(),
            fixedRow(
                div(id = "costSliders",
                    column(6, align = "center", 
                           sliderInput("culling", "Budget to preventing culling:",
                                       min = 0, max = 1000, value = INIT_CULLING_BUDGET, step = 5, width = "100%")),
                    column(6, align = "center",
                           sliderInput("scaring", "Budget to preventing scaring:",
                                       min = 0, max = 1000, value = INIT_SCARING_BUDGET, step = 5, width = "100%"))
                    ),
                bsTooltip(id = "costSliders", "How much of your budget will be invested in preventing culling of animals (e.g. the cost of a shooting licence), and in preventing scaring of animals (e.g. the cost of a scaring licence).", placement = "left", trigger = "hover", options = NULL)
                
            ),
            hr(),
            fixedRow(
                div(id = "buttonsPanel", style = "vertical-align: middle; padding-top:0em; padding-bottom:0em; padding-left:0em; padding-right:0em",
                    actionButton("nextStep", "GO !", width = 150,
                                 icon("paper-plane"),
                                 style="font-family: Courier New; font-size:200%; color: #fff; background-color: #D35E60; font-weight: bold"),
                    actionButton("resetGame", "Reset game", class = "butt"),
                    actionButton("newGame", "New game"),
                    actionButton("showScores", "Scores"),
                    actionButton("showAllIntro", "", icon("question-circle"))
                ),
            )
        ),
        column(1)
    ),
    fixedRow(
        column(1),
        column(5, align = "center",
               plotOutput("land_plot", height="auto", click = "land_plot_click"),
               bsTooltip(id = "land_plot", "The farming landscape; grey labelled sections are individual farms, red dots are the current animal positions.", placement = "left", trigger = "hover", options = NULL)
               
        ),
        column(5, align = "center",
               plotOutput("actions_user", height="auto", click = "act_plot_click"),
               bsTooltip(id = "actions_user", "Actions taken by the farmers in the previous year. Different colours represent the three different possible actions; killing animals, scaring animals off their land, or tending crops. The letter labels link the individual farmer to the individual farm on the farmland map.", placement = "top", trigger = "hover", options = NULL)
               
        ),
        #column(1)
    )
)

server <- function(input, output, session) {
    
    shinyjs::hide(id = "pop_panel")
    shinyjs::hide(id = "land_panel")
    shinyjs::hide(id = "buttonsPanel")
    shinyjs::hide(id = "pageTitle")
    shinyjs::hide(id = "input_sliders")
    shinyjs::hide(id = "budget_report")
    shinyjs::hide(id = "costSliders")
    
    #var_paras = reactiveValues(res_death_K = NULL)
    
    RUN = reactiveValues(id = NULL)
    
    ### 'GDATA' holds "game data", that is the GMSE data for a given game run.
    GDATA = reactiveValues(summary = NULL,
                           laststep = NULL,
                           observed_suggested = NULL,
                           yields = NULL,
                           year = NULL,
                           MaxYear = NULL,
                           achievedMaxYear = NULL,
                           extinction = NULL,
                           PLAYER_NAME = NULL,
                           live_scores = NULL,
                           score_display = NULL)
    CURRENT_BUDGET = reactiveValues(total = NULL,
                                    culling = NULL,
                                    scaring = NULL,
                                    leftover = NULL)
    USER_DISPLAY_SELECT = reactiveValues(user = NULL, act_plot_dat = NULL)
    
    ### This updates a "global" player name in response to a change in the playerName input.
    observeEvent(input$playerName, {
        GDATA$PLAYER_NAME = input$playerName
    })
    
    if(NEWSESSION == TRUE) {
        initModal1()
    }
    
    ### This has repeated observers for both next/back buttons even though some of them do the same thing.
    ### Would be nicer to have these respond to multiple inputs (where relevant) but I couldn't make this work quickly:
    observeEvent(input$toInit1, {
        removeModal()
        initModal1()
    })
    
    observeEvent(input$toInit2, {
        removeModal()
        initModal2()
    })

    observeEvent( input$backtoInit2, {
        removeModal()
        initModal2()
    })
    
    observeEvent(input$toInit3, {
        removeModal()
        initModal3()
    })
    
    observeEvent( input$backtoInit3, {
        removeModal()
        initModal3()
    })
    
    observeEvent( input$toInit4 , {
        removeModal()
        initModal4()
    })
    
    observeEvent( input$backtoInit4 , {
        removeModal()
        initModal4()
    })
    
    observeEvent(input$toInit5, {
        removeModal()
        initModal5()
    })
    
    observeEvent( input$backtoInit5 , {
        removeModal()
        initModal5()
    })
    
    observeEvent(input$toInit6, {
        removeModal()
        initModal6()
    })
    
    observeEvent(input$dismissInitModal, {
        NEWSESSION = FALSE
        setPlayerModal(playername = GDATA$PLAYER_NAME)
    })

    observeEvent(input$consentAgree, {
        shinyjs::toggle("confirmStart")
    })
    
    observeEvent(input$land_plot_click, {
        x = ceiling(dim(GDATA$laststep$LAND[,,3])[1]*input$land_plot_click$x)
        y = ceiling(dim(GDATA$laststep$LAND[,,3])[2]*input$land_plot_click$y)
        # Note the user number index tracked in USER_DISPLAY_SELECT has base 1, not 2 as in the GMSE internal data, hence -1.
        USER_DISPLAY_SELECT$user = GDATA$laststep$LAND[,,3][x,y]-1
    })
    
    ### Observer for monitoring clicks on action plot
    observeEvent(input$act_plot_click, {
        mins = USER_DISPLAY_SELECT$act_plot_dat-0.5
        maxs = USER_DISPLAY_SELECT$act_plot_dat+0.5
        picklevel = input$act_plot_click$x>mins & input$act_plot_click$x<maxs
        # Note 'user' numeric value has base 1, not 2 as in the GMSE internal data!
        USER_DISPLAY_SELECT$user = which(picklevel)
    })
    
    observeEvent(input$confirmStart, {
        
        removeModal()
        
        if(!grepl("^[A-Za-z0-9]+$", GDATA$PLAYER_NAME)) setPlayerModal(playername = "LettersOrNumbersOnlyPlease")
        
        shinyjs::hide(id = "newGame")
        shinyjs::hide(id = "showScores")
        shinyjs::show(id = "resetGame")        
        shinyjs::show(id = "nextStep")        
        shinyjs::show(id = "pop_panel")
        shinyjs::show(id = "land_panel")
        shinyjs::show(id = "buttonsPanel")
        shinyjs::show(id = "pageTitle")
        shinyjs::show(id = "input_sliders")
        shinyjs::show(id = "budget_report")
        shinyjs::show(id = "costSliders")
        
        waiter_show(html = init_waiting_screen, color = "black")
        
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
        GDATA$score_display = "split"
        ### Initialise budget reactiveValues:
        CURRENT_BUDGET$total = budget$total
        CURRENT_BUDGET$culling = budget$culling
        CURRENT_BUDGET$scaring = budget$scaring
        CURRENT_BUDGET$leftover = budget$remaining
        
        ## Add new game run session data to database and get new runID token.
        RUN$id = newRunRecord(session = as.character(session$token),
                             player = GDATA$PLAYER_NAME,
                             startTime = as.character(Sys.time()),
                             extinct = as.numeric(GDATA$extinction))
        
        ## Add GMSE paras for session to database:
        addRunPar(runID = RUN$id, paras = GDATA$paras, score_display = GDATA$score_display)
        
        ## Add initial time steps data to database:
        addInitGdata(runID = RUN$id, gd = GDATA$summary)
        
        ## Add initial yield data to database:
        addInitYieldData(runID = RUN$id, yields = GDATA$yields)
        
        ### Set max to culling/scaring slides based on MANAGER_BUDGET
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "culling",
                          max = GDATA$paras$MANAGER_BUDGET
                          )
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "scaring",
                          max = GDATA$paras$MANAGER_BUDGET
        )
        
        ### Set initial culling/scaring costs:
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "culling",
                          value = INIT_CULLING_BUDGET)
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "scaring",
                          value = INIT_SCARING_BUDGET)
        
        waiter_hide()
    })
    
    observeEvent(input$confirmReset, {
        
        if(GDATA$extinction == FALSE) {
            shinyjs::hide(id = "nextStep")
            shinyjs::hide(id = "resetGame")
            shinyjs::show(id = "newGame")
            shinyjs::show(id = "showScores")
            ### If extinct, this update would have happened already:
            updateRunRecord(runID = RUN$id, endTime = as.character(Sys.time()), extinct = GDATA$extinction)
            addScores(runID = RUN$id, gd = GDATA)
            
        }
        scoresModal(score_display = GDATA$score_display, total_scores = getScoreRank(RUN$id)$score_count)
        
        ### setPlayerModal() has confirmStart button
        #setPlayerModal(playername = PLAYER_NAME)

    })
    #
    
    observeEvent(input$cancelReset, {
        removeModal()
    })
    
    ### When CULLING cost is adjusted, update budget and check if still within limits. If not, adjust SCARING.
    observeEvent(input$culling, {
        
        req(GDATA$summary)
        
        CURRENT_BUDGET = updateCurrentBudget(
            budget = CURRENT_BUDGET,
            manager_budget = GDATA$paras$MANAGER_BUDGET,
            culling_budget = input$culling,
            scaring_budget = input$scaring)
        culling_b = CURRENT_BUDGET$culling
        scaring_b = CURRENT_BUDGET$scaring
        total_b = CURRENT_BUDGET$total
        
        #If spend on culling+scaring exceeds total budget:
        if((culling_b+scaring_b)>total_b) {
            scaring_b_adj = floor(total_b-culling_b)
            #scaring_cost_adj = scaring_b_adj/10
            updateSliderInput(session = getDefaultReactiveDomain(),
                              inputId = "scaring",
                              value = scaring_b_adj)
        }
    })
    
    ### When SCARING cost is adjusted, update budget and check if still within limits. If not, adjust CULLING.
    observeEvent(input$scaring, {
        
        req(GDATA$summary)
        
        CURRENT_BUDGET = updateCurrentBudget(
            budget = CURRENT_BUDGET,
            manager_budget = GDATA$paras$MANAGER_BUDGET,
            culling_budget = input$culling,
            scaring_budget = input$scaring)
        culling_b = CURRENT_BUDGET$culling
        scaring_b = CURRENT_BUDGET$scaring
        total_b = CURRENT_BUDGET$total
        
        # If spend on culling+scaring exceeds total budget:
        if((culling_b+scaring_b)>total_b) {
            # Max available to culling after new scaring cost input:
            culling_b_adj = floor(total_b-scaring_b)
            # This amounts to this updated cost for culling:
            #culling_cost_adj = culling_b_adj/10
            # Update the slider; this in turn should update
            updateSliderInput(session = getDefaultReactiveDomain(),
                              inputId = "culling",
                              value = culling_b_adj)
        }
    })
    
    observeEvent(input$nextStep, {
        
        req(GDATA$summary)
        
        ### User input
        culling_cost = budgetToCost(input$culling, minimum_cost = 10)
        scaring_cost = budgetToCost(input$scaring, minimum_cost = 10)
        
        costs_as_input = list(culling = culling_cost, scaring = scaring_cost)
        prev = GDATA$laststep
        prev = set_man_costs(prev, newcost = costs_as_input)
        
        ### Run next time step:
        nxt = try({gmse_apply_UROM(get_res = "Full", old_list = prev)}, silent = TRUE)
        
        if(class(nxt)!="try-error") {
            
            # Add appropriate outputs.
            GDATA$summary = append_UROM_output(dat = nxt, costs = costs_as_input, old_output = GDATA$summary)
            GDATA$yields = rbind(GDATA$yields, tapply(nxt$LAND[,,2],nxt$LAND[,,3],mean)) # Store per-user yield (before reset)
            
            # Add time new time step game data to database:
            print(GDATA$summary)
            addNewData(runID = RUN$id, gd = GDATA$summary)
            
            # Add new yield data to database:
            addYields(runID = RUN$id, yields = GDATA$yields)
            
            # Add "all para" parameters (parameters per time step, in case variable - also for debugging)
            addAllRunPar(runID = RUN$id, t = nrow(GDATA$summary), paras = getLastParas(GDATA$laststep, K = 5))
            
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
        
    })
    
    observeEvent(GDATA$achievedMaxYear, {
        if(GDATA$achievedMaxYear == TRUE) { 
            shinyjs::hide(id = "nextStep")
            shinyjs::hide(id = "resetGame")
            shinyjs::show(id = "newGame")
            shinyjs::show(id = "showScores")
            updateRunRecord(runID = RUN$id, endTime = as.character(Sys.time()), extinct = GDATA$extinction)
            addScores(runID = RUN$id, gd = GDATA)
            finishedModal()
        }
        
    })
    
    observeEvent(GDATA$extinction, {
        if(GDATA$extinction == TRUE) {
            culling_cost = budgetToCost(input$culling, minimum_cost = 10)
            scaring_cost = budgetToCost(input$scaring, minimum_cost = 10)
            addLastCostsOnExtinction(runID = RUN$id, cull_cost = culling_cost, scare_cost = scaring_cost)
            updateRunRecord(runID = RUN$id, endTime = as.character(Sys.time()), extinct = GDATA$extinction)
            addScores(runID = RUN$id, gd = GDATA)
            extinctionModal()
            shinyjs::hide(id = "nextStep")
            shinyjs::hide(id = "resetGame")
            shinyjs::show(id = "newGame")
            shinyjs::show(id = "showScores")
        }
    })
    
    observeEvent(input$closeScores, {
        setPlayerModal(playername = GDATA$PLAYER_NAME)
    })
    
    observeEvent(input$newGame, {
        setPlayerModal(playername = GDATA$PLAYER_NAME)
    })
    
    observeEvent(input$confirmFinished, {
        scoresModal(score_display = GDATA$score_display, total_scores = getScoreRank(RUN$id)$score_count)
    })
    
    observeEvent(input$confirmExtinction, {
        #removeModal()
        scoresModal(score_display = GDATA$score_display, total_scores = getScoreRank(RUN$id)$score_count)
    })
    
    observeEvent(input$resetGame, {
        # Show reset confirm modal, and calls confirmReset if yes:
        confirmResetModal()
    })
    
    observeEvent(input$showScores, {
        scoresModal(score_display = GDATA$score_display, total_scores = getScoreRank(RUN$id)$score_count)
    })
    
    observeEvent(input$showAllIntro, {
        allIntroModal()
    })

    output$pop_plot <- renderPlot({
        if(!is.null(GDATA$summary)) {
            plot_pop(GDATA$summary, yield_dat = GDATA$yields, track_range = FALSE,
                     extinction_message = GDATA$extinction
            )    
        }
    }, height = function() { session$clientData$output_pop_plot_width }
    )
    
    output$land_plot <- renderPlot({
        if(!is.null(GDATA$laststep)) {
            plot_land_res(GDATA$laststep$LAND, GDATA$laststep$RESOURCES, 
                          cols = GDATA$land_colors,
                          extinction_message = GDATA$extinction,
                          show_labels = TRUE,
                          selected_user = USER_DISPLAY_SELECT$user
            )
            
        }
    }, height = function() { session$clientData$output_land_plot_width }
    )
    
    output$budget_bar <- renderPlot({
        if(!is.null(CURRENT_BUDGET$culling)) {
            par(oma = c(0.5,0.5,0.5,0.5))
            par(mar = c(0.5,0.5,0.5,0.5))
            #par(mfrow = c(1,2))
            plotdat = matrix(c(CURRENT_BUDGET$culling, CURRENT_BUDGET$scaring, CURRENT_BUDGET$leftover), nrow = 3, ncol = 1)
            barplot(plotdat, col = c("#f46d43","#3288bd","#abdda4"), yaxt = "n", border = NA)
            #plot(x=1,y=1, ylim=c(0,CURRENT_BUDGET$total), type ="n", xaxt = "n", yaxt = "n", xlab ="n", ylab = "n", bty = "n")
            #text(0.75, 300, "test", adj = 0, cex = 3, )
        }
    })
    
    ### Plots actions per user in last time step
    output$actions_user <- renderPlot({
        if(!is.null(GDATA$laststep)) {
            # Extract all previous actions per user:
            prev_acts = GDATA$laststep$PREV_ACTS
            scare_cull = prev_acts[1,c(9,8),2:dim(prev_acts)[3]]
            tend_crops = prev_acts[2,10,2:dim(prev_acts)[3]] 
            # rows: scares, culls, tend_crops:
            acts = rbind(scare_cull,t(as.matrix(tend_crops)))
            #par(oma = c(0,0,0,6))
            par(mar = c(5,5,5,1.5))
            #bcols = matrix("black", nrow = 3, ncol = ncol(acts))
            #bcols = c(rep("black",4),rep("red",4))
            # if(!is.null(USER_DISPLAY_SELECT$user)) {
            #     bcols[,USER_DISPLAY_SELECT$user] = "red"
            # } 
            # act_plot_dat = barplot(acts, beside = FALSE, col = c("#D35E60","#9067A7","#56BA47"), space = 0.1, 
            #         names = LETTERS[1:ncol(acts)], ylab = "No. actions taken by farmer", xlab = "Farmer", 
            #         cex.lab = 2, cex.axis = 1.5, cex.names = 1.25, main = "", border = "red")
            USER_DISPLAY_SELECT$act_plot_dat = barplot(acts, beside = FALSE, col = c("#D35E60","#9067A7","#56BA47"), space = 0.1, 
                                   xaxt = "n", ylab = "No. actions taken by farmer", xlab = "Farmer", 
                                   cex.lab = 2, cex.axis = 1.5, cex.names = 1.25, main = "")
            USERS = LETTERS[1:ncol(acts)]
            USER_LAB_COLS = rep("white", ncol(acts))
            if(!is.null(USER_DISPLAY_SELECT$user)) {
                USER_LAB_COLS[USER_DISPLAY_SELECT$user] = "#D35E60"
            }
            xlab_ypos = 0-(max(acts, na.rm = T)*0.075)
            for(i in 1:ncol(acts)) {
                points(USER_DISPLAY_SELECT$act_plot_dat[i], y = xlab_ypos, cex = 4, xpd = T, pch = 21, bg = USER_LAB_COLS[i], col = "black")
                text(USERS[i], x = USER_DISPLAY_SELECT$act_plot_dat[i], y = xlab_ypos, xpd = T, cex = 1.25)
            }
            legend("top", inset=c(0,-0.25), legend = c("Culling","Scaring","Farming"), 
                   fill = c("#D35E60","#9067A7","#56BA47"), cex = 1.5,
                   ncol = 3, x.intersp=0.3, text.width = floor(ncol(acts)/3.5), xjust = 0, bty = "n", xpd = T)
        }
    ### This should track/set the width of the figures dynamically:
    }, height = function() { session$clientData$output_actions_user_width }
    )
    
    output$budgetRemaining <- renderText({
        paste0("€", CURRENT_BUDGET$leftover)
    })
    
    output$live_resScore <- renderText({
        GDATA$live_scores$res
    })
    
    output$live_yldScore <- renderText({
        GDATA$live_scores$yld
    })
    
    output$rank_total <- renderText({
      #req(RUN$id)
      getScoreRank(RUN$id)$total
    })
    
    output$rank_res <- renderText({
      #req(RUN$id)
      getScoreRank(RUN$id)$res
    })
    
    output$rank_yld <- renderText({
      #req(RUN$id)
      getScoreRank(RUN$id)$yld
    })
    
    output$rank_total_scores <- renderText({
      #req(RUN$id)
      getScoreRank(RUN$id)$score_count
    })
    
    output$highScores <- renderDataTable({
        req(RUN$id)
        
        scores = getScores(score_version = 1, limit = 10, rank = "total")
      
        if(!(RUN$id %in% scores$id)) {
            scores = scores[1:9,]
            current_score = getCurrentRunScore(RUN$id)
            scores = rbind(scores,current_score)
        }
        current_player = unique(scores$player[which(scores$id == RUN$id)])

        scores = subset(scores, select = c("player","mean_res","mean_yield","total","id"))
        
        # Make DT while hiding the "id" column:
        scores_dt = datatable(scores, colnames = c("Player","Population","Yield","TOTAL","id"), 
                              autoHideNavigation = TRUE, rownames = FALSE, filter = "none",
                              options=list(columnDefs = list(list(visible=FALSE, targets=c(4))), dom = 't', initComplete = JS(
                                  "function(settings, json) {",
                                  "$('body').css({'font-family': 'Courier New'});",
                                  "}"
                              ))
        )
        ### NOTE JS INSERTION ABOVE APPEARS TO CHANGE FONT THROUGHOUT BY DEFAULT, ONCE RUN.
        scores_dt = formatStyle(scores_dt, "player", target = "row", 
                                backgroundColor = styleEqual(current_player, "orange"), 
                                color = styleEqual(current_player, "white"))
        
        formatStyle(scores_dt, "id", target = "row",  backgroundColor = styleEqual(RUN$id, "darkred"), color = styleEqual(RUN$id, "white"))
    })
    
    output$highScores_res <- renderDataTable({ 
        req(RUN$id)
        
        scores = getScores(score_version = 1, limit = 10, rank = "res")
        
        if(!(RUN$id %in% scores$id)) {
            scores = scores[1:9,]
            current_score = getCurrentRunScore(RUN$id)
            scores = rbind(scores,current_score)
        }
        current_player = unique(scores$player[which(scores$id == RUN$id)])
        
        scores = subset(scores, select = c("player","mean_res","id"))
        
        # Make DT while hiding the "id" column:
        scores_dt = datatable(scores, colnames = c("Player","Population","id"), 
                              autoHideNavigation = TRUE, rownames = FALSE, filter = "none",
                              options=list(columnDefs = list(list(visible=FALSE, targets=c(2))), dom = 't', initComplete = JS(
                                  "function(settings, json) {",
                                  "$('body').css({'font-family': 'Courier New'});",
                                  "}"
                              ))
        )
        ### NOTE JS INSERTION ABOVE APPEARS TO CHANGE FONT THROUGHOUT BY DEFAULT, ONCE RUN.
        scores_dt = formatStyle(scores_dt, "player", target = "row", 
                                backgroundColor = styleEqual(current_player, "orange"), 
                                color = styleEqual(current_player, "white"))
        
        formatStyle(scores_dt, "id", target = "row",  backgroundColor = styleEqual(RUN$id, "darkred"), color = styleEqual(RUN$id, "white"))
    })
    
    output$highScores_yld <- renderDataTable({ 
        req(RUN$id)
        
        scores = getScores(score_version = 1, limit = 10, rank = "yield")
        
        if(!(RUN$id %in% scores$id)) {
            scores = scores[1:9,]
            current_score = getCurrentRunScore(RUN$id)
            scores = rbind(scores,current_score)
        }
        current_player = unique(scores$player[which(scores$id == RUN$id)])
        
        scores = subset(scores, select = c("player","mean_yield","id"))
        
        # Make DT while hiding the "id" column:
        scores_dt = datatable(scores, colnames = c("Player","Yield","id"), 
                              autoHideNavigation = TRUE, rownames = FALSE, filter = "none",
                              options=list(columnDefs = list(list(visible=FALSE, targets=c(2))), dom = 't', initComplete = JS(
                                  "function(settings, json) {",
                                  "$('body').css({'font-family': 'Courier New'});",
                                  "}"
                              ))
        )
        ### NOTE JS INSERTION ABOVE APPEARS TO CHANGE FONT THROUGHOUT BY DEFAULT, ONCE RUN.
        scores_dt = formatStyle(scores_dt, "player", target = "row", 
                                backgroundColor = styleEqual(current_player, "orange"), 
                                color = styleEqual(current_player, "white"))
        
        formatStyle(scores_dt, "id", target = "row",  backgroundColor = styleEqual(RUN$id, "darkred"), color = styleEqual(RUN$id, "white"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
