rm(list=ls())
library(shiny)
library(shinyjs)
library(waiter)
library(plotly)
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
        RES_DEATH_K  = round(runif(1, 1000, 6000)),
        LAMBDA = runif(1, 0.2, 0.4),
        RES_DEATH_TYPE  = 3,
        REMOVE_PR = runif(1, 0, 0.1),
        USER_BUDGET = 1500,
        CULLING = TRUE,
        SCARING = TRUE,
        TEND_CROPS = TRUE,
        TEND_CROP_YLD = runif(1, 0.1, 0.4),
        LAND_DIM_1 = 100,
        LAND_DIM_2 = 100,
        RESOURCE_INI = 1000
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


reset_waiting_screen <- tagList(
    spin_flower(),
    h4("Resetting game - running intial time steps...")
) 

init_waiting_screen <- tagList(
    spin_flower(),
    h4("Starting game...")
) 

ui <- fixedPage(
    shinyjs::useShinyjs(),
    use_waiter(),

    fixedRow(
        column(1),
        column(5, align = "center",
               div(id = "pop_plot_div", style = "padding: 1em",
                   plotOutput("pop_plot", height="auto")
                  )
        ),
        column(5, align = "center",
            fixedRow(
                div(id = "budget_report", style = "padding-top:4em; padding-left:1em; padding-right:1em, padding-bottom:1em",
                    column(6, align = "right",
                           div(style="text-align: center; vertical-align: middle; padding-top: 3em, padding-left: 3em",
                               span("Budget available", style="color:#D35E60; font-size:150%;"),br(),
                               span("(to set costs)", style="color:#D35E60; font-size:125%;"),
                           )
                    ),
                    column(6, align = "left", 
                           div(span(textOutput("budgetRemaining"), style="color:#D35E60; font-size:400%; font-weight: bold;vertical-align: middle; padding-top: 0em, padding-left: 3em"))
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
                    )
            ),
            hr(),
            fixedRow(
                div(id = "buttonsPanel", style = "vertical-align: middle; padding-top:0em; padding-bottom:0em; padding-left:0em; padding-right:0em",
                    actionButton("nextStep", "GO !", width = 150,
                                 icon("paper-plane"),
                                 style="font-size:200%; color: #fff; background-color: #D35E60; font-weight: bold"),
                    actionButton("resetGame", "Reset game"),
                    actionButton("newGame", "New game"),
                    actionButton("showScores", "Scores"),
                    actionButton("showAllIntro", "", icon("question-circle"))
                )
            )
        ),
        column(1)
    ),
    fixedRow(
        column(1),
        column(5, align = "center",
               plotOutput("land_plot", height="auto")
        ),
        column(5, align = "center",
               plotOutput("actions_user", height="auto")
        ),
        column(1)
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
                           extinction = NULL,
                           PLAYER_NAME = NULL)
    CURRENT_BUDGET = reactiveValues(total = NULL,
                                    culling = NULL,
                                    scaring = NULL,
                                    leftover = NULL)
    
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
    
    observeEvent(input$dismissInitModal, {
        NEWSESSION = FALSE
        setPlayerModal(playername = GDATA$PLAYER_NAME)
    })

    observeEvent(input$consentAgree, {
        shinyjs::toggle("confirmStart")
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
        GDATA$extinction = FALSE
        GDATA$land_colors = gdata$land_colors
        GDATA$paras = gdata$paras
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
        addRunPar(runID = RUN$id, paras = GDATA$paras)
        
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
        scoresModal()
        
        ### setPlayerModal() has confirmStart button
        #setPlayerModal(playername = PLAYER_NAME)

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
            
        } else {
            
            GDATA$extinction = TRUE

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
    
    ### This can probably be removed and replaced by the simple modalButton action
    observeEvent(input$confirmExtinction, {
        #removeModal()
        scoresModal()
    })
    
    observeEvent(input$resetGame, {
        # Show reset confirm modal, and calls confirmReset if yes:
        confirmResetModal()
    })
    
    observeEvent(input$showScores, {
        scoresModal()
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
                          col = GDATA$land_colors,
                          extinction_message = GDATA$extinction,
                          show_labels = TRUE
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
    
    output$budget_pie <- plotly::renderPlotly({
        if(!is.null(CURRENT_BUDGET$culling)) {
            plotdat = matrix(c(CURRENT_BUDGET$culling, CURRENT_BUDGET$scaring, CURRENT_BUDGET$leftover), nrow = 3, ncol = 1)
            plotdat = as.data.frame(plotdat, row.names = c("Culling","Scaring","Available"))
            plotdat$items = row.names(plotdat)
            colors = c("rgb(211,94,96)","rgb(144,103,167)","rgb(240, 245, 245)")
            #tfont = list(family = "sans serif", size = 22)
            fig2 = plot_ly(plotdat, 
                           labels = ~items, values = ~V1, type = "pie", 
                           textinfo = 'text+label',
                           #textfont = tfont,
                           hoverinfo = 'percent',
                           text = ~paste('$', V1),
                           marker = list(colors = colors, line = list(color = '#000000', width = 0)))
            fig2 = layout(fig2,
                          #font = tfont,
                          margin = list(l = 1, r = 1, b = 0, t = 0, pad = 0),
                          showlegend = FALSE,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            fig2
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
            barplot(acts, beside = FALSE, col = c("#D35E60","#9067A7","#56BA47"), space = 0.1, 
                    names = LETTERS[1:ncol(acts)], ylab = "No. actions taken by farmer", xlab = "Farmer", 
                    cex.lab = 2, cex.axis = 1.5, cex.names = 1.25, main = "")
            legend("top", inset=c(0,-0.25), legend = c("Culling","Scaring","Farming"), 
                   fill = c("#D35E60","#9067A7","#56BA47"), cex = 1.5,
                   ncol = 3, x.intersp=0.3, text.width = floor(ncol(acts)/3.5), xjust = 0, bty = "n", xpd = T)
        }
    ### This should track/set the width of the figures dynamically:
    }, height = function() { session$clientData$output_actions_user_width }
    )
    
    output$budgetRemaining <- renderText({
        paste("$", CURRENT_BUDGET$leftover)
    })
    
    output$highScores <- renderDataTable({
        req(RUN$id)
        scores = getScores()
        if(!(RUN$id %in% scores$id)) {
            scores = scores[1:9,]
            current_score = getCurrentRunScore(RUN$id)
            scores = rbind(scores,current_score)
        }
        current_player = unique(scores$player[which(scores$id == RUN$id)])

        scores = subset(scores, select = c("player","steps","mean_res","mean_yield","total","id"))
        # Make DT while hiding the "id" column:
        scores_dt = datatable(scores, colnames = c("Player","Time","Population","Yield","TOTAL","id"), autoHideNavigation = TRUE, rownames = FALSE, filter = "none",
                              options=list(columnDefs = list(list(visible=FALSE, targets=c(5))), dom = 't'))

        scores_dt = formatStyle(scores_dt, "player", target = "row",  backgroundColor = styleEqual(current_player, "orange"), color = styleEqual(current_player, "white"))
        
        formatStyle(scores_dt, "id", target = "row",  backgroundColor = styleEqual(RUN$id, "darkred"), color = styleEqual(RUN$id, "white"))

    })
    
    output$tableForTest <- renderDataTable({
        datatable(cars)
    })
    
    output$clientdataText <- renderText({
        cnames <- names(cdata)
        
        allvalues <- lapply(cnames, function(name) {
            paste(name, cdata[[name]], sep = " = ")
        })
        paste(allvalues, collapse = "\n")
    })
    
    output$sessiontoken = renderText({
        session$token
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
