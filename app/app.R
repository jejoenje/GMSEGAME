library(shiny)
library(shinyjs)
#library(shinybusy)
library(waiter)
library(plotly)
library(GMSE) # CURRENTLY NEEDS devtools::install_github("ConFooBio/GMSE", ref = "man_control")

source("app_helpers.R")
source("infoDialogs.R")

LAND_OWNERSHIP <<- TRUE
STAKEHOLDERS  <<- 4
MANAGER_BUDGET  <<- 1000
MANAGE_TARGET  <<- 2000
OBSERVE_TYPE  <<- 0
RES_MOVE_OBS  <<- TRUE
RES_DEATH_K  <<- 5000
LAMBDA <<- 0.3
RES_DEATH_TYPE  <<- 3
REMOVE_PR <<- 0.05
USER_BUDGET <<- 1500
CULLING <<- TRUE
SCARING <<- TRUE
TEND_CROPS  <<- TRUE
SHOWSUGGESTED  <<- FALSE
INIT_SCARING_COST <<- 55
INIT_CULLING_COST <<- 55
K  <<- 5
land_colors <<- sample(grey.colors(STAKEHOLDERS))

NEWSESSION = TRUE
START_OK = FALSE
RESET_ME = FALSE

initGame = function() {
    gdata = list()
    initdata = init_man_control(K = K)
    gdata$summary = initdata$summary
    gdata$laststep = initdata$gmse_list[[length(initdata$gmse_list)]]
    gdata$observed_suggested = initdata$observed_suggested
    gdata$yields = initdata$prev_yield
    return(gdata)
}

initBudget = function() {
    budget = list(
        total = MANAGER_BUDGET+2*10*10,
        culling = INIT_CULLING_COST*10,
        scaring = INIT_SCARING_COST*10,
        remaining = (MANAGER_BUDGET+2*10*10)-(INIT_CULLING_COST*10+INIT_SCARING_COST*10)
    )
    return(budget)
}

updateCurrentBudget = function(budget, manager_budget, culling_cost, scaring_cost) {
    budget$total = manager_budget+2*10*10
    budget$culling = culling_cost*10
    budget$scaring = scaring_cost*10
    budget$leftover = budget$total-(budget$culling+budget$scaring)
    return(budget)
}

reset_waiting_screen <- tagList(
    spin_flower(),
    h4("Running initial time steps...")
) 

gdata = initGame()
budget = initBudget()

ui <- fluidPage(

    ### from library(waiter)
    use_waiter(), # include dependencies
    #waiter_preloader(),
    
    titlePanel("GMSE-GAME"),
    
    fluidRow(
        column(1),
        column(4, wellPanel(style = "background: white",
                            plotOutput("pop_plot",width = "100%")
        )),
        column(3, wellPanel(style = "background: white",
                            plotOutput("land_plot",width = "100%")
        )),
        column(3, wellPanel(style = "background: white",
                            plotOutput("actions_user",width = "100%")
        )),
        column(1)
    ),
    fluidRow(
        column(7, wellPanel(style = "background: white",
                            
                            div(style="display:inline-block; vertical-align: middle;", 
                                plotly::plotlyOutput("budget_pie", height = 200, width = 200)    
                            ),
                            div(style="display:inline-block; vertical-align: top; padding-left: 2em; padding-right: 2em", 
                                span("Budget remaining", style="color:#D35E60; font-size:175%"),
                                span(textOutput("budgetRemaining"), style="color:#D35E60; font-size:350%; font-weight: bold")
                            ),
                            div(style="display:inline-block; vertical-align: middle; padding-left: 4em; padding-right: 4em", 
                                sliderInput("culling_cost_in", "Culling cost", 
                                            min = 10, max = (MANAGER_BUDGET/10)+10, value = INIT_CULLING_COST, step = 5, width = 250),
                                sliderInput("scaring_cost_in", "Scaring cost", 
                                            min = 10, max = (MANAGER_BUDGET/10)+10, value = INIT_SCARING_COST, step = 5, width = 250)
                            ),
                            div(style="display:inline-block; vertical-align: middle; padding-left: 2em", 
                                actionButton("nextStep", "GO !", width = 150,
                                             icon("paper-plane"),
                                             style="font-size:200%; color: #fff; background-color: #D35E60; font-weight: bold"),
                                br(),
                                br(),
                                actionButton("resetGame", "Reset game")
                            )
                            
        )),
        column(5,
               tableOutput("gdata_summary")
        )
    )
)

server <- function(input, output, session) {
    
    if(NEWSESSION == TRUE) {
        initModal()
        NEWSESSION <<- FALSE
        
    }
    
    GDATA = reactiveValues(summary = gdata$summary, 
                           laststep = gdata$laststep, 
                           observed_suggested = gdata$observed_suggested, 
                           yields = gdata$yields,
                           extinction = FALSE
    )
    CURRENT_BUDGET = reactiveValues(total = budget$total, 
                                    culling = budget$culling, 
                                    scaring = budget$scaring, 
                                    leftover = budget$remaining)

    ### When CULLING cost is adjusted, update budget and check if still within limits. If not, adjust SCARING.
    observeEvent(input$culling_cost_in, {
        
        #CURRENT_BUDGET$culling = input$culling_cost_in*10
        
        CURRENT_BUDGET = updateCurrentBudget(
            budget = CURRENT_BUDGET,
            manager_budget = MANAGER_BUDGET,
            culling_cost = input$culling_cost_in,
            scaring_cost = input$scaring_cost_in)
        culling_b = CURRENT_BUDGET$culling
        scaring_b = CURRENT_BUDGET$scaring
        total_b = CURRENT_BUDGET$total
        
        #If spend on culling+scaring exceeds total budget:
        if((culling_b+scaring_b)>total_b) {
            scaring_b_adj = total_b-culling_b
            scaring_cost_adj = scaring_b_adj/10
            updateSliderInput(session = getDefaultReactiveDomain(),
                              inputId = "scaring_cost_in",
                              value = scaring_cost_adj)
        }
    })
    
    ### When SCARING cost is adjusted, update budget and check if still within limits. If not, adjust CULLING.
    observeEvent(input$scaring_cost_in, {
        
        CURRENT_BUDGET = updateCurrentBudget(
            budget = CURRENT_BUDGET,
            manager_budget = MANAGER_BUDGET,
            culling_cost = input$culling_cost_in,
            scaring_cost = input$scaring_cost_in)
        culling_b = CURRENT_BUDGET$culling
        scaring_b = CURRENT_BUDGET$scaring
        total_b = CURRENT_BUDGET$total
        
        # If spend on culling+scaring exceeds total budget:
        if((culling_b+scaring_b)>total_b) {
            # Max available to culling after new scaring cost input:
            culling_b_adj = total_b-scaring_b
            # This amounts to this updated cost for culling:
            culling_cost_adj = culling_b_adj/10
            # Update the slider; this in turn should update
            updateSliderInput(session = getDefaultReactiveDomain(),
                              inputId = "culling_cost_in",
                              value = culling_cost_adj)
        }
    })
    
    observeEvent(input$nextStep, {
        
        ### User input
        costs_as_input = list(culling = input$culling_cost_in, scaring = input$scaring_cost_in)
        prev = GDATA$laststep
        prev = set_man_costs(prev, newcost = costs_as_input)
        
        ### Run next time step:
        nxt = try({gmse_apply_UROM(get_res = "Full", old_list = prev)}, silent = TRUE)
        
        if(class(nxt)!="try-error") {
            
            # Add appropriate outputs.
            GDATA$summary = append_UROM_output(dat = nxt, costs = costs_as_input, old_output = GDATA$summary)
            GDATA$yields = rbind(GDATA$yields, tapply(nxt$LAND[,,2],nxt$LAND[,,3],mean)) # Store per-user yield (before reset)
            # Reset time step
            nxt$LAND[,,2] = 1    # Reset landscape yield
            GDATA$laststep = nxt
            
        } else {
            
            GDATA$extinction = TRUE
            
            disable("nextStep")
            
        }
        
    })
    
    observeEvent(input$resetGame, {
        # Show reset confirm modal, and calls confirmReset if yes:
        confirmResetModal()
    })
    
    observeEvent(input$confirmReset, {
        
        waiter_show(html = reset_waiting_screen, color = "black")
        
        removeModal()
        ## Store existing data here and show scores.
        
        gdata = initGame()
        budget = initBudget()
        
        GDATA$summary = gdata$summary
        GDATA$laststep = gdata$laststep
        GDATA$observed_suggested = gdata$observed_suggested
        GDATA$yields = gdata$yields
        GDATA$extinction = FALSE
        
        CURRENT_BUDGET$total = budget$total
        CURRENT_BUDGET$culling = budget$culling
        CURRENT_BUDGET$scaring = budget$scaring
        CURRENT_BUDGET$leftover = budget$remaining
        
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "culling_cost_in",
                          value = INIT_CULLING_COST)
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "scaring_cost_in",
                          value = INIT_SCARING_COST)
        
        waiter_hide()
        
    })
    
    output$gdata_summary = renderTable({
        GDATA$summary
    })


    output$pop_plot <- renderPlot({
        if(!is.null(GDATA$summary)) {
            plot_pop(GDATA$summary, yield_dat = GDATA$yields, track_range = FALSE,
                     extinction_message = GDATA$extinction
            )    
        }
    })
    
    output$land_plot <- renderPlot({
        if(!is.null(GDATA$laststep)) {
            plot_land_res(GDATA$laststep$LAND, GDATA$laststep$RESOURCES, 
                          col = land_colors,
                          extinction_message = GDATA$extinction
            )
            
        }
    })
    
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
            par(mar = c(5,5,2,1.5))
            barplot(acts, beside = FALSE, col = c("#D35E60","#9067A7","#56BA47"), space = 0.1, 
                    names = c(1:ncol(acts)), ylab = "Actions", xlab = "Stakeholder", cex.lab = 2, cex.axis = 1.5, cex.names = 1.5)    
        }
    })
    
    output$budgetRemaining <- renderText({
        paste("$", CURRENT_BUDGET$leftover)
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
