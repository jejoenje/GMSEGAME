library(shiny)
library(shinyjs)
library(plotly)
### GMSE INSTALLATION SHOULD BE SPECIFIC man_control branch!
# install_github("ConFooBio/gmse", ref = "man_control")
library(GMSE)
source("app_helpers.R")

d.LAND_OWNERSHIP = TRUE
d.STAKEHOLDERS = 4
d.MANAGER_BUDGET = 1000
d.MANAGE_TARGET = 2000
d.OBSERVE_TYPE = 0
d.RES_MOVE_OBS = TRUE
d.RES_DEATH_K = 5000
d.LAMBDA = 0.3
d.RES_DEATH_TYPE = 3
d.REMOVE_PR = 0.05
d.USER_BUDGET = 1500
d.CULLING = TRUE
d.SCARING = TRUE
d.TEND_CROPS = TRUE
d.SHOWSUGGESTED = FALSE
d.K = 5

NEWSESSION = TRUE

ui <- fluidPage(
    useShinyjs(),
    
    titlePanel("GMSE-GAME"),
    
    tabsetPanel(id = "MainPanels", selected = "Setup", type = "pills",
        tabPanel(title = "", value = "Setup",
            h3("Game setup"),
            
            column(1),
            column(3,
                wellPanel(
                    h3("General environment"),
                    disabled(checkboxInput("LAND_OWNERSHIP", strong("Land ownership"), value = d.LAND_OWNERSHIP)),
                    sliderInput("STAKEHOLDERS", "No. of stakeholders (users)",
                                min = 4, max = 32, value = d.STAKEHOLDERS, step = 1),
                    sliderInput("MANAGER_BUDGET", "Manager budget",
                                       min = 100, max = 5000, value = d.MANAGER_BUDGET, step = 100),
                    sliderInput("MANAGE_TARGET", "Management population target (for initial time steps only)",
                                       min = 500, max = 10000, value = d.MANAGE_TARGET, step = 100),
                    selectInput("OBSERVE_TYPE", "Resource observation type", 
                                       choices = list("Density-based" = 0,
                                                      "Mark-recapture estimate" = 1, 
                                                      "Transect sampling (linear)" = 2,
                                                      "Transect sampling (block)" = 3
                                                      ), selected = d.OBSERVE_TYPE),
                    checkboxInput("RES_MOVE_OBS", strong("Resources move during observation"), value = d.RES_MOVE_OBS)
                )    
            ),
                
            column(3,
                wellPanel(
                    h3("Resource population controls"),
                    sliderInput("RES_DEATH_K", "K (resource carrying capacity)",
                                       min = 100, max = 10000, value = d.RES_DEATH_K, step = 100),
                    sliderInput("LAMBDA", "Lambda (resource max growth rate)",
                                       min = 0.1, max = 2, value = d.LAMBDA, step = 0.1),
                    disabled(selectInput("RES_DEATH_TYPE", "Resource removal (death) type", 
                                       choices = list("Density-independent" = 1, 
                                                      "Density-dependent" = 2,
                                                      "Both" = 3), selected = d.RES_DEATH_TYPE)),
                    sliderInput("REMOVE_PR", "Fixed mortality probability",
                                       min = 0, max = 0.95, value = d.REMOVE_PR, step = 0.05)
                )
            ),
            
            column(3,
                wellPanel(
                    h3("User action controls"),
                    sliderInput("USER_BUDGET", "User budget",
                                   min = 100, max = 5000, value = d.USER_BUDGET, step = 100),
                    disabled(checkboxInput("CULLING", "Culling", value = d.CULLING)),
                    disabled(checkboxInput("SCARING", "Scaring", value = d.SCARING)),
                    disabled(checkboxInput("TEND_CROPS", "Tending crops", value = d.TEND_CROPS))
                ),
                wellPanel(style = "background: white",
                    h3("Misc controls"),
                    column(6,numericInput("K", "No. of initial time steps",
                                          min = 2, max = 10, value = d.K, step = 1)
                    ),
                    column(6,checkboxInput("SHOWSUGGESTED", strong("Show suggested costs"), value = d.SHOWSUGGESTED)
                           ),
                    br(),
                    actionButton("runGame", "GO!", icon = icon("gamepad")),
                    actionButton("resetInputs", "Reset settings")
                )
            ),
        
            column(2) 
        
        ),
        tabPanel(title = "", value = "Main",
            
            fluidRow(
                column(1),
                column(4, wellPanel(style = "background: white",
                    plotOutput("pop_plot",width = "100%")
                )),
                column(3, wellPanel(style = "background: white",
                    plotOutput("land_plot",width = "100%")
                )),
                column(3, wellPanel(style = "background: white",
                    #plotOutput("actions_sum",width = "100%")
                    plotOutput("actions_user",width = "100%")
                )),
                column(1)
            ),
            fluidRow(
                column(2),
                column(8, wellPanel(style = "background: white",
                    
                    div(style="display:inline-block; vertical-align: middle;", 
                        plotly::plotlyOutput("budget_pie", height = 200, width = 200)    
                    ),
                    div(style="display:inline-block; vertical-align: top; padding-left: 2em; padding-right: 2em", 
                        span("Budget remaining", style="color:#D35E60; font-size:200%"),
                        span(textOutput("budgetRemaining"), style="color:#D35E60; font-size:400%; font-weight: bold")
                    ),
                    div(style="display:inline-block; vertical-align: middle; padding-left: 4em; padding-right: 4em", 
                        sliderInput("culling_cost_in", "Culling cost", 
                                    min = 10, max = (d.MANAGER_BUDGET/10)+10, value = 99, step = 10, width = 300),
                        sliderInput("scaring_cost_in", "Scaring cost", 
                                    min = 10, max = (d.MANAGER_BUDGET/10)+10, value = 99, step = 10, width = 300)
                    ),
                    div(style="display:inline-block; vertical-align: middle; padding-left: 2em", 
                        actionButton("nextStep", "GO !", width = 150,
                                     icon("paper-plane"), 
                                     style="font-size:200%; color: #fff; background-color: #D35E60; font-weight: bold"
                                     ),br(),
                        br(),
                        actionButton("resetGame", "Reset game")
                    )
                    
                )),
                column(2)
            )
            
        ), # e/o tabPanel
        tabPanel(title = "", value = "Testing",
            tableOutput("df_data_out"),
            tableOutput("df_yield_out")
        )  # e/o testing tabPanel
    )
    
)

server <- function(input, output, session) {
    
    GDATA = reactiveValues(summary = NULL, laststep = NULL, observed_suggested = NULL, yields = NULL)
    CHECK = reactiveValues(extinction = FALSE)
    CURRENT_BUDGET = reactiveValues(total = NULL, culling = NULL, scaring = NULL, leftover = NULL)
    NEWSESSION = reactiveValues(check = TRUE)
    
    observeEvent(input$runGame, {
        toggleSetup("Main")
        
        if(NEWSESSION$check == TRUE) {
            showModal(modalDialog(size = "l", footer = modalButton("OK!"),
                title = "Welcome to GMSE-GAME!",
                "This dialog will contain some initial explanation of how the game works, what the elements of the screen are, etc.",
                p(),
                "This message will only be shown once, so not after the game is reset."
            ))
            NEWSESSION$check = FALSE
        }
        
        ### Initial time steps:
        land_colors <<- sample(grey.colors(input$STAKEHOLDERS))
        LAND_OWNERSHIP <<- input$LAND_OWNERSHIP
        TEND_CROPS <<- input$TEND_CROPS
        SCARING <<- input$SCARING
        CULLING <<- input$CULLING
        OBSERVE_TYPE <<- as.numeric(input$OBSERVE_TYPE)
        RES_MOVE_OBS <<- input$RES_MOVE_OBS
        RES_DEATH_K <<- input$RES_DEATH_K
        LAMBDA <<- input$LAMBDA
        MANAGE_TARGET <<- input$MANAGE_TARGET
        STAKEHOLDERS <<- input$STAKEHOLDERS
        USER_BUDGET <<- input$USER_BUDGET
        MANAGER_BUDGET <<- input$MANAGER_BUDGET
        RES_DEATH_TYPE <<- as.numeric(input$RES_DEATH_TYPE)
        REMOVE_PR <<- input$REMOVE_PR
        
        initdata = init_man_control(K = input$K)
        GDATA$summary = initdata$summary
        GDATA$laststep = initdata$gmse_list[[length(initdata$gmse_list)]]
        GDATA$observed_suggested = initdata$observed_suggested
        GDATA$yields = initdata$prev_yield
        
        if(input$SHOWSUGGESTED==TRUE) {
            
            updateSliderInput(session = getDefaultReactiveDomain(), 
                               inputId = "culling_cost_in", 
                               max = (MANAGER_BUDGET/10)+10,
                               value = unique(GDATA$observed_suggested$culling))
            updateSliderInput(session = getDefaultReactiveDomain(), 
                               inputId = "scaring_cost_in", 
                               max = (MANAGER_BUDGET/10)+10,
                               value = unique(GDATA$observed_suggested$scaring))
            
            CURRENT_BUDGET = updateCurrentBudget(CURRENT_BUDGET,
                                                 manager_budget = input$MANAGER_BUDGET,
                                                 culling_cost = input$culling_cost_in, 
                                                 scaring_cost = input$scaring_cost_in)
            
        } else {
            
            updateSliderInput(session = getDefaultReactiveDomain(), 
                               inputId = "culling_cost_in", 
                               max = (MANAGER_BUDGET/10)+10,
                               value = (MANAGER_BUDGET/10+10)*0.5)
            updateSliderInput(session = getDefaultReactiveDomain(), 
                               inputId = "scaring_cost_in", 
                               max = (MANAGER_BUDGET/10)+10,
                               value = (MANAGER_BUDGET/10+10)*0.5)
            
            CURRENT_BUDGET = updateCurrentBudget(CURRENT_BUDGET, 
                                                 manager_budget = input$MANAGER_BUDGET,
                                                 culling_cost = input$culling_cost_in, 
                                                 scaring_cost = input$scaring_cost_in)
            
        }
        
        
        
    })
    
    observeEvent(input$resetInputs, {
        showModal(
            modalDialog(
                size = "m", 
                footer = NULL,
                easyClose = TRUE,
                title = "Warning!",
                "This will reset all inputs to their default values.",
                p(),
                div(style="float:left", actionButton("resetInputs_cancel","Cancel")),
                div(style="float:right", actionButton("resetInputs_ok","Reset")),
                br(),
                p(" "),
            )
        )
    })
    
    observeEvent(input$resetInputs_cancel, {
        removeModal()
    })
    
    observeEvent(input$resetInputs_ok, {
        removeModal()
        updateCheckboxInput(session = getDefaultReactiveDomain(), inputId = "LAND_OWNERSHIP", value = d.LAND_OWNERSHIP)
        updateSliderInput(session = getDefaultReactiveDomain(), inputId = "STAKEHOLDERS", value = d.STAKEHOLDERS)
        updateSliderInput(session = getDefaultReactiveDomain(), inputId = "MANAGER_BUDGET", value = d.MANAGER_BUDGET)
        updateSliderInput(session = getDefaultReactiveDomain(), inputId = "MANAGE_TARGET", value = d.MANAGE_TARGET)
        updateSelectInput(session = getDefaultReactiveDomain(), inputId = "OBSERVE_TYPE", selected = d.OBSERVE_TYPE)
        updateCheckboxInput(session = getDefaultReactiveDomain(), inputId = "RES_MOVE_OBS", value = d.RES_MOVE_OBS)
        updateSliderInput(session = getDefaultReactiveDomain(), inputId = "RES_DEATH_K", value = d.RES_DEATH_K)
        updateSliderInput(session = getDefaultReactiveDomain(), inputId = "LAMBDA", value = d.LAMBDA)
        updateSelectInput(session = getDefaultReactiveDomain(), inputId = "RES_DEATH_TYPE", selected = d.RES_DEATH_TYPE)
        updateSliderInput(session = getDefaultReactiveDomain(), inputId = "REMOVE_PR", value = d.REMOVE_PR)
        updateSliderInput(session = getDefaultReactiveDomain(), inputId = "USER_BUDGET", value = d.USER_BUDGET)
        updateCheckboxInput(session = getDefaultReactiveDomain(), inputId = "CULLING", value = d.CULLING)
        updateCheckboxInput(session = getDefaultReactiveDomain(), inputId = "SCARING", value = d.SCARING)
        updateCheckboxInput(session = getDefaultReactiveDomain(), inputId = "TEND_CROPS", value = d.TEND_CROPS)
        updateNumericInput(session = getDefaultReactiveDomain(), inputId = "K", value = d.K)
        updateCheckboxInput(session = getDefaultReactiveDomain(), inputId = "SHOWSUGGESTED", value = d.SHOWSUGGESTED)
    })
    
    
    observeEvent(input$resetGame, {
        showModal(
            modalDialog(
                size = "m", 
                footer = NULL,
                easyClose = TRUE,
                title = "Warning!",
                "This will end the game and take you back to the start!",
                p(),
                div(style="float:left", actionButton("reset_cancel","Cancel")),
                div(style="float:right", actionButton("reset_ok","Reset")),
                br(),
                p(" "),
            )
        )
    })
    
    observeEvent(input$reset_cancel, {
        removeModal()
    })
    
    observeEvent(input$reset_ok, {
        removeModal()
        toggleSetup("Setup")
        GDATA$summary = NULL
        GDATA$laststep = NULL
        GDATA$observed_suggested = NULL
        GDATA$yields = NULL
        CHECK$extinction = FALSE
        CURRENT_BUDGET$total = NULL
        CURRENT_BUDGET$culling = NULL
        CURRENT_BUDGET$scaring = NULL
        CURRENT_BUDGET$leftover = NULL
        
        CHECK$extinction = FALSE
        enable(id = "nextStep")
        enable(id = "culling_cost_in")
        enable(id = "scaring_cost_in")
    })
    
    
    ### When CULLING cost is adjusted, update budget and check if still within limits. If not, adjust SCARING.
    observeEvent(input$culling_cost_in, {

        CURRENT_BUDGET = updateCurrentBudget(CURRENT_BUDGET,
                                             manager_budget = input$MANAGER_BUDGET,
                                             culling_cost = input$culling_cost_in, 
                                             scaring_cost = input$scaring_cost_in)
        culling_b = CURRENT_BUDGET$culling
        scaring_b = CURRENT_BUDGET$scaring
        total_b = CURRENT_BUDGET$total
        
        # If spend on culling+scaring exceeds total budget:
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

        CURRENT_BUDGET = updateCurrentBudget(CURRENT_BUDGET,
                                             manager_budget = input$MANAGER_BUDGET,
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
            
            # If needed, set sliders to "suggested" costs:
            if(input$SHOWSUGGESTED == TRUE) {
                GDATA$observed_suggested = observed_suggested(nxt)
                updateNumericInput(session = getDefaultReactiveDomain(), 
                                   inputId = "culling_cost_in", 
                                   value = unique(GDATA$observed_suggested$culling))
                updateNumericInput(session = getDefaultReactiveDomain(), 
                                   inputId = "scaring_cost_in", 
                                   value = unique(GDATA$observed_suggested$scaring))    
            } 
            
            # Add appropriate outputs.
            GDATA$summary = append_UROM_output(dat = nxt, costs = costs_as_input, old_output = GDATA$summary)
            GDATA$yields = rbind(GDATA$yields, tapply(nxt$LAND[,,2],nxt$LAND[,,3],mean)) # Store per-user yield (before reset)
            # Reset time step
            nxt$LAND[,,2] = 1    # Reset landscape yield
            GDATA$laststep = nxt

        } else {
            
            CHECK$extinction = TRUE
            
            toggleState("nextStep")
            toggleState("culling_cost_in")
            toggleState("scaring_cost_in")
            
        }
        
    })
    
    output$df_data_out <- renderTable({
        temp = GDATA$summary
        temp[order(1:nrow(temp), decreasing = TRUE),]
    })
    
    output$df_yield_out <- renderTable({
        temp = GDATA$yields
        temp[order(1:nrow(temp), decreasing = TRUE),]
    })
    
    output$pop_plot <- renderPlot({
        if(!is.null(GDATA$summary)) {
            plot_pop(GDATA$summary, yield_dat = GDATA$yields, track_range = FALSE, extinction_message = CHECK$extinction)    
        }
    })
    
    output$land_plot <- renderPlot({
        if(!is.null(GDATA$laststep)) {
            plot_land_res(GDATA$laststep$LAND, GDATA$laststep$RESOURCES, 
                          col = land_colors, extinction_message = CHECK$extinction)
            
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
    
    ### Plots actions in last time step summed across all users
    output$actions_sum <- renderPlot({
        if(!is.null(GDATA$summary)) {
            yhi = ceiling(max(GDATA$summary[,c("culls","scares","tend_crops")],na.rm=T)/10)*10
            barplot(GDATA$summary[nrow(GDATA$summary)-1,c("culls","scares","tend_crops")], 
                    col = c("#D35E60","#9067A7","#56BA47"), ylim = c(0,yhi), names = c("Culls","Scares","Tend crop"), 
                    las = 2,
                    ylab = "Total number of actions")    
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
    
}

updateCurrentBudget = function(budget, manager_budget, culling_cost, scaring_cost) {
    budget$total = manager_budget+3*10*10
    budget$culling = culling_cost*10
    budget$scaring = scaring_cost*10
    budget$leftover = budget$total-(budget$culling+budget$scaring)
    return(budget)
}

toggleSetup = function(switchToTab) {
    
    if(switchToTab=="Main") {
        disable("REMOVE_PR")
        disable("K")
        disable("RES_DEATH_TYPE")
        disable("LAMBDA")
        disable("RES_DEATH_K")
        #disable("TEND_CROPS")
        #disable("SCARING")
        #disable("CULLING")
        disable("USER_BUDGET")
        disable("MANAGE_TARGET")
        disable("MANAGER_BUDGET")
        disable("STAKEHOLDERS")
        disable("LAND_OWNERSHIP")
        disable("runGame")
        disable("resetInputs")
        
    }
    
    if(switchToTab=="Setup") {
        enable("REMOVE_PR")
        enable("K")
        #enable("RES_DEATH_TYPE")
        enable("LAMBDA")
        enable("RES_DEATH_K")
        #enable("TEND_CROPS")
        #enable("SCARING")
        #enable("CULLING")
        enable("USER_BUDGET")
        enable("MANAGE_TARGET")
        enable("MANAGER_BUDGET")
        enable("STAKEHOLDERS")
        #enable("LAND_OWNERSHIP")
        enable("runGame")
        enable("resetInputs")
    }
    
    updateTabsetPanel(session = getDefaultReactiveDomain(), "MainPanels",
                      selected = switchToTab)
}

# Run the application 
shinyApp(ui = ui, server = server)
