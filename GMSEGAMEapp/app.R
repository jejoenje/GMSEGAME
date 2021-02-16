library(shiny)
library(shinyjs)
library(GMSE)
source("../app_plotting.R")

d.LAND_OWNERSHIP = TRUE
d.STAKEHOLDERS = 4
d.MANAGER_BUDGET = 1000
d.MANAGE_TARGET = 8000
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
d.SHOWSUGGESTED = TRUE
d.K = 5


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
            h3("Main Panel"),
            column(1),
            column(5,
                fluidRow(
                    plotOutput("pop_plot",width = "95%")
                ),
                fluidRow(
                    plotOutput("land_plot",width = "95%")
                )
            ),
            column(5,
                fluidRow(
                    numericInput("culling_cost_in", "Culling cost",
                                min = 10, max = MANAGER_BUDGET, value = MANAGER_BUDGET/2, step = 10),
                    numericInput("scaring_cost_in", "Scaring cost",
                                min = 10, max = MANAGER_BUDGET, value = MANAGER_BUDGET/2, step = 10),
                    actionButton("nextStep", "Next step"),
                    actionButton("resetGame", "Reset game")
                ),
                fluidRow(tableOutput("df_data_out"))
            ),
            column(1),
                 

            
                 
        )
    )
    
)

server <- function(input, output, session) {
    
    GDATA = reactiveValues(summary = NULL, laststep = NULL, land_dist = NULL, observed_suggested = NULL)
    
    CHECK = reactiveValues(extinction = FALSE)
    
    observeEvent(input$runGame, {
        toggleSetup("Main")
        
        ### Initial time steps:
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
        
        if(input$SHOWSUGGESTED==TRUE) {
            updateNumericInput(session = getDefaultReactiveDomain(), 
                               inputId = "culling_cost_in", 
                               value = unique(GDATA$observed_suggested$culling))
            updateNumericInput(session = getDefaultReactiveDomain(), 
                               inputId = "scaring_cost_in", 
                               value = unique(GDATA$observed_suggested$scaring))    
        } else {
            updateNumericInput(session = getDefaultReactiveDomain(), 
                               inputId = "culling_cost_in", 
                               value = floor(input$MANAGER_BUDGET/2))
            updateNumericInput(session = getDefaultReactiveDomain(), 
                               inputId = "scaring_cost_in", 
                               value = floor(input$MANAGER_BUDGET/2))
        }
        
        
        
    })
    
    observeEvent(input$resetInputs, {
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
        toggleSetup("Setup")
        CHECK$extinction = FALSE
        enable(id = "nextStep")
        enable(id = "culling_cost_in")
        enable(id = "scaring_cost_in")
    })
    
    observeEvent(input$nextStep, {
        
        ### User input
        costs_as_input = list(culling = input$culling_cost_in, scaring = input$scaring_cost_in)
        prev = GDATA$laststep
        prev = set_man_costs(prev, newcost = costs_as_input)
        
        ### Run next time step:
        nxt = try({gmse_apply_UROM(get_res = "Full", old_list = prev)}, silent = TRUE)
        
        if(class(nxt)!="try-error") {
            # Get suggested costs, and set sliders:
            if(input$SHOWSUGGESTED == TRUE) {
                GDATA$observed_suggested = observed_suggested(nxt)
                updateNumericInput(session = getDefaultReactiveDomain(), 
                                   inputId = "culling_cost_in", 
                                   value = unique(GDATA$observed_suggested$culling))
                updateNumericInput(session = getDefaultReactiveDomain(), 
                                   inputId = "scaring_cost_in", 
                                   value = unique(GDATA$observed_suggested$scaring))    
            } else {
                updateNumericInput(session = getDefaultReactiveDomain(), 
                                   inputId = "culling_cost_in", 
                                   value = costs_as_input$culling)
                updateNumericInput(session = getDefaultReactiveDomain(), 
                                   inputId = "scaring_cost_in", 
                                   value = costs_as_input$scaring)
            }
            
            # Add appropriate outputs.
            GDATA$summary = append_UROM_output(dat = nxt, costs = costs_as_input, old_output = GDATA$summary)
            # Reset time step
            GDATA$laststep = nxt

        } else {
            
            CHECK$extinction = TRUE
            
            toggleState("nextStep")
            toggleState("culling_cost_in")
            toggleState("scaring_cost_in")
            
            
            # print("STOP - POPULATION WIPED OUT")
            # output
        }
        
        
    })
    
    output$df_data_out <- renderTable({
        temp = GDATA$summary
        temp[order(1:nrow(temp), decreasing = TRUE),]
    })
    
    output$pop_plot <- renderPlot({
        plot_pop(GDATA$summary, track_range = FALSE, extinction_message = CHECK$extinction)
    })
    
    output$land_plot <- renderPlot({
        plot_land_res(GDATA$laststep$LAND, GDATA$laststep$RESOURCES)
    })
    
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
