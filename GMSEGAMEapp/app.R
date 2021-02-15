library(shiny)
library(shinyjs)
library(GMSE)
source("../app_plotting.R")

ui <- fluidPage(
    tags$style(HTML("
        .tabbable > .nav > li > a                  {background-color: white;  color:white}
        .tabbable > .nav > li[class=active]    > a {background-color: white; color:white}
    ")),
    useShinyjs(),
    
    titlePanel("GMSE-GAME"),
    
    tabsetPanel(id = "MainPanels", selected = "Setup", type = "pills",
        tabPanel(title = "", value = "Setup",
            h3("Game setup"),
            
            column(1),
            column(3,
                wellPanel(
                    h3("General environment"),
                    checkboxInput("LAND_OWNERSHIP", strong("Land ownership"), value = TRUE),
                    sliderInput("STAKEHOLDERS", "No. of stakeholders (users)",
                                min = 4, max = 32, value = 4, step = 1),
                    sliderInput("MANAGER_BUDGET", "Manager budget",
                                       min = 100, max = 5000, value = 1000, step = 100),
                    sliderInput("MANAGE_TARGET", "Management population target (for initial time steps only)",
                                       min = 500, max = 10000, value = 1500, step = 100),
                    selectInput("OBSERVE_TYPE", "Resource observation type", 
                                       choices = list("Density-based" = 0,
                                                      "Mark-recapture estimate" = 1, 
                                                      "Transect sampling (linear)" = 2,
                                                      "Transect sampling (block)" = 3
                                                      ), selected = 0),
                    checkboxInput("RES_MOVE_OBS", strong("Resources move during observation"), value = TRUE)
                )    
            ),
                
            column(3,
                wellPanel(
                    h3("Resource population controls"),
                    sliderInput("RES_DEATH_K", "K (resource carrying capacity)",
                                       min = 100, max = 10000, value = 3000, step = 100),
                    sliderInput("LAMBDA", "Lambda (resource max growth rate)",
                                       min = 0.1, max = 2, value = 0.3, step = 0.1),
                    selectInput("RES_DEATH_TYPE", "Resource removal (death) type", 
                                       choices = list("Density-independent" = 1, 
                                                      "Density-dependent" = 2,
                                                      "Both" = 3), selected = 3),
                    sliderInput("REMOVE_PR", "Fixed mortality probability",
                                       min = 0, max = 0.95, value = 0.05, step = 0.05)
                )
            ),
            
            column(3,
                wellPanel(
                    h3("User action controls"),
                    sliderInput("USER_BUDGET", "User budget",
                                   min = 100, max = 5000, value = 1500, step = 100),
                    checkboxInput("CULLING", "Culling", value = TRUE),
                    checkboxInput("SCARING", "Scaring", value = TRUE),
                    checkboxInput("TEND_CROPS", "Tending crops", value = TRUE)
                ),
                wellPanel(style = "background: white",
                    h3("Misc controls"),
                    column(6,numericInput("K", "No. of initial time steps",
                                          min = 2, max = 10, value = 5, step = 1)
                    ),
                    column(6,checkboxInput("SHOWSUGGESTED", strong("Show suggested costs"), value = FALSE)
                           ),
                    br(),
                    actionButton("runGame", "GO!", icon = icon("gamepad")),
                    disabled(actionButton("resetGame_setup", "Reset game"))
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
                    sliderInput("culling_cost_in", "Culling cost",
                                min = 10, max = 100001, value = 50, step = 10),
                    sliderInput("scaring_cost_in", "Scaring cost",
                                min = 10, max = 100001, value = 10, step = 10),
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
        
        updateNumericInput(session = getDefaultReactiveDomain(), 
                           inputId = "culling_cost_in", 
                           value = unique(GDATA$observed_suggested$culling))
    })
    
    observeEvent(input$resetGame_setup, {
        toggleSetup("Setup")
    })
    
    observeEvent(input$resetGame, {
        toggleSetup("Setup")
    })
    
    observeEvent(input$nextStep, {
        
        ### User input
        costs_as_input = list(culling = input$culling_cost_in, scaring = input$culling_cost_in)
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
            }
            
            # Add appropriate outputs.
            GDATA$summary = append_UROM_output(dat = nxt, costs = costs_as_input, old_output = GDATA$summary)
            # Reset time step
            GDATA$laststep = nxt

        } else {
            # print("STOP - POPULATION WIPED OUT")
            # output
        }
        
        
    })
    
    output$df_data_out <- renderTable({
        temp = GDATA$summary
        temp[order(1:nrow(temp), decreasing = TRUE),]
    })
    
    output$pop_plot <- renderPlot({
        plot_pop(GDATA$summary, track_range = FALSE)
    })
    
    output$land_plot <- renderPlot({
        plot_land_res(GDATA$laststep$LAND, GDATA$laststep$RESOURCES)
    })
    
}

toggleSetup = function(switchToTab) {
    toggleState("REMOVE_PR")
    toggleState("K")
    toggleState("RES_DEATH_TYPE")
    toggleState("LAMBDA")
    toggleState("RES_DEATH_K")
    toggleState("TEND_CROPS")
    toggleState("SCARING")
    toggleState("CULLING")
    toggleState("USER_BUDGET")
    toggleState("MANAGE_TARGET")
    toggleState("MANAGER_BUDGET")
    toggleState("STAKEHOLDERS")
    toggleState("LAND_OWNERSHIP")
    
    toggleState("runGame")
    toggleState("resetGame_setup")
    
    ### Needs an "if" to clear game output when resetGame_setup is pressed
    
    updateTabsetPanel(session = getDefaultReactiveDomain(), "MainPanels",
                      selected = switchToTab)
}

# Run the application 
shinyApp(ui = ui, server = server)
