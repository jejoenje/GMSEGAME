library(shiny)
library(shinyjs)
library(GMSE)
source("../app_plotting.R")

ui <- fluidPage(
    useShinyjs(),
    
    titlePanel("GMSE-GAME"),
    
    tabsetPanel(id = "MainPanels", selected = "Setup", type = "pills",
        tabPanel(title = "", value = "Setup",
            h3("Game setup"),
            
            fluidRow(
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
                           selectInput("OBSERVE_TYPE", "Resource removal (death) type", 
                                       choices = list("Mark-recapture estimate" = 1, 
                                                      "Transect sampling (linear)" = 2,
                                                      "Transect sampling (block)" = 3
                                                      ), selected = 1),
                           checkboxInput("RES_MOVE_OBS", strong("Resources move during observation"), value = TRUE)
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
                column(2)    
            ),
            fluidRow(
                column(1),
                column(3),
                column(3,
                    wellPanel(
                        h3("Misc controls"),
                        sliderInput("K", "No. of initial time steps",
                                    min = 2, max = 10, value = 5, step = 1),
                        actionButton("runGame", "GO!", icon = icon("gamepad")),
                        disabled(actionButton("resetGame_setup", "Reset game"))
                        
                    )
                ),
                column(3),
                column(2),
                
            )
            
        ),
        tabPanel(title = "", value = "Main",
                 h3("Main Panel"),
                 tableOutput("df_data_out"),
                 #plotOutput("popPlot"),
                 #plotOutput("landPlot"),
                 actionButton("resetGame", "Reset game")
                 
        )
    )
    
)

server <- function(input, output, session) {
    
    GDATA = reactiveValues(summary = NULL, laststep = NULL)
    
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
            
        # Appends output:
        #gamedat = init_steps$summary
        # Extracts last time step (last `old_list`)
        #laststep = init_steps$gmse_list[[length(init_steps$gmse_list)]]
        #plot_land_res(prev$LAND, prev$RESOURCES)
    })
    
    # output$popPlot <- renderPlot(gamedat, {
    #     plot_pop(gamedat, track_range = FALSE)
    # })
    
    observeEvent(input$resetGame_setup, {
        toggleSetup("Setup")
    })
    
    observeEvent(input$resetGame, {
        toggleSetup("Setup")
    })
    
    output$df_data_out <- renderTable(GDATA$summary)
    
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
