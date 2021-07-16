# initModal = function() {
#   showModal(modalDialog(size = "l", footer = actionButton("dismissInitModal", "Ok!"),
#                         #title = "Welcome to GMSE-GAME!",
#                         includeMarkdown("introduction.Rmd")
#   ))  
# }

initModal1 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("toInit2", "Next", class="butt"), actionButton("dismissInitModal", "Skip introduction", class="butt", style="float:left")),
                        #title = "Welcome to GMSE-GAME!",
                        includeMarkdown("introduction1.Rmd")
  ))  
}


initModal2 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("toInit1", "Previous", class = "butt"),
                                                     actionButton("toInit3", "Next", class = "butt"), 
                                                     actionButton("dismissInitModal", "Skip introduction", class = "butt", style="float:left")),
                        #title = "Welcome to GMSE-GAME!",
                        includeMarkdown("introduction2.Rmd")
  ))  
}


initModal3 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("backtoInit2", "Previous", class = "butt"),
                                                     actionButton("toInit4", "Next", class = "butt"), 
                                                     actionButton("dismissInitModal", "Skip introduction", class = "butt", style="float:left")),
                        #title = "Welcome to GMSE-GAME!",
                        includeMarkdown("introduction3.Rmd")
  ))  
}

initModal4 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("backtoInit3", "Previous", class = "butt"),
                                                     actionButton("toInit5", "Next", class = "butt"), 
                                                     actionButton("dismissInitModal", "Skip introduction", class = "butt", style="float:left")),
                        includeMarkdown("introduction4.Rmd")
  ))  
}

initModal5 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("backtoInit4", "Previous", class = "butt"),
                                                     actionButton("toInit6", "Next", class = "butt"),
                                                     actionButton("dismissInitModal", "Skip introduction", class = "butt", style="float:left")),
                        includeMarkdown("introduction5.Rmd")
  ))  
}

initModal6 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("backtoInit5", "Previous", class = "butt"),
                                                     actionButton("dismissInitModal", "Okay, go!", class = "butt", style = "color: #fff; background-color: #D35E60; font-weight: bold")),
                        includeMarkdown("introduction6.Rmd")
  ))  
}

allIntroModal = function() {
  showModal(modalDialog(size = "l", footer = tagList(modalButton("Close")), easyClose = TRUE,
                        includeMarkdown("introduction1.Rmd"),
                        includeMarkdown("introduction2.Rmd"),
                        includeMarkdown("introduction3.Rmd"),
                        includeMarkdown("introduction4.Rmd"),
                        includeMarkdown("introduction5.Rmd")
  ))  
}

setPlayerModal = function(playername) {
  showModal(modalDialog(size = "l", footer = actionButton("confirmStart", "Go!"),
                        #title = div(style="padding-left: 10%; padding-right: 10%", "What is your player name?"),
                        div(style="padding-left: 10%; padding-right: 10%",
                        includeMarkdown("consentText.Rmd"),
                        ),
                        div(style="padding-left: 10%; padding-right: 10%; font-weight: bold;",
                            checkboxInput("consentAgree", "I consent to the above", value = FALSE, width = NULL)
                        ),
                        hr(),
                        div(style="padding-left: 10%; padding-right: 10%;", h3("What is your player name?")),
                        div(style="padding-left: 10%; padding-right: 10%; font-size: 0.75em",
                            textInput("playerName", label = NULL, value = playername, width = NULL, placeholder = NULL),
                            "",
                            tags$div(tags$ul(
                              tags$li(tags$span("Letters and numbers only please; and no spaces.")),
                              tags$li(tags$span("We only ask for a nickname so you can keep track of your game scores on the leaderboard.")),
                              tags$li(tags$span("Using your real name means we will record this and that you consent to us doing so; if you do not, please use a nickname.")),
                            ))
                        )
                        
                      )
            )
}

confirmResetModal = function() {
  showModal(modalDialog(size = "m", footer = tagList(actionButton("cancelReset", "Cancel", class = "butt"),actionButton("confirmReset", "Yes, reset.", class = "butt")),
                        title = span(style = "font-family: Courier New; font-weight: bold; color:darkred", "Are you sure you want to reset?"),
                        span(style = "font-family: Courier New;","Resetting the game means you go back to the start!")
                        )
            )
}

finishedModal = function() {
  showModal(modalDialog(size = "m", footer = actionButton("confirmFinished", "Ok", class = "butt"),
                        title = span(style = "font-family: Courier New; font-weight: bold; color:darkred","You've reached the final management year!"),
                        span(style = "font-family: Courier New","Well done, you have reached the maximum number of management years. The grazing animal population has not gone extinct.")
  )
  )
}

extinctionModal = function() {
  showModal(modalDialog(size = "m", footer = actionButton("confirmExtinction", "Ok", class = "butt"),
                        title = span(style = "font-family: Courier New; font-weight: bold; color:darkred","Population extinct!"),
                        span(style = "font-family: Courier New","No resources left to manage!")
                      )
  )
}

scoresModal = function(score_display = "total", total_scores = NULL) {
  
  if(score_display == "total") {
    showModal(modalDialog(size = "l", footer = tagList(actionButton("closeScores", "New Game", class = "butt")), easyClose = TRUE,
                          title = span(style = "font-size:1.75em; font-family: Courier New; font-weight: bold;","Top 10 High scores: Total"),
                          span(style = "font-size:1.25em; font-weight: bold;", 
                               "You are", 
                               span(style = "font-size:1.5em; color:red;", textOutput("rank_total",inline=T)), 
                               "out of ", 
                               span(style = "font-size:1.5em; color:darkred;", total_scores),"!"
                          ),
                          dataTableOutput("highScores")
    ))  
  }
  
  if(score_display == "split") {
    showModal(modalDialog(size = "l", footer = tagList(actionButton("closeScores", "New Game", class = "butt")), easyClose = TRUE,
                          title = span(style = "font-size:2em; font-family: Courier New; font-weight: bold;","Top 10 High scores"),
                          div(style="display: inline-block;vertical-align:top; width: 400px;", 
                              span(style="font-size:1.5em;align:center; text-align:center;", "Animal score"),
                              p(),
                              span(style = "font-size:1.25em; font-weight: bold;", 
                                   "You are", 
                                   span(style = "font-size:1.5em; color:red;", textOutput("rank_res",inline=T)), 
                                   "out of ", 
                                   span(style = "font-size:1.5em; color:darkred;", total_scores),"!"
                              ),
                              dataTableOutput("highScores_res")),
                          div(style="display: inline-block;vertical-align:top; width: 50px;"," "),
                          div(style="display: inline-block;vertical-align:top; width: 400px;", 
                              span(style="font-size:1.5em;align:center; text-align:center; ", "Farming score"),
                              p(),
                              span(style = "font-size:1.25em; font-weight: bold;", 
                                   "You are", 
                                   span(style = "font-size:1.5em; color:green;", textOutput("rank_yld",inline=T)), 
                                   "out of ", 
                                   span(style = "font-size:1.5em; color:darkred;", total_scores),"!"
                              ),
                              dataTableOutput("highScores_yld")),
    ))  
  }
  
}
