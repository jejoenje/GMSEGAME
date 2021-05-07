# initModal = function() {
#   showModal(modalDialog(size = "l", footer = actionButton("dismissInitModal", "Ok!"),
#                         #title = "Welcome to GMSE-GAME!",
#                         includeMarkdown("introduction.Rmd")
#   ))  
# }

initModal1 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("toInit2", "Next"), actionButton("dismissInitModal", "Skip introduction")),
                        #title = "Welcome to GMSE-GAME!",
                        includeMarkdown("introduction1.Rmd")
  ))  
}


initModal2 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("toInit1", "Previous"),
                                                     actionButton("toInit3", "Next"), 
                                                     actionButton("dismissInitModal", "Skip introduction")),
                        #title = "Welcome to GMSE-GAME!",
                        includeMarkdown("introduction2.Rmd")
  ))  
}


initModal3 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("backtoInit2", "Previous"),
                                                     actionButton("toInit4", "Next"), 
                                                     actionButton("dismissInitModal", "Skip introduction")),
                        #title = "Welcome to GMSE-GAME!",
                        includeMarkdown("introduction3.Rmd")
  ))  
}

initModal4 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("backtoInit3", "Previous"),
                                                     actionButton("toInit5", "Next"), 
                                                     actionButton("dismissInitModal", "Skip introduction")),
                        includeMarkdown("introduction4.Rmd")
  ))  
}

initModal5 = function() {
  showModal(modalDialog(size = "l", footer = tagList(actionButton("backtoInit4", "Previous"),
                                                     actionButton("dismissInitModal", "Okay, go!")),
                        includeMarkdown("introduction5.Rmd")
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
  showModal(modalDialog(size = "s", footer = actionButton("confirmStart", "Go!"),
                        title = div(style="padding-left: 10%; padding-right: 10%", "What is your player name?" ),
                        #div(style="padding-left: 20%; padding-right: 20%","This can be anything, we used it to make a scoreboard!"),
                        div(style="padding-left: 10%; padding-right: 10%",
                            textInput("playerName", label = NULL, value = playername, width = NULL, placeholder = NULL),
                            "(Letters and numbers only please; and no spaces)"
                            )
                        
                        )
            )
}

confirmResetModal = function() {
  showModal(modalDialog(size = "m", footer = tagList(modalButton("Cancel"),actionButton("confirmReset", "Yes, reset.")),
                        title = "Are you sure you want to reset?",
                        "Resetting the game means you go back to the start!"
                        )
            )
}

extinctionModal = function() {
  showModal(modalDialog(size = "m", footer = actionButton("confirmExtinction", "Ok"),
                        title = "Population extinct!",
                        "No resources left to manage!"
                      )
  )
}

scoresModal = function() {
  showModal(modalDialog(size = "l", footer = tagList(modalButton("Close"),actionButton("closeScores", "New Game")), easyClose = TRUE,
                        title = "High scores",
                        dataTableOutput("highScores")
                        
  ))  
}
