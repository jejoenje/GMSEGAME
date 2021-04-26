initModal = function() {
  showModal(modalDialog(size = "l", footer = actionButton("dismissInitModal", "Ok!"),
                        title = "Welcome to GMSE-GAME!",
                        "This dialog will contain some initial explanation of how the game works, what the elements of the screen are, etc.",
                        p(),
                        "This message will only be shown once, so not after the game is reset."
  ))  
}

setPlayerModal = function(playername) {
  showModal(modalDialog(size = "s", footer = actionButton("confirmStart", "Go!"),
                        title = div(style="padding-left: 10%; padding-right: 10%", "What is your player name?"),
                        #div(style="padding-left: 20%; padding-right: 20%","This can be anything, we used it to make a scoreboard!"),
                        div(style="padding-left: 10%; padding-right: 10%",textInput("playerName", label = NULL, value = playername, width = NULL, placeholder = NULL))
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
  showModal(modalDialog(size = "l", footer = tagList(actionButton("closeScores", "Ok")),
                        title = "High scores",
                        dataTableOutput("highScores")
                        
  ))  
}
