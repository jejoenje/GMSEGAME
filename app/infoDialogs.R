initModal = function() {
  showModal(modalDialog(size = "l", footer = actionButton("confirmStart", "Go!"),
                        title = "Welcome to GMSE-GAME!",
                        "This dialog will contain some initial explanation of how the game works, what the elements of the screen are, etc.",
                        p(),
                        "This message will only be shown once, so not after the game is reset."
  ))  
}

confirmResetModal = function() {
  showModal(modalDialog(size = "m", footer = tagList(modalButton("Cancel"),actionButton("confirmReset", "Yes, reset.")),
                        title = "Are you sure you want to reset?",
                        "Resetting the game means you go back to the start!"
                        )
            )
}

extinctionModal = function() {
  showModal(modalDialog(size = "m", footer = tagList(modalButton("Ok.")),
                        title = "Population extinct!",
                        "No resources left to manage!"
                      )
  )
}



  
