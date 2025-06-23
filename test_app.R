if (interactive()) {
  library(shiny)
  library(shinyjs)
  shinyApp(
    ui = fluidPage(
      shinyjs::useShinyjs(),  # Set up shinyjs
      actionButton("btn", "Click me"),
      textInput("element", "Watch what happens to me")
    ),
    server = function(input, output) {
      observeEvent(input$btn, {
        # Run a simply shinyjs function
        toggle("element")
      })
    }
  )
}
