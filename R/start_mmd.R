start_mmd <- function() {
    ## Define the UI and server files for the app:
    app <- shiny::shinyApp(ui = ui, server = server)
    ## Define a folder that contains a CSS sheet:
    shiny::addResourcePath(prefix = "style", directoryPath = system.file("css", package = "AnthropMMD"))
    ## Run the app:
    shiny::runApp(app)
}

StartMMD <- function() {
    .Deprecated("start_mmd")
    start_mmd()
}
