# CCC Review Dashboard Application
# Main application file

# Load required libraries
library(shiny)

# Source UI and Server files
source("R/global.R")
source("R/ui.R")
source("R/server.R")


# Run the application
shinyApp(ui = ui, server = server)

