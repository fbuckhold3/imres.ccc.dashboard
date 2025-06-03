# CCC Review Dashboard Application
# Main application file

# Load required libraries
library(shiny)

# Source UI and Server files
source("R/ui.R")
source("R/server.R")
source("R/global.R")

# Run the application
shinyApp(ui = ui, server = server)
