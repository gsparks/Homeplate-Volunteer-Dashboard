library(shiny)
library(shinydashboard)
library(ggplot2)


header <- dashboardHeader(title = "Homeplate Volunteer Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'),
             href = "https://www.homeplateyouth.org")
  )
)

body <- dashboardBody(
  fluidRow(
    box(plotOutput("hourDist"))
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {
  source('get_volunteer_data.R')
  volunteer_data <- get_volunteer_data()

   output$hourDist <- renderPlot({
     ggplot(volunteer_data, aes(total_hours)) +
       geom_bar()
   })
}

# Run the application
shinyApp(ui = ui, server = server)

