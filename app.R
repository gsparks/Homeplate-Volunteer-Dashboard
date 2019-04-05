library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(dplyr)


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
  hour_data <- volunteer_data %>% select(name, hours_2018, current_year_hours, total_hours)
  melted_hours <- melt(data = hour_data, id.vars = "name", measure.vars = c('hours_2018', 'current_year_hours', 'total_hours'))

   output$hourDist <- renderPlot({
     ggplot(volunteer_data, aes(total_hours)) +
       geom_bar()
   })
}

# Run the application
shinyApp(ui = ui, server = server)

