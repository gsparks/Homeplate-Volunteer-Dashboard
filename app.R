library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)


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
    box(plotOutput("hourBar"))
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {
  # Pull Volunteer Data
  # Once when the server starts
  source('get_volunteer_data.R')
  volunteer_data <- get_volunteer_data()
  hour_data <- volunteer_data %>% select(name, hours_2018, current_year_hours, total_hours)
  melted_hours <- melt(data = hour_data,
                       id.vars = "name",
                       measure.vars = c('hours_2018', 'current_year_hours', 'total_hours'))

  output$hourBar <- renderPlot({
    ggplot(data=melted_hours, aes(x=variable, y=value, label=value)) +
      geom_bar(stat="identity") +
      labs(title="Homeplate Volunteer Hours", y="Hours", x="Year", caption="Hours updated daily.") +
      theme_economist() +
      theme(plot.title = element_text(hjust = 0.5)) +
      stat_summary(aes(label = ..y..),
                       fun.y = 'sum',
                       geom = 'text',
                       col = 'white',
                       vjust = 1.5)
   })
}

# Run the application
shinyApp(ui = ui, server = server)
