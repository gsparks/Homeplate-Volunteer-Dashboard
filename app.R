library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)


header <- dashboardHeader(title = "Homeplate Volunteer Dashboard",
                          titleWidth = 320)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'),
             href = "https://www.homeplateyouth.org")
  )
)

body <- dashboardBody(
  fluidRow(
    column(8, box(width='70%', plotOutput("hourBar"))),
    column(4, box(width='30%', selectInput("volunteer", "Volunteer:",
                                           c('All', as.character(melted_hours$name)))))
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
  # Melt columns to create years for bar chart
  melted_hours <- melt(data = hour_data,
                       id.vars = "name",
                       measure.vars = c('hours_2018', 'current_year_hours', 'total_hours'))
  melted_hours$variable <- as.character(melted_hours$variable)
  melted_hours$name <- as.character(melted_hours$name)
  # Rename the years to be more clear
  melted_hours[melted_hours$variable == 'hours_2018', "variable"] <- '2018'
  melted_hours[melted_hours$variable == 'current_year_hours', "variable"] <- '2019'
  melted_hours[melted_hours$variable == 'total_hours', "variable"] <- 'Total'

  output$hourBar <- renderPlot({
    ggplot(data = filter(melted_hours, name == input$volunteer), aes(x=variable, y=value, label=value)) +
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
