#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Old Faithful Geyser Data"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Get data from Homeplate airtable
  response <- GET("https://api.airtable.com/v0/apppms4ZuxVLyJySd/Volunteers?maxRecords=3&view=Master%20List",
                  query = list(api_key = "keyNaoXggyh686bGO"))

  text_json_response <- content(response, as="text")
  raw_data <- fromJSON(text_json_response)
  data <- raw_data[[1]][2][,1]
  volunteer_data <- data.frame(data[["Name"]],
                               data[["Status"]],
                               data[["Telephone Number"]],
                               data[["Company/Organization"]],
                               data[["Current Year Hours Count"]],
                               data[["Total Hours Served"]],
                               data[["2018 Hours"]],
                               data[["Email Address"]],
                               data[["Birthday"]])
  colnames(volunteer_data) <- c("name", "status", "phone_number",
                                "org", "current_year_hours", "total_hours",
                                "hours_2018", "email", "birthday")

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application
shinyApp(ui = ui, server = server)

