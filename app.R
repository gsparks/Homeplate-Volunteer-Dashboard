library(shiny)
source('get_volunteer_data.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Homeplate Volunteer Hours"),

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
         plotOutput("hourPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  volunteer_data <- get_volunteer_data()

   output$hourPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- volunteer_data[['total_hours']]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application
shinyApp(ui = ui, server = server)

