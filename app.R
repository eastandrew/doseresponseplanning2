#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that interpolates equidistant points on log-scale for dose repsonse planning
ui <- fluidPage(
  
  # Application title
  titlePanel("Log Equidistant Concentration and Effect Planning"),
  titlePanel("with start and ending concentration"),
  
  # Sidebar with a slider input for number of bins, starting concentration, and ending concentration
  sidebarLayout(
    sidebarPanel(
      sliderInput("numT",
                  "Number of Treatments:",
                  min = 1,
                  max = 20,
                  value = 5),
      numericInput("startconc", label=h3("Starting Concentration"), value=0.05),
      numericInput("endconc", label=h3("Ending Concentration"), value=0.5)
    ),
    
    # Show a plot of the generated concentrations across bio effect of 100% across number of treatments
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Define server logic required to fit values and plot
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    vect <- c()
    vect[1] <- input$startconc
    multiplier <- (exp((log(input$endconc)-log(input$startconc))/(input$numT-1)))
    #multiplier <- (10^((log10(maxx)-log10(minx))/(binsx-1)))
    for (i in 2:(input$numT)){
      vect[i] <- vect[i-1]*multiplier
    }
    #vect
    plot(vect,c(seq(1,0,length.out=input$numT)), log="x", type="b", pch=NA, xlab="log visualize concentration", ylab="effect")
    text(vect,c(seq(1,0,length.out=input$numT)),round(vect,3))
    text((max(vect)*0.5), 0.8, bquote(paste("Factor: ",.(round(multiplier,3)))))
    text((max(vect)*0.5), 0.7, "OECD max = 3.2")

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

