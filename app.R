
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that interpolates equidistant points on log-scale for dose repsonse planning
# or uses constant multiplicative factor across a number of treatments from a starting concentration
ui <- fluidPage(
    
    # Application title
    titlePanel("Concentration Planning:"),
    "with either start and ending concentration or constant multiplicative factor.",
    
    # Sidebar with a slider input for number of bins, starting concentration, and ending concentration
    sidebarLayout(
        sidebarPanel(
            checkboxInput("checkbox", label=h5("Known Ending Concentration"), value = TRUE),
            sliderInput("numT",label=h5("Number of Treatments:"),min = 1,max = 20,value = 5),
            numericInput("startconc", label=h5("Starting Concentration"), value=0.05),
            numericInput("endconc", label=h5("Ending Concentration"), value=0.5),
            numericInput("changefactor", label = h5("Change Factor"), value = exp(1))
        ),
        
        # Show a plot of the generated concentrations across bio effect of 100% across number of treatments
        mainPanel(
            plotOutput("scatterPlot", width=450, height=450)
        )
    )
)

# Define server logic required to fit values and plot
server <- function(input, output) {
    
    output$scatterPlot <- renderPlot({
        if (input$checkbox==T) {
            vect <- c()
            vect[1] <- input$startconc
            multiplier <- (exp((log(input$endconc)-log(input$startconc))/(input$numT-1)))
            #multiplier <- (10^((log10(maxx)-log10(minx))/(binsx-1)))
            for (i in 2:(input$numT)){
                vect[i] <- vect[i-1]*multiplier
            }
            #vect
            
            par(mai=c(1,1,0.15,0.15), lwd=2, font=1, font.lab=1, font.axis=1, cex.lab=1.5, cex.axis=1.5, bty="l", family="sans")
            plot(vect,c(seq(1,0,length.out=input$numT)), log="x", type="c", xlab="Log Visualized Concentration", ylab="Effect")
            text(vect,c(seq(1,0,length.out=input$numT)),round(vect,3), srt=45)
            text(grconvertX(0.8, from="ndc", to="user"),grconvertY(0.75, from="ndc", to="user"), bquote(bold(paste("Current Factor = ",bold(.(round(multiplier,3)))))))
            text(grconvertX(0.8, from="ndc", to="user"),grconvertY(0.8, from="ndc", to="user"), "OECD max = 3.2", font=2)
           
        } else {
            numtreats <- input$numT
            startinglow <- input$startconc
            factorstart <- input$changefactor
            
            df <- data.frame(effmag=seq(1,0,length.out=numtreats))
            df$treatstart <- startinglow
            df$factor <- factorstart
            df$ID <- as.numeric(row.names(df))-1
            df$treat <- df$treatstart*(df$factor^df$ID)
            #df
            par(mai=c(1,1,0.15,0.15), lwd=2, font=1, font.lab=1, font.axis=1, cex.lab=1.5, cex.axis=1.5, bty="l", family="sans")
            plot(1,1,pch=NA, ylim=c(0,1),xlim=c(min(df$treat),max(df$treat)),xlab="Log Visualized Concentration",ylab="Effect",log="x")
            points(effmag~treat, data=df, type="c")
            text(effmag~treat, data=df, bquote(.(round(df$treat,3))), srt=45)
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
