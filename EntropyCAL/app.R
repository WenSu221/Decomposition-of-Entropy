#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(dplyr)
library(shiny)

# load data ####
entropyCAL <- read.csv("entropyCALtable.csv")
entropye0 <- read.csv("entropye0table.csv")
entropyec0 <- read.csv("entropyctable.csv")

# Define UI ####
ui <- fluidPage(
    titlePanel("Comparison between period and cohort life table entropy, 
               and entropy in CAL perspective"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "country",
                label = "country in reference",
                choices = c("SWE","DNK","FRATNP","GBRTENW","NOR",
                            "FIN","ITA","GBRSCO","NLD")),
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input,output,session){
    output$plot <- renderPlot(
        plot(c(1878,2017),c(0,1),xlab = "Years",ylab = "Entropy index", col=0)
        
        )
    }

# Run the application 
shinyApp(ui = ui, server = server)
