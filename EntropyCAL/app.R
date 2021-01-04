#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(dplyr)
library(ggplot2)
library(shiny)

# load data ####

data <- read.csv("entropy_table.csv")

# Define UI ####
ui <- fluidPage(
    titlePanel("Comparison between period and cohort life table entropy, 
               and entropy in CAL perspective"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "population",
                label = "population in reference",
                choices = unique(data$country)),
        ),
        mainPanel(
            plotOutput(outputId = "plot",height = "400px",
                       width = "500px")
            )
    )
)

server <- function(input,output){
    
    annotate_data <- reactive({
        filter(data,country == input$population)
    })
    
    output$plot <- renderPlot({
        ggplot(data = annotate_data(),
               mapping = aes(year,entropy,color = measure))+
            geom_line()+
            coord_cartesian(ylim = c(0,1))
        
    }, res = 100)

}

# Run the application 
shinyApp(ui = ui, server = server)
