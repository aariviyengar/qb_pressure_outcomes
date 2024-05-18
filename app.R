#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggimage)
library(ggthemes)
library(dplyr)
out_of_pocket <- read_csv("pocket_data.csv")|>
  arrange(name)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Compare Quarterback Outcomes When Outside the Pocket"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           selectInput(inputId="qbs",label="Select Quarterbacks",choices=out_of_pocket$name,multiple=TRUE),
           submitButton(text="Create Chart")),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("pocketPlot"),
           width=12,
           height=9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    get_qbs <- reactive({
      chart_data <- data.frame()
      req(input$qbs)
      for(qb in input$qbs){
         qb_data <- out_of_pocket|>
           filter(name==qb)
         chart_data <- rbind(chart_data,qb_data)
       }
     chart_data$Outcome <- factor(chart_data$Outcome,levels=c("Touchdown","Completion","Scramble","Incompletion","Interception","Sack"))
     chart_data
    })
      output$pocketPlot <- renderPlot({
      chart_data <- get_qbs()|>
        arrange(name)
      ggplot(chart_data,aes(x=name,fill=Outcome))+
        geom_bar()+
        scale_fill_manual(values=c("gold","green4","mediumseagreen","palegreen","red3","darkorange1"))+
        theme_fivethirtyeight()+
        labs(title="QB Outcomes When Outside the Pocket",
             subtitle = "Selected Quarterbacks in 2023 | Data From the nflverse",
             caption = "By Aariv Iyengar | @AarivAnalytics")
      
    },height = 900, width = 1260
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
