#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)

# Set your working directory
setwd("C:/Users/rsmar/OneDrive/Documents/Spring 2019/RFolder/Ag_Forest_Data_Final-")

#### Load Data
WB_Data <- read_csv("./Processed/WB_Fixed.csv")

#### Define UI ----
ui <- fluidPage(theme = shinytheme("yeti"),
                titlePanel("Land Cover Tradeoffs"),
                sidebarLayout(
                  sidebarPanel(

      # Select country to plot
      selectInput(inputId = "color", 
                      label = "Country",
                      choices = c("Afghanistan", "Argentina", "Bangledesh", 
                                  "Belgium", "Brazil", "Cameroon", "Canada", 
                                  "India", "Indonesia", "Iran", "Kenya", "Mexico", 
                                  "Nicaragua", "Norway", "Pakistan", "Spain", 
                                  "South Africa"), 
                      selected = "Brazil"),
                    
      # Select Land Use
      checkboxGroupInput(inputId = "y",
                         label = "Land Use",
                         choices = c("Agriculture", "Forest"),
                         selected = "Agriculture", "Forest"),
      
      # Select date range 
      sliderInput(inputId = "x",
                  label = "Date",
                  min = as.Date("1960-04-09"),
                  max = as.Date("2016-04-09"),
                  value = c(as.Date("1992-04-09"), as.Date("2014-04-09")))),
      
    #Output: Description, lineplot, and reference 
    mainPanel(
      plotOutput("lineChart", brush = brushOpts(id = "lineChart_brush")), 
      tableOutput("mytable")
    )))
      

#### Define server ----
server <- function(input,ouput) {
  
    #Define reactive formatting for filtering within columns
    filtered_country_data <- reactive({
      WB_Data %>%
        filter(Year >= input$x[1] & Year <= input$x[2]) %>%
        filter(Country %in% input$fill)
    })
     
    #Create a ggplot for the type of plot              
    output$lineChart <- renderPlot({
      ggplot(filtered_country_data(), 
             aes_string(x = "Year", y = input$y, 
                        color = "Country")) +
        geom_point(alpha = 0.8, size = 2) +
        theme_classic(base_size = 14) +
        labs(x = "Date", y = expression("Percentage Land Use"), color = "Country") +
        scale_fill_distiller(palette = "YlOrBr", guide = "colorbar", direction = 1)
    })
    
}
    

#### Run the application ----
shinyApp(ui = ui, server = server)

