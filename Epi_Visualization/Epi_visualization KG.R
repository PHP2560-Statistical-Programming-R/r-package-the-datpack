#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


#Making Cleaned dataset into a CSV file
exported_data<-write.table(Fatalities_clean, file="Fatalities_clean.csv",sep=",",row.names=F)

test_dataset_fatalities <- read.csv("Fatalities_clean.csv", stringsAsFactors = FALSE)

ui <- pageWithSidebar(
  
  # App title: Epi Visualization
  headerPanel("Epi Visualization"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
  
  # Input: Selector for variable to plot against mpg ----
  selectInput("variable", "Variable:", 
              c("Fatalities" = "Fatal",
                "State" = "state_full",
                "Income" = "income",
                "Year" = "year")),
  sliderInput(inputId = "year",
              label = "Years:",
              min = 1982,
              max = 1988,
              value = 1),
  
  # Input: Checkbox for whether outliers should be included ----
  checkboxInput("outliers", "Show outliers", TRUE)
  
),
  
  # Main panel for displaying outputs ----
  mainPanel()
)

dataset<-Fatalities_clean

server <- function(input, output) {

}
  

shinyApp(ui, server)





#ui <- fluidPage() (basic structure of the app)
#server <- function(input, output) {}
#shinyApp(ui = ui, server = server)

      
      
      
      
      
      
  


