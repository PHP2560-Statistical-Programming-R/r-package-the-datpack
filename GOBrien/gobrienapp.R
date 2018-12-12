#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
# Getting the data
#exported_data<-write.table(Fatalities_clean, file="Fatalities_clean",sep=",",row.names=F)

#test_dataset_fatalities <- read.csv("Fatalities_clean.csv", stringsAsFactors = FALSE)
# po

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  
  # App title: Epi Visualization
  headerPanel("Epi Visualization"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Selector for variable to plot against mpg ----
    selectInput("variable", "Variable 1:", 
                c("Fatalities" = "Fatal",
                  "State" = "state_full",
                  "Income" = "income",
                  "Year" = "year")),
    selectInput("variable", "Variable 2:", 
                c("None" = "NA", "Fatalities" = "Fatal",
                  "State" = "state_full",
                  "Income" = "income",
                  "Year" = "year")),
    selectInput("variable", "Variable 3:", 
                c("None" = "NA","Fatalities" = "Fatal",
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
  mainPanel(
    h4("This app provides tools to help visualize epidemiologic data"),
    h5("The mapping functions allow users to visualize the prevalence of their variable of interest at the national and regional levels. 
       The graph functions allow users to plot and visualize their data in many ways?? Mention features.")
    )
  )
server <- function(input, output) {
  
}


shinyApp(ui, server)


