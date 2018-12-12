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
    helpText("Create demographic maps and plot with 
<<<<<<< HEAD
             information from the US Traffic Fatalities Panel Data"),
    # Input: Selector for variable to plot against mpg ----
    selectInput("variable 1", 
                label = "Choose a variable to display",
                choices = c("Fatalities" = "Fatal",
                            "State" = "state_full",
                            "Income" = "income",
                            "Year" = "year")),
    selectInput("variable 1", "Choose a variable to display", 
                choices = c("None" = "NA", "Fatalities" = "Fatal",
                            "State" = "state_full",
                            "Income" = "income",
                            "Year" = "year")),
    selectInput("variable 3", "Choose a variable to display", 
                choices = c("None" = "NA","Fatalities" = "Fatal",
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
  
=======
               information from the US Traffic Fatalities Panel Data"),
  # Input: Selector for variable to plot against mpg ----
  selectInput("variable 1", 
              label = "Choose a variable to display",
              choices = c("Fatalities" = "Fatal",
                  "State" = "state_full",
                 "Income" = "income",
                  "Year" = "year")),
  selectInput("variable 1", "Choose a variable to display", 
              choices = c("None" = "NA", "Fatalities" = "Fatal",
                "State" = "state_full",
                "Income" = "income",
                "Year" = "year")),
  selectInput("variable 3", "Choose a variable to display", 
              choices = c("None" = "NA","Fatalities" = "Fatal",
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

>>>>>>> b0de55c7219ee5f59a2ce1b371780bbf4f62409e
  # Main panel for displaying outputs ----
mainPanel(
    h4("This app provides tools to help visualize epidemiologic data"),
    h5("The mapping functions allow users to visualize the prevalence of their variable of 
<<<<<<< HEAD
       interest at the national and regional levels. The graph functions allow users to plot 
       and visualize their data in many ways?? Mention features."),
    tags$head(
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 14px} ")), 
    tabsetPanel(type = "tabs", 
                tabPanel("Exploratory Data Analysis", plotOutput("ScatterMatrix", width = "100%", 
                                                                 height = "580px"),
                         textInput("text_scatt", label = "Interpretation", value = "Enter text...")), 
                tabPanel("Basic Graphing", plotOutput("BoxPlot", height = "580px"),
                         textInput("text_box", label = "Interpretation", value = "Enter text...")),
                tabPanel("Epi Tools", br(),verbatimTextOutput("lmResults"),
                         textInput("text_summary", label = "Interpretation", value = "Enter text...")),
                tabPanel("National Map",  plotOutput("added", height = "580px"),
                         textInput("text_added", label = "Interpretation", value = "Enter text...")),
                tabPanel("Regional Maps",  plotOutput("MMPlot", height = "580px"),
                         textInput("text_mmp", label = "Interpretation", value = "Enter text...")),
                tabPanel("Help",  htmlOutput("inc"))
    )
    )
)

server <- function(input, output) {
=======
        interest at the national and regional levels. The graph functions allow users to plot 
        and visualize their data in many ways?? Mention features."),
tags$head(
    tags$style(type='text/css', 
                 ".nav-tabs {font-size: 14px} ")), 
    tabsetPanel(type = "tabs", 
        tabPanel("Exploratory Data Analysis", plotOutput("ScatterMatrix", width = "100%", 
                                                         height = "580px"),
              textInput("text_scatt", label = "Interpretation", value = "Enter text...")), 
        tabPanel("Basic Graphing", plotOutput("BoxPlot", height = "580px"),
              textInput("text_box", label = "Interpretation", value = "Enter text...")),
        tabPanel("Epi Tools", br(),verbatimTextOutput("lmResults"),
              textInput("text_summary", label = "Interpretation", value = "Enter text...")),
        tabPanel("National Map",  plotOutput("added", height = "580px"),
              textInput("text_added", label = "Interpretation", value = "Enter text...")),
        tabPanel("Regional Maps",  plotOutput("MMPlot", height = "580px"),
              textInput("text_mmp", label = "Interpretation", value = "Enter text...")),
        tabPanel("Help",  htmlOutput("inc"))
    )
  )
  )

server <- function(input, output) {

}
>>>>>>> b0de55c7219ee5f59a2ce1b371780bbf4f62409e
  
}


shinyApp(ui, server)

#Things to Figure out:
#  1. Upload dataset
#  2. What tabs to include
#  3. Functions
<<<<<<< HEAD







=======
>>>>>>> b0de55c7219ee5f59a2ce1b371780bbf4f62409e



