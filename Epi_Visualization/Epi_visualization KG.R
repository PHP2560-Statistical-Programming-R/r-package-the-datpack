#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(epiR)
library(epitools)
library(incidence)
library(AER)
library(epiDisplay)
library(devtools)
library(roxygen2)
library(epicalc)
library(maps)
library(usmap)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rprev)
library(ggmap)
library(mapdata)
library(viridis)



#Making Cleaned dataset into a CSV file
exported_data<-write.table(Fatalities_clean, file="Fatalities_clean.csv",sep=",",row.names=F)

#Cleaning the Fatalities Dataset
tbl <- state.x77 %>%
  as_tibble(rownames = "state") %>%
  bind_cols(state_name = str_to_lower(state.abb)) %>%
  rename(value_x = Income) %>%
  select(state_name, value_x)

state_abbs <- tibble(state_full = str_to_lower(state.name), abb = str_to_lower(state.abb))
tbl_m <- left_join(tbl, state_abbs, by = c("state_name" = "abb")) %>%
  rename(id = state_full)

Fatalities_clean <- Fatalities %>%
  left_join(state_abbs, by = c("state" = "abb"))


ui <- pageWithSidebar(
  
  # App title: Epi Visualization
  headerPanel("Epi Visualization"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    helpText("Create demographic maps and plot with 
             information from the US Traffic Fatalities Panel Data"),
    # Input: Selector for variable to plot against mpg ----
    selectInput("variable 1", 
                label = "Choose a variable to display",
                choices = c("Fatalities" = "Fatal",
                            "State" = "state_full",
                            "Income" = "income",
                            "Year" = "year")),
    selectInput("variable 2", 
                label = "Choose a variable to display",
                choices = c("None" = "None",
                            "Fatalities" = "Fatal",
                            "State" = "state_full",
                            "Income" = "income",
                            "Year" = "year")),
    selectInput("variable 3", 
                label = "Choose a variable to display",
                choices = c("None" = "None",
                            "Fatalities" = "Fatal",
                            "State" = "state_full",
                            "Income" = "income",
                            "Year" = "year")),
    sliderInput(inputId = "year",
                label = "Years:",
                min = 1982,
                max = 1988,
                value = 1),
    
    # Input: Checkbox for whether outliers should be included ----
    checkboxInput("outliers", "Show outliers", TRUE),
    
    fileInput("file", "Upload csv data-file:"),
    
    #Using action buttoms to load sample dataset
    #change the color of the buttom to contrast with previous blank
    actionButton("myLoader", "Load test dataset",  
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
  
  # Main panel for displaying outputs ----
mainPanel(
    h4("This app provides tools to help visualize epidemiologic data"),
    h5("The mapping functions allow users to visualize the prevalence of their variable 
        of interest at the national and regional levels. The graph functions allow users 
        to plot and visualize their data in many ways?? Mention features."), 
  tags$head(
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 14px} ")), 
  tabsetPanel(type = "tabs", 
        tabPanel("Exploratory Data Analysis", plotOutput("ScatterMatrix", width = "100%", height = "580px"),
              textInput("text_scatt", label = "Interpretation", value = "Enter text...")), 
        tabPanel("Basic Plots", plotOutput("BoxPlot", height = "580px"),
              textInput("text_box", label = "Interpretation", value = "Enter text...")),
        tabPanel("Epi Tools", br(),verbatimTextOutput("lmResults"),
              textInput("text_summary", label = "Interpretation", value = "Enter text...")), 
        tabPanel("National Map",  plotOutput("diagnostics", height = "580px"),
              textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
        tabPanel("Regional Maps",  plotOutput("added", height = "580px"),
              textInput("text_added", label = "Interpretation", value = "Enter text...")),
        tabPanel("Help",  htmlOutput("inc"))
    )
    )
  )

server <- function(input, output) {
  ##Argument names:
  #Using the header names from the data 
  ArgNames <- reactive({
    Names <- names(formals("read.csv")[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  ### Data import:
  Dataset <- reactive({
    
    # User has not uploaded a file yet
    if (is.null(input$file) && input$myLoader==0) {
      return(data.frame())
    }
    
    #loading test dataset
    if (input$myLoader && is.null(input$file)){
      return("Fatalities_clean.csv")
    }
    
    #loading csv. when data has been uploaded
    
    args <- grep(paste0("^","read.csv","__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^","read.csv","__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call("read.csv",c(list(input$file$datapath),argList)))
    return(Dataset)
  })
}


shinyApp(ui, server)

#Things to Figure out:
#  1. Upload dataset
#  2. What tabs to include



