#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(AER)


#Making Cleaned dataset into a CSV file
#exported_data<-write.table(Fatalities_clean, file="Fatalities_clean.csv",sep=",",row.names=F)

#test_dataset_fatalities <- read.csv("Fatalities_clean.csv", stringsAsFactors = FALSE)

ui <- pageWithSidebar(
  
  # App title: Epi Visualization
  headerPanel("Epi Visualization"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    helpText("Create demographic maps and plot with 
             information from the US Traffic Fatalities Panel Data"),
    # Input: Selector for variable to plot against mpg ----
    selectInput("variable", 
                label = "Choose a variable to display",
                choices = c("Fatalities" = "Fatal",
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
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    
    #add block between each part
    hr(),
    
    # Variable selection:
    #Independent Numeric variable selection:
    htmlOutput("varselect_num"),
    
    #Independent Categorical variable selection:
    htmlOutput("varselect_cat"),
    
    #Dependent variable selection:
    htmlOutput("outcomeselect"),
    
    #Because next part is the download file part, so we add a line to block between variable selection and 
    #file download
    
    hr(),
    
    #Name of dataset
    
    htmlOutput("datasetnameout"),
    
    #Name on report
    textInput("name", "Author name", value = "Name"),
    
    #Radio buttons for choosing format
    radioButtons('format', "Document format", c('PDF', 'HTML', 'Word'), inline = TRUE),
    
    #Download button
    downloadButton('downloadReport')
    , width=3),
    
  
  
  # Main panel for displaying outputs ----
  mainPanel(
    h4("This app provides tools to help visualize epidemiologic data"),
    h5("The mapping functions allow users to visualize the prevalence of their variable of interest at the national and regional levels. 
       The graph functions allow users to plot and visualize their data in many ways?? Mention features."), 
    tags$head(
         tags$style(type='text/css', 
                    ".nav-tabs {font-size: 14px} ")), 
    tabsetPanel(type = "tabs", 
                tabPanel("Scatterplots", plotOutput("ScatterMatrix", width = "100%", height = "580px"),
                         textInput("text_scatt", label = "Interpretation", value = "Enter text...")), 
                tabPanel("Boxplots", plotOutput("BoxPlot", height = "580px"),
                         textInput("text_box", label = "Interpretation", value = "Enter text...")),
                tabPanel("Summary statistics", br(),verbatimTextOutput("lmResults"),
                         textInput("text_summary", label = "Interpretation", value = "Enter text...")), 
                tabPanel("Diagnostic plots",  plotOutput("diagnostics", height = "580px"),
                         textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                tabPanel("Added variable plots",  plotOutput("added", height = "580px"),
                         textInput("text_added", label = "Interpretation", value = "Enter text...")),
                tabPanel("Marginal model plots",  plotOutput("MMPlot", height = "580px"),
                         textInput("text_mmp", label = "Interpretation", value = "Enter text...")),
                tabPanel("Help",  htmlOutput("inc"))
                )
    )
  )
library(shiny)
library(ggplot2)
library(readr)
library(MASS)
library(epicalc)
library(maps)
library(usmap)
library(ggplot2)
library(dplyr)
library(httr)
library(tidyverse)
library(rprev)

#Load sample dataset available in the AER package
library(AER)
data(Fatalities)

us_map <- map_data("state")

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

server <- function(input, output) {
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
      return(Fatalities_clean)
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
  
  
  # Select variables part 1:
  output$varselect_num <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Independent Numeric Variable selection:    
    selectInput("varnum", "Explantory Variables (Numeric):",
                names(Dataset()), multiple =TRUE)
  })
  
  # Select variables part 2:
  output$varselect_cat <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Independent Categorical Variable selection:    
    selectInput("varcat", "Explantory Variables (Categorical):",
                names(Dataset()), multiple =TRUE)
  })
  
  # Select variables part 3:
  output$outcomeselect <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Dependent Variable selection:
    selectInput("outcome","Outcome Variable:",
                names(Dataset()), names(Dataset()))
  })
  
  # Dataset Name:
  output$datasetnameout <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    if (input$myLoader && is.null(input$file)){
      textInput("datasetame", "Name of dataset", value = "birthwt")
    }
    else if (is.null(input$file)==FALSE){
      textInput("datasetame", "Name of dataset", value = "Enter text...")
    }
  })
  
  
}


shinyApp(ui, server)





#ui <- fluidPage() (basic structure of the app)
#server <- function(input, output) {}
#shinyApp(ui = ui, server = server)


