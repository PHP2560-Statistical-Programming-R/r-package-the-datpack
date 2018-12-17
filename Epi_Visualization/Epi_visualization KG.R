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
library(shinythemes)
library(httr)


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

#Building the App
ui <-fluidPage(
  theme = shinytheme("superhero"),          
  # App title: Epi Visualization
  headerPanel("Epi Visualization"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    helpText("Create demographic maps and plot with information dataset 
             of choice or from the test dataset: The US Traffic Fatalities 
             Panel Dataset"),
    
    fileInput("file", "Upload csv data-file:"),
    
    #Using action buttoms to load sample dataset
    #change the color of the buttom to contrast with previous blank
    actionButton("myLoader", "Load test dataset",  
                 style="color: #fff; background-color: #337ab7; 
                 border-color: #2e6da4"),
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
    h5("Welcome to Epi Visualization! This app provides tools to help visualize 
       epidemiologic data. The graph functions allow users to plot and visualize 
       their data in many ways. The mapping functions allow users to visualize the 
       prevalence of their variable of interest at the national level."), 
    tags$head(
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 14px} ")), 
    tabsetPanel(type = "tabs", 
                tabPanel("View the Data",
                         tableOutput(outputId = "view")), 
                tabPanel("Summary Statistics", 
                         tableOutput(outputId = "summary")),
                tabPanel("BarPlot", 
                         plotOutput(outputId = "Barplot", height = "580px"),
                         plotOutput(outputId = "Stacked", height = "580px"),
                         plotOutput(outputId = "Grouped", height = "580px"),
                         textInput("text_summary", label = "Interpretation", value = "Enter text...")), 
                tabPanel("Boxplot",  
                         plotOutput(outputId = "Boxplot", height = "580px"),
                         plotOutput(outputId = "Dot", height = "580px"),
                         textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                tabPanel("Histogram",
                         plotOutput(outputId = "Histogram", height = "580px"),
                         plotOutput(outputId = "Density", height = "580px")),
                tabPanel("Scatterplot",
                         plotOutput(outputId = "Scatter", height = "580px"),
                         plotOutput(outputId = "Scatter_line", height = "580px")),
                tabPanel("Linear Regression",
                         plotOutput(outputId = "Linear", height = "580px")),
                tabPanel("Help",  htmlOutput("inc"))
    )
))


#action buttom instead of reactive

#View Data, Summary Stats, Barplot (stacked and grouped), Boxplot (box, dot box), Histogram (density plot), 
#Scatterplot (scatter, scatter with line), linear regression 

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
      textInput("datasetame", "Name of dataset", value = "")
    }
    else if (is.null(input$file)==FALSE){
      textInput("datasetame", "Name of dataset", value = "Enter text...")
    }
})

  #Exploratory Data Analysis 
  output$view <- renderTable({
    dataset <- Dataset()
    head(dataset,n=10)
  })

  #Summary Stats
  output$summary <- renderTable({
    dataset<- Dataset()
    apply(dataset, 2, mean)
    apply(dataset, 2, sd)
    apply(dataset, 2, median)
    apply(dataset, 2, min)
    apply(dataset, 2, max)
    apply(dataset, 2, length)
    summary(dataset)
  })
  
  output$Barplot <- renderPlot({
    ggplot(data=Dataset, aes(x=x, fill=x)) + 
      geom_bar( ) +
      scale_fill_brewer(palette = "Paired")+
      labs(title="title", x="xlab", y="ylab")
  })
  
  output$Stacked <- renderPlot({
    ggplot(data=Dataset, aes(fill=fill, y=y, x=x)) +
      geom_bar( stat="identity")
  })
  
  output$Grouped <-renderPlot({
    ggplot(data=Dataset, aes(x=x, y=y, fill=fill)) +
      geom_bar(position="dodge", stat="identity") + 
      scale_fill_brewer(palette = "Paired")+ theme_bw()+ facet_wrap(~"fill")
  })

  output$Boxplot <-renderPlot({
    boxplot(y~x, data=data, notch=TRUE,
            main="title", xlab="xlab", ylab="ylab")
  })
}

shinyApp(ui, server)

#View Data, Summary Stats, Barplot (stacked and grouped), Boxplot (box, dot box), Histogram (density plot), 
#Scatterplot (scatter, scatter with line), linear regression 


