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
library(AER)
library(devtools)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(httr)
library(pastecs)
library(rmarkdown)


#test_dataset_fatalities <- read.csv("Fatalities_clean.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
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
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    
    #add block between each part
    hr(),
    
    # Variable selection:
    #Variable selection for x:
    htmlOutput("xvar"),
    
    #Variable Selection for y:
    htmlOutput("yvar"),
    
    #Fill variable selection:
    htmlOutput("fillvar"),
    
    #Title for generated plot
    htmlOutput("title"),
    
    #Label of x-axis
    htmlOutput("xlab"),
    
    #Label of y-axis
    htmlOutput("ylab"),
    
    #Because next part is the download file part, so we add a line to block between variable selection and 
    #file download
    
    hr(),
    
    #Name of dataset
    
    htmlOutput("datasetnameout"),
    
    #Name on report
    textInput("name", "Author name", value = "Name"),
    
    #Radio buttons for choosing format
    radioButtons('format', "Document format", c('HTML'), inline = TRUE),
    
    #Download button
    downloadButton('downloadReport')
    , width=3),
  
  
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
                tabPanel("Scatterplot",
                         plotOutput(outputId = "Scatter", height = "580px"),
                         textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                tabPanel("Scatter Line",
                         plotOutput(outputId = "Scatter_line", height = "580px"),
                         textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                tabPanel("Barplot",
                        plotOutput(outputId = "Barplot", height = "580px"),
                        textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                tabPanel("Boxplot",  
                         plotOutput(outputId = "Boxplot", height = "580px"),
                         textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                tabPanel("Histogram",
                         plotOutput(outputId = "Histogram", height = "580px"),
                         textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                tabPanel("Density Plot",
                         plotOutput(outputId = "Density", height = "580px"),
                         textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                tabPanel("Linear Regression",
                         plotOutput(outputId = "Linear", height = "580px"),
                         textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
                tabPanel("Help",  p(tags$h2("Guide to Epi Visualization"), 
                                    tags$br(), 
                                    tags$b("Follow these instructions to evaluate and visualize your data using our shiny app. 
                                           Below are specific instructions for using each of the visualization tabs. Choose to use the pre-loaded Fatalities data or choose to upload your own data in csv format. 
                                           Download the plots generated from this shiny app as a pdf, html, or word document. Enjoy!"),
                                    tags$h3("View the Data:"), "This displays the first 10 observations in your data. Explore the data and assess type of variables that are in your data.",
                                    tags$br(), 
                                    tags$h3("Summary Statistics:"), "Explore the data by selecting a single variable to get summmary statistics of it or selecting two varibles to assess the two variables' associations.",
                                    tags$br(), 
                                    tags$h3("Scatterplot:"), "Select x and y variables",
                                    tags$br(), 
                                    tags$h3("Scatter Line:"), "Select x and y variables",
                                    tags$br(), 
                                    tags$h3("Boxplot:"), "Select x and y variables",
                                    tags$br(),
                                    tags$h3("Histogram:"), "Select x and y variables",
                                    tags$br(), 
                                    tags$h3("Linear Regression:"), "Select x and y variables",
                                    tags$br(),
                                    tags$h3("Additional links to guide you through Epi Visualization")
                ))
    )
    ))


###setup the server and loaded data


#Load sample dataset available in the AER package
#clean data for loading
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

server <- (function(input, output) {
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
  output$xvar <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Independent Numeric Variable selection:    
    selectInput("xvar", "X Variable",
                names(Dataset()), names(Dataset()))
  })
  
  # Select variables part 2:
  output$yvar <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Independent Categorical Variable selection:    
    selectInput("yvar", "Y Variable",
                names(Dataset()), names(Dataset()))
  })
  
  # Select variables part 3:
  output$fillvar <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Dependent Variable selection:
    selectInput("fillvar","Fill Variable:",
                names(Dataset()), names(Dataset()))
  })
  
  #Title Name
  output$title <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    #Input title name
    textInput("title", "Title of Graph", value = "Enter text...")
  })
  
  
  #X-axis label
  output$xlab <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    #Input label name
    textInput("xlab", "Label of X-axis", value = "Enter text...")
  })
  
  #Y-axis label
  output$ylab <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    #Input label name
    textInput("ylab", "Label of Y-axis", value = "Enter text...")
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
    dataset <- Dataset()
    stat.desc(dataset, basic=F)
  })
  
  #Scatter Plot
  output$Scatter <- renderPlot({
    if (is.null(input$xvar)) return(NULL)
    else if (length(input$yvar)==1){
      plot(as.formula(paste(input$yvar,"~",input$xvar)),data=Dataset(),xlab=input$xlab,ylab=input$ylab,main=input$title)
    }
    else if (length(input$varnum)>1){
      pairs(as.formula(paste("~",paste(c(input$xvar,input$yvar),collapse="+"))),data=Dataset())
    }
  })
  
  #Scatter Line
  output$Scatter_line <- renderPlot({
    if (is.null(input$xvar)) return(NULL)
    else if (length(input$xvar)==1){
      plot(as.formula(paste(input$yvar,"~",input$xvar)),data=Dataset(),type="b",xlab=input$xlab,ylab=input$ylab,main=input$title)
    }
    else if (length(input$xvar)>1){
      pairs(as.formula(paste("~",paste(c(input$yvar,input$xvar),collapse="+"))),data=Dataset())
    }
  })
  
  #Barplot
  output$Barplot<-renderPlot({
  ggplot(data=Dataset(), aes_string(x=input$xvar, y=input$yvar)) + 
      geom_bar(stat="identity", fill="Dark Blue")+ 
    labs(title=input$title, x=input$xlab, y=input$ylab)
  })
  
  #Boxplot
  output$Boxplot <- renderPlot({
    #plot_ly(y = ~input$yvar, type = "box", boxpoints = "all", jitter = 0.3,pointpos = -1.8) 
    ggplot(Dataset(),aes_string(x=input$xvar,y=input$yvar))+
      geom_boxplot(colour='blue',height = 400,width = 600)+
      labs(title=input$title, x=input$xlab, y=input$ylab)
  })
  
  #Histogram   
  output$Histogram <- renderPlot({
    dataset <- Dataset()
    ggplot(data=dataset, aes_string(input$xvar)) + 
      geom_histogram(aes_string(input$yvar),col="blue", fill="light blue", alpha=.5) + 
      geom_density(col=2) + theme_classic() + 
      labs(title=input$title, x=input$xlab, y=input$ylab)
  })
  
  #Density plot
  output$Density<- renderPlot({
    dataset <- Dataset()
    ggplot(data=dataset, aes_string(input$xvar)) + 
      geom_histogram(aes_string(y ="..density.."), 
                     col="blue", 
                     fill="light blue", 
                     alpha=.5) + 
      geom_density(col=2) + 
      labs(title=input$title, x=input$xlab, y=input$ylab)
  })
  
  #Linear Regression  
  output$Linear <- renderPlot({
    dataset<- Dataset()
    ggplot(dataset, aes_string(input$xvar, input$yvar, color = input$fillvar)) +
      geom_point(shape = 16, size = 5, show.legend = TRUE) +
      theme_minimal() +
      scale_color_gradient(low = "light blue", high = "dark blue")+
      labs(title=input$title, x=input$xlab, y=input$ylab, color = "legend")+ geom_smooth(method = 'lm', se = TRUE)
  })

}
)

shinyApp(ui, server)


#scatter plot: (data=data, x=data$x, y=data$y, graph="scatter",fill=data$fill, title="Title of plot", xlab="x-axis label",  ylab="y-axis label", legend="Title of legend fill")
#scatter plot with correlating line: (data=data, x=data$x, y=data$y, graph="scatterline",fill=data$fill, title="Title of plot", xlab="x-axis label",  ylab="y-axis label", legend="Title of legend fill")
#linear regression: (data=data, x=data$x, y=data$y, graph="linreg",fill=data$fill, title="Title of plot", xlab="x-axis label",  ylab="y-axis label", legend="Title of legend fill")



#ui <- fluidPage() (basic structure of the app)
#server <- function(input, output) {}
#shinyApp(ui = ui, server = server)

#output$Scatterline <- renderPlot({
#if (is.null(input$xvar)) return(NULL)
#else if (length(input$xvar)==1){
#pic<-(ggplot(data=Dataset(), aes(input$xvar, input$yvar)) +  
#                      geom_point() +
#                     labs(title="input$title", x="input$xlab", y="input$ylab")+geom_smooth())
#paste(pic)
#}
#else if (length(input$xvar)>1){
#pairs(as.formula(paste("~",paste(c(input$xvar,input$yvar),collapse="+"))),data=Dataset())
#}
#})


# output$Histogram <- renderPlot({
#  if (is.null(input$yvar)) return(NULL)
# else if (length(input$xvar)>1){
#  plot(as.formula(paste(input$yvar)),data=Dataset(),type="h",xlab=input$xlab,ylab=input$ylab,main=input$title)
#plot_ly(as.formula(paste(y = input$xvar, type = "box", boxpoints = "all", jitter = 0.3,pointpos = -1.8)))
#  }
# else if (length(input$xvar)>1){
#  pairs(as.formula(paste("~",paste(c(input$yvar,input$xvar),collapse="+"))),data=Dataset())
#}
#})

#output$Histogram <- renderPlot({
#if (is.null(input$yvar)) return(NULL)
#else if (length(input$xvar)>1){
#     gg<-hist(data=Dataset(),input$xvar,col = "#75AADB", border = "white", xlab = input$xlab, main = input$title)
#   #plot(as.formula(paste(input$yvar)),data=Dataset(),type="h",xlab=input$xlab,ylab=input$ylab,main=input$title)
#plot_ly(as.formula(paste(y = input$xvar, type = "box", boxpoints = "all", jitter = 0.3,pointpos = -1.8)))
#    return(gg)
#}
# else if (length(input$xvar)>1){
#  pairs(as.formula(paste("~",paste(c(input$yvar,input$xvar),collapse="+"))),data=Dataset())
#  }
# }) 

#output$Histogram<-renderPlot({
#bins <- seq(min(input$xvar), max(input$xvar))

# hist(data=Dataset(),input$xvar,col = "#75AADB", border = "white", xlab = input$xlab, main = input$title)
#  hist(input$xvar,xlab = input$xlab,col = "blue",border = "dark blue")
# })


#output$Barplot<-renderPlot({
#  pic1<-ggplot(data=Dataset(), aes(x=input$xvar, y=input$yvar)) + geom_bar(stat="identity")
# paste(pic1)
#})
