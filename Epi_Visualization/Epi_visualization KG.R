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
library(leaflet)


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
        tabPanel("Exploratory Data Analysis", plotOutput("ScatterMatrix", width = "100%", height = "580px"),
              textInput("text_scatt", label = "Interpretation", value = "Enter text...")), 
        tabPanel("Basic Plots", plotOutput("BoxPlot", height = "580px"),
              textInput("text_box", label = "Interpretation", value = "Enter text...")),
        tabPanel("Epi Tools", br(),verbatimTextOutput("lmResults"),
              textInput("text_summary", label = "Interpretation", value = "Enter text...")), 
        tabPanel("National Map",  plotOutput("diagnostics", height = "580px"),
             textInput("text_diagno", label = "Interpretation", value = "Enter text...")),
        tabPanel("Help",  htmlOutput("inc"))
    ),
  tableOutput(outputId = "Exploratory_Data_Analysis"),
  plotOutput(outputId = "Basic_Plots"),
  tableOutput(outputId = "Epi_Tools"),
  plotOutput(outputId = "National_Map")
    )
  )

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
#  output$Exploratory_Data_Analysis <- renderTable({
#    dataset <- Dataset()
#    summary(dataset)
#  })
#  output$view <- renderTable({
#    head(Dataset(), n = 10)
# })
  
  #Basic Plots
  output$Basic_Plots<- renderPlot(function(Dataset, x, y, graph, fill, title, xlab, ylab, legend){    ###add argument for error bars
    Dataset[complete.cases(Dataset), ]
    if(graph == "bar"){                           #boxplot function
      pic<-  ggplot(data=Dataset, aes(x=x, fill=x)) + 
        geom_bar( ) +
        scale_fill_brewer(palette = "Paired")+
        labs(title="title", x="xlab", y="ylab")
      #table1 = table(data$x)  ## get the cross tab
      #pic<-barplot(table1, beside = TRUE, legend = levels(data$x), col=c("lightblue","darkblue"),main="title", xlab="xlab", ylab = "ylab")  
      return(pic)
      #barplot(table(data$x), col=c("lightblue","darkblue"),main="title", xlab="xlab", ylab = "ylab")
    } else if(graph=="bargroup"){                                           
      pic<-ggplot(data=Dataset, aes(x=x, y=y, fill=fill)) +
        geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette = "Paired")+theme_bw()+facet_wrap(~"fill")
      return(pic)
    } else if(graph=="barstack"){                                           
      pic<-ggplot(data=Dataset, aes(fill=fill, y=y, x=x)) +
        geom_bar( stat="identity")
      return(pic)
    } else if(graph=="boxplot"){                                                             
      pic<-boxplot(y~x, data=Dataset, notch=TRUE,
                   main="title", xlab="xlab", ylab="ylab")
      return(pic)
    } else if(graph=="dotboxplot"){                                         
      pic<-plot_ly(y = ~y, type = "box", boxpoints = "all", jitter = 0.3,pointpos = -1.8) 
      return(pic)
    } else if(graph=="hist"){                                               
      pic<-ggplot(data=Dataset, aes(x)) +                    
        geom_histogram(col="black", aes(fill=..count..)) +
        scale_fill_gradient("Count", low="light blue", high="navy")+
        labs(title="title", x="xlab", y="ylab")
      return(pic)
    } else if(graph=="densityhist"){                                                           
      pic<-ggplot(data=Dataset, aes(x)) + 
        geom_histogram(aes(y =..density..),col="blue", fill="light blue", alpha=.5) + 
        geom_density(col=2) + 
        labs(title="title", x="xlab", y="ylab")
      return(pic)
    } else if(graph=="scatter"){                                           
      pic<-ggplot(Dataset, aes(x, y, color = fill)) +
        geom_point(shape = 16, size = 5, show.legend = TRUE) +
        theme_minimal() +
        #scale_color_gradient(color = "Blues")+
        labs(title="title", x="xlab", y="ylab", color = "legend")
      return(pic)
    } else if(graph=="scatterline"){                                      
      pic<-ggplot(Dataset, aes(x, y, color = fill)) +
        geom_point(shape = 16, size = 5, show.legend = TRUE) +
        theme_minimal() +
        scale_color_gradient(low = "light blue", high = "dark blue")+
        labs(title="title", x="xlab", y="ylab", color = "legend")+geom_smooth()
      return(pic)
    } else if(graph=="linreg"){                                             
      pic<-ggplot(Dataset, aes(x, y, color = fill)) +
        geom_point(shape = 16, size = 5, show.legend = TRUE) +
        theme_minimal() +
        scale_color_gradient(low = "light blue", high = "dark blue")+
        labs(title="title", x="xlab", y="ylab", color = "legend")+ geom_smooth(method = 'lm', se = TRUE)
      return(pic)
    } 
  })
  
  #Epi Tools
  output$Epi_Tools <-renderTable(
    epi.2by2(Dataset, method = "cohort.count", conf.level = 0.95, units = 100, 
             homogeneity = "breslow.day", outcome = "as.columns")
  )
  
  # National Map
  output$National_Map<- renderPlot(function(Dataset,existing_cases,population,state,year) {
    us_map <- map_data("state")
    Dataset %>%
      group_by(!! sym(state), !! sym(year)) %>%
      mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%
      #left_join(state_abbs, by = c("state" = "abb")) %>%
     right_join(us_map, by = c("state_full" = "region")) %>%
      ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
      geom_polygon(color = "white") + ggtitle("National Prevalence of Car Fatalities 1982-1988") +
      theme_void() + 
      scale_fill_viridis(name = "Prevalence")
   })
}

shinyApp(ui, server)

#Things to Figure out:
#  1. Upload dataset
#  2. Getting Functions to work 


