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
exported_data<-write.table(Fatalities_clean, file="Fatalities_clean",sep=",",row.names=F)

test_dataset_fatalities <- read.csv("Fatalities_clean.csv", stringsAsFactors = FALSE)


ui <- fluidPage()
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

      
      
      
      
      
      
  


