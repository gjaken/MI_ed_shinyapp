library(shiny)
library(ggplot2)
library(data.table)

shinyUI(pageWithSidebar(
  
    # Application title
    headerPanel("Michigan Educational Financial Data"),
    
    # Sidebar with a select box input for year
        sidebarPanel(
#             sliderInput("range",
#                         "Select Range for the Map",
#                         min = 7000,
#                         max = 50000,
#                         value = c(7000,10000)
#                         ), # Need to have this dynamically read from dt, or else scale all to 100
             
            uiOutput("outputSlider"), #<-- in ui.R sidebarPanel()
            
            selectInput("fldnm",
                        "Select Financial Measure",
                        choices = c("Expenditure per Pupil"    = "EXP.PER.PUPIL.COUNTY",
                                    "Revenue per Pupil"        = "REV.PER.PUPIL.COUNTY",
                                    "Teacher Salary per Pupil" = "TCHR_SA.PER.PUPIL.COUNTY",
                                    "Student/Teacher Ratio"    = "PUPIL.PER.TCHR.COUNTY"),
                        selected = "EXP.PER.PUPIL.COUNTY"
                        ) # input for dataset choice 
            
            
#         selectInput("year", 
#                     "Select year:", 
#                     choices = 2004:2012)
        ),
        
    
    mainPanel(
#         h3("Plots and Tables"),
#         
#         plotOutput("histPlot1"),
#         verbatimTextOutput("summary1"),
#         
#         tableOutput("testTable")
        
        h3("Michigan Education: Financial Data Choropleth"),
        
        plotOutput("MIcounty.facet.map", height="600px")
        
    )   
))
