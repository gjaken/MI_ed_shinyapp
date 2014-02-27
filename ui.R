library(shiny)
library(ggplot2)
library(data.table)

shinyUI(pageWithSidebar(
  
    # Application title
    headerPanel("Michigan Educational Financial Data"),
    
    # Sidebar with a select box input for year
    sidebarPanel( 
        uiOutput("outputSlider"), #<-- in ui.R sidebarPanel()
        
        selectInput("fldnm",
                    "Select Financial Measure",
                    choices = c("Average Teacher Salary"   = "TCHR_SAL.AVG.COUNTY",
                                "Expenditure per Pupil"    = "EXP.PER.PUPIL.COUNTY",
                                "Revenue per Pupil"        = "REV.PER.PUPIL.COUNTY",                                    
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
        
#         h3("Michigan Education: Financial Data "),
        
        tabsetPanel(
#             tabPanel("State Summary", 
#                      tableOutput("stateTotals.dt"),
#                      plotOutput("stateTotals.plot")),
            tabPanel("County Summary", # also, a Histogram, faceted on year
                     plotOutput("MIcounty.facet.map", height="600px")),
            tabPanel("County Summary by Year" # Choropleth and Histogram, by single year
                     ),            
            tabPanel("County Drilldown Comparison" #, table of county totals comparison, and plot; 2 selectors
                     )
            )
        
        
    )   
))
