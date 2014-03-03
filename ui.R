library(shiny)
library(ggplot2)
library(data.table)

shinyUI(pageWithSidebar(
  
    # Application title
    headerPanel("Michigan Educational Financial Data"),
    
    # Sidebar with a select box input for year
    sidebarPanel( 
        uiOutput("outputSlider"), 
        
        selectInput("fldnm", "Select Financial Measure",
                    choices = c("Average Teacher Salary"   = "TCHR_SAL.AVG.COUNTY",
                                "Expenditure per Pupil"    = "EXP.PER.PUPIL.COUNTY",
                                "Revenue per Pupil"        = "REV.PER.PUPIL.COUNTY",                                    
                                "Student/Teacher Ratio"    = "PUPIL.PER.TCHR.COUNTY"),
                    selected = "EXP.PER.PUPIL.COUNTY"
                    ), # input for dataset choice 
        
        
#         checkboxGroupInput("year", 
#                     "Select year:", 
#                     choices = 2004:2012,
#                     selected = 2004:2012),

        selectInput("year", "Select year for county comparison",
                    choices = 2004:2012,
                    selected = 2012
                    ),

        uiOutput("outputSelecter.County1"), # county1 selection menu. "ALCONA" is default.

        downloadButton("download.1014", label = "Download Bulletin 1014 Dataset (2004-2012)")
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
            tabPanel("State Summary",
                     h3("Statewide Education Finances"),
                     plotOutput("stateTotals.plot", height="700px", width="600px"),
                     tableOutput("stateTotals.dt")),
            tabPanel("County Summary", # also, a Histogram, faceted on year
                     h3("County Education Finance Choropleths"),
                     plotOutput("MIcounty.facet.map", height="800px")),   
            tabPanel("County Comparison",
                     h3(textOutput("year.header")),                     
                     tableOutput("county.comp.table")), # table of county totals comparison, and plot; 2 selectors
            tabPanel("Explore Bulletin1014",
                     h3("Bulletin 1014 (2004-2012), not adjusted for inflation."),
                     dataTableOutput("bulletin1014.full.dt")
                     )
            )
        
        
    )   
))
