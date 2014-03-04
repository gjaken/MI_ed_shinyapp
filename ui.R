library(shiny)
library(ggplot2)
library(data.table)

shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Michigan Educational Financial Data"),
    
    # Sidebar with a select box input for year
    sidebarPanel( 
        
        # Tab 1: Statewide Summary
        conditionalPanel(condition = "input.tabs == 'Statewide Summary'"                         
         ),
        
        # Tab 2: County-by-County Summary
        conditionalPanel(condition = "input.tabs == 'County-by-County Summary'",
                         uiOutput("outputSlider"), 
                         
                         selectInput("fldnm", "Select Financial Measure",
                                     choices = c("Average Teacher Salary"   = "TCHR_SAL.AVG.COUNTY",
                                                 "Expenditure per Pupil"    = "EXP.PER.PUPIL.COUNTY",
                                                 "Revenue per Pupil"        = "REV.PER.PUPIL.COUNTY",                                    
                                                 "Student/Teacher Ratio"    = "PUPIL.PER.TCHR.COUNTY"),
                                     selected = "EXP.PER.PUPIL.COUNTY"
                         ) # input for dataset choice                          
        ),        
        
        # Tab 3: County Comparison
        conditionalPanel(condition = "input.tabs == 'County Comparison'",
                         selectInput("year", "Select year for county comparison",
                                     choices = 2004:2012,
                                     selected = 2012
                         ),

                         uiOutput("outputSelecter.County1"), # county1 selection menu. "ALCONA" is default.
                         uiOutput("outputSelecter.County2") # county2 selection menu. "ALCONA" is default.                         
        ),
        
        # Tab 4: Explore Bulletin1014
        conditionalPanel(condition = "input.tabs == 'Explore Bulletin1014'",
                         downloadButton("download.1014", label = "Download Bulletin 1014 Dataset (2004-2012)")                
        )
               
    ),
   
    mainPanel(    
        tabsetPanel(id = "tabs",
                    
            tabPanel("Statewide Summary",
                     h3("Statewide Education Finances, inflation-adjusted"),
                     plotOutput("stateTotals.plot", height="700px", width="600px"),
                     tableOutput("stateTotals.dt")
                     ),
            
            tabPanel("County-by-County Summary", # also, a Histogram, faceted on year
                     h3("County-by-County Education Finance Choropleths, inflation-adjusted"),
                     plotOutput("MIcounty.facet.map", height="800px")
                     ),   
            
            tabPanel("County Comparison",
                     h3(textOutput("year.header")),                     
                     tableOutput("county.comp.table")
                     ), # table of county totals comparison, and plot; 2 selectors
            
            tabPanel("Explore Bulletin1014",
                     h3("Bulletin 1014 (2004-2012), not adjusted for inflation."),
                     dataTableOutput("bulletin1014.full.dt")
            )
            
        )                
    )   
))
