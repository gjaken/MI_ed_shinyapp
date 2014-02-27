library(shiny)
library(ggplot2)
library(data.table)


# Read in dataset ---------------------------------------------------------

# bulletin1014.dt <- fread("bulletin1014.dt.csv")       # Note: NOT Adjusted for Inflation
bulletin1014.county <- fread("bulletin1014.county.csv") # Note: Adjusted for Inflation
MIcounty.map.dt <- fread("MIcounty.map.dt.csv")         # Note: Adjusted for Inflation
# setkey if it seems useful

# create totals data at state level
bulletin1014.state <- bulletin1014.county[, list(PUPIL.NUM.STATE = sum(PUPIL.NUM.COUNTY), 
                                                 TOTREV.STATE    = sum(TOTREV.COUNTY),
                                                 TOTEXP.STATE    = sum(TOTEXP.COUNTY),
                                                 TCHR.NUM.STATE  = sum(TCHR.NUM.COUNTY),
                                                 TCHR.SAL.STATE  = sum(TCHR.SAL.COUNTY)), 
                                          by = YEAR]

# create new ratio variables
bulletin1014.state[, `:=` (REV.PER.PUPIL.STATE     = TOTREV.STATE / PUPIL.NUM.STATE,
                           EXP.PER.PUPIL.STATE     = TOTEXP.STATE / PUPIL.NUM.STATE,
                           TCHR_SA.PER.PUPIL.STATE = TCHR.SAL.STATE / PUPIL.NUM.STATE,
                           TCHR_SAL.AVG.STATE      = TCHR.SAL.STATE / TCHR.NUM.STATE,
                           PUPIL.PER.TCHR.STATE    = PUPIL.NUM.STATE / TCHR.NUM.STATE)]


## Round Data for County Map
MIcounty.map.dt[, `:=` (EXP.PER.PUPIL.COUNTY  = round(EXP.PER.PUPIL.COUNTY,-2),
                        REV.PER.PUPIL.COUNTY  = round(REV.PER.PUPIL.COUNTY,-2),
                        TCHR_SAL.AVG.COUNTY   = round(TCHR_SAL.AVG.COUNTY,-2),
                        PUPIL.PER.TCHR.COUNTY = round(PUPIL.PER.TCHR.COUNTY,0)
                        )]

# shinyServer function ----------------------------------------------------
shinyServer(
    function(input, output) {
        
        output$outputSlider <- renderUI({    
            # set min, max, med, sd variables based on dataset            
            c.med <- median(MIcounty.map.dt[[input$fldnm]])
            c.sd  <- sd(MIcounty.map.dt[[input$fldnm]])
            c.min <- min(MIcounty.map.dt[[input$fldnm]])
            c.max <- max(MIcounty.map.dt[[input$fldnm]])
            
            # dynamically generate slider based on dataset
            sliderInput("inputSlider",
                        "Slider",
                        min = c.min,
                        max = c.max,
                        value = c(max(c.med - c.sd, c.min),
                                  min(c.med + c.sd, c.max))
                        )
                        
        })
    
        output$MIcounty.facet.map <- renderPlot({
            # code to set range
            if(is.null(input$inputSlider)) # Check for renderUI inputs before loading. If null, then return for now
                return()
            
            MIcounty.map.dt[[input$fldnm]] <- pmax(MIcounty.map.dt[[input$fldnm]], input$inputSlider[1]) # sets min
            MIcounty.map.dt[[input$fldnm]] <- pmin(MIcounty.map.dt[[input$fldnm]], input$inputSlider[2]) # sets max
            
            # select dataset and color    
            fld.clr <- switch(input$fldnm,
                           "EXP.PER.PUPIL.COUNTY"  = "darkgreen",
                           "REV.PER.PUPIL.COUNTY"  = "darkblue",
                           "TCHR_SAL.AVG.COUNTY"   = "darkorchid",
                           "PUPIL.PER.TCHR.COUNTY" = "darkred")
             
            # code for chart
            p<- ggplot(data = MIcounty.map.dt, 
                       aes(x = long, y = lat, group = group)) + 
                labs(title = "Michigan Education: Per Pupil Finances") +
                geom_polygon(aes_string(fill = input$fldnm)) + 
                scale_fill_gradient(high = fld.clr, low = "white") + 
                facet_wrap(~ YEAR, ncol = 3) + 
                geom_path(color = "black", linestyle = 2) +
                coord_equal() +
                theme(axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank())
            
            print(p)
        })
        
        
    #    ## histPlot1 & summary1
    #     output$histPlot1 <- renderPlot({
    #         # Plot a histogram of AVG.TOTEXP
    #         # based on year selected
    #         x<- bulletin1014.dt[YEAR==input$year,AVG.TOTEXP]
    #         breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
    #         bwidth <- breaks[2]-breaks[1]
    #         p<- qplot(AVG.TOTEXP, data = bulletin1014.dt[YEAR==input$year], binwidth = bwidth)
    #         
    #         print(p)
    #     })
    #     
    #     output$summary1<- renderPrint({
    #         summary(bulletin1014.dt[YEAR==input$year,AVG.TOTEXP])
    #     })
    # 
    #     ## testTable
    #     output$testTable<- renderTable({
    #         bulletin1014.dt[YEAR==input$year]
    #     })
    
    }
)
