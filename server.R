library(shiny)
library(ggplot2)
library(data.table)
library(reshape2)

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
        
        output$download.1014 <- downloadHandler(    
            filename = "bulletin1014.full.csv",
            content = function(file) {
                data <- fread("bulletin1014.full.csv")
                write.csv(data, file)
            }
        )
        
        
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
        
        output$stateTotals.dt <- renderTable({
            bulletin1014.state[, list(YEAR, "Revenue per Pupil" = REV.PER.PUPIL.STATE, "Expenditure per Pupil" = EXP.PER.PUPIL.STATE, "Teacher Salary per Pupil" = TCHR_SA.PER.PUPIL.STATE, "Average Teacher Salary" = TCHR_SAL.AVG.STATE, "Student / Teacher Ratio" = PUPIL.PER.TCHR.STATE)]
        })
        
        output$stateTotals.plot <- renderPlot({
            # facet label function
            fin.data.names <- list(
                "REV.PER.PUPIL.STATE" = "Revenue per Pupil",
                "EXP.PER.PUPIL.STATE" = "Expenditure per Pupil",
                "TCHR_SA.PER.PUPIL.STATE" = "Teacher Salary per Pupil",
                "TCHR_SAL.AVG.STATE" = "Average Teacher Salary",
                "PUPIL.PER.TCHR.STATE" = "Student / Teacher Ratio")
            
            fin.data_labeller <- function(variable, value) {
                return(fin.data.names[value])
            }
            
            # melt and filter for plotting                 
            p <- ggplot(data = melt(bulletin1014.state[, list(YEAR, REV.PER.PUPIL.STATE, EXP.PER.PUPIL.STATE, TCHR_SA.PER.PUPIL.STATE, TCHR_SAL.AVG.STATE, PUPIL.PER.TCHR.STATE)],
                                    id="YEAR")) +
                geom_line(aes(x = YEAR, y = value, color = variable, name = "datasets"), size = 1) +
                facet_grid(variable ~ ., scale = 'free_y', labeller = fin.data_labeller)+
                xlab("Year") + 
                ylab("Values (in 2012 $)") +
                ggtitle("Michigan Educational Financial Data") +
                theme(legend.position = "none")

            print(p)                        
        })
        
        output$county.comp.table <- renderTable({
            total <- t(bulletin1014.county[DISTCOUNTY == input$county1 & YEAR == input$year,
                                           list("Expenditure" = round(TOTEXP.COUNTY),
                                                "Revenue" = round(TOTREV.COUNTY),
                                                "Teacher Salary" = round(TCHR.SAL.COUNTY),
                                                "Number of Teachers" = round(TCHR.NUM.COUNTY),
                                                "Number of Pupils" = round(PUPIL.NUM.COUNTY))])
            
            per.pupil <- t(bulletin1014.county[DISTCOUNTY == input$county1 & YEAR == input$year,
                                               list("Expenditure" = round(EXP.PER.PUPIL.COUNTY),
                                                    "Revenue" = round(REV.PER.PUPIL.COUNTY),
                                                    "Teacher Salary" = round(TCHR_SA.PER.PUPIL.COUNTY),
                                                    "Number of Teachers" = "",
                                                    "Number of Pupils" = "")])
            
            
            per.teacher <- t(bulletin1014.county[DISTCOUNTY == input$county1 & YEAR == input$year,
                                                 list("Expenditure" = "",
                                                      "Revenue" = "",
                                                      "Teacher Salary" = round(TCHR_SAL.AVG.COUNTY),
                                                      "Number of Teachers" = "",
                                                      "Number of Pupils" = round(PUPIL.PER.TCHR.COUNTY))])    
            
            
            county.comparison <- cbind(total, per.pupil, per.teacher)
            colnames(county.comparison) <- c("Total", "Per Pupil", "Per Teacher")  
            
            county.comparison
        })
        
        output$year.header <- renderText({
            paste("County Comparison for",input$year)
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
