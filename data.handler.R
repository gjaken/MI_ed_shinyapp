## Michigan Educational Financial Data
## ShinyApp data visualization and exploration project
## Open Data Michigan

library(data.table)
library(stringr)
library(maps)
library(maptools)




# Import MI Ed Data & CPI -------------------------------------------------------------

# Input years to analyze
year.to.include <- 2004:2012
date.range <- c('2004-12-31', '2012-12-31') # used with cpi

ct_years <- length(year.to.include)
bulletin1014.list <- list() 
for (i in 1:ct_years) {
    fldnm <- paste("y", year.to.include[i], sep = "") # dynamic field name for list
    
    # Dynamically assign field to list for each year of data
    bulletin1014.list[[fldnm]] <- fread(paste(year.to.include[i], "Bulletin1014.csv", sep = ""))
    bulletin1014.list[[fldnm]]$YEAR <- year.to.include[i] # add a column specifying the data year
    
    setnames(bulletin1014.list[[fldnm]], toupper(names(bulletin1014.list[[fldnm]]))) # ensure case
    
    ## Add things here:
    # governor & party
    # party in house, in senate (breakups)
    # recession flag
}

# Inflation data
cpi <- fread('MI_ed_shinyapp/FRED.cpi.headline.annual.csv')

# Combine data, and standardize -------------------------------------------

bulletin1014.dt<- rbindlist(bulletin1014.list) # Merge data tables using rbindlist
bulletin1014.dt$id<- bulletin1014.dt[,paste(YEAR,DCODE,sep="")] # add id column

bulletin1014.dt$DISTCOUNTY <- str_replace_all(bulletin1014.dt$DISTCOUNTY," (COUNT)(.)?$","") # standardize county naming (without 'county')
bulletin1014.dt$DISTCOUNTY <- str_replace_all(bulletin1014.dt$DISTCOUNTY,"\\.","") # standardize county naming (without 'county')

bulletin1014.dt <- bulletin1014.dt[T.SAL != 0 & P.TCHR != 0] # filter out academies & unhelpful data points, that throw off T.Sal calcs


# Aggregate by year, county    --------------------------------------------

# sum totals of data into counties, for comparison
bulletin1014.county <- bulletin1014.dt[, list(TOTEXP.COUNTY = sum(TOTEXP),
                                              TOTREV.COUNTY = sum(TOTREV),
                                              TCHR.SAL.COUNTY = sum(T.SAL),
                                              TCHR.NUM.COUNTY = sum(P.TCHR),
                                              PUPIL.NUM.COUNTY = sum(AVG.FTE)
                                              ),                                       
                                       by= list(YEAR, DISTCOUNTY)]

# inflation adjust
cpi$adjuster <- cpi[Date == date.range[2], Value] / cpi[, Value] # create cpi adjustor, to inflation adjust
cpi <- cpi[Date >= date.range[1] & Date <= date.range[2]] # date.range = 2004-12-31, 2012-12-31
cpi <- cpi[order(Date)] # to match ordered date format of .SD 

# inflation adjust and combine adjust back with original dt    
bulletin1014.county[, `:=` (TOTEXP.COUNTY = TOTEXP.COUNTY * cpi$adjuster,
                            TOTREV.COUNTY = TOTREV.COUNTY * cpi$adjuster,
                            TCHR.SAL.COUNTY = TCHR.SAL.COUNTY * cpi$adjuster), 
                    by = DISTCOUNTY]

# create new ratio variables
bulletin1014.county[, `:=` (EXP.PER.PUPIL.COUNTY     = TOTEXP.COUNTY / PUPIL.NUM.COUNTY,
                            REV.PER.PUPIL.COUNTY     = TOTREV.COUNTY / PUPIL.NUM.COUNTY,
                            TCHR_SA.PER.PUPIL.COUNTY = TCHR.SAL.COUNTY / PUPIL.NUM.COUNTY,
                            TCHR_SAL.AVG.COUNTY      = TCHR.SAL.COUNTY / TCHR.NUM.COUNTY,
                            PUPIL.PER.TCHR.COUNTY    = PUPIL.NUM.COUNTY / TCHR.NUM.COUNTY)]                    
            


# Import map and combine --------------------------------------------------

MIcounty.map.dt<- data.table(map_data("county", "michigan")) # import as data table
MIcounty.map.dt$subregion<- toupper(MIcounty.map.dt$subregion) # standardize case
bulletin1014.county$subregion <- bulletin1014.county$DISTCOUNTY # provide column to match
MIcounty.map.dt <- merge(MIcounty.map.dt, bulletin1014.county,by="subregion", allow.cartesian=TRUE) # join tables

county.centers<- MIcounty.map.dt[,list(clat = mean(lat), clong = mean(long)),by=subregion] # create county name tags
MIcounty.map.dt<- merge(MIcounty.map.dt,county.centers, by="subregion", allow.cartesian=TRUE) # THIS IS THE FINAL MI COUNTY DATA TABLE


# Write output for Shiny to upload ----------------------------------------

write.csv(bulletin1014.dt, "MI_ed_shinyapp/bulletin1014.dt.csv", row.names=FALSE)
write.csv(MIcounty.map.dt, "MI_ed_shinyapp/MIcounty.map.dt.csv", row.names=FALSE)
write.csv(bulletin1014.county, "MI_ed_shinyapp/bulletin1014.county.csv", row.names=FALSE)
