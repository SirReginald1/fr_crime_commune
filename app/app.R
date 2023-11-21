if(!require(shiny)){install.packages("shiny")}
if(!require(shinyWidgets)){install.packages("shinyWidgets")}
if(!require(rvest)){install.packages("rvest")}
if(!require(bslib)){install.packages("bslib")}
if(!require(jsonlite)){install.packages("jsonlite")}
if(!require(httr)){install.packages("httr")}
if(!require(kableExtra)){install.packages("kableExtra")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggmap)){install.packages("ggmap")}
if(!require(sf)){install.packages("sf")}
if(!require(rnaturalearth)){install.packages("rnaturalearth")}
if(!require(rnaturalearthdata)){install.packages("rnaturalearthdata")}
if(!require(devtools)){install.packages("devtools")}
if(!require(rnaturalearthhires)){devtools::install_github("ropensci/rnaturalearthhires")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(tmap)){install.packages("tmap")}

library(frCrimeData) # Just in case

source('ui.R')
source('server.R')

assign("set_fr_crime_commune",readRDS("Data/set_fr_crime_commune.rds"),envir = .GlobalEnv)

shinyApp(
    ui = ui,
    server = server
)
