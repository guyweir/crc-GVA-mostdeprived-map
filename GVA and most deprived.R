##### Processing and wrangling #####

library(tidyverse)
library(readxl)

#' First we need to load in GVA data by local authority
#' this will be split into quintiles or deciles, and choropleth mapped
#' we will then add on marker dots for the most deprived lsoa's in England and Wales (relative to each country separately)

#' list regional GVA files
gvafileslist <- list.files(pattern = "regional")
gvafiles <- lapply(gvafileslist, read_excel, sheet = "Current Price", skip = 1) 
names(gvafiles) <- gvafileslist

#' next filter data to include totals only and 2018
allgva <- bind_rows(gvafiles)
allgva <- allgva %>% filter(`SIC07 description` == "All industries", Region != "Scotland", Region != "Northern Ireland") %>% 
  select(Region, `LAD code`, `LA name`, `20183`) %>% 
  rename("2018" = "20183")

#' add quintiles
allgva$gvaquins <- ntile(allgva$`2018`,5)

#### get the IMD for england and Wales ####

#' England
IMD19Eng <- read_excel("File_2_-_IoD2019_Domains_of_Deprivation.xlsx", 
                                                    sheet = "IoD2019 Domains")
IMD19Eng <- IMD19Eng %>% select(`LSOA code (2011)`,
                                `Local Authority District name (2019)`,
                                `Local Authority District code (2019)`, 
                                `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
                                `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% 
  rename(`IMD Rank (1 is most deprived)` = `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
         `IMD decile (1 is most deprived)` = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`
  )

#' Wales
IMD19Wales <- read_excel("welsh-index-multiple-deprivation-2019-index-and-domain-ranks-by-small-area.xlsx", 
                                                                                                       sheet = "WIMD_2019_ranks", skip = 2)

IMD19Wales <- IMD19Wales %>% select(`LSOA Code`, `Local Authority Name (Eng)`, `WIMD 2019`) %>% 
  rename(`LSOA code (2011)` = `LSOA Code`,
         `Local Authority District name (2019)` = `Local Authority Name (Eng)`,
         `IMD Rank (1 is most deprived)` = `WIMD 2019`)

IMD19Wales$`IMD decile (1 is most deprived)` <- ntile(IMD19Wales$`IMD Rank (1 is most deprived)`, 10)

#' merge IMD England and Wales into one file ####
IMD19all <- bind_rows(IMD19Eng, IMD19Wales)

#' bit of a tidy up
rm(IMD19Eng, IMD19Wales, gvafileslist, gvafiles)

#### MAPS!!! #####

suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))

#' go fetch boundary files and centriods to merge in
LADbounds <- geojson_sf("https://opendata.arcgis.com/datasets/0e07a8196454415eab18c40a54dfbbef_0.geojson")
LSOAcentroids <- geojson_sf("https://opendata.arcgis.com/datasets/b7c49538f0464f748dd7137247bbc41c_0.geojson")

#' merge data in
LADbounds <- merge(LADbounds, allgva,by.x = "lad19cd", by.y = "LAD code", all.y = T)

LSOAcentroids <- merge(LSOAcentroids, IMD19all, by.x = "lsoa11cd", by.y = "LSOA code (2011)")
LSOAcentroids <- LSOAcentroids %>% filter(`IMD decile (1 is most deprived)` == 1)  # select just most deprived 10%

#' colour pallete


factpal <- colorFactor("BuPu",domain = as.factor(LADbounds$gvaquins),n = 5 ,ordered = TRUE )
binpal <- colorQuantile("Blues", LADbounds$`2018`, n = 5)

#' hover labels
labels <- sprintf("<strong>%s</strong><br/>%s GVA (£ mil)<sup></sup>",
                  LADbounds$lad19nm,
                  format(LADbounds$`2018`, big.mark = ",")) %>% 
  lapply(htmltools::HTML)

#map element
m2 <- leaflet(LADbounds, height = "600px", options = list(padding = 100)) %>% setView(-3.5,53.2, 5.5) %>% 
  setMapWidgetStyle(list(background = "white")) %>% addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1) ) %>% 
  addMapPane(name = "toplayer", zIndex = 420) %>% #layer orders to make sure LSOA markers render on top.
  addMapPane(name = "nottoplayer", zIndex = 410) %>% 

  addPolygons(fillColor = ~factpal(LADbounds$gvaquins),
              stroke = F, smoothFactor = 0.2, fillOpacity = 0.9) %>% 

  addPolygons(label = labels, fillOpacity = 0, opacity = 0,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              options = leafletOptions(pane = "nottoplayer")) %>%

  addCircleMarkers(data = LSOAcentroids, group = "circlegw",
                 radius = 2,
                 stroke = F,
                 color = "red", opacity = 0.85, fillOpacity = 0.85,
                 options = leafletOptions(pane = "toplayer")) %>% 
  
  addLegend(pal = factpal, values = LADbounds$gvaquins, labels = levels(LADbounds$gvaquins), position = "bottomright", title = "GVA Quintiles <br>(1 = low)") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2



#' other web page elements
#add title to page
library(htmltools)

#page element title
title <- tags$div(HTML("COVID-19 deaths per 100,000 and deprivation,<br> March to May 2020, England and Wales</br>"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 22px; text-align: center"
)

#page element data sources
sources <- tags$div(HTML("Sources: Indices of Multiple Deprivation, MHCLG; Welsh Index of Multiple Deprivation 2019; Deaths involving COVID-19 by local area and deprivation, ONS<br> Analysis: WPI Economics on behalf of CRC, originally produced for Trust for London<br>Note: Deprivation ranks are relative to England and Wales separately"), 
                    style = "font-family: Open Sans;color: #2A2A2A;font-style: italic; font-size: 12px; text-align: left"
)
