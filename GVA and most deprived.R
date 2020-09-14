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
allgva <- allgva %>% filter(`SIC07 description` == "All industries") %>% 
  select(Region, `LAD code`, `LA name`, `20183`) %>% 
  rename("2018" = "20183")

#' NOTE: the ONS GVA data is using a LAD code set that is 2019 for E&W but 2015 for Scotland! Have to manually adjust Glasgow City and South Lanarkshire to make it work.
#' so S12000044 North Lanarkshire becomes S12000029
#' S12000046 Glasgow city becomes S12000049
allgva$`LAD code`[allgva$`LAD code` == "S12000044"] <- "S12000050"
allgva$`LAD code`[allgva$`LAD code` == "S12000046"] <- "S12000049"

#`grab the population data for per-capita derived`
library(nomisr)
#x <- nomis_data_info()
#y <- nomis_get_metadata("NM_31_1")
#g <- nomis_get_metadata("NM_31_1", concept = "GEOGRAPHY")

pops <- nomis_get_data("NM_31_1", time = "2018", sex = "Total", measures = 20100)
pops2 <- filter(pops,GEOGRAPHY_TYPECODE == "434") #'   434 local authorities 2019 448 is as of April 2015
pops2 <- filter(pops2,AGE_CODE == 0) #' all ages
pops2 <- select(pops2, c("GEOGRAPHY_NAME", "GEOGRAPHY_CODE","OBS_VALUE"))

#' merge in with GVA data
allgva <- merge(allgva,pops2[,c(2:3)], by.x = "LAD code", by.y = "GEOGRAPHY_CODE", all.x = T)
allgva <- rename(allgva, "Population 2018" ="OBS_VALUE")
allgva$gva2018percapita <- as.numeric(allgva$`2018`*1000000/allgva$`Population 2018`) # multiplied by 1 million as ONS data per million
allgva$gva2018percapita <- round(allgva$gva2018percapita,0)

#' add quintiles
allgva$gvaquins <- ntile(allgva$`2018`,5)
allgva$gvaquinspercapita <- ntile(allgva$gva2018percapita,5)

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

####### SCOTLAND! #######
simd.df <- read_excel("SIMD+2020v2+-+ranks.xlsx", sheet = "SIMD 2020v2 ranks")
simd.df <- select(simd.df, Data_Zone, SIMD2020v2_Rank) #prune
simd.df <- simd.df %>% mutate(deciles = ntile(SIMD2020v2_Rank,10)) #deciles
simd.df <- simd.df %>% filter(deciles == 1) #keep most deprived decile


#get the centroids
datazonecentroids <- read_sf("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/4/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryMultipoint&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")
datazonecentroids <- merge(datazonecentroids, simd.df, by.x = "DataZone", by.y = "Data_Zone")

##### Northern Ireland #####
niimd.df <- read_excel("NIMDM17_SA - for publication.xls", sheet = "MDM")
niimd.df <- niimd.df[,c(3,5)]
niimd.df <- niimd.df %>% rename(`IMD Rank` = 2)
niimd.df <- niimd.df %>% mutate(deciles = ntile(`IMD Rank`,10))
#select the most deprived decile
niimd.df <- filter(niimd.df,deciles == 1 )


#get the centroids
library(rgdal)
library(rgeos)
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))


NI.soas <- readOGR(layer = "SA2011", dsn = "SA2011_Esri_Shapefile_0") #file too big for github have to DL from https://www.nisra.gov.uk/publications/small-area-boundaries-gis-format
temp <- SpatialPointsDataFrame(gCentroid(NI.soas, byid=TRUE), 
                               NI.soas@data, match.ID=FALSE)
#NI.centroids <- st_as_sf(temp)
NI.centroids <- spTransform(temp, CRS("+proj=longlat +datum=WGS84")) #need to convert coordinates to long lat from UTM/BNG
NI.centroids <- st_as_sf(NI.centroids)

#merge in the NI IMD data
NI.centroids <- merge(NI.centroids,niimd.df, by = "SA2011")
#NI.centroids <- NI.centroids %>% select(SA2011,`IMD Rank`, geometry)

#### MAPS!!! #####



#' go fetch boundary files and centriods to merge in


#LADbounds <- geojson_sf("https://opendata.arcgis.com/datasets/0e07a8196454415eab18c40a54dfbbef_0.geojson") #2019 boundaries
LADbounds <- geojson_sf("https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson") #2019 ultra generalised
#LADbounds <- geojson_sf("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_3.geojson") #2015 boundaries



LSOAcentroids <- geojson_sf("https://opendata.arcgis.com/datasets/b7c49538f0464f748dd7137247bbc41c_0.geojson")

#' merge data in
LADbounds <- merge(LADbounds, allgva,by.x = "lad19cd", by.y = "LAD code", all.y = T)

LSOAcentroids <- merge(LSOAcentroids, IMD19all[,c(1,5)], by.x = "lsoa11cd", by.y = "LSOA code (2011)")
LSOAcentroids <- LSOAcentroids %>% filter(`IMD decile (1 is most deprived)` == 1)  # select just most deprived 10%

#' colour pallete


factpal <- colorFactor("BuPu",domain = as.factor(LADbounds$gvaquinspercapita),n = 5 ,ordered = TRUE )
binpal <- colorQuantile("Blues", LADbounds$gva2018percapita, n = 5)

#' hover labels
labels <- sprintf("<strong>%s</strong><br/>%s GVA per capita<sup></sup>",
                  LADbounds$lad19nm,
                  format(LADbounds$gva2018percapita, big.mark = ",")) %>% 
  lapply(htmltools::HTML)

####function for circles to additional legend####
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.7, position){
  
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:",
                           sizes, "px", "; position: relative; left: ",max(sizes)-(sizes/2)-5,"px")
  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes,";position:relative; left: ",max(sizes)-(sizes),"px","; bottom: ",
                           10,"px",";margin-top: 12px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity, position = position))
}


#map element
m2 <- leaflet(LADbounds, height = "700px", options = list(padding = 100)) %>% setView(-3.5,54.6, 6) %>% 
  setMapWidgetStyle(list(background = "white")) %>% addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1) ) %>% 
  addMapPane(name = "toplayer", zIndex = 420) %>% #layer orders to make sure LSOA markers render on top.
  addMapPane(name = "nottoplayer", zIndex = 410) %>% 

  addPolygons(fillColor = ~factpal(LADbounds$gvaquinspercapita),
              stroke = F, smoothFactor = 0.2, fillOpacity = 0.7) %>% 

  addPolygons(label = labels, fillOpacity = 0, opacity = 0,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              options = leafletOptions(pane = "nottoplayer")) %>%
#deprived areas in E&W
  addCircleMarkers(data = LSOAcentroids, group = "circlegw",
                 radius = 1.5,
                 stroke = F,
                 color = "#00E1BA", opacity = 0.85, fillOpacity = 0.85,
                 options = leafletOptions(pane = "toplayer")) %>% 
#deprived areas in scotland
  addCircleMarkers(data = datazonecentroids, group = "circlegw",
                   radius = 1.5,
                   stroke = F,
                   color = "#00E1BA", opacity = 0.85, fillOpacity = 0.85,
                   options = leafletOptions(pane = "toplayer")) %>% 
  
  #deprived areas in NI
  
  addCircleMarkers(data = NI.centroids, group = "circlegw",
                   radius = 1.5,
                   stroke = F,
                   color = "#00E1BA", opacity = 0.85, fillOpacity = 0.85,
                   options = leafletOptions(pane = "toplayer")) %>%
  
  addLegendCustom(colors = c("#00E1BA"), 
                  labels = c("Most deprived 10% n'hood"),
                  
                  sizes = c(10), position = "topright" ) %>% 
  
  addLegend(pal = factpal, values = LADbounds$gvaquinspercapita, labels = levels(LADbounds$gvaquinspercapita), position = "topright", title = "GVA Quintiles <br>(1 = low)") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2



#' other web page elements
#add title to page
library(htmltools)

#page element title
title <- tags$div(HTML("Gross Value Added (GVA) per head by Local Authority and most deprived neighbourhoods;<br> 2018; England and Wales</br>"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 16px; text-align: center"
)

#page element data sources
sources <- tags$div(HTML("Note: Deprivation ranks are relative to each individual country <br>
Sources: Regional gross value added (balanced) by industry: local authorities by NUTS1 region,ONS; 
Indices of Multiple Deprivation 2019, MHCLG; Welsh Index of Multiple Deprivation 2019;
Northern Ireland Multiple Deprivation Measure 2017, NISRA; 
Scottish index of multiple deprivation 2020, Scottish Gov't;
Mid-year population estimates 2018, ONS<br>
Analysis: WPI Economics on behalf of CRC"), 
                    style = "font-family: Open Sans;color: #2A2A2A;font-style: italic; font-size: 12px; text-align: left"
)


combo <- htmltools::tagList(title,m2,sources) #I think this makes a combined html object
browsable(combo)
htmltools::save_html(combo, "index.html", background = "#FFFCF1") 

#' work out %ge of deprived LSOAs by GVA quintile
LSOAcentroids2 <- merge(LSOAcentroids, allgva[,c(1,8)], by.x = "Local Authority District code (2019)",by.y = "LAD code" ) #note LSOAcentriods already filtered to only include most deprived n'hoods.

t1 <- LSOAcentroids2 %>% group_by(gvaquinspercapita) %>% summarise(`most deprived neighbourhood count` = sum(`IMD decile (1 is most deprived)`))
t1$percent <- t1$`most deprived neighbourhood count`/sum(t1$`most deprived neighbourhood count`)*100
view(t1)

#extra map for print

m2 <- leaflet(LADbounds, height = "700px", options = list(padding = 100)) %>% setView(-3.5,54.6, 6) %>% 
  setMapWidgetStyle(list(background = "white")) %>% addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1) ) %>% 
  addMapPane(name = "toplayer", zIndex = 420) %>% #layer orders to make sure LSOA markers render on top.
  addMapPane(name = "nottoplayer", zIndex = 410) %>% 
  
  addPolygons(fillColor = ~factpal(LADbounds$gvaquinspercapita),
              stroke = F, smoothFactor = 0.2, fillOpacity = 0) %>% 
  
  addPolygons(label = labels, fillOpacity = 0, opacity = 0,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              options = leafletOptions(pane = "nottoplayer")) %>%
  #deprived areas in E&W
  addCircleMarkers(data = LSOAcentroids, group = "circlegw",
                   radius = 1.5,
                   stroke = F,
                   color = "#00E1BA", opacity = 0.85, fillOpacity = 0.85,
                   options = leafletOptions(pane = "toplayer")) %>% 
  #deprived areas in scotland
  addCircleMarkers(data = datazonecentroids, group = "circlegw",
                   radius = 1.5,
                   stroke = F,
                   color = "#00E1BA", opacity = 0.85, fillOpacity = 0.85,
                   options = leafletOptions(pane = "toplayer")) %>% 
  
  #deprived areas in NI
  
  addCircleMarkers(data = NI.centroids, group = "circlegw",
                   radius = 1.5,
                   stroke = F,
                   color = "#00E1BA", opacity = 0.85, fillOpacity = 0.85,
                   options = leafletOptions(pane = "toplayer")) %>%
  
  addLegendCustom(colors = c("#00E1BA"), 
                  labels = c("Most deprived 10% n'hood"),
                  
                  sizes = c(10), position = "topright" ) %>% 
  
  #addLegend(pal = factpal, values = LADbounds$gvaquinspercapita, labels = levels(LADbounds$gvaquinspercapita), position = "topright", title = "GVA Quintiles <br>(1 = low)") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2
