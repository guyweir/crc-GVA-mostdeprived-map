#Further analysis work out population in deprived LSOAs found in the highest GVA quintiles.
library(nomisr)
library(tidyverse)
library(readxl)
library(rgdal)
library(rgeos)
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))

#get lsoa pops for E&W, Scot and NI then merge into centroids sf files.
lsoapops <- nomis_get_data("NM_2010_1", time = "2018", sex = "Total", measures = 20100, C_AGE = 200, geography = "TYPE298" )
lsoapops <- select(lsoapops, c("GEOGRAPHY_NAME", "GEOGRAPHY_CODE","OBS_VALUE"))

LSOAcentroids <- geojson_sf("https://opendata.arcgis.com/datasets/b7c49538f0464f748dd7137247bbc41c_0.geojson")
LSOAcentroids <- merge(LSOAcentroids, lsoapops, by.x = "lsoa11cd", by.y = "GEOGRAPHY_CODE", all.x = T)



NI.pops <- read_excel("SAPE18_SA_Totals.xlsx", 
                      sheet = "Tabular", range = "A17:S4552")
NI.pops <- NI.pops %>% select(Area_Code, `2018`)

NI.soas <- readOGR(layer = "SA2011", dsn = "SA2011_Esri_Shapefile_0") #file too big for github have to DL from https://www.nisra.gov.uk/publications/small-area-boundaries-gis-format
temp <- SpatialPointsDataFrame(gCentroid(NI.soas, byid=TRUE), 
                               NI.soas@data, match.ID=FALSE)
#NI.centroids <- st_as_sf(temp)
NI.centroids <- spTransform(temp, CRS("+proj=longlat +datum=WGS84")) #need to convert coordinates to long lat from UTM/BNG
NI.centroids <- st_as_sf(NI.centroids)
NI.centroids <- merge(NI.centroids, NI.pops, by.x = "SA2011", by.y = "Area_Code", all.x = T)


datazonecentroids <- read_sf("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/4/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryMultipoint&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")
scotlandpops <- sum(datazonecentroids$TotPop2011)
EWpops <- sum(LSOAcentroids2$OBS_VALUE)
NI.popscnt <- sum(na.omit(NI.centroids$`2018`))

#tidyup remove interim files
rm(NI.pops, lsoapops, temp, NI.soas)

#add in the various IMD deciles
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
rm(IMD19Eng, IMD19Wales)


#'Scotland
####### SCOTLAND! #######
simd.df <- read_excel("SIMD+2020v2+-+ranks.xlsx", sheet = "SIMD 2020v2 ranks")
simd.df <- select(simd.df, Data_Zone, SIMD2020v2_Rank) #prune
simd.df <- simd.df %>% mutate(deciles = ntile(SIMD2020v2_Rank,10)) #deciles


##### Northern Ireland #####
niimd.df <- read_excel("NIMDM17_SA - for publication.xls", sheet = "MDM")
niimd.df <- niimd.df[,c(3,5)]
niimd.df <- niimd.df %>% rename(`IMD Rank` = 2)
niimd.df <- niimd.df %>% mutate(deciles = ntile(`IMD Rank`,10))


#merge in the imd deciles
LSOAcentroids <- merge(LSOAcentroids, IMD19all[,c(1,2,3,5)], by.x = "lsoa11cd", by.y = "LSOA code (2011)")
datazonecentroids <- merge(datazonecentroids, simd.df, by.x = "DataZone", by.y = "Data_Zone")
NI.centroids <- merge(NI.centroids,niimd.df, by = "SA2011")

#GVA data
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
rm(gvafileslist, gvafiles)


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
rm(pops, pops2)

LADbounds <- geojson_sf("https://opendata.arcgis.com/datasets/1d78d47c87df4212b79fe2323aae8e08_0.geojson") #2019 full resolution
#' merge data in
LADbounds <- merge(LADbounds, allgva,by.x = "lad19cd", by.y = "LAD code", all.y = T)


#' totallivingindepriveddecileUK <- sum(scotlandpops, EWpops, NI.popscnt)
#' round(totallivingindepriveddecileUK,-4)
#' 
#' #' work out %ge of deprived LSOAs by GVA quintile
#' LSOAcentroids2 <- merge(LSOAcentroids2, allgva[,c(1,8)], by.x = "Local Authority District code (2019)",by.y = "LAD code", all.x = T) #note LSOAcentriods already filtered to only include most deprived n'hoods.
#' 
#' 
#' t1 <- LSOAcentroids2 %>% group_by(gvaquinspercapita) %>% summarise(`most deprived neighbourhood count` = sum(`IMD decile (1 is most deprived)`))
#' t1$percent <- t1$`most deprived neighbourhood count`/sum(t1$`most deprived neighbourhood count`)*100
#' view(t1)
#' t1 <- as.tibble(t1) %>% select(-geometry)
#write.csv(t1, "deprived nhoods per GVA quintile.csv", row.names = F)


#spatial query to select only deprived areas in GVA = 5 districts.
#'NOTE this method allocates deprived n'hoods to parent Local Authority areas who have high GVA via spatial method. 
#'The LAD boundary file is generalised so points on polygon borders may be allocated to the wrong LAD.
#'did it like this for speed and our final figure will be rounded.


##############################
###### MOST DEPRIVED ########
##############################


# GVA5bounds <- LADbounds %>% filter(gvaquinspercapita ==5) #5 is highest GVA
# LSOA_GVA5 <- st_intersection(LSOAcentroids2,GVA5bounds)
# DZ_GVA5 <- st_intersection(datazonecentroids,GVA5bounds)
# NI_GVA5 <- st_intersection(NI.centroids, GVA5bounds)
# 
# #list the LSOAcodes to serve as filter for population data.
# lsoaCALCS <- lsoapops %>% filter(GEOGRAPHY_CODE %in% LSOA_GVA5$lsoa11cd)
# dzCALCS <- datazonecentroids %>% filter(DataZone %in% DZ_GVA5$DataZone)
# NICALCS <- NI.pops %>% filter(Area_Code %in% NI_GVA5$SA2011)
# 
# #Population count of people in deprived n'hoods also in high GVA (quintile 5) local authorities)
# bigsum <- sum(lsoaCALCS$OBS_VALUE) + sum(dzCALCS$TotPop2011) + sum(NICALCS$`2018`)
# bigsum
# 
# #proportion of most deprived LSOAs in GVA5 areas
# count_GVA5_depLSOAS <- nrow(LSOA_GVA5) + nrow(DZ_GVA5) + nrow(NICALCS)
# totdeprivednhoodsUK <- nrow(LSOAcentroids2) + nrow(datazonecentroids) + nrow(NI.centroids)
# proportion <- count_GVA5_depLSOAS / totdeprivednhoodsUK
# nrow(LSOA_GVA5)
# 
# check <- nrow(LSOA_GVA5)/nrow(LSOAcentroids2)
# check


# #Count of population on MOST deprived n'hppds
# lsoamost <- lsoapops %>% filter(GEOGRAPHY_CODE %in% LSOAcentroids$lsoa11cd)
# 
# datazonecentroids <- read_sf("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/4/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryMultipoint&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")
# dzmost <- datazonecentroids %>% filter(DataZone %in% datazonecentroids$DataZone)
# 
# NImost <- NI.pops %>%  filter(Area_Code %in% NI.centroids$SA2011)
# 
# all_mostpops <- sum(lsoamost$OBS_VALUE) + sum(dzmost$TotPop2011) + sum(NImost$`2018`)
# 
# 
# bigsum / all_mostpops * 100


##############################
###### LEAST DEPRIVED ########
##############################

#%ge of least deprived in bottom GVA quintile of LAs
# GVA5boundsLOW <- LADbounds %>% filter(gvaquinspercapita ==1) #1 is lowest GVA
# LSOA_GVA5 <- st_intersection(LSOAcentroidsLEAST,GVA5boundsLOW)
# DZ_GVA5 <- st_intersection(datazonecentroidsLEAST,GVA5boundsLOW)
# NI_GVA5 <- st_intersection(NI.centroidsLEAST, GVA5boundsLOW) #NOTE THERE ARE NO LAs in NI with high GVA.
# 
# #list the LSOAcodes to serve as filter for population data.
# lsoaCALCS <- lsoapops %>% filter(GEOGRAPHY_CODE %in% LSOA_GVA5$lsoa11cd)
# dzCALCS <- datazonecentroids %>% filter(DataZone %in% DZ_GVA5$DataZone)
# NICALCS <- NI.pops %>% filter(Area_Code %in% NI_GVA5$SA2011)
# 
# #Population count of people in LEAST deprived n'hoods also in high GVA (quintile 5) local authorities)
# bigsumleast <- sum(lsoaCALCS$OBS_VALUE) + sum(dzCALCS$TotPop2011) + sum(NICALCS$`2018`)
# bigsumleast
# 
# #proportion of LEAST deprived LSOAs in GVA5 areas
# count_GVA5_depLSOAS <- nrow(LSOA_GVA5) + nrow(DZ_GVA5) + nrow(NICALCS)
# totdeprivednhoodsUK <- nrow(LSOAcentroids2) + nrow(datazonecentroids) + nrow(NI.centroids)
# proportion <- count_GVA5_depLSOAS / totdeprivednhoodsUK
# nrow(LSOA_GVA5)
# 
# check <- nrow(LSOA_GVA5)/nrow(LSOAcentroids2)
# check
# 
# ############Have to read in the centroids again as were filtered to only have least deprived ###############
# #Count of population on LEAST deprived n'hppds
# LSOAcentroids <- geojson_sf("https://opendata.arcgis.com/datasets/b7c49538f0464f748dd7137247bbc41c_0.geojson")
# lsoaleast <- lsoapops %>% filter(GEOGRAPHY_CODE %in% LSOAcentroidsLEAST$lsoa11cd)
# 
# datazonecentroids <- read_sf("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/4/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryMultipoint&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")
# dzleast <- datazonecentroids %>% filter(DataZone %in% datazonecentroidsLEAST$DataZone)
# 
# 
# #NI
# NI.soas <- readOGR(layer = "SA2011", dsn = "SA2011_Esri_Shapefile_0") #file too big for github have to DL from https://www.nisra.gov.uk/publications/small-area-boundaries-gis-format
# temp <- SpatialPointsDataFrame(gCentroid(NI.soas, byid=TRUE), 
#                                NI.soas@data, match.ID=FALSE)
# 
# #NI.centroids <- st_as_sf(temp)
# NI.centroids <- spTransform(temp, CRS("+proj=longlat +datum=WGS84")) #need to convert coordinates to long lat from UTM/BNG
# NI.centroids <- st_as_sf(NI.centroids)
# NI.pops <- read_excel("SAPE18_SA_Totals.xlsx", 
#                       sheet = "Tabular", range = "A17:S4552")
# NI.pops <- NI.pops %>% select(Area_Code, `2018`)
# NI.centroids <- merge(NI.centroids, NI.pops, by.x = "SA2011", by.y = "Area_Code", all.x = T)
# 
# 
# all_leastpops <- sum(lsoaleast$OBS_VALUE) + sum(dzleast$TotPop2011) + sum(NIleast$`2018`)
# bigsumleast / all_leastpops * 100




#MAKE INTO TABLES

#Adds the GVA quintiles via spatial query

gva_nhoods <- list()
for (i in 1:5){
  gvaEW <- LADbounds %>% filter(gvaquinspercapita == i) 
  GVAlsoa <- st_intersection(LSOAcentroids,gvaEW)
  GVAlsoa$"GVA quintile" <- i
  gva_nhoods[[i]]<-GVAlsoa
}
gva_nhoods <- do.call(bind_rows, gva_nhoods)


gva_nhoodsS <- list()
for (i in 1:5){
  gvaEW <- LADbounds %>% filter(gvaquinspercapita == i) 
  GVAlsoa <- st_intersection(datazonecentroids,gvaEW)
  GVAlsoa$"GVA quintile" <- i
  gva_nhoodsS[[i]]<-GVAlsoa
}
gva_nhoodsS <- do.call(bind_rows, gva_nhoodsS)

gva_nhoodsNI <- list()
for (i in c(1,2,3,5)){
  gvaEW <- LADbounds %>% filter(gvaquinspercapita == i) 
  GVAlsoa <- st_intersection(NI.centroids,gvaEW)
  GVAlsoa$"GVA quintile" <- i
  gva_nhoodsNI[[i]]<-GVAlsoa
}

gva_nhoodsNI <- do.call(bind_rows, gva_nhoodsNI)


#make the data from which we construct the summary table
lsoa_table <- tibble(lsoa_code = c(gva_nhoods$lsoa11cd, 
                                   gva_nhoodsS$DataZone, 
                                   as.character(gva_nhoodsNI$SA2011)),
                     
                     imddecile = c(gva_nhoods$IMD.decile..1.is.most.deprived., 
                                   gva_nhoodsS$deciles, 
                                   gva_nhoodsNI$deciles),
                     
                     pops = c(gva_nhoods$OBS_VALUE, 
                              gva_nhoodsS$TotPop2011, 
                              gva_nhoodsNI$X2018),
                     
                     GVAquin = c(gva_nhoods$gvaquinspercapita, 
                                 gva_nhoodsS$gvaquinspercapita, 
                                 gva_nhoodsNI$gvaquinspercapita)
)

t1 <- lsoa_table %>% filter(imddecile == 1)  %>% group_by(GVAquin)  %>%  summarise("Population IMD decile 1"  = sum(na.omit(pops)))
t10 <- lsoa_table %>% filter(imddecile == 10)  %>% group_by(GVAquin)  %>%  summarise("Population IMD decile 10"  = sum(na.omit(pops)))
tall <- lsoa_table %>%  group_by(GVAquin)  %>%  summarise("Population all"  = sum(na.omit(pops)))

final <- merge(t1, t10, by = "GVAquin")
final <- merge(final, tall, by = "GVAquin")

#######################
####### REGIONAL ######
#######################

#grab the LSOA to REGION lookup
lsoa_region_lookup <- read_csv("lsoa to region lookup.csv")
regionnames <- read_csv("Region names and codes EN as at 12_10 (GOR).csv")

lsoa_region_lookup <- merge(lsoa_region_lookup, regionnames, by.x = "rgn", by.y = "GOR10CD")
lsoa_region_lookup <- select(lsoa_region_lookup,lsoa11,rgn,GOR10NM)

lsoa_table <- merge(lsoa_table, lsoa_region_lookup, by.x = "lsoa_code", by.y = "lsoa11", all.x = T)
lsoa_table$GOR10NM[str_detect(lsoa_table$lsoa_code,"S")] <- "Scotland"
lsoa_table$GOR10NM[str_detect(lsoa_table$lsoa_code,"N")] <- "Northern Ireland"

t1_reg <- lsoa_table %>% group_by(GOR10NM) %>% filter(imddecile == 1) %>% summarise("Population IMD decile 1" = sum(na.omit(pops)))
t10_reg <- lsoa_table %>% group_by(GOR10NM) %>% filter(imddecile == 10) %>% summarise("Population IMD decile 10" = sum(na.omit(pops)))
tall_reg <- lsoa_table %>% group_by(GOR10NM) %>% summarise("Population all" = sum(na.omit(pops)))

final_reg <- merge(t1_reg, t10_reg, by = "GOR10NM")
final_reg <- merge(final_reg, tall_reg, by = "GOR10NM")



