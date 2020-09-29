#Further analysis work out population in deprived LSOAs found in the highest GVA quintiles.

#get lsoa pops for E&W.
lsoapops <- nomis_get_data("NM_2010_1", time = "2018", sex = "Total", measures = 20100, C_AGE = 200, geography = "TYPE298" )
lsoapops <- select(lsoapops, c("GEOGRAPHY_NAME", "GEOGRAPHY_CODE","OBS_VALUE"))
LSOAcentroids2 <- merge(LSOAcentroids2, lsoapops, by.x = "lsoa11cd", by.y = "GEOGRAPHY_CODE", all.x = T)

NI.pops <- read_excel("SAPE18_SA_Totals.xlsx", 
                      sheet = "Tabular", range = "A17:S4552")
NI.pops <- NI.pops %>% select(Area_Code, `2018`)
NI.centroids <- merge(NI.centroids, NI.pops, by.x = "SA2011", by.y = "Area_Code", all.x = T)


scotlandpops <- sum(datazonecentroids$TotPop2011)
EWpops <- sum(LSOAcentroids2$OBS_VALUE)
NI.popscnt <- sum(na.omit(NI.centroids$`2018`))

totallivingindepriveddecileUK <- sum(scotlandpops, EWpops, NI.popscnt)
round(totallivingindepriveddecileUK,-4)

#' work out %ge of deprived LSOAs by GVA quintile
LSOAcentroids2 <- merge(LSOAcentroids2, allgva[,c(1,8)], by.x = "Local Authority District code (2019)",by.y = "LAD code", all.x = T) #note LSOAcentriods already filtered to only include most deprived n'hoods.


t1 <- LSOAcentroids2 %>% group_by(gvaquinspercapita) %>% summarise(`most deprived neighbourhood count` = sum(`IMD decile (1 is most deprived)`))
t1$percent <- t1$`most deprived neighbourhood count`/sum(t1$`most deprived neighbourhood count`)*100
view(t1)
t1 <- as.tibble(t1) %>% select(-geometry)
#write.csv(t1, "deprived nhoods per GVA quintile.csv", row.names = F)


#spatial query to select only deprived areas in GVA = 5 districts.
#'NOTE this method allocates deprived n'hoods to parent Local Authority areas who have high GVA via spatial method. 
#'The LAD boundary file is generalised so points on polygon borders may be allocated to the wrong LAD.
#'did it like this for speed and our final figure will be rounded.


##############################
###### MOST DEPRIVED ########
##############################

GVA5bounds <- LADbounds %>% filter(gvaquinspercapita ==5) #5 is highest GVA
LSOA_GVA5 <- st_intersection(LSOAcentroids2,GVA5bounds)
DZ_GVA5 <- st_intersection(datazonecentroids,GVA5bounds)
NI_GVA5 <- st_intersection(NI.centroids, GVA5bounds)

#list the LSOAcodes to serve as filter for population data.
lsoaCALCS <- lsoapops %>% filter(GEOGRAPHY_CODE %in% LSOA_GVA5$lsoa11cd)
dzCALCS <- datazonecentroids %>% filter(DataZone %in% DZ_GVA5$DataZone)
NICALCS <- NI.pops %>% filter(Area_Code %in% NI_GVA5$SA2011)

#Population count of people in deprived n'hoods also in high GVA (quintile 5) local authorities)
bigsum <- sum(lsoaCALCS$OBS_VALUE) + sum(dzCALCS$TotPop2011) + sum(NICALCS$`2018`)
bigsum

#proportion of most deprived LSOAs in GVA5 areas
count_GVA5_depLSOAS <- nrow(LSOA_GVA5) + nrow(DZ_GVA5) + nrow(NICALCS)
totdeprivednhoodsUK <- nrow(LSOAcentroids2) + nrow(datazonecentroids) + nrow(NI.centroids)
proportion <- count_GVA5_depLSOAS / totdeprivednhoodsUK
nrow(LSOA_GVA5)

check <- nrow(LSOA_GVA5)/nrow(LSOAcentroids2)
check


#Count of population on MOST deprived n'hppds
lsoamost <- lsoapops %>% filter(GEOGRAPHY_CODE %in% LSOAcentroids$lsoa11cd)

datazonecentroids <- read_sf("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/4/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryMultipoint&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")
dzmost <- datazonecentroids %>% filter(DataZone %in% datazonecentroids$DataZone)

NImost <- NI.pops %>%  filter(Area_Code %in% NI.centroids$SA2011)

all_mostpops <- sum(lsoamost$OBS_VALUE) + sum(dzmost$TotPop2011) + sum(NImost$`2018`)


bigsum / all_mostpops * 100


##############################
###### LEAST DEPRIVED ########
##############################

#%ge of least deprived in bottom GVA quintile of LAs
GVA5boundsLOW <- LADbounds %>% filter(gvaquinspercapita ==1) #1 is lowest GVA
LSOA_GVA5 <- st_intersection(LSOAcentroidsLEAST,GVA5boundsLOW)
DZ_GVA5 <- st_intersection(datazonecentroidsLEAST,GVA5boundsLOW)
NI_GVA5 <- st_intersection(NI.centroidsLEAST, GVA5boundsLOW) #NOTE THERE ARE NO LAs in NI with high GVA.

#list the LSOAcodes to serve as filter for population data.
lsoaCALCS <- lsoapops %>% filter(GEOGRAPHY_CODE %in% LSOA_GVA5$lsoa11cd)
dzCALCS <- datazonecentroids %>% filter(DataZone %in% DZ_GVA5$DataZone)
NICALCS <- NI.pops %>% filter(Area_Code %in% NI_GVA5$SA2011)

#Population count of people in LEAST deprived n'hoods also in high GVA (quintile 5) local authorities)
bigsumleast <- sum(lsoaCALCS$OBS_VALUE) + sum(dzCALCS$TotPop2011) + sum(NICALCS$`2018`)
bigsumleast

#proportion of LEAST deprived LSOAs in GVA5 areas
count_GVA5_depLSOAS <- nrow(LSOA_GVA5) + nrow(DZ_GVA5) + nrow(NICALCS)
totdeprivednhoodsUK <- nrow(LSOAcentroids2) + nrow(datazonecentroids) + nrow(NI.centroids)
proportion <- count_GVA5_depLSOAS / totdeprivednhoodsUK
nrow(LSOA_GVA5)

check <- nrow(LSOA_GVA5)/nrow(LSOAcentroids2)
check

############Have to read in the centroids again as were filtered to only have least deprived ###############
#Count of population on LEAST deprived n'hppds
LSOAcentroids <- geojson_sf("https://opendata.arcgis.com/datasets/b7c49538f0464f748dd7137247bbc41c_0.geojson")
lsoaleast <- lsoapops %>% filter(GEOGRAPHY_CODE %in% LSOAcentroidsLEAST$lsoa11cd)

datazonecentroids <- read_sf("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/4/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryMultipoint&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")
dzleast <- datazonecentroids %>% filter(DataZone %in% datazonecentroidsLEAST$DataZone)


#NI
NI.soas <- readOGR(layer = "SA2011", dsn = "SA2011_Esri_Shapefile_0") #file too big for github have to DL from https://www.nisra.gov.uk/publications/small-area-boundaries-gis-format
temp <- SpatialPointsDataFrame(gCentroid(NI.soas, byid=TRUE), 
                               NI.soas@data, match.ID=FALSE)

#NI.centroids <- st_as_sf(temp)
NI.centroids <- spTransform(temp, CRS("+proj=longlat +datum=WGS84")) #need to convert coordinates to long lat from UTM/BNG
NI.centroids <- st_as_sf(NI.centroids)
NI.pops <- read_excel("SAPE18_SA_Totals.xlsx", 
                      sheet = "Tabular", range = "A17:S4552")
NI.pops <- NI.pops %>% select(Area_Code, `2018`)
NI.centroids <- merge(NI.centroids, NI.pops, by.x = "SA2011", by.y = "Area_Code", all.x = T)


all_leastpops <- sum(lsoaleast$OBS_VALUE) + sum(dzleast$TotPop2011) + sum(NIleast$`2018`)
bigsumleast / all_leastpops * 100




#MAKE INTO TABLES

#Adds the GVA quintiles via spatial query

LSOAcentroids <- geojson_sf("https://opendata.arcgis.com/datasets/b7c49538f0464f748dd7137247bbc41c_0.geojson")


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

#add imd deciles.
gva_nhoods <- merge(gva_nhoods,IMD19all, by.x = "lsoa11cd", by.y = "LSOA code (2011)", all.x = T)

simd.df <- read_excel("SIMD+2020v2+-+ranks.xlsx", sheet = "SIMD 2020v2 ranks")
simd.df <- select(simd.df, Data_Zone, SIMD2020v2_Rank) #prune
simd.df <- simd.df %>% mutate(deciles = ntile(SIMD2020v2_Rank,10)) #deciles
gva_nhoodsS <- merge(gva_nhoodsS, simd.df, by.x = "DataZone", by.y = "Data_Zone", all.x = T)

##### Northern Ireland #####
niimd.df <- read_excel("NIMDM17_SA - for publication.xls", sheet = "MDM")
niimd.df <- niimd.df[,c(3,5)]
niimd.df <- niimd.df %>% rename(`IMD Rank` = 2)
niimd.df <- niimd.df %>% mutate(deciles = ntile(`IMD Rank`,10))

gva_nhoodsNI <- merge(gva_nhoodsNI, niimd.df, by.x = "SA2011", by.y = "SA2011", all.x = T)

#make the data from which we construct the summary table
lsoa_table <- tibble(lsoa_code = c(gva_nhoods$lsoa11cd, 
                                   gva_nhoodsS$DataZone, 
                                   gva_nhoodsNI$SA2011),
                     
                     imddecile = c(gva_nhoods$`IMD decile (1 is most deprived)`, 
                                   gva_nhoodsS$deciles, 
                                   gva_nhoodsNI$deciles),
                     
                     pops = c(gva_nhoods$X2018, 
                              gva_nhoodsS$TotPop2011, 
                              gva_nhoodsNI$X2018),
                     
                     GVAquin = c(gva_nhoods$gvaquinspercapita, 
                                 gva_nhoodsS$gvaquinspercapita, 
                                 gva_nhoodsNI$gvaquinspercapita)
)

t1 <- lsoa_table %>% filter(imddecile == 1)  %>% group_by(GVAquin)  %>% summarise("Total pop" = sum(na.omit(pops)))




