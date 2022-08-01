

install.packages("tidygeocoder")
require(tidygeocoder)
require(tm)
require(sf)
require(nngeo)
require(parallel)

head(organizations_no_pat)


pat_sample<- patentes_reg[1:100,]

pat_count <- patentes_reg %>% 
  group_by(app_name) %>% 
  summarize(count=n())


pat_count_han <- patentes_han %>% 
  group_by(Clean_name) %>% 
  summarize(count=n())


head(patentes_reg,1)
head(patentes_han)
colnames(pat_sample)

lat_longs <- pat_sample %>%
  geocode(street = address, 
          city = city, 
          postalcode = postal_code,
          country = ctry_code, 
          method = 'osm')



head(ranking2022)

uni_rankings <- ranking2022[,1:2]
uni_rankings <- distinct(uni_rankings)

colnames(uni_rankings)

uni_rankings$place <- paste0(uni_rankings$University,", ",uni_rankings$Country)

tmaptools::geocode_OSM("Casa de Nariño, Bogotá",as.sf=T) 


geocode_unis <- apply()

lat_longs <- uni_rankings %>%
  geocode(address = place, 
          method = 'osm')

#lat_longs$geolocation <- paste0(lat_longs$lat,",",lat_longs$long)

colSums(is.na(ranking_geoloc))

ranking_geoloc <- lat_longs

nrow(lat_longs)
lat_longs <- left_join(lat_longs, H2020_organization[,c("geolocation","organisationID")],
                       by="geolocation")
nrow(lat_longs)

ranking_geoloc <- ranking_geoloc[!is.na(ranking_geoloc$lat),]


ranking_geoloc <- st_as_sf(ranking_geoloc ,coords=c('long','lat'),crs=4326)

universities_sf <- universities
universities_sf$geolocation2 <- universities_sf$geolocation
universities_sf <- universities_sf %>% separate(geolocation, c('lat', 'long'),sep=",")
universities_sf <- universities_sf[!is.na(universities_sf$lat),]
universities_sf <- st_as_sf(universities_sf,coords=c('long','lat'),crs=4326)


ranking_geoloc <- st_join(ranking_geoloc,universities_sf[,c("organisationID")])

ranking_geoloc_NA <- filter(ranking_geoloc,is.na(ranking_geoloc$organisationID))

ranking_geoloc_NA <- st_join(ranking_geoloc_NA,universities_sf[,c("organisationID")],
                             join = st_nn, k = 1, maxdist = 5000, parallel=8)

colSums(is.na(ranking_geoloc_NA))
colnames(ranking_geoloc_NA)
colnames(ranking_geoloc_NA)[10] <- "organisationID"



ranking_geoloc_NA <- left_join(ranking_geoloc_NA,
                               universities[ ,c("organisationID","name")],
                                                              by="organisationID")


require(leaflet)
leaflet() %>% addTiles() %>% addCircleMarkers(data=ranking_geoloc,label=ranking_geoloc$place) %>% 
  addCircleMarkers(data=universities_sf,label=universities_sf$name, color="red")

