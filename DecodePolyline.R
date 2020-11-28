#Load packages
library(xfun)
xfun::pkg_attach(c('googlePolylines','tidyverse','sf','units','smoothr'), install=TRUE)
#Read data
polycity <- read.table('polygoncity.csv', header=TRUE, sep=',', stringsAsFactors=FALSE, allowEscapes=TRUE, quote="\"")
#Transform data
polycity <- polycity %>%
            mutate(polygons=strsplit(polygons, ",")) %>% #Split multiple polygons
            unnest(polygons) %>% #Split to multiple lines
            mutate(polygons=str_replace_all(polygons, c("^\\[" = "", "\\]$" = "", "\"" = ""))) %>% #Delete starts and finishes with [] and quote \
            filter(nchar(polygons)>15) #Delete small polygons / lines
#Create empty sf object
polycitysf <- st_sf(country_code=polycity$country_code,
                    city_code=polycity$city_code,
                    geometry=st_sfc(lapply(1:nrow(polycity), function(x) st_geometrycollection())), crs=4326)
#Loop decode polygons
for (i in c(1:nrow(polycity))){
    coordinates <- bind_rows(decode(polycity$polygons[[i]]))
    polycitysf$geometry[i] <- coordinates %>%
                              st_as_sf(coords=c("lon","lat"), crs=4326) %>%
                              summarise(geometry=st_combine(geometry)) %>%
                              st_cast("POLYGON") %>% 
                              st_geometry()
}
#Merge by city
polycityfinal <- polycitysf %>% 
                 group_by(country_code,city_code) %>%
                 summarise(geometry=st_union(geometry)) %>%
                 ungroup() %>%
                 as.data.frame() %>%
                 st_sf() %>% 
                 fill_holes(threshold=set_units(0.5,km^2)) # Merge small spaces
#Merge by country
kml_polygons <- polycityfinal %>% 
                filter(country_code=="PE") %>% 
                select(Name=city_code)
#Export to KML
st_write(kml_polygons, "PE.kml", driver="kml", delete_dsn=TRUE)
