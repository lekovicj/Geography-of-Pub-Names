library(tidyverse)
library(osmdata)
library(jsonlite)


# this could be handy: http://www.realalehunter.co.uk/pubs?page=2
 
# so could this http://www.beerandpub.com/industry-briefings/alcohol-consumption-data
 
# and this http://www.beerandpub.com/statistics
 

queries <- c("East Midlands","East of England", "London", "North East England","North West England","South East England","South West England","West Midlands","Yorkshire and the Humber","Scotland", "Northern Ireland", "Wales")


for (i in 1:length(queries)) {
    
    Sys.sleep(1)

### but using OSMdata ####
q0 <- add_osm_feature(opq = opq(bbox = queries[i]), 
                       key = "amenity", value = "pub", 
                      key_exact = T, value_exact = T)
x <- osmdata_sf(q0)
 

osmPubs <- x$osm_points %>% 
    as_data_frame() %>%
    distinct()
    #bind_rows(x$osm_polygons %>% as_data_frame()) %>%
    #bind_rows(x$osm_multipolygons)

pubsClean <- osmPubs %>% 
    filter(!is.na(name)) %>%
    #select(name, city = addr.city, postcode = addr.postcode, geometry) %>%
    mutate(lon =  purrr::map_dbl(.x = geometry, 1)) %>% #flatten() %>%
    mutate(lat = purrr::map_dbl(.x = geometry, 2)) %>% #flatten() %>%
    select(name,lat,lon ) %>%
    as_data_frame()

ggplot(data = pubsClean) + 
    geom_point(mapping = aes(x = lon, y = lat), size = 0.01, alpha = 0.5)+
    theme_minimal()+
    coord_map()


write_csv(pubsClean, path = "data/osmPubs.csv", append = T, col_names = F)

fullOSMPubs <- read_csv(file = "data/osmPubs.csv", col_names = c("name", "lat", "lon")) 

fullOSMPubs <- fullOSMPubs %>%
    distinct()

ggplot(data = fullOSMPubs) + 
    geom_point(mapping = aes(x = lon, y = lat), size = 0.01, alpha = 0.1)+
    theme_minimal()+
    coord_map()

print(queries[i])

}


