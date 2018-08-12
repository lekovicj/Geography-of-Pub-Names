library(tidyverse)
library(osmdata)
library(jsonlite)


# this could be handy: http://www.realalehunter.co.uk/pubs?page=2

# so could this http://www.beerandpub.com/industry-briefings/alcohol-consumption-data

# and this http://www.beerandpub.com/statistics

queries <- c("Scotland", "Northern Ireland","East Midlands","East of England", "London", "North East England","North West England","South East England","South West England","West Midlands","Yorkshire and the Humber", "Wales")

osmdata::overpass_status()

for (i in 1:length(queries)) {
    
    ### but using OSMdata ####
    q0 <- add_osm_feature(opq = opq(bbox = queries[i]), 
                          key = "amenity", value = "pub", 
                          key_exact = T, value_exact = T)
    x <- osmdata_sf(q0)
    
    y <- osmdata_sp(q0)
    
    ## create points
    
    points <- x$osm_points %>% 
        as_data_frame() %>%        
        mutate(long =  purrr::map_dbl(.x = geometry, 1)) %>% 
        mutate(lat = purrr::map_dbl(.x = geometry, 2))
    
    #summarise polygons
    
    polygons <- fortify(y$osm_polygons) %>% 
        as_data_frame()  
    
    polygons$id <- as.character(polygons$id)
    
    y$osm_polygons@data$id <- (rownames(y$osm_polygons@data))
    
    polygons <- polygons %>% 
        left_join(y = y$osm_polygons@data, by = "id") %>% 
        filter(order == 1)%>% 
        as_data_frame()
    
    #summarise multiplpolygons
    
    #multipolygons <- fortify(y$osm_multipolygons) %>% as_data_frame()
    
    #y$osm_multipolygons@data$id <- as.character(rownames(y$osm_multipolygons@data))
    
    #multipolygons <- multipolygons %>% 
    #    left_join(y = y$osm_multipolygons@data, by = "id") %>% 
   #     filter(order == 1) %>% 
    #    as_data_frame()
    
    ### merge all data
    
    osmPubs <- bind_rows(points, polygons) %>% 
        as_data_frame() %>%
        distinct() 

    pubsClean <- osmPubs %>% 
        mutate(name = if_else(is.na(name), true = as.character(alt_name), false = as.character(name))) %>%
        filter(!is.na(name)) %>%
        #select(name, city = addr.city, postcode = addr.postcode, geometry) %>%
        select(name,lat,long) %>%
        rename(lon = long) %>%
        as_data_frame()
    
    ggplot(data = pubsClean) + 
        geom_point(mapping = aes(x = lon, y = lat), size = 0.01, alpha = 0.5)+
        theme_minimal()+
        coord_map()
    
    write_csv(pubsClean, path = "data/osmPubs.csv", append = T, col_names = F)
    print(queries[i])
    
    Sys.sleep(10)
}

fullOSMPubs <- read_csv(file = "data/osmPubs.csv", col_names = c("name", "lat", "lon")) 

fullOSMPubs <- fullOSMPubs %>%
    distinct()

ggplot(data = fullOSMPubs) + 
    geom_point(mapping = aes(x = lon, y = lat), size = 0.01, alpha = 0.1)+
    theme_minimal()+
    coord_map()


### now get postcodes ####
pubs <- read_csv(file = "data/osmPubs.csv", col_names = F) 

colnames(pubs) <- c("name", "lat", "lon")

for(i in 20001:23427){ 
    
    postcodeCall <- paste0("https://api.postcodes.io/postcodes?lon=",pubs$lon[i],
                           "&lat=",pubs$lat[i])
    
    postcodeRAWData <- fromJSON(txt = postcodeCall, flatten = T)
    
    print( postcodeRAWData$status) 
    
    postcodeData <- as_data_frame(list(name = pubs$name[i], lat = pubs$lat[i],lon = pubs$lon[i]))
    
    ifelse(test = is.null(postcodeRAWData$result), 
           yes =  postcodeData <- postcodeData,
           
           no = postcodeData <- postcodeData %>%
               full_join(postcodeRAWData$result %>% mutate(name = pubs$name[i])) %>% 
               filter(distance == min(distance)) )
    
    
    write_csv(postcodeData, path = "data/osmPostcodeData.csv", append = T, col_names = F)
    
    print(pubs$name[i])
    print(i)
    #Sys.sleep(1)
    
}


pubsPlusPostcodes <- read_csv(file = "data/osmPostcodeData.csv")

