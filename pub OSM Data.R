library(tidyverse)
library(geosphere)
library(extrafont)

decimals <- 1


# Read in and process Pub name data ---------------------------------------

pubs <- read_csv(file = "data/osmPubs.csv", col_names = F) 

colnames(pubs) <- c("name", "lat", "lon")

pubs <- pubs %>%
    select(name,  lon, lat) %>%
    #filter(lat != "\\N") %>%
    #filter(lon != "\\N") %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon)) %>%
    unique() %>%
    mutate(lat0 = round(lat, decimals)) %>%
    mutate(lon0 = round(lon, decimals)) %>%
    mutate(id = row_number())
    

pubs <- pubs %>%
    mutate(name = stringr::str_to_title(name)) %>%
    mutate(nameClean = str_to_lower(name)) %>%
    
    mutate(nameClean = str_replace_all(nameClean, pattern = "&", replacement = "and")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "@", replacement = "at")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "'", replacement = "")) %>%
    
    mutate(nameClean = str_replace_all(nameClean, pattern = "[^[:alnum:]]", replacement = " ")) %>%
    
    mutate(nameClean = str_replace_all(nameClean, pattern = ",", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "[.]", replacement = "")) %>%
    
    mutate(nameClean = str_replace_all(nameClean, pattern = "\\)", replacement = " ")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "\\(", replacement = " ")) %>%
    
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)the(?:$|\\W)", replacement = " ")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)ltd(?:$|\\W)", replacement = " ")) %>%
    
    mutate(nameClean = str_trim(string = nameClean)) %>%
    
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)pub(?:$|\\W)", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)restaurant(?:$|\\W)", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)wet sales only(?:$|\\W)", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)wet sales(?:$|\\W)", replacement = "")) %>%
    
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)ph(?:$|\\W)", replacement = " public house ")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)p h(?:$|\\W)", replacement = " public house ")) %>%
    
    mutate(inn = str_detect(nameClean, "(?:^|\\W)inn(?:$|\\W)")) %>%
    mutate(ph = str_detect(nameClean, "(?:^|\\W)public house(?:$|\\W)")) %>%
    mutate(tavern = str_detect(nameClean, "(?:^|\\W)tavern(?:$|\\W)")) %>%
    mutate(bar = str_detect(nameClean, "(?:^|\\W)bar(?:$|\\W)")) %>%
    mutate(new = str_detect(nameClean, "(?:^|\\W)new(?:$|\\W)")) %>%
    mutate(old = str_detect(nameClean, "(?:^|\\W)old(?:$|\\W)")) %>%
    mutate(hotel = str_detect(nameClean, "(?:^|\\W)hotel(?:$|\\W)")) %>%
    
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)hotel(?:$|\\W)", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)public house(?:$|\\W)", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)inn(?:$|\\W)", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)bar(?:$|\\W)", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)tavern(?:$|\\W)", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)old(?:$|\\W)", replacement = "")) %>%
    mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)new(?:$|\\W)", replacement = "")) %>%
    
    mutate(asIs = if_else( tavern | bar | ph | hotel | inn, 
                           true = FALSE, false = TRUE)) %>%
    
    mutate(the = str_detect(string = str_to_lower(name), pattern = "(?:^|\\W)the(?:$|\\W)")) %>% # from https://superuser.com/questions/903168/how-should-i-write-a-regex-to-match-a-specific-word
    mutate(ye = str_detect(string = str_to_lower(name), pattern = "(?:^|\\W)ye(?:$|\\W)")) %>%
    mutate(nameClean = str_trim(nameClean)) %>%
    group_by(nameClean)%>%
    mutate(nameFreq = n()) %>%
    ungroup(nameClean)%>%
    arrange(desc(nameFreq)) 


# Calculate Name Metrics --------------------------------------------------
nameFrequencies <- pubs %>%
    group_by(nameClean)%>%
    summarise(nameFreq = n(), 
              maxLat = max(lat), 
              maxLon = max(lon),
              minLat = min(lat), 
              minLon = min(lon), 
              meanLat = mean(lat), 
              meanLon = mean(lon)) %>%
    arrange(desc(nameFreq)) 


ggplot(nameFrequencies %>% filter(str_detect(nameClean, "")) %>% filter(row_number() < 30))+
    geom_col(aes(x = reorder(x = str_to_upper(nameClean), X = nameFreq), y = nameFreq), fill = "red")+
    geom_text(aes(x = reorder(x = str_to_upper(nameClean), X = nameFreq), y = nameFreq+15, label = nameFreq))+
    theme_minimal()+
    coord_flip()+
    ggplot2::ggtitle(label = "Most Common Pub Names")+
    theme(line = element_blank(), axis.title = element_blank(), axis.text.x = element_blank())


# Add typical names -------------------------------------------------------

typicalNames <- pubs %>%
    select(name, nameClean) %>%
    group_by(nameClean,name) %>%
    summarise(count = n()) %>%
    arrange(nameClean, desc(count)) %>%
    group_by(nameClean) %>%
    #mutate(count = n()) %>%
    top_n(n = 1, wt = row_number()) %>%
    arrange(desc(count))%>%
    select(nameClean, nameTypical = name) %>%
    ungroup() 

pubs <- pubs %>%
    left_join(y = typicalNames, by = "nameClean")

rm("typicalNames")


# Categorise based on types -----------------------------------------------

nameTypes <- pubs %>%
    select(nameClean, inn, ph, tavern, bar, hotel, asIs) %>%
    gather(key = "type", value = "TorF", 2:7) %>%
    filter(TorF) %>%
    mutate(type = str_replace(string = type, "ph", "public house")) %>%
    mutate(type = str_replace(string = type, "asIs", "as is")) %>%
    group_by(nameClean, type) %>%
    count() %>%
    arrange(nameClean) 



# Create some data for uk outline -----------------------------------------


UK <- map_data(map = "world", region = "UK") 

library(raster)
UK2 <- getData('GADM', country='United Kingdom', level=0)
UK2 <- spTransform(x = UK2, CRSobj = CRS("+proj=longlat +datum=WGS84") )
UK2 <- fortify(UK2)


# also this looks sexy...
districts <-  getData('GADM', country='United Kingdom', level=2)
districts <- spTransform(districts, CRS("+proj=longlat +datum=WGS84"))

districts$areaSqKM <- area(districts) /1000000
sum(districts$areaSqKM)

plot(districts)

#districts@data <- districts@data %>% mutate(NAME_2 = if_else(TYPE_2 == "London Borough", true = "London", false = NAME_2)) 

districtsDF <- fortify(districts@data)

pubsSpatial <- pubs %>% 
    dplyr::select(lon, lat)

coordinates(pubsSpatial) <- ~lon+lat
pubsSpatial <- SpatialPoints(pubsSpatial, proj4string = CRS("+proj=longlat +datum=WGS84"))

mergedPubs <-  over(x = pubsSpatial, y = districts, returnList = F)

spacialflat <-  pubsSpatial@coords %>% 
    as_data_frame() %>%
    mutate(id = row_number())

mergedPubs <- mergedPubs %>%
    mutate(id = row_number())


mergedPubs <- left_join(x = mergedPubs, y= spacialflat, by = "id")


detach(package:raster, unload = T)
library(tidyverse)

mergedPubs <- mergedPubs %>% 
    dplyr::select(region = NAME_1, district = NAME_2, lat, lon, areaSqKM)

pubs <-  pubs %>%
    left_join(mergedPubs, by = c("lat", "lon")) %>%
    filter(!is.na(region))

rm(list = c( "districts", "mergedPubs", "pubsSpatial", "spacialflat"))

pubs %>% 
    group_by(district) %>% 
    summarise(n = n(), area = mean(areaSqKM), density = n/area) %>% # View()
    arrange(desc(density)) 

pubs %>% group_by(district) %>% count() %>% arrange(desc(n)) %>% ungroup() %>% filter(row_number() < 20 ) %>%
    ggplot()+
    geom_col(aes(x = reorder(x = (district), X = n), y = n), fill = "red")+
    geom_text(aes(x = reorder(x = (district), X = n), y = n, label = n), colour= "white", hjust = 1.2)+
    theme_minimal()+
    coord_flip()+
    theme(line = element_blank(), axis.title = element_blank())


### omg this works


# Create summaries for districts ------------------------------------------
library(tidyverse)

namesPerRegion <- pubs %>% 
    group_by(nameClean, district, nameFreq) %>%
    summarise(count = n(),lon = mean(lon),lat = mean(lat)) %>%
    arrange(desc(count), nameClean) %>%
    mutate(proportion =  count/nameFreq) %>%
    ungroup()  %>%
    left_join(y = pubs %>% group_by(district) %>% count(), by = "district") %>%
    rename(regionTotal = n) %>%
    mutate(proportionOfRegion = count/regionTotal)




# Name plotting function --------------------------------------------------


makeMeAMap <- function(pubName = "ship", pubColour = "red", sizing = 0.7, pubBackground = "blue") {
    
    
    typeSummary <- nameTypes %>% 
        ungroup() %>%
        filter(nameClean == pubName) %>%
        #filter(type != "as is") %>%
        #filter(type != "hotel") %>%
        #filter(type != "bar") %>%
        arrange((n)) %>%
        mutate(lat = desc(row_number (n)) + 56) %>%
        mutate(prop = n/sum(n)) %>%
        mutate(prop5 = prop *5) %>%
        mutate(prop5Run = cumsum(prop5)) %>%
        mutate(lonStartText = -14.5) %>%
        mutate(lonStart = lonStartText) %>%
        mutate(lonStart = lag(lonStart+prop5Run, default = -14.5)) %>%
        mutate(lonEnd = lonStart + prop5) %>%
        mutate(type = if_else(type == "as is",  str_replace(type, pattern = "as is", nameClean), paste(nameClean, type, sep = " "))) %>%
        rename(count = n) %>%
        mutate(type = str_to_title(type))
    
    
    regionLabel <- namesPerRegion %>% 
        filter(nameClean == pubName) %>%
        arrange(desc(proportion), desc(proportionOfRegion)) %>%
        filter(row_number() == 1) %>%
        mutate(label = paste0(count," in ",district))

    ### plot the pubs by name ####
    
    pubs %>%
        filter(nameClean == pubName) %>%
        ggplot() +
        geom_blank(data = NULL, aes(x = 2, y = 60))+
        geom_blank(data = NULL, aes(x = -20, y = 50))+
        
        
        ggplot2::annotate(geom = "text", x = -12, y = 56.5, ## plotting pub name
                          label = str_wrap(str_to_upper(pubName), width = 5),
                          size = 14, colour = pubColour,
                          hjust = 0.5, vjust = 0,
                          family = "Open Sans", fontface = "bold",
                          lineheight = 0.8) +
        
        #### add labels for top region
        geom_text(data = regionLabel, 
                  aes(x = lon, y = 49.5, label = label), 
                  size = 4, colour = pubBackground,
                  hjust = 1, vjust = -0.5, 
                  nudge_x = -0.1, nudge_y = -0.2,
                  family = "Open Sans", fontface = "italic")+
        
        geom_segment(data = regionLabel, 
                     aes(x = lon, y = 49.5, xend = lon, yend = lat), 
                     colour = pubBackground, size = 0.5, lineend = "square")+
        
       #uk map
        geom_path(data = UK, ##plotting map of GB
                  mapping = aes(x = long, y = lat, group = group),
                  colour = pubBackground, lineend = "round", 
                  linejoin = "round", linetype = "solid", 
                  size = 0.5)+ 
        
        #### add typical name
        geom_label(data = pubs %>% filter(nameClean == pubName) %>% filter(lat == max(lat)) %>% mutate(name = str_replace_all(name, " ", "\n")), 
                   aes(x = lon+0.3, y = lat+0.3, label = name), 
                   size = 3, colour = pubColour,
                   hjust = 0, vjust = 0, 
                   family = "Open Sans", fontface = "italic", 
                   fill = "white", label.size=NA)+
        
        geom_segment(data = pubs %>% filter(nameClean == pubName) %>% filter(lat == max(lat)), 
                     aes(x = lon+0.01, y = lat+0.01, xend = lon+0.3, yend = lat+0.3), colour = pubColour)+
         
        #plotting actual points
        geom_point(aes(x = lon, y = lat), colour = pubColour, shape = 20, size = sizing) + 
        #geom_point(aes(x = lon, y = lat), size = 0.001, colour = pubBackground, shape = 20) + 
        
        ###break line
        geom_segment(aes(x = -14, xend = -10, y = 56, yend = 56), size = 1, colour = pubBackground)+
        
        # add type metrics
        geom_segment(data = typeSummary, 
                     aes(x = lonStart, xend = lonEnd, y = lat, yend = lat), 
                     colour = pubColour, size = 1)+
        
        geom_segment(data = typeSummary, 
                     aes(x = lonStartText, xend = lonStartText+5, y = lat, yend = lat), 
                     colour = pubBackground, size = 0.3, linetype = "dotted")+
        
        ##add details for types
        geom_text(data = typeSummary, aes(x = lonStartText, y = lat, label = type), 
                  size = 4, colour = pubColour,
                  hjust = 0, vjust = -0.5,
                  family = "Open Sans")+
        
        ## add numerics for types
        geom_text(data = typeSummary, 
                  aes(x = lonStartText, y = lat, label = count), 
                  size = 6, colour = pubBackground,
                  hjust = 1, vjust = -0.5, 
                  nudge_x = -0.1, nudge_y = -0.2,
                  family = "Open Sans", fontface = "italic")+
        
    coord_map(projection = "mercator")+
        
        theme(
            line = element_blank(),
            text = element_blank(),
            title = element_blank(),
            rect = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white", colour = "white"),
            plot.background = element_rect(fill = "white", colour = "white")
        ) 
    
    
}


#colorRampPalette(colors = c("#005377","#3E2F5B","#901132", "#ED6028")) -> colourScale

#colorRampPalette(colors = c("blue","red","green","orange")) -> colourScale


makeMeAMap(pubColour = "red", pubBackground = "blue", sizing = 1, pubName = "ship") 


# Loop to create all name maps --------------------------------------------

nameFrequenciesSubset <- nameFrequencies %>% filter(nameFreq > 50)

for (i in 1: nrow(nameFrequenciesSubset) ) {

    #colourScaleColours <- colourScale(n = nrow(nameFrequenciesSubset))
    
    makeMeAMap(pubName = nameFrequencies$nameClean[i], 
               pubColour = "red",
               sizing = 1-nameFrequencies$nameFreq[i]/460+0.001, 
               pubBackground = "blue")

    
    ggsave(filename = paste0("images/", nameFrequencies$nameClean[i], ".png"),
        dpi = 350, scale = 1,
        width = 6, height = 6,
        units = "in", device = "png")
    
    print(nameFrequencies$nameClean[i])
    print(i)
    
}


# Plot all the pubs -------------------------------------------------------


pubs %>%
    #filter(tavern) %>%
    ggplot() +
    geom_blank(data = NULL, aes(x = 3, y = 60))+
    geom_blank(data = NULL, aes(x = -7, y = 50))+
    
    ## plotting title
    ggplot2::annotate(geom = "text",x = -3, y = 54, 
                      label = "LOTS\nOF\nPUBS",
                      size = 25, colour = "red", 
                      hjust = 0.5, vjust = 0.45,
                      family = "Open Sans", fontface = "bold",
                      lineheight = 0.8, angle = 0) +
    
    # plotting points
    geom_point(aes(x = lon, y = lat), shape = 20, size = 0.01, colour = "blue", alpha = 0.3) + 
    theme(
        line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        rect = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white")
    ) +
    coord_map(projection = "mercator")


ggsave(filename = paste0("images/","lots of pubs", ".png"),
       dpi = 360, scale = 1,
       width = 6, height = 6,
       units = "in", device = "png")

#####get metadata on types ####

pubs %>% 
    filter(str_detect(nameClean, "black"))%>%
    group_by(nameClean) %>%
    summarise(nameFreq = mean(nameFreq)) %>%
    distinct() %>%
    arrange(desc(nameFreq)) 

#black
#white
#green
#jolly
#lord
#admiral
#moon & moonraker
#horse
#bull
#horse
#kings
#queens
#cheese
#arms

makeMeMoreMaps <- function(keyword = "green") {
    
    nameFreqFiltered <- nameFrequencies %>% 
        filter(str_detect(nameClean, keyword)) 
    
    for (i in 1:nrow(nameFreqFiltered)) {
        
        makeMeAMap(pubName = nameFreqFiltered$nameClean[i], 
                   pubColour = "red",
                   sizing = 1-nameFreqFiltered$nameFreq[i]/460+0.001)
        
        
        ggsave(filename = paste0("images/MoreOSMMaps/",keyword,"-", nameFreqFiltered$nameClean[i], ".png"),
               dpi = 350, scale = 1,
               width = 6, height = 6,
               units = "in", device = "png")
        
        print(nameFreqFiltered$nameClean[i])
        print(paste(i," of ", nrow(nameFreqFiltered)) )
        
    }}


