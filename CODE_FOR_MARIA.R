### Exercise 2: Rivers of Spain

# Set here the path to the folder were you have spain_provinces.Rda and the shapefiles HydroRIVERS_v10_eu and hybas_eu_lev04_v1c
shapes<-"C:/Users/jgaleano/Desktop/ADAM/SESSION_1/shps/"

library(sf)
library(tidyverse)

esp  <- read_sf(dsn = shapes, "ESP_adm2")

load(paste(shapes, "spain_provinces.Rda", sep=""))

esp<-esp|>filter(NAME_1!="Islas Canarias")

ggplot(esp) + 
  geom_sf()+
  theme_bw()

esp_country <-esp %>% 
  group_by(ISO) %>% 
  summarise(NAME_prov = unique(ISO)) %>% 
  # st_buffer(0.5) %>% 
  st_cast() 

# note: the st_buffer function helps us cleaning sliver, however it can create some misalignment between layers. 

ggplot(esp_country) + 
  geom_sf()+
  theme_bw()

# Let's get the bbox of our map
st_bbox(esp)

# Define a bbox for filtering data from both the rivers and basin shapefiles 
bbox_wkt <- "POLYGON((
  -9.301806  35.170582,
  -9.301806  43.791527,
   4.328195  43.791527,
   4.328195  35.170582,
  -9.301806  35.170582  
))"


# Read data on rivers
country_rivers  <- read_sf(dsn = shapes, "HydroRIVERS_v10_eu",
                           wkt_filter = bbox_wkt) 

# Read data on basins
basin  <- read_sf(dsn = shapes, "hybas_eu_lev04_v1c",
                           wkt_filter = bbox_wkt) 


country_rivers<-country_rivers|>
  mutate(ORD_FLOW=as.factor(ORD_FLOW))



library(terra)

# Convert to SpatVector
river_vect <- vect(country_rivers)
basin_vect <- vect(basin)
esp_vect <- vect(esp)

# Perform intersection
rivers_esp_vect <- intersect(river_vect, basin_vect)
rivers_esp <- st_as_sf(rivers_esp_vect)

rivers_esp_vect2 <- intersect(rivers_esp_vect, esp_vect)
rivers_esp2 <- st_as_sf(rivers_esp_vect2)

rivers_esp2<-rivers_esp2|>
  mutate(ORD_FLOW=as.factor(ORD_FLOW))

mysizes <-c(.1,
            .075,
            .05,
            .025,
            .012,
            .007)

ggplot() +
  geom_sf(data = rivers_esp2, aes(linewidth = ORD_FLOW, color = as.factor(HYBAS_ID))) +
  scale_linewidth_manual(values = mysizes * 25) +
  theme_void() +
  theme(legend.position = "none",
        plot.background =  element_rect(fill = "black"))

ggsave(paste(images,"Rivers_4.png",sep=""), # name of the file of the image
       scale = 1, 
       dpi = 300,     
       height =25, #25  #10 
       width = 24)
