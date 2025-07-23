### Rivers of Spain

# Set here the path to the folder were you have spain_provinces.Rda and the shapefiles HydroRIVERS_v10_eu and hybas_eu_lev04_v1c

shapes<-"C:/Users/jgaleano/Desktop/ADAM/SESSION_1/shps/"

library(sf)
library(tidyverse)

load(paste(shapes, "spain_provinces.Rda", sep="")) # READ SF_OBJECT OF SPAIN BY PROVINCES 

esp<-esp|>filter(NAME_1!="Islas Canarias") # GET RIDE OF THE CANARY ISLANDS 

# PLOT FOR VISUAL INSPECTION

ggplot(esp) + 
  geom_sf()+
  theme_bw()

# Let's get the bbox of object esp

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

# Convert to SpatVector

library(terra)
river_vect <- vect(country_rivers)
basin_vect <- vect(basin)
esp_vect <- vect(esp)

# Perform intersection rivers and basin

rivers_esp_vect <- intersect(river_vect, basin_vect)
rivers_esp <- st_as_sf(rivers_esp_vect) # convert to sf object

# Perform intersection rivers inside the spanish boundaries

rivers_esp_vect2 <- intersect(rivers_esp_vect, esp_vect) 
rivers_esp2 <- st_as_sf(rivers_esp_vect2)  # convert to sf object

rivers_esp2<-rivers_esp2|>
  mutate(ORD_FLOW=as.factor(ORD_FLOW)) # coerce variable ORD_FLOW to factor 

# Define 7 sizes to connect the with the 7 levels of ORD_FLOW

mysizes <-c(.1,
            .075,
            .05,
            .025,
            .012,
            .007) 

# PLOT YOUR MAP 

ggplot() +
  geom_sf(data = rivers_esp2, aes(linewidth = ORD_FLOW, color = as.factor(HYBAS_ID))) +
  scale_linewidth_manual(values = mysizes * 25) + # YOU CAN ADJUST THE LINEWIDTH OF THE RIVERS CHANGING 25 TO ANY OTHER VALUE
  theme_void() +
  theme(legend.position = "none",
        plot.background =  element_rect(fill = "black"))

# SAVE YOUR MAP AS A PNG FILE

ggsave("Rivers.png", # name of the file of the image
       scale = 1, 
       dpi = 300,     
       height =25, 
       width = 24)
