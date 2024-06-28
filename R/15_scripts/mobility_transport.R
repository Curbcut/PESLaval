#Loading up libraries
source("R/01_startup.R")
library(gbfs)
library(sf)
library(tidytransit)
library(osmdata)
library(tidygeocoder)


#Setting up the cancensus api key
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc", install = TRUE)

#Setting up cache path for faster loading (edit for your own folder)
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

#Grabbing the Laval shapefiles
laval_csd <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CSD", 
                                  geo_format = "sf")

laval_ct <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CT", 
                                  geo_format = "sf")

mtlcma_sf <- cancensus::get_census(dataset = "CA21", 
                                regions = list(CSD = 24462), 
                                level = "CMA", 
                                geo_format = "sf")

# Bus Stop Location -------------------------------------------------------
#Grabbing GTFS data from the STL
stl_gtfs <- read_gtfs("https://translate.google.com/website?sl=fr&tl=en&hl=en&prev=search&u=https://www.stlaval.ca/datas/opendata/GTF_STL.zip")

#Grabbing bus stops, converting it to be usable to plot, and removing duplicates
#Removing duplicates doesn't necessarily reflect real life as there are usually
#two stops at one intersection, but makes map readability significantly better
bus_stops <- st_as_sf(stl_gtfs$stops, coords = c("stop_lon", "stop_lat"), crs = 4326) |> 
  distinct(stop_name, .keep_all = TRUE)

laval_bbox <- st_bbox(laval_ct)

#Plotting the bus stops
ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey", color = "black") +
  geom_sf(data = laval_ct, fill = "white", color = "black") +
  geom_sf(data = bus_stops, color = "indianred3", size = 0.65) +
  theme_minimal() +
  labs(title = "Arrêts d'autobus de la Société de transport de Laval 2024") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

# Cycle Paths -------------------------------------------------------------
laval_bbox <- st_bbox(laval_ct)

cycle_osm <- opq(bbox = laval_bbox, timeout = 300) |> 
  add_osm_feature(key = "highway", value = "cycleway") |> 
  osmdata_sf()

cycle_lvl <- cycle_osm$osm_lines |> 
  st_intersection(laval_ct)

ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey", color = "black") +
  geom_sf(data = laval_csd, fill = "white", color = "black") +
  geom_sf(data = cycle_lvl, color = "darkred", size = 5) +
  coord_sf() +
  theme_minimal() +
  labs(title = "Pistes cyclables à Laval 2024") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))
# Bixi Stations -----------------------------------------------------------
#Grabbing all Bixi station information
bixi_stations <- get_station_information(city = "Montreal")

#Converting the bixi station information into a usable coordinate system
bixi_sf <- st_as_sf(bixi_stations, coords = c("lon", "lat"), crs = 4326) |> 
  filter(name != "cpatel") |> 
  st_intersection(laval_ct)

#Mapping out the stations onto the city of Laval
ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey", color = "black") +
  geom_sf(data = laval_ct, fill = "white", color = "black") +
  geom_sf(data = bixi_sf, color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Stations Bixi à Laval 2024") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

# Points de vente (transport collectif) -----------------------------------
vending_locations <- tibble::tribble(
  ~name, ~latitude, ~longitude, ~accessible,
  "IGA Extra Auteuil", 45.6282351, -73.7530393, "Partiellement accessible",
  "Marché Métro Plus Messier Auteuil", 45.6197691, -73.7457887, "Partiellement accessible",
  "Pharmaprix Vimont", 45.6123331, -73.7379836, "Accessible",
  "Boni-Soir Chomedey", 45.5356598, -73.7356945, "Non accessible",
  "Dépanneur Samson", 45.5329987, -73.7515077, "Partiellement accessible",
  "Pharmaprix Samson", 45.5291308, -73.7564682, "Partiellement accessible",
  "Dépanneur Ultra SHPD", 45.5400341, -73.7676854, "Partiellement accessible",
  "Uniprix Saint-Martin", 45.5465966, -73.7756677, "Partiellement accessible",
  "Tabagie Chomedey", 45.5593529, -73.7669883, "Partiellement accessible",
  "Distributrice automatique de titres au terminus Le Carrefour", 45.5679670, -73.7500224, "Partiellement accessible",
  "Uniprix Chomedey", 45.5443549, -73.7413382, "Accessible",
  "Pharmaprix Centre Saint-Martin", 45.5434702, -73.7499175, "Partiellement accessible",
  "Jean Coutu Jacques Bourget", 45.59221, -73.66986, "Partiellement accessible",
  "Uniprix Santé Chantal Zeidan", 45.5938495, -73.6642460, "Non accessible",
  "Dépanneur O'Choix", 45.5660860, -73.8466308, "Partiellement accessible",
  "Jean Coutu Nicolas Rompré", 45.5779977, -73.8096451, "Partiellement accessible",
  "Dépanneur Guillemette", 45.58345,-73.79055, "Non accessible",
  "Billetterie métropolitaine et distributrice automatique de titres au terminus Montmorency", 45.5580379, -73.7211826, "Partiellement accessible",
  "COOP Cégep Montmorency", 45.5599827, -73.7193324, "Accessible",
  "Jean Coutu Jacques Bourget et Serge Dupras", 45.56603, -73.70100, "Partiellement accessible",
  "Dépanneur Maximax", 45.5582818, -73.6968398, "Non accessible",
  "Billetterie métropolitaine et distributrice automatique de titres au terminus Cartier", 45.5607445, -73.6816115, "Partiellement accessible",
  "Jean Coutu Jacques Bourget et Nick Campanelli", 45.5601260, -73.7125456, "Partiellement accessible",
  "Proxim Laval-des-Rapides", 45.549125843978764, -73.71375164102292, "Partiellement accessible",
  "Boni-Soir JR", 45.542120929401854, -73.87510982720579, "Partiellement accessible",
  "Dépanneur Wilson", 45.55120877457926, -73.87157585421808, "Non accessible",
  "Métro Plus Laval-Ouest", 45.54846742538103, -73.86429331386367, "Partiellement accessible",
  "Marché Belle-Rive", 45.56238800338405, -73.68066488793866, "Non accessible",
  "Métro Plus de la Concorde", 45.57380752195818, -73.6850926485661, "Partiellement accessible",
  "Jean Coutu Kahwati et Khoukaz", 45.56841020226774, -73.68940960995434, "Partiellement accessible",
  "Jean Coutu Benoit Desmarais et Alicia Desmarais", 45.6695431085693, -73.57593584525262, "Accessible",
  "Dépanneur de la Fabrique", 45.61345621105629, -73.64897271410416, "Non accessible",
  "Jean Coutu Véronique Paquet", 45.604684359931746, -73.64999792330536, "Partiellement accessible",
  "Dépanneur Principale", 45.52879786092138, -73.82162478860103, "Partiellement accessible",
  "Métro Denigil", 45.52695412693936, -73.812016200656, "Partiellement accessible",
  "Via Cassia Café et marché", 45.52389441549621, -73.85219401923001, "Partiellement accessible",
  "Jean Coutu Jean Perreault et Nicolas Rompré", 45.59065807446514, -73.78530505222554, "Partiellement accessible",
  "Dépanneur Sainte-Rose", 45.62803859980775, -73.78249583406077, "Partiellement accessible",
  "Dépanneur 7 jours", 45.606939603115535, -73.79145585771093, "Accessible",
  "Boni-Soir Vimont", 45.62091412987006, -73.72439105305266, "Non accessible"
)

ggplot(vending_locations) +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = laval_csd, fill = "white", color = "black") +
  geom_point(aes(x = longitude, y = latitude, color = accessible)) +
  scale_color_manual(values = c("Accessible" = "springgreen3", 
                                "Partiellement accessible" = "darkorange",
                                "Non accessible" = "firebrick3")) +
  labs(title = "Points de vente STL") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.title = element_blank(), legend.position = "bottom",
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))
