source("app/logic/seabass/telemetry_wrangle.R")
etn_monthyear_individual_sum
library(sf)
prepped_data <- prep_minicharts_inputs(TEL_deployments, etn_monthyear_individual_sum)
#had to load sf otherwise will throw an error about TEL_deployments being sfc_point
debugonce(prep_minicharts_inputs)
prepped_data$anim_df
head(readRDS("data/TEL_animals.rds"))
head(readRDS("data/TEL_deployments.rds"))
detections <- readRDS("data/TEL_detections.rds")


# Summarise by FishID -----------------------------------------------------------------------
# https://ocean-tracking-network.github.io/jb-acoustic-telemetry/07-plotting-data/index.html
animal_id_summary <- 
  detections %>% 
  group_by(animal_id) %>%
  summarise(dets = length(animal_id),
            stations = length(unique(station_name)),
            min = min(date_time), 
            max = max(date_time), 
            tracklength = max(date_time)-min(date_time)) %>% 
  as.data.frame()
animal_id_summary


# Summarise by station ----------------------------------------------------
stationsum <- 
  detections %>% 
  group_by(station_name) %>%
  summarise(detections = length(animal_id),
            start = min(date_time),
            end = max(date_time),
            uniqueID = length(unique(animal_id)), det_days=length(unique(as.Date(date_time)))) %>% 
  as.data.frame()
stationsum

# Spatial plots -----------------------------------------------------------
# examine by station and FishID:
stationFishID <- 
  detections %>% 
  group_by(station_name, animal_id) %>%
  summarise(lat = mean(deploy_latitude), 
            lon = mean(deploy_longitude), 
            dets = length(animal_id), 
            logdets = log(dets))
library(ggplot2)
movMap <- ggplot()+
  geom_path(data = detections, aes(x = deploy_longitude, y = deploy_latitude, col = animal_id))+
  geom_point(data = stationFishID, aes(x = lon, y = lat, size = logdets, col = animal_id))+
  facet_wrap(~animal_id)
movMap


# Network analysis --------------------------------------------------------
# https://ocean-tracking-network.github.io/jb-acoustic-telemetry/05-network-analysis/index.html
network_analysis_data <- detections%>%
  arrange(date_time)%>%
  group_by(animal_id) %>%
  mutate(to = dplyr::lead(station_name),
         to_latitude = dplyr::lead(deploy_latitude),
         to_longitude = dplyr::lead(deploy_longitude)) %>%
  group_by(station_name, to) %>%
  summarise(visits = n(),
            latitude = mean(deploy_latitude),
            longitude = mean(deploy_longitude),
            to_latitude = mean(to_latitude),
            to_longitude = mean(to_longitude)
            )%>%
  rename(from = station_name)%>%
  na.omit()

# Create a data frame of receiver vertices.
receivers <- 
  network_analysis_data %>%
  group_by(from) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    visits = sum(visits)
  )

library(mapview)
library(leaflet.minicharts)
library(sf)

# First lets create a basemap with all the residence times at each receivers mapped
basemap <-
  receivers %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mapview(color.regions = "white",
          cex = "visits",
          alpha = 0,
          homebutton = F,
          legend = F)

m <-
  basemap@map %>% 
  addFlows(lng0 = network_analysis_data$longitude,
           lat0 = network_analysis_data$latitude,
           lng1 = network_analysis_data$to_longitude,
           lat1 = network_analysis_data$to_latitude,
           flow = network_analysis_data$visits,
           color = "black",
           opacity = 0.8)

m


# Try out year_monthly network --------------------------------------------
# Additional interactive plot with a year-month toggle



library(dplyr)
library(lubridate)
library(leaflet)
library(sf)
library(htmltools)
library(scales)

# Add year-month field ----------------------------------------------------
detections_monthly <-
  detections %>%
  mutate(year_month = format(as.Date(date_time), "%Y-%m"))

# Summarise movements by month -------------------------------------------
monthly_network <-
  detections_monthly %>%
  arrange(date_time) %>%
  group_by(animal_id) %>%
  mutate(
    to = dplyr::lead(station_name),
    to_latitude = dplyr::lead(deploy_latitude),
    to_longitude = dplyr::lead(deploy_longitude),
    to_month = dplyr::lead(year_month)
  ) %>%
  filter(year_month == to_month) %>%
  group_by(year_month, station_name, to) %>%
  summarise(
    visits = n(),
    latitude = mean(deploy_latitude, na.rm = TRUE),
    longitude = mean(deploy_longitude, na.rm = TRUE),
    to_latitude = mean(to_latitude, na.rm = TRUE),
    to_longitude = mean(to_longitude, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(from = station_name) %>%
  na.omit()

# Receiver summaries per month -------------------------------------------
monthly_receivers <-
  monthly_network %>%
  group_by(year_month, from) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    visits = sum(visits),
    .groups = "drop"
  )

# Build leaflet map -------------------------------------------------------
months_available <- sort(unique(monthly_network$year_month))

m_monthly <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron)

for (ym in months_available) {
  
  # subset month
  net_sub <- 
    monthly_network %>% 
    filter(year_month == ym)
  
  rec_sub <- 
    monthly_receivers %>% 
    filter(year_month == ym)
  
  # receiver points -------------------------------------------------------
  
  m_monthly <- 
    m_monthly %>%
    
    addCircleMarkers(
      data = rec_sub,
      
      lng = ~longitude,
      lat = ~latitude,
      
      radius = ~rescale(visits, to = c(4, 12)),
      
      stroke = TRUE,
      weight = 1,
      
      color = "black",
      fillColor = "orange",
      fillOpacity = 0.7,
      
      popup = ~paste0(
        "<b>Receiver:</b> ", from,
        "<br><b>Visits:</b> ", visits,
        "<br><b>Month:</b> ", year_month
      ),
      
      group = ym
    )
  
  # movement lines --------------------------------------------------------
  
  for(i in seq_len(nrow(net_sub))) {
    
    m_monthly <- 
      m_monthly %>%
      
      addPolylines(
        lng = c(
          net_sub$longitude[i],
          net_sub$to_longitude[i]
        ),
        
        lat = c(
          net_sub$latitude[i],
          net_sub$to_latitude[i]
        ),
        
        weight = rescale(
          net_sub$visits[i],
          to = c(1, 6)
        ),
        
        opacity = 0.7,
        color = "steelblue",
        
        popup = paste0(
          "<b>From:</b> ", net_sub$from[i],
          "<br><b>To:</b> ", net_sub$to[i],
          "<br><b>Transitions:</b> ", net_sub$visits[i],
          "<br><b>Month:</b> ", net_sub$year_month[i]
        ),
        
        group = ym
      )
  }
}

# Layer control -----------------------------------------------------------

m_monthly <- 
  m_monthly %>%
  
  addLayersControl(
    overlayGroups = months_available,
    
    options = layersControlOptions(
      collapsed = FALSE
    )
  ) %>%
  
  hideGroup(months_available[-1])

# Show map ----------------------------------------------------------------

m_monthly


# monthly test ------------------------------------------------------------

# Create empty leaflet map ------------------------------------------------

m_monthly <- leaflet()

# Available months --------------------------------------------------------

months_available <- 
  sort(unique(monthly_network$year_month))

# Add monthly layers ------------------------------------------------------

for (ym in months_available) {
  
  # subset month
  net_sub <- 
    monthly_network %>%
    filter(year_month == ym)
  rec_sub <- 
    monthly_receivers %>%
    filter(year_month == ym)
  self_visits <- net_sub %>%
    filter(from == to)
  flows_between <- net_sub %>%
    filter(from != to)
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = self_sf$visits
  )
 
  
  self_sf <- self_visits %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  m_monthly <-
    basemap_monthly %>%
    addCircleMarkers(
      data = self_sf,
      radius = 18,
      color = ~pal(visits),
      fillOpacity = 1,
      weight = 1,
      label = ~as.character(visits),
      labelOptions = labelOptions(
        noHide = TRUE,
        direction = "center",
        textOnly = TRUE,
        style = list(
          "font-weight" = "bold",
          "color" = "black",
          "font-size" = "12px"
        )
        ),
      group = "self_visits"
    )
  m_monthly <- m_monthly %>%
    addFlows(
      lng0 = flows_between$longitude,
      lat0 = flows_between$latitude,
      lng1 = flows_between$to_longitude,
      lat1 = flows_between$to_latitude,
      color = "black",
      flow = flows_between$visits,
      maxThickness = 5,
      minThickness = 1,
      opacity = 0.6
    )
  
  m_monthly
 
}

# subset month
rec_sub <- 
  monthly_receivers %>%
  filter(year_month == "2021-01")

net_sub <- 
  monthly_network %>%
  filter(year_month == "2021-01")
basemap_monthly <-
  rec_sub %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mapview(color.regions = "white",
          cex = "visits",
          alpha = 0,
          homebutton = F,
          legend = F)

m_monthly <-
  basemap_monthly@map %>% 
  addFlows(lng0 = net_sub$longitude,
           lat0 = net_sub$latitude,
           lng1 = net_sub$to_longitude,
           lat1 = net_sub$to_latitude,
           flow = net_sub$visits,
           color = "black",
           opacity = 0.8)

m_monthly

# PAM data
load(file.path("data", "DTO_DUC2_PpData.Rdata"))
stations_PAM <- 
  PAM_data %>% 
  group_by(Station) %>%
  summarise(positive_hours = sum(PPM),
            start = min(datetime),
            end = max(datetime),
            active_hours = length(unique(datetime))) %>% 
  as.data.frame()
stationsum
monthly_PAM <-
  PAM_data %>%
  arrange(datetime) %>%
  mutate(month = lubridate::month(datetime))%>%
  group_by(month, Station)%>%
  summarise(
    positive_hours = sum(PPM),
      latitude = mean(decLat),
    longitude = mean(decLon))

basemap_PAM <-
  monthly_PAM %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mapview(color.regions = "white",
          cex = "positive_hours",
          alpha = 0,
          homebutton = F,
          legend = F)
basemap_PAM
