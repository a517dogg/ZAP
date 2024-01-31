
library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)

# parcels from: https://data.cityofrochester.gov/datasets/zoning-districts/explore?location=43.136890%2C-77.621594%2C5.97
parcels <- read_sf("C:/Users/am4337/Downloads/City_Of_Rochester_Tax_Parcel_Records_Snapshot__2020.shp") %>%
  st_transform(6541)

# current zoning from: https://data.cityofrochester.gov/maps/68ba587e9b6048cb8eb12f941688255e/about
zoning <- read_sf("ZAP/currentZoning/City_of_Rochester_Zoning%2C_Preservation%2C_and_Overlay_Districts.shp") %>%
  st_transform(6541)

ZAP <- read_sf("ZAP/Jan2024_draftZoning/DRAFT_ZAP_Districts_Sept2023.shp") %>%
  st_transform(6541)

# basic stats:
# 420 rows in current zoning, so 420 zoning districts
# 542 rows in the proposed zoning, so adding 122 districts

# find area

# old zoning file imprecisely drawn, need to reproject bc of overlapping vertices

zoningNAD83 <- zoning %>% st_transform(6541)

zoningAreas <- zoningNAD83 %>% 
  mutate(area = st_area(zoningNAD83)) %>%
  group_by(CATEGORY) %>%
  summarize(
    totalarea = sum(area),
    numDistricts = n()
  ) %>%
  st_drop_geometry() %>%
  mutate(acres= units::set_units(totalarea, "acres")) %>%
  arrange(desc(totalarea))

ZAPareas <- ZAP %>% 
  dplyr::filter(NAME != "O-A") %>% #airport overlay not relevant
  mutate(area = st_area(ZAP %>% dplyr::filter(NAME != "O-A"))) %>% 
  group_by(NAME) %>%
  summarize(
    totalarea = sum(area),
    numDistricts = n()
  ) %>%
  st_drop_geometry() %>%
  mutate(acres = units::set_units(totalarea, "acres")) %>%
  arrange(desc(totalarea))

# for plotting
ZAPwgs <- ZAP %>% st_transform(4326) %>%
  # don't plot the airport overlay
  filter(NAME != "O-A")

parcelsJoin <- parcels %>%
  mutate(parcelArea = st_area(parcels)) %>%
  st_join(ZAP, st_overlaps, largest = TRUE) %>%
  #st_join(st_make_valid(ZAPwgs), st_overlaps) %>%
  st_join(zoning, st_overlaps, largest = TRUE)

parcelsSummed <- parcelsJoin %>% 
  group_by(CATEGORY, NAME) %>% 
  st_drop_geometry() %>%
  summarize(
    parcelCount = n(), 
    parcelArea = sum(parcelArea),
    parcelValue = sum(CURRENT_TO))

# library(networkD3)
library(plotly)

# for sankey, from node is CATEGORY (old zoning)
# to node is NAME (ZAP)

nodeswithnums <- tibble(
  nodename = c(unique(parcelsSummed$CATEGORY),
               unique(parcelsSummed$NAME)),
  type = c(rep("Current zoning", times = length(unique(parcelsSummed$CATEGORY))),
           rep("ZAP", times = length(unique(parcelsSummed$NAME))))
) %>%
  mutate(nodenumber = row_number() - 1)

flowvals <- parcelsSummed %>%
  left_join(
    nodeswithnums %>% 
      filter(type == "Current zoning") %>%
      dplyr::select(-type),
    by = c("CATEGORY" = "nodename")
  ) %>% 
  rename("sourcenode" = "nodenumber") %>%
  left_join(
    nodeswithnums %>%
      filter(type == "ZAP") %>%
      dplyr::select(-type),
    by = c("NAME" = "nodename")
  ) %>%
  rename("targetnode" = "nodenumber")

countSankey <- plot_ly(
  type = "sankey",
  domain = list(c(x = c(0, 1), y = c(0, 1))),
  orientation = "h",
  arrangement = "snap",
  node = list(
    label = nodeswithnums$nodename,
    pad = 15, 
    thickness = 15
  ),
  link = list(
    source = flowvals$sourcenode,
    target = flowvals$targetnode,
    value = flowvals$parcelCount
  )
) %>%
  layout(
    title = "Counts of parcels moving from current zoning to ZAP",
    xaxis = list(showgrid = F, zeroline = F, title = "", showticklabels = FALSE),
    yaxis = list(showgrid = F, zeroline = F, title = "", showticklabels = FALSE)
  )

htmlwidgets::saveWidget(countSankey, file = "~/ZAP/countSankey.html")

areaSankey <- plot_ly(
  type = "sankey",
  domain = list(c(x = c(0, 1), y = c(0, 1))),
  orientation = "h",
  arrangement = "snap",
  node = list(
    label = nodeswithnums$nodename,
    pad = 15, 
    thickness = 15
  ),
  link = list(
    source = flowvals$sourcenode,
    target = flowvals$targetnode,
    value = units::set_units(flowvals$parcelArea, "acres")
  )
) %>%
  layout(
    title = "Areas of parcels (in acres) moving from current zoning to ZAP",
    xaxis = list(showgrid = F, zeroline = F, title = "", showticklabels = FALSE),
    yaxis = list(showgrid = F, zeroline = F, title = "", showticklabels = FALSE)
  )

htmlwidgets::saveWidget(areaSankey, file = "~/ZAP/areaSankey.html")

valueSankey <- plot_ly(
  type = "sankey",
  domain = list(c(x = c(0, 1), y = c(0, 1))),
  orientation = "h",
  arrangement = "snap",
  # hovertext = ~paste0("$", flowvals$parcelValue, " moving from ", flowvals$CATEGORY, " to ", flowvals$NAME),
  valueformat = "$,g",
  node = list(
    label = nodeswithnums$nodename,
    pad = 15, 
    thickness = 15
  ),
  link = list(
    source = flowvals$sourcenode,
    target = flowvals$targetnode,
    value = flowvals$parcelValue
  )
) %>%
  layout(
    title = "Assessed value of parcels (in acres) moving from current zoning to ZAP",
    xaxis = list(showgrid = F, zeroline = F, title = "", showticklabels = FALSE),
    yaxis = list(showgrid = F, zeroline = F, title = "", showticklabels = FALSE)
  )

htmlwidgets::saveWidget(valueSankey, file = "~/ZAP/valueSankey.html")
