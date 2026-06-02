
library(tidyverse)
library(sf)

current_zoning <- read_sf("https://maps.cityofrochester.gov/server/rest/services/Open_Data/Zoning_Districts_Open_Data/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
    rename(Zoning_current = LABEL)

parcels_2022 <- read_sf("https://maps.cityofrochester.gov/server2/rest/services/Open_Data/TaxParcel2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

current_zoning %>% st_geometry() %>% plot()

zap_2023 <- read_sf("DRAFT_ZAP_Districts_Sept2023/DRAFT_ZAP_DISTRICTS_Sept2023.shp") %>%
    rename(Zoning_2023 = NAME)

zap_2026 <- read_sf("ZAP_Districts_final_draft_Apr26.shp") %>%
    rename(Zoning_2026 = NAME)

