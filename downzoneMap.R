downzoned <- parcelsJoin %>%
  filter(
    (NAME == "LDR" & CATEGORY != "R-1 Low Density Residential") |
      (NAME == "MDR" & CATEGORY != "R-1 Low Density Residential" & CATEGORY != "R-2 Medium Density Residential")
  ) %>%
  mutate(
    popup = paste(
      SITEADDRES, "<br>",
      "Current: ", CATEGORY, "<br>",
      "ZAP: ", NAME
    ),
    fill = case_when(
      NAME == "LDR" ~ "Blue",
      NAME == "MDR" ~ "Orange"
    )
  )

downzoneMap <- leaflet(downzoned %>% st_transform(4326)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    popup = ~popup, 
    weight = .2,
    color = ~fill, fill = TRUE)

htmlwidgets::saveWidget(downzoneMap, file = "~/ZAP/downzoneMap.html")