downzoned <- parcelsJoin %>%
  filter(
    (NAME == "LDR" & CATEGORY != "R-1 Low Density Residential") |
      (NAME == "MDR" & CATEGORY != "R-1 Low Density Residential" & CATEGORY != "R-2 Medium Density Residential")
  ) %>%
  mutate(
    NewlyNonConforming = case_when(
      (NAME == "LDR" & CLASSDSCRP %in% c("1 Family Res", "Res vac land", "Inn/lodge", "Det row bldg")) ~ FALSE,
      (NAME == "MDR" & CLASSDSCRP %in% c("Apartment", "Animal welfr")) ~ TRUE,
      NAME == "MDR" ~ FALSE,
      TRUE ~ TRUE
    ),
    popup = paste(
      SITEADDRES, "<br>",
      "Current: ", CATEGORY, "<br>",
      "ZAP: ", NAME, "<br>",
      ifelse(NewlyNonConforming, "Newly nonconforming", "Still conforms")
    ),
    fill = case_when(
      NAME == "LDR" ~ "Blue",
      NAME == "MDR" ~ "Orange"
    ),
    weight = case_when(
      NewlyNonConforming == TRUE ~ .5,
      NewlyNonConforming == FALSE ~ .1
    )
  )

downzoneMap <- leaflet(downzoned %>% st_transform(4326)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    popup = ~popup, 
    weight = ~weight,
    color = ~fill, fill = TRUE)

htmlwidgets::saveWidget(downzoneMap, file = "~/ZAP/downzoneMap.html")

# downzoned %>%
#   count(NewlyNonConforming, NAME)
