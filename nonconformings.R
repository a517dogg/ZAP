# looking at conforming LDR as a whole

LDRclasses <- c("1 Family Res", "Res vac land", "Inn/lodge", "Det row bldg", "Aged - home")
MDRclasses <- c(LDRclasses, "2 Family Res", "3 Family Res", "Res Multiple")

conformings <- parcelsJoin %>%
  mutate(
    Conforming = case_when(
      NAME == "LDR" & CLASSDSCRP %in% LDRclasses ~ TRUE,
      NAME == "LDR" & ! CLASSDSCRP %in% LDRclasses ~ FALSE,
      NAME == "MDR" & CLASSDSCRP %in% MDRclasses ~ TRUE,
      NAME == "MDR" & ! CLASSDSCRP %in% MDRclasses ~ FALSE,
      TRUE ~ NA
    ),
    ConformingCurrent = case_when(
      CATEGORY == "R-1 Low Density Residential" & CLASSDSCRP %in% LDRclasses ~ TRUE,
      CATEGORY == "R-1 Low Density Residential" & ! CLASSDSCRP %in% LDRclasses ~ FALSE,
    )
  )


nonconformingLDR <- conformings %>%
  filter(! Conforming) %>%
  filter(NAME == "LDR") %>%
  mutate(
    popup = paste0(
      SITEADDRES, "<br>",
      "Nonconforming because:", "<br>",
      CLASSDSCRP, " in ", NAME
    )
  )

nonconformingMap <- leaflet(nonconformingLDR %>% st_transform(4326)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = ZAP %>% filter(NAME == "LDR") %>% st_transform(4326),
    weight = .1, opacity = .1, color = "Grey"
  ) %>%
  addPolygons(
    weight = .2, 
    fill = TRUE,
    popup = ~popup
  ) %>%
  addLegend(
    title = "Non conforming uses in LDR",
    "topright",
    colors = c("Grey", "Blue"), labels = c("LDR zones", "Non conforming parcels")
  )

htmlwidgets::saveWidget(nonconformingMap, file = "~/ZAP/nonconformingMap.html")
