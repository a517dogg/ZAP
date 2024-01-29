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
    )
  )


nonconformingLDR <- conformings %>%
  filter(! Conforming) %>%
  filter(NAME == "LDR") %>%
  mutate(
    popup = paste0(
      SITEADDRES, "<br>",
      "Nonconforming because:", "<br>",
      CLASSDSCRP
    )
  )

nonconformingMap <- leaflet(nonconformingLDR %>% st_transform(4326)) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addPolygons(
    weight = .2, 
    fill = TRUE,
    popup = ~popup
  )

htmlwidgets::saveWidget(nonconformingMap, file = "~/ZAP/nonconformingMap.html")