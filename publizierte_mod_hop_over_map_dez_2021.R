mod_hopover_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("hopover_map"),
                           height = "800px")
    
  )
}

mod_hopover_map_server <- function(input, output, session, rec_pos, selected_rows){
  ns <- session$ns
  observeEvent(selected_rows(), {
    leaflet_proxy <- leaflet::leafletProxy(ns("hopover_map"), session)
    centre_lon <- mean(range(rec_pos()$lon, na.rm=TRUE))
    centre_lat <- mean(range(rec_pos()$lat, na.rm=TRUE))
    lat <- rec_pos()[selected_rows(), "lat"]
    lon <- rec_pos()[selected_rows(), "lon"]
    if(is.null(selected_rows())) {
      leaflet::setView(leaflet_proxy, centre_lon, centre_lat, zoom=6)}else{
        if(is.na(lat)) {
          leaflet::setView(leaflet_proxy, centre_lon, centre_lat, zoom=6)}else{
            leaflet_proxy%>%
              leaflet::setView(lon, lat, zoom= 18)}
      }
  }, ignoreNULL= FALSE)#leaflet proxy
  
  
  
  #creates the map
  output$hopover_map <-
    leaflet::renderLeaflet({
      leaflet::leaflet(rec_pos()) %>% 
        leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Satellit") %>%
        
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.DE, group = "OpenstreetMap") %>%
        leaflet::addLayersControl(baseGroups = c("ESRI Satellit", "OpenstreetMap")) %>%
        leaflet::addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "hectares") %>%
        leaflet::addCircleMarkers(lng = ~lon,
                                  lat = ~lat,
                                  label = ~purrr::map(ID, shiny::HTML),
                                  color = "grey",
                                  stroke = 0.05,
                                  #fillColor = ~ colour,
                                  fillOpacity = 1,
                                  labelOptions = leaflet::labelOptions(noHide = T, direction = "right", 
                                                                       style = list(
                                                                         "font-size" = "13px",
                                                                         "font-weight" = "bold"
                                                                         
                                                                       )))
    })
  
  
}