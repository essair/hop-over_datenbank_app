#testing_filters with data table

library(shiny)
library(dplyr)
library(purrr)
library(leaflet)


#hopover_pos <- "aktuelle_datenbankstand_21_07_15.csv" %>%
hopover_pos <- "aktuelle_datenbankstand_21_07_15_empty_Statt_NA.csv" %>%
  readr::read_delim(col_names = c("ID", "lat", "lon", "Straßenname", "Strassentyp", "Anzahl_spuren",
                                  "Art", "Höhe", "Laenge", "beidseitige_HopOver", "Errichtungsjahr",
                                  "Gewässerunterführung", "Bau", "Bemerkung"), locale = readr::locale(decimal_mark = "."), delim = ",")%>%
  mutate(ID = as.integer(ID),
         Strassentyp = as.factor(Strassentyp),
         Art = as.factor(Art), 
         beidseitige_HopOver = as.factor(beidseitige_HopOver),
         Gewässerunterführung =as.factor(Gewässerunterführung))



ui <-  fluidPage(
  titlePanel("trying out datatable to show selection!! "),
    #Create a new Row in the UI for selectInputs
  sidebarLayout(
    sidebarPanel(
      sliderInput("Laenge",
                  "Länge:",
                  min=min(hopover_pos$Laenge, na.rm=TRUE),
                  max=max(hopover_pos$Laenge, na.rm=TRUE),
                  round=TRUE,
                  value= c(100, 500)),
      sliderInput("Höhe",
                  "minimal Höhe:",
                  min=min(hopover_pos$Höhe, na.rm=TRUE),
                  max=max(hopover_pos$Höhe, na.rm=TRUE),
                  step=1,
                  value= 4),
      selectInput("Strassentyp",
                  "Straßentyp:",
                  choices = c("Wähle" = "", levels(hopover_pos$Strassentyp)), multiple = TRUE, selected=unique(levels(hopover_pos$Strassentyp))),
      
      sliderInput("Anzahl_spuren",
                  "Anzahl Spuren:",
                  step=1,
                  ticks=TRUE,
                  min=min(hopover_pos$Anzahl_spuren, na.rm=TRUE),
                  max=max(hopover_pos$Anzahl_spuren, na.rm=TRUE),
                  value= 4),
      
      selectInput("Art",
                  "Art der Struktur:",
                  choices = c("Wähle" = "", levels(hopover_pos$Art)), multiple = TRUE, selected=c(unique(levels(hopover_pos$Art)), "NA")),
      
      selectInput("beidseitige_HopOver",
                  "Beidseitige Hop-Over?:",
                  c("All",
                    unique(as.character(hopover_pos$beidseitige_HopOver)))),
      
      selectInput("Gewässerunterführung",
                  "Gewässerunterführung oder sonst. Unterführung:",
                  c("All",
                    unique(as.character(hopover_pos$Gewässerunterführung)))),
      width=2),
    # Create a new row for the table.
    mainPanel(
      fluidRow(
        column(12, DT::dataTableOutput("table")),
        column(12, leaflet::leafletOutput("mappy", height="600px"))),
        width=10)
    )
  )

server <- function(input, output) {
  data_selection <- reactive({    
    data <- hopover_pos
    data <- data%>%
      filter(Laenge >=input$Laenge[1] & Laenge <=input$Laenge[2])
    # message(Laenge)
    # print(input$Laenge)
    data <- data%>%
      filter(Höhe >= input$Höhe)
    data <- data%>%
      filter(Strassentyp %in% input$Strassentyp)
    data <- data%>%
      filter(Anzahl_spuren == input$Anzahl_spuren)
    data <- data%>%
      filter(Art %in% input$Art)
    if (input$beidseitige_HopOver != "All") {
      data <- data%>%
        filter(beidseitige_HopOver == input$beidseitige_HopOver)
    }
    if (input$Gewässerunterführung != "All") {
      data <- data%>%
        filter(Gewässerunterführung == input$Gewässerunterführung)
    }
     data <- data%>%
     select(- any_of(c("Bemerkung", "Bau")))
     #"lat", "lon"
      })
  
  # map_view <- reactive({ 
  #   centre_lon <- mean(range(data_selection()$lon, na.rm=TRUE))
  #   centre_lat <-mean(range(data_selection()$lat, na.rm=TRUE))
  #   leaflet_proxy <- leaflet::leafletProxy("mappy")
  #   leaflet::setView(leaflet_proxy, centre_lon, centre_lat, zoom=6)
  #   })  ### this approach didn'T really get me anywhere either!
  
  #print(data_selection)
  # Filter data based on selections
  output$table <- DT::renderDataTable(
    DT::datatable(data_selection()%>%
                    select(-any_of(c("lat", "lon"))),
                  rownames=FALSE, class="hover compact stripe", options=list(scrollY= "200px", paging =FALSE)))
  
  output$mappy <- renderLeaflet({
    shiny::validate(need(nrow(data_selection())>0, message= "Oops, keine passende Hop-Over Strukturen gefunden!"))
    leaflet::leaflet() %>%
      leaflet::addTiles()%>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Satellit")%>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.DE, group = "OpenstreetMap") %>%
      leaflet::addLayersControl(baseGroups = c("ESRI Satellit", "OpenstreetMap")) %>%
      leaflet::addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "hectares") %>%
      leaflet::addCircleMarkers(data= data_selection(),
                                lng= ~lon,
                                lat= ~lat,
                                label = ~purrr::map(ID, shiny::HTML),
                                color = "grey",
                                stroke = 0.05,
                                #fillColor = ~ colour,
                                fillOpacity = 1,
                                labelOptions = leaflet::labelOptions(noHide = T, direction = "right", 
                                                                     style = list(
                                                                       "font-size" = "13px",
                                                                       "font-weight" = "bold")))%>%
     leaflet::setView(mean(range(data_selection()$lon, na.rm=TRUE)), mean(range(data_selection()$lat, na.rm=TRUE)),
                      zoom=7)# this line is not working output ID is missing!
   })

  
}

shinyApp(ui, server)



##bits and bobs
output$mappy <- renderLeaflet({
  observeEvent(data_selection(), {
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Satellit") %>%
      leaflet::addCirclesMarkers(lng = ~ lon,
                                 lat = ~ lat, 
                                 label = ~ purrr::map(ID, shiny::HTML),
                                 colour=grey)
    
mappy <- leaflet::leaflet() %>%
        leaflet::addTiles()%>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Satellit")%>%
        leaflet::addCircleMarkers(data= hopover_pos,
                                  lng= ~lon,
                                  lat= ~lat,
                                  label = ~purrr::map(ID, shiny::HTML),
                                  color = "grey",
                                  stroke = 0.05,
                                  #fillColor = ~ colour,
                                  fillOpacity = 1,
                                  labelOptions = leaflet::labelOptions(noHide = T, direction = "right", 
                                                                       style = list(
                                                                         "font-size" = "13px",
                                                                         "font-weight" = "bold")))%>%
  leaflet::setView(mean(hopover_pos$lon, na.rm=TRUE), mean(hopover_pos$lat, na.rm=TRUE), zoom= 6)
})


