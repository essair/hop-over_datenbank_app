#testing_filters with data table

library(shiny)
library(shinyWidgets)
library(dplyr)
library(purrr)
library(tidyr)
library(leaflet)


#hopover_pos <- "aktuelle_datenbankstand_21_07_15.csv" %>%
hopover_pos <- "aktuelle_datenbankstand_21_07_15_empty_Statt_NA.csv" %>%
  readr::read_delim(col_names = c("ID", "lat", "lon", "Straßenname", "Strassentyp", "Anzahl_spuren",
                                  "Art", "Höhe", "Laenge", "beidseitige_HopOver", "Errichtungsjahr",
                                  "Gewässerunterführung", "Bau", "Bemerkung"), locale = readr::locale(decimal_mark = "."), delim = ",")%>%
  mutate(ID = as.integer(ID),
         Strassentyp = as.factor(Strassentyp),
         Art = as.factor(tidyr::replace_na(Art, "unklar")), 
         beidseitige_HopOver = as.factor(tidyr::replace_na(beidseitige_HopOver, "unklar")),
         Gewässerunterführung =as.factor(Gewässerunterführung),
         Anzahl_spuren = tidyr::replace_na(Anzahl_spuren, 2),
         Höhe = tidyr::replace_na(Höhe, 4),
         Laenge = tidyr::replace_na(Laenge, 1))



ui <-  fluidPage(
  titlePanel("Hop-Over Auswahl App"),
    #Create a new Row in the UI for selectInputs
  sidebarLayout(
    sidebarPanel(
      sliderInput("Laenge",
                  "Länge der Struktur:",
                  min=min(hopover_pos$Laenge, na.rm=TRUE),
                  max=max(hopover_pos$Laenge, na.rm=TRUE),
                  round=TRUE,
                  value= c(1, 1200)),
      sliderInput("Höhe",
                  "minimale Höhe:",
                  min=min(hopover_pos$Höhe, na.rm=TRUE),
                  max=max(hopover_pos$Höhe, na.rm=TRUE),
                  step=1,
                  value= 2),
      selectInput("Strassentyp",
                  "Straßentyp:",
                  choices = c("Wähle" = "", levels(hopover_pos$Strassentyp)), multiple = TRUE, selected=unique(levels(hopover_pos$Strassentyp))),
      
      sliderInput("Anzahl_spuren",
                  "Anzahl Spuren:",
                  step=1,
                  ticks=TRUE,
                  min=min(hopover_pos$Anzahl_spuren, na.rm=TRUE),
                  max=max(hopover_pos$Anzahl_spuren, na.rm=TRUE),
                  value=c(2, 8)),
      
      selectInput("Art",
                  "Art der Struktur:",
                  choices = c("Wähle" = "", levels(hopover_pos$Art)), multiple = TRUE, selected=c(unique(levels(hopover_pos$Art)), "NA")),
      
      selectInput("beidseitige_HopOver",
                  "Beidseitige Hop-Over?:",
                  c("Alle",
                    unique(as.character(hopover_pos$beidseitige_HopOver)))),
      
      selectInput("Gewässerunterführung",
                  "Gewässerunterführung oder sonst. Unterführung:",
                  c("Alle",
                    unique(as.character(hopover_pos$Gewässerunterführung)))),
      width=2, colour="dark green"),
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
      filter(Anzahl_spuren >=input$Anzahl_spuren[1] & Anzahl_spuren <=input$Anzahl_spuren[2])
    data <- data%>%
      filter(Art %in% input$Art)
    if (input$beidseitige_HopOver != "Alle") {
      data <- data%>%
        filter(beidseitige_HopOver == input$beidseitige_HopOver)
    }
    if (input$Gewässerunterführung != "Alle") {
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
                  rownames=FALSE, class="hover compact stripe", options=list(scrollY= "200px", paging =FALSE),
                  selection= 'single'))
  
  
  observeEvent(input$table_rows_selected, ignoreNULL = FALSE,{
    if (!is.null(input$table_rows_selected)){
      the_row <- data_selection()[input$table_rows_selected, ]
    lat <- the_row$lat
    lon <- the_row$lon
    message("rows selected info")
    print(c(lat, lon))
    leafletProxy( "mappy", data=data_selection())%>%
           leaflet::setView(lon, lat, zoom=18)
    }else {leafletProxy( "mappy", data=data_selection())%>%
        leaflet::setView(10.063, 51.152, zoom=6)}
  })
  
  # output$table_row_selected <- renderText({
  #   table_row_selected()
  # })

  # observe({
  #   mouse_selection <- input$table_rows_selected()
  #   leafletProxy( "mappy", data=data_selection())%>%
  #     leaflet::setView(mouse_selection()$lon, mouse_selection()$lat, zoom=12)
  # })
  
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
                                color = "orange",
                                stroke = 0.05,
                                #fillColor = ~ colour,
                                fillOpacity = 1,
                                labelOptions = leaflet::labelOptions(noHide = T, direction = "right", 
                                                                     style = list(
                                                                       "font-size" = "13px",
                                                                       "font-weight" = "bold")))%>%
      leaflet::setView(10.063, 51.152, zoom=6)# this line is not working output ID is missing!
            # leaflet::setView(mean(range(data_selection()$lon, na.rm=TRUE)), mean(range(data_selection()$lat, na.rm=TRUE)),
      #                 zoom=7)# this line is not working output ID is missing!
   })

  
}

shinyApp(ui, server)

