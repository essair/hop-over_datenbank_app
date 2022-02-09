#testing_filters with data table

library(shiny)
library(shinyWidgets)
library(dplyr)
library(purrr)
library(tidyr)
library(leaflet)


#hopover_pos <- "aktuelle_datenbankstand_21_07_15.csv" %>%
# hopover_pos <- "aktuelle_datenbankstand_21_07_15_empty_Statt_NA.csv" %>%
#   readr::read_delim(col_names = c("ID", "lat", "lon", "Straßenname", "Strassentyp", "Anzahl_spuren",
#                                   "Art", "Höhe", "Laenge", "beidseitige_HopOver", "Errichtungsjahr",
#                                   "Gewässerunterführung", "Bau", "Bemerkung"), locale = readr::locale(decimal_mark = "."), delim = ",")%>%
#   mutate(ID = as.integer(ID),
#          Strassentyp = as.factor(Strassentyp),
#          Art = as.factor(tidyr::replace_na(Art, "unklar")), 
#          beidseitige_HopOver = as.factor(tidyr::replace_na(beidseitige_HopOver, "unklar")),
#          Gewässerunterführung =as.factor(Gewässerunterführung),
#          Anzahl_spuren = tidyr::replace_na(Anzahl_spuren, 2),
#          Höhe = tidyr::replace_na(Höhe, 4),
#          Laenge = tidyr::replace_na(Laenge, 1))

###read_in_data
hopover_pos <- read.csv("/home/sarah/git/hop-over_datenbank_app/Datenbank_Tab_app_formatiert_02_2022.csv", sep=";", na.strings =c("NA", ""))%>%
  mutate(strassentyp = as.factor(strassentyp),
         art = as.factor(tidyr::replace_na(art, "unklar")), 
         beidseitige_hop_over = as.factor(tidyr::replace_na(beidseitige_hop_over, "unklar")),
         ueberfuehrungsbauwerk = tidyr::replace_na(ueberfuehrungsbauwerk, "info_folgt"),
         ueberfuehrungsbauwerk = as.factor(ueberfuehrungsbauwerk),
         anzahl_fahrspuren = tidyr::replace_na(anzahl_fahrspuren,0),
         laenge_1 = tidyr::replace_na(laenge_1, 0),
         hoehe = tidyr::replace_na(hoehe, 0),
         dtv = " ",
         kandidat= " ", 
         bewertung = " ",
         koordinaten = ifelse(is.na(gps_koordinaten_breite), "nein", "ja"))%>%
  rename(lat = gps_koordinaten_breite,
         lon = gps_koordinaten_laenge,
         jahr = jahr_der_fertigstellung,
         fahrstreifen = anzahl_fahrspuren)%>%
  select(-bezeichnung_bildschirmfoto, -errichtungsjahr, -standort_nr_untersuchung, -bereits_in_app_hochgeladen_stand_21.01.2022, -X)%>%
  relocate(c(koordinaten, bundesland, strassentyp, fahrstreifen, art, hoehe,
             laenge_1, laenge_2, laenge_3, laenge_3, laenge_4, beidseitige_hop_over, ueberfuehrungsbauwerk, dtv, abstand_zu_strasse, kandidat, bewertung), .after= strasse)
           


# %>%
#   select(-standort_nr_untersuchung, -)
         # anzahl_fahrspuren = tidyr::replace_na(anzahl_fahrspuren, 2),
         # Höhe = tidyr::replace_na(Höhe, 4),
         # Laenge = tidyr::replace_na(Laenge, 1))





ui <-  fluidPage(
  titlePanel( div(column(width = 6, h2("Hop-Over Auswahl App")), 
                  column(width = 6, img(src="20200827_offizielles_Logo.png", align= "right", height="100px"))),
              windowTitle="OekoFor Hop-Over Auswahl App"),
    #Create a new Row in the UI for selectInputs
  sidebarLayout(
    sidebarPanel(
      sliderInput("laenge_1",
                  "Länge der Struktur:",
                  min=min(hopover_pos$laenge_1, na.rm=TRUE),
                  max=max(hopover_pos$laenge_1, na.rm=TRUE),
                  round=TRUE,
                  value= c(min(hopover_pos$laenge_1, na.rm=TRUE), max(hopover_pos$laenge_1, na.rm=TRUE))),
      sliderInput("hoehe",
                  "minimale Höhe:",
                  min=min(hopover_pos$hoehe, na.rm=TRUE),
                  max=max(hopover_pos$hoehe, na.rm=TRUE),
                  step=1,
                  value= 0),
      selectInput("strassentyp",
                  "Straßentyp:",
                  choices = c("Wähle" = "", levels(hopover_pos$strassentyp)), multiple = TRUE, selected=unique(levels(hopover_pos$strassentyp))),
      
      sliderInput("fahrstreifen",
                  "Anzahl Spuren:",
                  step=1,
                  ticks=TRUE,
                  min=min(hopover_pos$fahrstreifen, na.rm=TRUE),
                  max=max(hopover_pos$fahrstreifen, na.rm=TRUE),
                  value=c(min(hopover_pos$fahrstreifen, na.rm=TRUE), max(hopover_pos$fahrstreifen, na.rm=TRUE))),
      
      selectInput("art",
                  "Art der Struktur:",
                  choices = c("Wähle" = "", levels(hopover_pos$art)), multiple = TRUE, selected=c(unique(levels(hopover_pos$art)), "NA")),
      
      selectInput("beidseitige_hop_over",
                  "Beidseitige Hop-Over?:",
                  c("Alle",
                    unique(as.character(hopover_pos$beidseitige_hop_over)))),

      selectInput("uebefuehrungsbauwerk",
                  "Gewässerunterführung oder sonst. Unterführung:",
                  c("Alle",
                    unique(as.character(hopover_pos$uebefuehrungsbauwerk)))),
      width=2),
    # Create a new row for the table.
    mainPanel(
       conditionalPanel(condition = "output.warnstat == 'Error'",
                        verbatimTextOutput("warnmsg")),
      fluidPage(
        column(6, height="750px", DT::dataTableOutput("table")),
        column(6, height="800px", leaflet::leafletOutput("mappy")),
        width=10)
    )
  )
)

server <- function(input, output) {
  data_selection <- reactive({    
    data <- hopover_pos
    data <- data%>%
      filter(laenge_1 >=input$laenge_1[1] & laenge_1 <=input$laenge_1[2])
    # message(Laenge)
    # print(input$Laenge)
    data <- data%>%
      filter(hoehe >= input$hoehe)
    data <- data%>%
      filter(strassentyp %in% input$strassentyp)
    data <- data%>%
      filter(fahrstreifen >=input$fahrstreifen[1] & fahrstreifen <=input$fahrstreifen[2])
    data <- data%>%
      filter(art %in% input$art)
    # if (input$beidseitige_hop_over!= "Alle") {
    #   data <- data%>%
    #     filter(beidseitige_hop_over == input$beidseitige_hop_over)
    # }
    # if (input$ueberfuehrungsbauwerk != "Alle") {
    #   data <- data%>%
    #     filter(ueberfuehrungsbauwerk == input$ueberfuehrungsbauwerk)
    # }
     # data <- data%>%
     # select("ID", "lat", "lon", "strassenname", "strassentyp", "anzahl_fahrspuren",
     #                           "art", "hoehe", "laenge_1", "beidseitige_hop_over", "errichtungsjahr", "jahr_der_fertigstellung",
     #                               "ueberfuehrungsbauwerk", "Bemerkung")
     # #"lat", "lon"
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
    data_selection()%>%
      select(-any_of(c("lat", "lon")))%>%
      DT::datatable(rownames=FALSE, class="hover compact stripe", options=list(scrollY= "750px",
                                                                             paging =FALSE, scrollX=TRUE, autoWidth=TRUE, searching=FALSE, # searching removes the search bar
                                                                             columnDefs=list(list(targets=ncol(.)-1, width="800px"))),
                  selection= 'single', filter="top"))
  
  
  observeEvent(input$table_rows_selected, ignoreNULL = FALSE,{
    if (!is.null(input$table_rows_selected)){
      the_row <- data_selection()[input$table_rows_selected, ]
      lat <- the_row$lat
      lon <- the_row$lon
      message("rows selected info")
      print(c(lat, lon))
      leafletProxy( "mappy", data=data_selection())%>%
        leaflet::setView(lon, lat, zoom=18)
      if( is.na(lat) | is.na(lon)){showNotification("Keine Koordinaten für diesen Standort verfügbar!", type="error", duration=5)}
    }else {leafletProxy( "mappy", data=data_selection())%>%
        leaflet::setView(10.063, 51.152, zoom=6)}
  })
  
  # errstat <- reactive({
  #   ifelse (length(which(is.na(data_selection()$lat))) >=1, T, F)
  # })
  # 
  # output$warnmsg <- renderPrint({
  #   if (errstat()){
  #     print("Warnung - Manche Tabelleneinträge haben keine Koordinaten")
  #     } else {
  #     print("No error")
  #   }
  # })
  # output$warnstat <- renderText({ifelse(errstat(),"Error","No error") })
  # outputOptions(output, "warnstat", suspendWhenHidden=FALSE)
  
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

