Hop-Over_app_bestehende_teil

#install necessary packages
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)


source("mod_hopover_map.R")


rec_pos <- "aktuelle_datenbankstand_21_07_15.csv" %>%
  readr::read_delim(col_names = c("ID", "lat", "lon", "Straßenname", "Straßentyp", "Anzahl spuren",
                                  "Art", "Höhe", "Länge", "beidseitige Hop-Over", "Errichtungsjahr",
                                  "Gewässerunterführung", "Bau", "Bemerkung"), locale = readr::locale(decimal_mark = "."), delim = ",")%>%
  mutate(ID = as.integer(ID))#, 
#Errichtungsjahr= as.integer(Errichtungsjahr),
#Länge = as.integer(Länge))


centre_lon <- mean(range(rec_pos$lon, na.rm=TRUE))
centre_lat <- mean(range(rec_pos$lat, na.rm=TRUE))


#selected <- blah$data_table_rows_selected


ui <- fluidPage(titlePanel("Hop-over Standorte Vorauswahl", windowTitle= "FE Überflug Hop-Over Standorte - OekoFor WebApp"),
                mainPanel(
                  fluidRow(column(6, mod_hopover_map_ui("hopover_map_ui_1")),
                           column(6, DT::dataTableOutput("blah"))))) ##add in slider for hop-over length numericInput("num", "Number one", value = 0, min = 0, max = 300),

server <- function(input, output, session){
  blah <-datatable(rec_pos,
                   options = list(paging = FALSE,    ## paginate the output
                                  #pageLength = 15,  ## number of rows to output for each page
                                  #scrollX = TRUE,   ## enable scrolling on X axis
                                  #scrollY = TRUE,   ## enable scrolling on Y axis
                                  autoWidth = TRUE, ## use smart column width handling
                                  #server = FALSE,
                                  striped= TRUE,
                                  ## use client-side processing
                                  dom = 'Bfrtip',
                                  #buttons = c('csv', 'excel'),
                                  columnDefs = list(list(targets = '_all', className = 'dt-center'))#list(targets = c(1:2), visible = FALSE)
                   ),
                   extensions = 'Buttons',
                   selection = 'single', ## enable selection of a single row
                   filter = 'top',              ## include column filters at the bottom
                   rownames = FALSE                ## don't show row numbers/names
  )
  
  output$blah <- DT::renderDataTable(blah)
  
  
  
  
  callModule(mod_hopover_map_server, "hopover_map_ui_1", rec_pos = reactive(rec_pos), selected_row = reactive(input$blah_rows_selected)) #rec_status = reactive(rec_status)
  
  #selected <- blah$data_table_rows_selected
  
  
  # server <- function(input, output) {
  #   output$blah <- DT::renderDataTable(blah)
}

# Run the application
shinyApp(ui = ui, server = server)