#testing_filters with data table

library(shiny)
library(dplyr)
library(purrr)
library(data.table)


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
  fluidRow(
    column(2,
           sliderInput("Laenge",
                       "Länge:",
                       min=min(hopover_pos$Laenge, na.rm=TRUE),
                       max=max(hopover_pos$Laenge, na.rm=TRUE),
                       round=TRUE,
                       value= c(100, 500))
    ),
    column(2,
           sliderInput("Höhe",
                       "minimal Höhe:",
                       min=min(hopover_pos$Höhe, na.rm=TRUE),
                       max=max(hopover_pos$Höhe, na.rm=TRUE),
                       step=1,
                       value= 4)
    ),
    column(2,
           selectInput("Strassentyp",
                       "Straßentyp:",
                       choices = c("Wähle" = "", levels(hopover_pos$Strassentyp)), multiple = TRUE, selected=unique(levels(hopover_pos$Strassentyp)))
    ),
    column(2,
            sliderInput("Anzahl_spuren",
                        "Anzahl Spuren:",
                        step=1,
                        ticks=TRUE,
                        min=min(hopover_pos$Anzahl_spuren, na.rm=TRUE),
                        max=max(hopover_pos$Anzahl_spuren, na.rm=TRUE),
                        value= 4)
    )
    ),
    fluidRow(
      column(2,
             selectInput("Art",
                         "Art der Struktur:",
                         choices = c("Wähle" = "", levels(hopover_pos$Art)), multiple = TRUE, selected=c(unique(levels(hopover_pos$Art)), "NA"))
      ),
      column(2,
             selectInput("beidseitige_HopOver",
                         "Beidseitige Hop-Over?:",
                         c("All",
                           unique(as.character(hopover_pos$beidseitige_HopOver))))
      ),
      column(2,
             selectInput("Gewässerunterführung",
                         "Gewässerunterführung oder sonst. Unterführung:",
                         c("All",
                           unique(as.character(hopover_pos$Gewässerunterführung))))
             )
  ),
  # Create a new row for the table.
  mainPanel(DT::dataTableOutput("table"))
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
      select(- any_of(c("lat", "lon")))})
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable(data_selection(),
                                                    rownames=FALSE, class="hover compact stripe", options=list(scrollY= "800px", paging =FALSE)))
  
  ##LEAFLET MAP
  
}

shinyApp(ui, server)




