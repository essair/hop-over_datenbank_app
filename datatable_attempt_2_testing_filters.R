#testing_filters with data table

library(shiny)
library(dplyr)
library(purrr)


hopover_pos <- "aktuelle_datenbankstand_21_07_15.csv" %>%
  readr::read_delim(col_names = c("ID", "lat", "lon", "Straßenname", "Strassentyp", "Anzahl_spuren",
                                  "Art", "Höhe", "Länge", "beidseitige_HopOver", "Errichtungsjahr",
                                  "Gewässerunterführung", "Bau", "Bemerkung"), locale = readr::locale(decimal_mark = "."), delim = ",")%>%
  mutate(ID = as.integer(ID),
         Strassentyp = as.factor(Strassentyp),
         Art = as.factor(Art), 
         beidseitige_HopOver = as.factor(beidseitige_HopOver),
         Gewässerunterführung =as.factor(Gewässerunterführung))

ui <-  fluidPage(
  titlePanel("trying out datatable to show selection!! "),
  
  # Create a new Row in the UI for selectInputs
  # fluidRow(
  #   column(2,
  #          sliderInput("Länge",
  #                      "Länge:",
  #                      min=min(hopover_pos$Länge, na.rm=TRUE),
  #                      max=max(hopover_pos$Länge, na.rm=TRUE),
  #                      round=TRUE,
  #                      value= c(100, max(hopover_pos$Länge, na.rm=TRUE))
  #          )
  #   ),
  #   column(2,
  #          sliderInput("Höhe",
  #                      "minimal Höhe:",
  #                      min=min(hopover_pos$Höhe, na.rm=TRUE),
  #                      max=max(hopover_pos$Höhe, na.rm=TRUE),
  #                      step=1,
  #                      round=TRUE,
  #                      value= 4)
  #   ),
  #   column(2,
  #          selectInput("Strassentyp",
  #                      "Straßentyp:",
  #                      choices = c("Wähle" = "", levels(hopover_pos$Strassentyp)), multiple = TRUE, selected=unique(levels(hopover_pos$Strassentyp)))
  #   ),
  #   column(2,
  #           sliderInput("Anzahl_spuren",
  #                       "Anzahl Spuren:",
  #                       step=2,
  #                       ticks=TRUE,
  #                       min=min(hopover_pos$Anzahl_spuren, na.rm=TRUE),
  #                       max=max(hopover_pos$Anzahl_spuren, na.rm=TRUE),
  #                       value= 4)
  #   )
  #   ),
    fluidRow(
      column(2,
             selectInput("Art",
                         "Art der Struktur:",
                         choices = c("Wähle" = "", levels(hopover_pos$Art)), multiple = TRUE, selected=unique(levels(hopover_pos$Art)))
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
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- hopover_pos
    # data <- data[data$Länge >=input$Länge[1] & data$Länge <=input$Länge[2] & !is.na(data$Länge), ]
    # message(Länge)
    # print(input$Länge)
    # data <- data[data$Höhe >= input$Höhe[1], ]
    # message(Höhe)
    # print(input$Höhe)
    # if (input$Strassentyp != "All") {
    #   data <- data[data$Strassentyp == input$Strassentyp,]
    # }
    # message(Strassentyp)
    # print(input$Strassentyp)
    # data <- data[data$Anzahl_spuren == input$Anzahl_spuren[1],]
    # }
    if (input$Art != "All") {
     data <- data[data$Art == input$Art,]
    }
    if (input$beidseitige_HopOver != "All") {
     data <- data[data$beidseitige_HopOver == input$beidseitige_HopOver,]
   }
    if (input$Gewässerunterführung != "All") {
      data <- data[data$Gewässerunterführung == input$Gewässerunterführung,]
   }
  data [, c(-0, -2, -3)]
}, rownames=FALSE))
  
}

shinyApp(ui, server)
