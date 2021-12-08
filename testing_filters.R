##filters
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


make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = c("Wähle" = "", levs), multiple = TRUE, selected=unique(levs)) # multiselector is somehow not working! selected = levs, 
  } else {
    # Not supported
    NULL
  }
}


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      make_ui(hopover_pos$Länge, "Länge"),
      make_ui(hopover_pos$Höhe, "Höhe"),
        make_ui(hopover_pos$Strassentyp, "Strassentyp"),
         make_ui(hopover_pos$Anzahl_spuren, "Anzahl_spuren"),
         make_ui(hopover_pos$Art, "Art"),
         make_ui(hopover_pos$beidseitige_HopOver, "beidseitige_HopOver"),
         make_ui(hopover_pos$Gewässerunterführung, "Gewässerunterführung")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
                    
filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}                    

server <- function(input, output, session) {
  selected <- reactive({
    message("Länge")
    print(input$Länge)
    print(filter_var(hopover_pos$Länge, input$Länge))
    message("Höhe")
    print(input$Höhe)
    print(filter_var(hopover_pos$Höhe, input$Höhe))
    
    filter_var(hopover_pos$Länge, input$Länge) &
      filter_var(hopover_pos$Höhe, input$Höhe) &
      filter_var(hopover_pos$Strassentyp, input$Strassentyp) &
      filter_var(hopover_pos$Anzahl_spuren, input$Anzahl_spuren) & 
      filter_var(hopover_pos$Art, input$Art) &
      filter_var(hopover_pos$beidseitige_HopOver, input$beidseitige_HopOver) &
      filter_var(hopover_pos$Gewässerunterführung, input$Gewässerunterführung)
  })
  
  output$data <- renderTable(hopover_pos[selected(), which(!names(hopover_pos) %in% c("lat", "lon"))])
}

shinyApp(ui, server)
