library(shiny)

# Resource Path für statische Dateien (HTML Lösungen)
addResourcePath("res", "solutions")

# Hardcoded student data
student_data <- data.frame(
  id = rep(1:26, each = 2),
  name = c(
    "Chiara Aurora Barbaca", "Janine Andermatt", "Emilie Chenaux", "Emilia Laura Banzer",
    "Fabienne Durand", "Tobias Biedermann", "Bastien Egli", "Monja Leandra Burri",
    "Nadine Federer", "Anja Sabina Fend", "Moira Johanna Frey", "Nicolas Simon Gabl",
    "Emilia Luise Knop", "Michele Alessia Hollenstein", "Oliver Gabriel Kofler", "Leila Ibraimi",
    "Irma Despina Kurth", "Mara Jaeggi", "Janis Noah Luginbuehl", "Laura Angela Jaeggi",
    "Annika Eva Mantsch", "Ece Aysu Kirac", "Madina Marti", "Amelie Kuehn",
    "Julian Merlin Meier", "Noemi Cristina Martin Tejedor", "Helen Meier", "Anna Julia Matt",
    "Annina Mirjam Meister", "Arulini Murugavel", "Noel Louis Niffeler", "Ridha Puran",
    "Huelya Oezmen", "Carla Rentsch", "Roberta Santoli", "Thomas Schneider",
    "Svenja Victoria Schaerer", "Nic Sobhani", "Lukas Andrea Scheurer", "Ann Jolyne Stettler",
    "Florence Carina Medea Stoffel", "Natasa Stojanovic", "Anna Sophie Stulz", "Elina Beata Isabella Stoop",
    "Irina Ana Tadic", "Isabelle Alexa Tscharner", "Simona Cristina Thoeny", "Alexandra Danielle Waelti",
    "Rea Vollmer", "Yosiga Yogendran", "Katsiaryna Zengin", "Nadine Zuberbuehler"
  ), stringsAsFactors = FALSE
)

student_choices <- setNames(student_data$id, student_data$name)
student_choices <- student_choices[order(names(student_choices))]

ui <- fluidPage(
  # Titel mit Logo aus dem www-Ordner
  titlePanel(
    windowTitle = "Musterlösungen R you Ready HS25",
    title = div(
      img(src = "logo.png", height = "50px", style = "margin-right: 15px; vertical-align: middle;"),
      "Student Solution Portal",
      style = "display: flex; align-items: center;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("id", "Name auswählen:", 
                  choices = c("Bitte wählen..." = "", student_choices),
                  selectize = FALSE), 
      
      radioButtons("type", "Berichtstyp:", 
                   choices = list("Analysis" = "analysis", "Processing" = "processing")),
      hr(),
      actionButton("show_report", "Vorschau anzeigen", class = "btn-primary btn-block"),
      downloadButton("download_report", "HTML Herunterladen", class = "btn-block"),
      br(),
      textOutput("status")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Vorschau", uiOutput("report_view"))
      )
    )
  ),
  
  hr(),
  
  # Hinweis-Box am Ende
  div(
    style = "background-color: #f9f9f9; border: 1px solid #ddd; padding: 20px; border-radius: 5px; margin: 20px 0;",
    h4(strong("Wichtiger Hinweis zur Musterlösung")),
    p("Dies ist eine von mehreren möglichen Lösungen für die Analysis- und Processing-Skripte. Da die Berichte individuell an Ihren spezifischen Datensatz angepasst wurden, bedeuten Abweichungen von dieser Vorlage ", 
      strong("nicht,"), " dass Ihr Code oder Ihre Ergebnisse falsch sind."),
    p("Das zugrunde liegende Skript ist parametrisiert, um alle 52 Datensätze abzudecken. Daher sind einige Code-Abschnitte im Bericht ausgeblendet oder werden nicht ausgeführt. 
       Der vollständige Code kann über die Schaltfläche ", strong("„</> Code“"), " (oben rechts) eingesehen werden. Bitte beachten Sie, dass diese zusätzlichen Teile primär der technischen Veranschaulichung dienen.")
  )
)

server <- function(input, output, session) {
  
  rel_path <- reactive({
    req(input$id)
    paste0(input$type, "/", input$type, "_", input$id, ".html")
  })
  
  observeEvent(input$show_report, {
    path <- file.path("solutions", rel_path())
    if (file.exists(path)) {
      output$report_view <- renderUI({
        tags$iframe(src = paste0("res/", rel_path()), 
                    width = "100%", height = "850px", style = "border:none;")
      })
      updateTabsetPanel(session, "tabs", selected = "Vorschau")
    } else {
      output$status <- renderText(paste("Datei nicht gefunden:", rel_path()))
    }
  })
  
  output$download_report <- downloadHandler(
    filename = function() { paste0(input$type, "_", input$id, ".html") },
    content = function(file) {
      path <- file.path("solutions", rel_path())
      if (file.exists(path)) file.copy(path, file)
    }
  )
}

shinyApp(ui, server)