library(shiny)

# Resource Path für statische Dateien (HTML Lösungen)
addResourcePath("res", "solutions")

# Hardcoded student data with anonymized names
# Note: AnMan, AnMat, AnSte, and AnStu use 5 letters to ensure uniqueness.
student_data <- data.frame(
  id = rep(1:26, each = 2),
  name = c(
    "ChBa", "JaAn", "EmCh", "EmBa", "FaDu", "ToBi", "BaEg", "MoBu",
    "NaFe", "AnFe", "MoFr", "NiGa", "EmKn", "MiHo", "OlKo", "LeIb",
    "IrKu", "MaJa", "JaLu", "LaJa", "AnMan", "EcKi", "MaMa", "AmKu",
    "JuMe", "NoTe", "HeMe", "AnMat", "AnMe", "ArMu", "NoNi", "RiPu",
    "HuOe", "CaRe", "RoSa", "ThSc", "SvSc", "NiSo", "LuSc", "AnSte",
    "FlSt", "NaSt", "AnStu", "ElSt", "IrTa", "IsTs", "SiTh", "AlWa",
    "ReVo", "YoYo", "KaZe", "NaZu"
  ), stringsAsFactors = FALSE
)

# Create named vector for selection (Name = ID)
student_choices <- setNames(student_data$id, student_data$name)
student_choices <- student_choices[order(names(student_choices))]

ui <- fluidPage(
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
      helpText(HTML("<b>Identifikation:</b> Suchen Sie nach den ersten zwei Buchstaben Ihres Vornamens und Nachnamens (z.B. <i>Max Mustermann</i> &rarr; <b>MaMu</b>). <br><br><i>Hinweis: Bei Namensdoppelungen wurden drei Buchstaben des Nachnamens verwendet (z.B. MaMus).</i>")),
      br(),
      
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
  
  div(
    style = "background-color: #f9f9f9; border: 1px solid #ddd; padding: 20px; border-radius: 5px; margin: 20px 0;",
    h4(strong("Wichtiger Hinweis zur Musterlösung")),
    p("Dies ist eine von mehreren möglichen Lösungen für die Analysis- und Processing-Skripte. Da die Berichte individuell an Ihren spezifischen Datensatz angepasst wurden, bedeuten Abweichungen von dieser Vorlage ", 
      strong("nicht,"), " dass Ihr Code oder Ihre Ergebnisse falsch sind."),
    p("Der vollständige Code kann über die Schaltfläche ", strong("„</> Code“"), " (oben rechts) eingesehen werden.")
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