# Shiny dashboard for the runrgetics sprint-training analysis.
# Launched via runrgetics::launch_sprint_dashboard(data_dir); the data folder is
# passed through the RUNRGETICS_DASHBOARD_DATA environment variable.

library(shiny)
library(shinydashboard)
library(ggplot2)

# Round numeric columns for compact table display.
num_round <- function(df, digits = 2) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, digits = digits)
  df
}

data_dir <- Sys.getenv("RUNRGETICS_DASHBOARD_DATA", unset = ".")
session_files <- list.files(data_dir, pattern = "\\.csv(\\.gz)?$",
                            recursive = TRUE, full.names = TRUE)
names(session_files) <- if (length(session_files)) {
  sub(paste0("^", normalizePath(data_dir), .Platform$file.sep), "",
      normalizePath(session_files))
} else character(0)

ui <- dashboardPage(
  dashboardHeader(title = "runrgetics — sprint training"),
  dashboardSidebar(
    selectInput("file", "Session file", choices = session_files),
    numericInput("map", "Maximal aerobic power (W/kg)",
                 value = 20, min = 5, max = 40, step = 0.5),
    sliderInput("threshold", "Sprint speed threshold (m/s)",
                value = 5, min = 2, max = 9, step = 0.5),
    uiOutput("sprint_ui")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("vb_n", width = 3),
      valueBoxOutput("vb_al", width = 3),
      valueBoxOutput("vb_la", width = 3),
      valueBoxOutput("vb_aer", width = 3)
    ),
    tabBox(
      width = 12,
      tabPanel(
        "Kinematics",
        plotOutput("speed_distance", height = "360px"),
        br(),
        DT::DTOutput("sprint_table")
      ),
      tabPanel(
        "Bioenergetics",
        fluidRow(
          column(6, plotOutput("decomp", height = "360px")),
          column(6, plotOutput("energy", height = "360px"))
        ),
        br(),
        DT::DTOutput("bioe_table")
      )
    )
  )
)

server <- function(input, output, session) {

  # Detected sprints for the selected file + threshold (read once, reused).
  detected <- reactive({
    req(input$file)
    md <- runrgetics::read_gpexe_csv(input$file)
    sprints <- runrgetics::detect_sprints(md, threshold = input$threshold)
    list(motion = md, sprints = sprints)
  })

  # Bioenergetic decomposition for the selected MAP.
  bioe <- reactive({
    d <- detected()
    validate(need(nrow(d$sprints) > 0, "No sprints detected — lower the threshold."))
    runrgetics::analyze_training_bioenergetics(
      d$motion, sprints = d$sprints, maximal_aerobic_power = input$map)
  })

  output$sprint_ui <- renderUI({
    d <- detected()
    if (nrow(d$sprints) == 0) return(helpText("No sprints detected."))
    selectInput("sprint", "Sprint", choices = d$sprints$sprint_id)
  })

  pct <- function(x) paste0(round(x), "%")

  output$vb_n <- renderValueBox(
    valueBox(nrow(detected()$sprints), "Sprints", icon = icon("bolt"), color = "navy"))
  output$vb_al <- renderValueBox(
    valueBox(pct(bioe()$summary$mean_pct_alactic), "Alactic", color = "aqua"))
  output$vb_la <- renderValueBox(
    valueBox(pct(bioe()$summary$mean_pct_lactic), "Lactic", color = "orange"))
  output$vb_aer <- renderValueBox(
    valueBox(pct(bioe()$summary$mean_pct_aerobic), "Aerobic", color = "olive"))

  output$speed_distance <- renderPlot({
    d <- detected()
    validate(need(nrow(d$sprints) > 0, "No sprints detected — lower the threshold."))
    runrgetics::plot_workout_sprints(d$motion, sprints = d$sprints)
  })

  output$sprint_table <- DT::renderDT({
    d <- detected()
    validate(need(nrow(d$sprints) > 0, "No sprints detected — lower the threshold."))
    cmp <- runrgetics::compare_workout_sprints(d$motion, sprints = d$sprints)
    DT::datatable(num_round(cmp), options = list(dom = "tip", pageLength = 10),
                  rownames = FALSE)
  })

  output$decomp <- renderPlot({
    d <- detected()
    req(input$sprint)
    runrgetics::plot_sprint_bioenergetics(
      d$motion, sprint_id = as.integer(input$sprint), sprints = d$sprints,
      maximal_aerobic_power = input$map)
  })

  output$energy <- renderPlot({
    d <- detected()
    validate(need(nrow(d$sprints) > 0, "No sprints detected — lower the threshold."))
    runrgetics::plot_workout_energy(d$motion, sprints = d$sprints,
                                    maximal_aerobic_power = input$map)
  })

  output$bioe_table <- DT::renderDT(
    DT::datatable(num_round(bioe()$per_sprint),
                  options = list(dom = "tip", pageLength = 10), rownames = FALSE))
}

shinyApp(ui, server)
