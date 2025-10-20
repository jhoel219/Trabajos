# app.R - Sistema Profesional SIR para Predicción de Riesgo en Salud

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(readxl)
library(docxtractr)
library(officer)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(caret)
library(scales)

# ============================================================================
# TEMA Y ESTILOS PROFESIONALES
# ============================================================================
custom_css <- tags$head(
  tags$style(HTML("
    :root {
      --primary: #1E3A8A;
      --secondary: #0F766E;
      --accent: #DC2626;
      --light-bg: #F8FAFC;
      --dark-text: #0F172A;
      --border: #E2E8F0;
    }
    
    body {
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      background-color: var(--light-bg);
      color: var(--dark-text);
    }
    
    .main-header .logo {
      background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%);
      border-bottom: 3px solid var(--accent);
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      font-weight: 700;
      letter-spacing: 0.5px;
      font-size: 18px;
    }
    
    .main-header .navbar {
      background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%) !important;
    }
    
    .sidebar {
      background-color: #FFFFFF;
      border-right: 1px solid var(--border);
    }
    
    .sidebar .sidebar-menu .active a {
      background-color: var(--primary) !important;
      color: white !important;
      border-left: 4px solid var(--accent);
    }
    
    .sidebar .sidebar-menu a {
      color: var(--dark-text);
      transition: all 0.3s ease;
      border-left: 4px solid transparent;
    }
    
    .sidebar .sidebar-menu a:hover {
      background-color: #F1F5F9;
      border-left-color: var(--primary);
    }
    
    .box {
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      border-top: 3px solid var(--primary);
      border-radius: 6px;
      border: 1px solid var(--border);
    }
    
    .box.box-primary {
      border-top-color: var(--primary);
    }
    
    .box-header.with-border {
      border-bottom: 1px solid var(--border);
      background: #FAFBFC;
    }
    
    .box-title {
      font-weight: 600;
      color: var(--dark-text);
      font-size: 15px;
    }
    
    .btn-primary {
      background-color: var(--primary);
      border-color: var(--primary);
      font-weight: 600;
      transition: all 0.3s ease;
    }
    
    .btn-primary:hover {
      background-color: #1e3a8a;
      box-shadow: 0 4px 12px rgba(30, 58, 138, 0.3);
    }
    
    .btn-success {
      background-color: var(--secondary);
      border-color: var(--secondary);
      font-weight: 600;
    }
    
    .btn-success:hover {
      background-color: #0d635c;
      box-shadow: 0 4px 12px rgba(15, 118, 110, 0.3);
    }
    
    .btn-danger {
      background-color: var(--accent);
      border-color: var(--accent);
      font-weight: 600;
    }
    
    .dataTables_wrapper {
      background-color: white;
      border-radius: 6px;
    }
    
    .dataTable thead th {
      background-color: #F1F5F9;
      color: var(--dark-text);
      font-weight: 600;
      border-bottom: 2px solid var(--border);
      padding: 12px 15px;
    }
    
    .dataTable tbody td {
      padding: 10px 15px;
      border-bottom: 1px solid var(--border);
    }
    
    .dataTable tbody tr:hover {
      background-color: #F8FAFC;
    }
    
    .info-box {
      box-shadow: 0 2px 4px rgba(0,0,0,0.08);
      border-radius: 6px;
      border-left: 4px solid var(--primary);
    }
    
    .small-box {
      border-radius: 6px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.08);
    }
    
    .small-box > .inner {
      padding: 20px;
    }
    
    .small-box h3 {
      font-size: 32px;
      font-weight: 700;
      margin: 0;
    }
    
    .badge {
      padding: 6px 12px;
      border-radius: 20px;
      font-weight: 600;
      font-size: 11px;
    }
    
    .badge-danger {
      background-color: #FEE2E2;
      color: #991B1B;
    }
    
    .badge-warning {
      background-color: #FEF3C7;
      color: #92400E;
    }
    
    .badge-success {
      background-color: #DBEAFE;
      color: #0C4A6E;
    }
    
    .content-header {
      margin-bottom: 25px;
    }
    
    .content-header h1 {
      font-size: 28px;
      font-weight: 700;
      margin: 0;
      color: var(--dark-text);
    }
    
    .help-text {
      color: #64748B;
      font-size: 13px;
      margin-top: 5px;
    }
    
    .file-input-wrapper {
      position: relative;
    }
    
    .shiny-input-container {
      margin-bottom: 15px;
    }
    
    label {
      font-weight: 600;
      color: var(--dark-text);
      margin-bottom: 8px;
      font-size: 14px;
    }
    
    .shiny-input-container:not(.shiny-input-container-inline) > label {
      display: block;
    }
    
    input[type='text'], input[type='number'], select, textarea {
      border: 1px solid var(--border);
      border-radius: 4px;
      padding: 8px 12px;
      font-size: 14px;
      transition: border-color 0.3s ease;
    }
    
    input[type='text']:focus, input[type='number']:focus, select:focus {
      border-color: var(--primary);
      outline: none;
      box-shadow: 0 0 0 3px rgba(30, 58, 138, 0.1);
    }
    
    .nav-tabs > li > a {
      border: none;
      color: var(--dark-text);
      font-weight: 600;
      border-bottom: 3px solid transparent;
    }
    
    .nav-tabs > li.active > a {
      background-color: transparent;
      color: var(--primary);
      border-bottom-color: var(--primary);
    }
  "))
)

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

parse_uploaded <- function(file){
  req(file)
  ext <- tools::file_ext(file$datapath)
  
  if(ext %in% c('xls','xlsx')){
    df <- read_excel(file$datapath)
    return(as.data.frame(df))
  }
  
  if(ext %in% c('csv','txt')){
    df <- read.csv(file$datapath, stringsAsFactors = FALSE)
    return(df)
  }
  
  if(ext == 'docx'){
    doc <- read_docx(file$datapath)
    tbls <- docx_extract_all_tbls(doc)
    
    if(length(tbls) >= 1){
      df <- as.data.frame(tbls[[1]], stringsAsFactors = FALSE)
      return(df)
    }
    
    txt <- docx_summary(doc)$text
    return(data.frame(text = paste(txt, collapse = '\n')))
  }
  
  stop('Formato no soportado')
}

cvd_score <- function(df){
  # Extraer valores de manera segura
  age <- if("age" %in% names(df)) df$age else if("Age" %in% names(df)) df$Age else 50
  bmi <- if("bmi" %in% names(df)) df$bmi else if("BMI" %in% names(df)) df$BMI else 25
  sbp <- if("systolic" %in% names(df)) df$systolic else if("SBP" %in% names(df)) df$SBP else 120
  smoke <- if("smoking" %in% names(df)) as.numeric(df$smoking) else 0
  diab <- if("diabetes" %in% names(df)) as.numeric(df$diabetes) else 0
  
  # Asegurar que sean numéricos
  age <- as.numeric(age)
  bmi <- as.numeric(bmi)
  sbp <- as.numeric(sbp)
  smoke <- as.numeric(smoke)
  diab <- as.numeric(diab)
  
  score <- 0.03 * pmax(age, 0, na.rm = TRUE) + 
    0.02 * pmax(bmi, 0, na.rm = TRUE) + 
    0.01 * pmax(sbp, 0, na.rm = TRUE) + 
    0.5 * smoke + 0.7 * diab
  
  prob <- 1 / (1 + exp(-((-6) + score)))
  return(pmax(pmin(prob, 1), 0))
}

resp_score <- function(df){
  # Extraer valores de manera segura
  age <- if("age" %in% names(df)) df$age else if("Age" %in% names(df)) df$Age else 50
  smoking <- if("smoking" %in% names(df)) as.numeric(df$smoking) else 0
  existing <- if("respiratory_history" %in% names(df)) as.numeric(df$respiratory_history) else 0
  
  # Asegurar que sean numéricos
  age <- as.numeric(age)
  smoking <- as.numeric(smoking)
  existing <- as.numeric(existing)
  
  score <- 0.025 * pmax(age, 0, na.rm = TRUE) + 0.8 * smoking + 1.0 * existing
  prob <- 1 / (1 + exp(-((-5) + score)))
  return(pmax(pmin(prob, 1), 0))
}

sir_sim <- function(beta, gamma, S0, I0, R0, days){
  N <- S0 + I0 + R0
  S <- numeric(days + 1)
  I <- numeric(days + 1)
  R <- numeric(days + 1)
  
  S[1] <- S0; I[1] <- I0; R[1] <- R0
  
  for(t in 1:days){
    dS <- -beta * S[t] * I[t] / N
    dI <- beta * S[t] * I[t] / N - gamma * I[t]
    dR <- gamma * I[t]
    
    S[t + 1] <- max(S[t] + dS, 0)
    I[t + 1] <- max(I[t] + dI, 0)
    R[t + 1] <- max(R[t] + dR, 0)
  }
  
  data.frame(day = 0:days, Susceptibles = S, Infectados = I, Recuperados = R)
}

risk_classification <- function(prob_cvd, prob_resp){
  max_prob <- pmax(prob_cvd, prob_resp)
  ifelse(max_prob >= 0.7, "???? Riesgo Alto", 
         ifelse(max_prob >= 0.5, "???? Riesgo Moderado", 
                "???? Riesgo Bajo"))
}

# ============================================================================
# INTERFAZ DE USUARIO (UI)
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "SIR Analytics - Sistema de Predicción de Riesgo en Salud",
    titleWidth = 450
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "menu",
      menuItem("???? Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("???? Cargar Datos", tabName = "upload", icon = icon("upload")),
      menuItem("???? Explorar Datos", tabName = "preview", icon = icon("magnifying-glass")),
      menuItem("???? Predicciones", tabName = "predict", icon = icon("heart-pulse")),
      menuItem("???? Simulador SIR", tabName = "sir", icon = icon("chart-line")),
      menuItem("?????? Información", tabName = "about", icon = icon("circle-info"))
    )
  ),
  
  dashboardBody(
    custom_css,  # Usar el CSS corregido
    
    tabItems(
      # ===== DASHBOARD =====
      tabItem(tabName = "dashboard",
              div(class = "content-header",
                  h1("Bienvenido a SIR Analytics"),
                  p("Sistema integral para análisis de riesgo cardiovascular y respiratorio", 
                    style = "font-size: 16px; color: #64748B; margin-top: 10px;")
              ),
              fluidRow(
                infoBoxOutput("pacientes_box"),
                infoBoxOutput("alto_box"),
                infoBoxOutput("moderado_box"),
                infoBoxOutput("bajo_box")
              ),
              fluidRow(
                box(
                  title = "¿Cómo usar esta plataforma?",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  HTML("<div style='line-height: 1.8;'>
              <h4 style='margin-top: 0;'>Paso 1: Cargar Datos</h4>
              <p>Vaya a la sección <strong>Cargar Datos</strong> e importe su archivo Excel, CSV o DOCX con información clínica.</p>
              
              <h4>Paso 2: Explorar Datos</h4>
              <p>Revise la estructura de sus datos y asigne columnas según corresponda.</p>
              
              <h4>Paso 3: Predicciones</h4>
              <p>Genere predicciones de riesgo cardiovascular y respiratorio para cada paciente.</p>
              
              <h4>Paso 4: Análisis SIR</h4>
              <p>Explore la propagación teórica de enfermedades usando el modelo epidemiológico SIR.</p>
            </div>")
                )
              )
      ),
      
      # ===== CARGA DE DATOS =====
      tabItem(tabName = "upload",
              div(class = "content-header", h1("Cargar y Procesar Datos")),
              fluidRow(
                box(
                  title = "???? Importar Archivo",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  fileInput('file1', 'Seleccione archivo',
                            accept = c('.xlsx','.xls','.csv','.txt','.docx'),
                            placeholder = 'Arrastra archivo aquí o haz clic'),
                  p(class = "help-text",
                    "Formatos soportados: Excel (.xlsx, .xls), CSV, TXT, DOCX")
                ),
                box(
                  title = "?????? Configuración",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  checkboxInput('has_labels', 
                                '¿Incluye columnas de resultado (outcome)?', 
                                value = FALSE),
                  br(),
                  actionButton('process', 'Procesar Archivo', 
                               class = "btn-success", 
                               icon = icon("play"),
                               style = "width: 100%; padding: 12px;")
                )
              ),
              fluidRow(
                box(
                  title = "??????? Vista Previa de Datos",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(
                    DT::dataTableOutput('table_preview'),
                    type = 8, color = "#1E3A8A"
                  )
                )
              )
      ),
      
      # ===== EXPLORACIÓN DE DATOS =====
      tabItem(tabName = "preview",
              div(class = "content-header", h1("Análisis Exploratorio")),
              fluidRow(
                box(
                  title = "???? Estadísticas Descriptivas",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(
                    DT::dataTableOutput('summary_stats'),
                    type = 8, color = "#1E3A8A"
                  )
                ),
                box(
                  title = "???? Mapeo de Columnas",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  uiOutput('colmap_ui')
                )
              )
      ),
      
      # ===== PREDICCIONES =====
      tabItem(tabName = "predict",
              div(class = "content-header", h1("Análisis de Riesgo")),
              fluidRow(
                box(
                  title = "???? Predicciones por Paciente",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(
                    DT::dataTableOutput('predictions'),
                    type = 8, color = "#1E3A8A"
                  )
                )
              ),
              fluidRow(
                box(
                  title = "???? Distribución de Probabilidades",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(
                    plotlyOutput('prob_hist'),
                    type = 8, color = "#1E3A8A"
                  )
                ),
                box(
                  title = "???? Clasificación de Riesgo",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(
                    plotlyOutput('risk_dist'),
                    type = 8, color = "#1E3A8A"
                  )
                )
              )
      ),
      
      # ===== SIMULADOR SIR =====
      tabItem(tabName = "sir",
              div(class = "content-header", h1("Simulador SIR - Modelo Epidemiológico")),
              fluidRow(
                box(
                  title = "?????? Parámetros SIR",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  numericInput('beta', '?? (Beta) - Tasa de Transmisión', 
                               value = 0.3, min = 0, max = 1, step = 0.01),
                  numericInput('gamma', '?? (Gamma) - Tasa de Recuperación', 
                               value = 0.1, min = 0, max = 1, step = 0.01),
                  numericInput('S0', 'Susceptibles Iniciales (S???)', value = 999, min = 0),
                  numericInput('I0', 'Infectados Iniciales (I???)', value = 1, min = 0),
                  numericInput('R0', 'Recuperados Iniciales (R???)', value = 0, min = 0),
                  numericInput('days', 'Días a simular', value = 160, min = 1, step = 10),
                  br(),
                  actionButton('run_sir', 'Ejecutar Simulación',
                               class = "btn-danger",
                               icon = icon("play"),
                               style = "width: 100%; padding: 12px;")
                ),
                box(
                  title = "???? Curva SIR",
                  status = "info",
                  solidHeader = TRUE,
                  width = 8,
                  withSpinner(
                    plotlyOutput('sir_plot', height = "500px"),
                    type = 8, color = "#1E3A8A"
                  )
                )
              )
      ),
      
      # ===== INFORMACIÓN =====
      tabItem(tabName = "about",
              div(class = "content-header", h1("Información del Sistema")),
              fluidRow(
                box(
                  title = "?????? Acerca de SIR Analytics",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  HTML("
              <div style='line-height: 1.9;'>
              <h3>Modelo SIR</h3>
              <p>El modelo <strong>SIR (Susceptible-Infected-Recovered)</strong> es un modelo epidemiológico compartimentado que describe la dinámica de enfermedades infecciosas.</p>
              
              <h4>Componentes:</h4>
              <ul>
                <li><strong>S (Susceptibles):</strong> Población que puede contraer la enfermedad</li>
                <li><strong>I (Infectados):</strong> Población actualmente infectada</li>
                <li><strong>R (Recuperados):</strong> Población que se recuperó e inmunizó</li>
              </ul>
              
              <h4>Ecuaciones SIR:</h4>
              <p style='background: #F1F5F9; padding: 15px; border-radius: 6px; font-family: monospace;'>
                dS/dt = -??·S·I/N<br/>
                dI/dt = ??·S·I/N - ??·I<br/>
                dR/dt = ??·I
              </p>
              
              <h4>Parámetros Clave:</h4>
              <ul>
                <li><strong>?? (Beta):</strong> Tasa de transmisión (contactos infectivos por unidad de tiempo)</li>
                <li><strong>?? (Gamma):</strong> Tasa de recuperación (inversa del período infeccioso)</li>
                <li><strong>R???:</strong> Número básico de reproducción (??/??)</li>
              </ul>
              
              <h3>Disclaimer</h3>
              <p style='background: #FEE2E2; padding: 15px; border-radius: 6px; border-left: 4px solid #DC2626;'>
                <strong>?????? Aviso Legal:</strong> Esta aplicación es una herramienta demostrativa y educativa. 
                No reemplaza evaluaciones médicas profesionales. Los modelos utilizados son ejemplos simplificados 
                que requieren datos validados y calibración clínica para uso en entornos sanitarios reales.
              </p>
              </div>
            ")
                )
              )
      )
    )
  )
)

# ============================================================================
# LÓGICA DEL SERVIDOR (SERVER)
# ============================================================================

server <- function(input, output, session){
  rv <- reactiveValues(
    data = NULL, 
    mapped = NULL, 
    predictions = NULL, 
    sir = NULL,
    pacientes_procesados = 0,
    riesgo_alto = 0,
    riesgo_moderado = 0,
    riesgo_bajo = 0
  )
  
  # Cargar archivo
  observeEvent(input$process, {
    req(input$file1)
    tryCatch({
      df <- parse_uploaded(input$file1)
      rv$data <- df
      rv$mapped <- df  # Inicializar mapped con los datos originales
      showNotification("??? Archivo cargado correctamente", type = "message", duration = 3)
    }, error = function(e){
      showNotification(paste("??? Error:", e$message), type = "error", duration = 5)
    })
  })
  
  # InfoBoxes para el dashboard
  output$pacientes_box <- renderInfoBox({
    infoBox("Pacientes Procesados", rv$pacientes_procesados, 
            icon = icon("users"), color = "blue", fill = TRUE)
  })
  
  output$alto_box <- renderInfoBox({
    infoBox("Riesgo Alto", rv$riesgo_alto, 
            icon = icon("triangle-exclamation"), color = "red", fill = TRUE)
  })
  
  output$moderado_box <- renderInfoBox({
    infoBox("Riesgo Moderado", rv$riesgo_moderado, 
            icon = icon("circle-exclamation"), color = "yellow", fill = TRUE)
  })
  
  output$bajo_box <- renderInfoBox({
    infoBox("Riesgo Bajo", rv$riesgo_bajo, 
            icon = icon("circle-check"), color = "green", fill = TRUE)
  })
  
  # Vista previa
  output$table_preview <- DT::renderDataTable({
    req(rv$data)
    DT::datatable(
      rv$data, 
      options = list(
        pageLength = 10, 
        scrollX = TRUE, 
        dom = 'ftip'
      ),
      rownames = FALSE
    )
  })
  
  # Estadísticas
  output$summary_stats <- DT::renderDataTable({
    req(rv$data)
    num <- rv$data %>% select(where(is.numeric))
    if(ncol(num) == 0) return(data.frame(Mensaje = "No hay columnas numéricas"))
    
    summary_tbl <- data.frame(
      Variable = names(num),
      Mín = sapply(num, min, na.rm = TRUE),
      Media = round(sapply(num, mean, na.rm = TRUE), 2),
      Máx = sapply(num, max, na.rm = TRUE)
    )
    
    DT::datatable(summary_tbl, 
                  options = list(dom = 't', pageLength = 15),
                  rownames = FALSE)
  })
  
  # Mapeo de columnas
  output$colmap_ui <- renderUI({
    req(rv$data)
    cols <- names(rv$data)
    
    tagList(
      p(class = "help-text", "Asigne columnas relevantes para el análisis"),
      selectInput('map_age', '???? Edad', choices = c('--- Seleccionar ---', cols)),
      selectInput('map_bmi', '?????? IMC', choices = c('--- Seleccionar ---', cols)),
      selectInput('map_sbp', '???? Presión Sistólica', choices = c('--- Seleccionar ---', cols)),
      selectInput('map_smoke', '???? Fumador (0/1)', choices = c('--- Seleccionar ---', cols)),
      selectInput('map_diab', '???? Diabetes (0/1)', choices = c('--- Seleccionar ---', cols)),
      br(),
      actionButton('apply_map', 'Aplicar Mapeo', class = "btn-primary", 
                   icon = icon("check"), style = "width: 100%;")
    )
  })
  
  observeEvent(input$apply_map, {
    req(rv$data)
    df <- rv$data
    
    # Aplicar mapeo solo si se seleccionó una columna
    if(input$map_age != '--- Seleccionar ---') {
      df$age <- as.numeric(df[[input$map_age]])
    }
    if(input$map_bmi != '--- Seleccionar ---') {
      df$bmi <- as.numeric(df[[input$map_bmi]])
    }
    if(input$map_sbp != '--- Seleccionar ---') {
      df$systolic <- as.numeric(df[[input$map_sbp]])
    }
    if(input$map_smoke != '--- Seleccionar ---') {
      df$smoking <- as.numeric(df[[input$map_smoke]])
    }
    if(input$map_diab != '--- Seleccionar ---') {
      df$diabetes <- as.numeric(df[[input$map_diab]])
    }
    
    rv$mapped <- df
    showNotification("??? Mapeo aplicado. Vea Predicciones.", type = "message", duration = 3)
  })
  
  # Predicciones
  output$predictions <- DT::renderDataTable({
    df <- rv$mapped
    req(df)
    
    # Calcular predicciones
    df$prob_cvd <- cvd_score(df)
    df$prob_resp <- resp_score(df)
    df$riesgo <- risk_classification(df$prob_cvd, df$prob_resp)
    
    rv$predictions <- df
    
    # Actualizar estadísticas del dashboard
    rv$pacientes_procesados <- nrow(df)
    rv$riesgo_alto <- sum(df$riesgo == "???? Riesgo Alto")
    rv$riesgo_moderado <- sum(df$riesgo == "???? Riesgo Moderado")
    rv$riesgo_bajo <- sum(df$riesgo == "???? Riesgo Bajo")
    
    # Preparar datos para mostrar
    result_df <- df %>%
      select(any_of(c('age', 'bmi', 'smoking', 'diabetes', 'systolic')), 
             prob_cvd, prob_resp, riesgo) %>%
      mutate(
        prob_cvd = percent(prob_cvd, accuracy = 0.1),
        prob_resp = percent(prob_resp, accuracy = 0.1)
      )
    
    DT::datatable(
      result_df,
      options = list(
        pageLength = 10, 
        scrollX = TRUE, 
        dom = 'ftip'
      ),
      rownames = FALSE
    )
  })
  
  # Gráfico de distribución
  output$prob_hist <- renderPlotly({
    req(rv$predictions)
    df <- rv$predictions
    
    plot_ly() %>%
      add_histogram(x = ~df$prob_cvd, name = 'Cardiovascular',
                    marker = list(color = '#1E3A8A'), nbinsx = 20) %>%
      add_histogram(x = ~df$prob_resp, name = 'Respiratorio',
                    marker = list(color = '#0F766E'), nbinsx = 20) %>%
      layout(
        title = "Distribución de Probabilidades de Riesgo",
        barmode = 'overlay',
        xaxis = list(title = 'Probabilidad'),
        yaxis = list(title = 'Número de Pacientes'),
        hovermode = 'x unified'
      )
  })
  
  # Gráfico clasificación riesgo
  output$risk_dist <- renderPlotly({
    req(rv$predictions)
    df <- rv$predictions
    
    risk_counts <- df %>%
      group_by(riesgo) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(
        color = case_when(
          grepl('Alto', riesgo) ~ '#DC2626',
          grepl('Moderado', riesgo) ~ '#F59E0B',
          TRUE ~ '#10B981'
        )
      )
    
    plot_ly(risk_counts, x = ~riesgo, y = ~count, type = 'bar',
            marker = list(color = ~color)) %>%
      layout(
        title = "Pacientes por Nivel de Riesgo",
        xaxis = list(title = ''),
        yaxis = list(title = 'Cantidad'),
        showlegend = FALSE
      )
  })
  
  # Simulador SIR
  observeEvent(input$run_sir, {
    req(input$beta, input$gamma, input$S0, input$I0, input$R0, input$days)
    sim <- sir_sim(input$beta, input$gamma, input$S0, input$I0, input$R0, input$days)
    rv$sir <- sim
    showNotification("??? Simulación completada", type = "message", duration = 2)
  })
  
  # Gráfico SIR
  output$sir_plot <- renderPlotly({
    req(rv$sir)
    sim <- rv$sir
    
    plot_ly(sim, x = ~day) %>%
      add_lines(y = ~Susceptibles, name = 'Susceptibles',
                line = list(color = '#3B82F6', width = 3)) %>%
      add_lines(y = ~Infectados, name = 'Infectados',
                line = list(color = '#EF4444', width = 3)) %>%
      add_lines(y = ~Recuperados, name = 'Recuperados',
                line = list(color = '#10B981', width = 3)) %>%
      layout(
        title = "Dinámica SIR - Evolución Temporal",
        xaxis = list(title = 'Días'),
        yaxis = list(title = 'Número de Individuos'),
        hovermode = 'x unified'
      )
  })
}

shinyApp(ui, server)
