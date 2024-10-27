# Cargar librerías necesarias
library(shiny)
library(bslib)
library(dplyr)
library(highcharter)
library(gemini.R)
library(htmltools)
library(markdown)
library(tm)
library(dotenv)
library(bsicons)
library(thematic)
library(vembedr)

# Cargar las variables de entorno del archivo .env
dotenv::load_dot_env()

# Establecer la clave API de gemini usando la variable de entorno
api_key <- Sys.getenv("GEMINI_API_KEY")

# Verificar si se cargó la clave API
if (is.null(api_key) || api_key == "") {
  stop("No se encontró la clave API. Asegúrate de definir GEMINI_API_KEY en el archivo .env.")
}

# Establecer la clave API de gemini
setAPI(api_key)

# Cargar el archivo CSV con los discursos
file_path <- "Sesión Especial 09-10-2024 .csv"
if (file.exists(file_path)) {
  discursos_df <- read.csv(file_path, stringsAsFactors = FALSE)
} else {
  stop("El archivo no se encuentra en el directorio de trabajo.")
}

# Validar que el DataFrame tiene las columnas necesarias
required_columns <- c("fuerza_politica", "diputado", "text", "video_id")
if (!all(required_columns %in% colnames(discursos_df))) {
  stop("El DataFrame 'discursos_df' no contiene las columnas necesarias: fuerza_politica, diputado, text, video_id.")
}

# Definir el tema personalizado con bslib
# theme <- bs_theme(
#   bg = "#f7f9fc",
#   fg = "#343a40",
#   primary = "#007bff",
#   secondary = "#6c757d",
#   success = "#28a745",
#   info = "#17a2b8",
#   base_font = font_google("Poppins"),
#   heading_font = font_google("Nunito"),
#   code_font = font_google("Source Code Pro"),
#   "border-radius" = "10px",
#   "input-border-color" = "#ced4da"
# )

# Habilitar temático para ajustar el estilo de las gráficas a la paleta CSS
thematic::thematic_shiny()

# Función para generar una nube de palabras eliminando stopwords
generar_nube_palabras <- function(texto) {
  stopwords_es <- c(stopwords("es"), "que", "para", "como", "más", "muy", "pero", "también", "por", "entre", "sobre", "nos", "las", "los", "una", "a", "de", "del", "y", "e", "con", "el", "la", "su")
  
  palabras <- strsplit(tolower(texto), "\\W+")[[1]]
  palabras <- palabras[palabras != ""]
  palabras <- palabras[!palabras %in% stopwords_es]
  
  freq_palabras <- as.data.frame(table(palabras))
  colnames(freq_palabras) <- c("word", "n")
  freq_palabras <- freq_palabras[nchar(as.character(freq_palabras$word)) >= 3, ]
  freq_palabras <- freq_palabras[freq_palabras$n > 1, ]
  
  if (nrow(freq_palabras) == 0) {
    return(highchart() %>%
             hc_title(text = "Nube de Palabras - Sin datos suficientes") %>%
             hc_subtitle(text = "No hay suficientes palabras para generar la nube") %>%
             hc_chart(backgroundColor = "#f7f9fc"))
  }
  
  hchart(freq_palabras, "wordcloud", hcaes(name = word, weight = n)) %>%
    hc_title(text = "Nube de Palabras del Discurso") %>%
    hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.weight}") %>%
    hc_chart(backgroundColor = "#f7f9fc")
}

# Función para obtener la respuesta del modelo de Gemini con contexto mejorado
obtener_respuesta_sobre_discurso <- function(discurso, pregunta) {
  prompt <- paste(
    "Eres un experto en discursos políticos. A continuación te proporciono el discurso completo en el parlamento argentino del diputado/a:",
    "\n\n", discurso, 
    "\n\nTu tarea es la siguiente: ", pregunta, 
    "\nPor favor, brinda una respuesta clara basada en el discurso proporcionado."
  )
  
  tryCatch({
    respuesta <- gemini(prompt)
    return(respuesta)
  }, error = function(e) {
    return("Ups! no tengo datos para esa selección, por favor probá con otra.")
  })
}

# Definir la UI
ui <- page_navbar(
  theme = 
    #theme,
    bs_theme(
    bg = "#ffffff", fg = "#343a40", primary = "#98b2c9",
    secondary = "#6c757d",
    # base_font = font_google("Merriweather"),
    # heading_font = font_google("Playfair Display"),
    # code_font = font_google("Source Code Pro")
      base_font = font_google("Poppins"),
      heading_font = font_google("Nunito"),
      code_font = font_google("Source Code Pro"),
      "border-radius" = "10px",
      "input-border-color" = "#ced4da"
  ),
  title = div("ParlamentarIA", 
              style = "font-weight: bold; font-size: 24px; font-family: 'Playfair Display';"),
  
  # Descripción añadida debajo del título con ajuste de estilo
  nav_panel(
    title = "Análisis del Discurso",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(
            style = "margin-bottom: 10px; padding: 5px;",  # Reducir espacio
            p(HTML("ParlamentarIA es un <strong>sistema avanzado</strong> de monitoreo asistido por <strong>inteligencia artificial</strong> que permite <strong>analizar la actividad legislativa</strong> en tiempo real. Con acceso a discursos parlamentarios, el sistema proporciona <strong>análisis detallados</strong>, resúmenes automáticos y permite obtener <strong>insights clave</strong> sobre las posturas y argumentos de los legisladores. 
  Los usuarios simplemente seleccionan el <strong>partido</strong>, el <strong>diputado</strong> y el <strong>discurso</strong> para analizarlo en segundos. La herramienta también monitorea debates como el <strong>veto al presupuesto universitario</strong>, realizado el 9 de octubre, donde el oficialismo y sus aliados mantuvieron su postura apoyando el veto presidencial al aumento salarial de los docentes universitarios."),
              style = "text-align: left; font-size: 13px;"
            )
          )
          
        )
      ),
      
      # Mantener el contenido ya existente
      fluidRow(
        column(
          width = 3,
          card(
            card_header("Seleccione los parámetros", icon("filter"), style = "font-family: 'Merriweather'; font-size: 18px;"),
            card_body(
              selectInput("partido", "Seleccione el Partido:", choices = unique(discursos_df$fuerza_politica)),
              uiOutput("candidato_ui"),
              
              # Selector para tipo de análisis
              radioButtons(
                "pregunta_tipo", "Línea de análisis:",
                choices = list("Predefinida" = "predefinida", "Personalizada" = "personalizada"),
                selected = "predefinida"
              ),
              
              # Mostrar selectInput para análisis predefinidos solo cuando está seleccionada esa opción
              conditionalPanel(
                condition = "input.pregunta_tipo == 'predefinida'",
                selectInput("pregunta_predefinida", "Seleccione una línea de análisis predefinida:",
                            choices = list(
                              "Identificar cual es la posición del diputado/a frente al tema planteado"="Identificar cual es la posición del diputado/a frente al tema planteado",
                              "Efectuar un resumen de la exposición" = "Efectuar un resumen de la exposición",
                              "Destacar los 3 principales argumentos de la exposición" = "Destacar los 3 principales argumentos de la exposición",
                              "Mencionar los elementos propositivos del discurso" = "Mencionar los elementos propositivos del discurso",
                              "Identificar el mayor problema expuesto y los posibles culpables" = "Identificar el mayor problema expuesto y los posibles culpables"
                            ))
              ),
              
              # Mostrar textAreaInput solo si se selecciona la opción personalizada
              conditionalPanel(
                condition = "input.pregunta_tipo == 'personalizada'",
                textAreaInput("pregunta_personalizada", "Escribí tu propia orden de análisis:", "", height = "100px")
              ),
              
              actionButton("analizar", "Analizar Discurso", 
                           class = "btn btn-primary btn-lg btn-block mt-3",
                           style = "font-size: 14px; padding: 10px;"),
              actionButton("limpiar", "Limpiar Pregunta", 
                           class = "btn btn-secondary btn-lg btn-block mt-2", 
                           style = "font-size: 14px; padding: 10px;")
            )
          )
        ),
        
        column(
          width = 6,
          card(
            max_height = 450,
            full_screen = TRUE,
            card_header(
              "Contenido del Discurso",
              tooltip(icon("info-circle"), "Elija entre ver el video o la nube de palabras."),
              style = "font-family: 'Merriweather'; font-size: 18px;"
            ),
            card_body(
              navset_card_tab(
                tabPanel("Video", div(uiOutput("video_embed"), style = "min-height: 400px;")),
                tabPanel("Nube de Palabras", highchartOutput("nube_palabras", height = "400px"))
              )
            )
          )
        ),
        
        column(
          width = 3,
          card(
            full_screen = TRUE,
            card_header("Resultados del análisis", 
                        class = "bg-secondary",
                        icon("robot"),
                        style = "font-family: 'Playfair Display'; font-size: 20px;"),
            card_body(
              div(
                style = "background-color: #f8f9fa; padding: 15px; border: 1px solid #DDD; border-radius: 10px;",
                h5(" ", style = "font-family: 'Merriweather';"),
                uiOutput("respuesta_llm")
              )
            )
          )
        )
      )
    )
  ),
  # Panel de "Acerca de la Herramienta" con mejoras de estilo
  nav_panel(
    title = "Acerca de la Herramienta",
    fluidPage(
      # Instrucciones en forma de tarjetas con disposición 2x2
      fluidRow(
        column(
          width = 6,  # 6 de 12 para tener dos columnas por fila
          card(
            card_header("Funcionamiento", icon("cogs"),
                        #class = "bg-primary text-white"
                        ),
            card_body(
              p("Esta herramienta permite analizar discursos parlamentarios de manera avanzada, 
                utilizando videos oficiales publicados por la Cámara de Diputados de Argentina. 
                Ofrece la capacidad de seleccionar cualquier discurso, procesarlo con técnicas 
                de inteligencia artificial, y generar análisis detallados, resúmenes automáticos 
                y visualización interactiva de datos clave.", style = "text-align: justify;")
            ),
            style = "box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;"
          )
        ),
        column(
          width = 6,
          card(
            card_header("Contexto", icon("signal"),
                        #class = "bg-info text-white"
                        ),
            card_body(
              p("La herramienta monitorea el debate parlamentario sobre el veto al 
                presupuesto universitario, realizado el 9 de octubre. En dicho debate, 
                el oficialismo y sus aliados lograron mantener su postura, 
                apoyando el veto presidencial que rechazó el aumento salarial a
                los docentes universitarios en todo el país.", style = "text-align: justify;")
            ),
            style = "box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;"
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          card(
            card_header("Modelo utilizado", icon("pencil-alt"),
                       # class = "bg-success text-white"
                       ),
            card_body(
              p("El análisis de los discursos se basa en el modelo avanzado Gemini 
                de Google, diseñado para comprender el contexto político de manera 
                profunda. Este modelo permite interpretar con precisión las 
                intervenciones parlamentarias y generar respuestas claras y 
                específicas a preguntas complejas sobre las posturas y argumentos 
                expuestos.", style = "text-align: justify;")
            ),
            style = "box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;"
          )
        ),
        column(
          width = 6,
          card(
            card_header("Privacidad y Seguridad", icon("shield-alt"),
                        #class = "bg-warning text-dark"
                        ),
            card_body(
              p("La herramienta garantiza la privacidad de los 
                usuarios al no almacenar datos personales ni conservar información 
                de los análisis fuera del entorno seguro en el que se ejecuta. 
                Todo el procesamiento de los discursos es temporal y se realiza 
                de forma local, asegurando la confidencialidad total de los datos.", 
                style = "text-align: justify;")
            ),
            style = "box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;"
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          card(
            card_header("Créditos y código", icon("user-circle"),
                        class = "bg-primary text-black"),
            p(HTML("Esta herramienta fue desarrollada por <strong>Pedro Orden</strong>, 
                     sociólogo y científico de datos. Puedes encontrar más información sobre su 
                     trabajo en su <a href='https://linktr.ee/pedroorden' target='_blank'>Linktree</a>. 
                     El código de la aplicación está disponible de forma abierta en 
                     <a href='https://github.com/pedroorden/parlamentaria' target='_blank'>GitHub</a>, 
                     y puede ser reutilizado bajo la licencia 
                     <a href='https://creativecommons.org/licenses/by-nc/4.0/' target='_blank'>Creative Commons Attribution-NonCommercial 4.0</a>. 
                     Si reutilizas el código, por favor atribúyeme como creador, pero no se permite su uso con fines comerciales."), 
              style = "text-align: justify;"),
            style = "box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;"
          )
        )
      )
    )
  )
)

# Definir el servidor de la aplicación
server <- function(input, output, session) {
  
  # Actualizar menú de candidatos
  output$candidato_ui <- renderUI({
    req(input$partido)
    candidatos <- unique(discursos_df$diputado[discursos_df$fuerza_politica == input$partido])
    if (length(candidatos) == 0) {
      return(h4("No hay candidatos disponibles para este partido."))
    }
    selectInput("candidato", "Seleccione el Candidato:", choices = candidatos)
  })
  
  # Reactivo para el discurso seleccionado
  discurso_seleccionado <- eventReactive(input$candidato, {
    req(input$candidato)
    discurso <- discursos_df$text[discursos_df$diputado == input$candidato]
    return(discurso)
  })
  
  # Cargar el video automáticamente con vembedr
  observeEvent(input$candidato, {
    req(input$candidato)
    video_id <- discursos_df$video_id[discursos_df$diputado == input$candidato]
    
    output$video_embed <- renderUI({
      tryCatch({
        if (length(video_id) == 0 || is.na(video_id) || video_id == "") {
          showNotification("Ups, no tengo un video para mostrar para este candidato.", type = "error")
          return(h4("No se encontró un video para este discurso."))
        }
        embed_url(paste0("https://www.youtube.com/watch?v=", video_id)) %>% 
          use_bs_responsive() %>% 
          use_align("center") %>%
          use_rounded(10)
      }, error = function(e) {
        showNotification("Ups, ocurrió un error al cargar el video.", type = "error")
        return(h4("No se pudo cargar el video para este discurso."))
      })
    })
  })
  
  
  # Generar la nube de palabras
  observeEvent(input$candidato, {
    req(input$candidato)
    discurso <- discurso_seleccionado()
    output$nube_palabras <- renderHighchart({
      generar_nube_palabras(discurso)
    })
  })
  
  # Analizar el discurso
  observeEvent(input$analizar, {
    req(input$candidato)
    
    # Obtener la orden de análisis, ya sea predefinida o personalizada
    pregunta <- if (!is.null(input$pregunta_personalizada) && input$pregunta_personalizada != "") {
      input$pregunta_personalizada
    } else {
      input$pregunta_predefinida
    }
    
    discurso <- discurso_seleccionado()
    respuesta <- obtener_respuesta_sobre_discurso(discurso, pregunta)
    
    output$respuesta_llm <- renderUI({
      HTML(markdownToHTML(text = respuesta, fragment.only = TRUE))
    })
  })
  
  # Limpiar pregunta
  observeEvent(input$limpiar, {
    updateTextAreaInput(session, "pregunta_personalizada", value = "")
  })
  
  # Mostrar detalles del candidato
  output$detalles_candidato <- renderText({
    req(input$candidato)
    paste("Detalles del candidato", input$candidato, "del partido", input$partido, ".")
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
