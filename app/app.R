#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(htmltools)
library(bslib)
library(shades)
library(thematic)
library(fresh)
library(gt)

color_base = "#365c8d"
color_principal = "#3a87c8"
color_fondo = "#FFFFFF"
color_negativo = "#e5084b"
color_detalle = "#964191" #"#770072" |> lighten(0.2)
color_texto = "#555" #"#3a1582" |> lighten(0.2)
#
color_base <- "#5460E0"
#color_principal = color_base |> saturation(delta(-0.05)) |> as.character()
#color_fondo = color_base |> brightness(delta(-0.65)) |> saturation(delta(-0.4)) |> as.character()
#color_fondo = "white"
#color_detalle = color_base |> brightness(delta(-0.3)) |> saturation(delta(-0.5)) |> as.character()
#color_texto = color_base |> chroma(70) |> lightness(95) |> as.character()

swatch(c(color_base, color_principal, color_fondo, color_detalle))


thematic_shiny(font = "auto",
               bg = color_fondo, fg = color_texto, 
               accent = color_detalle)

#elri <- readRDS("app/base_elri_preprocesada.rds") |> 
 #filter(modulo != "Salud mental" )

elri <- readRDS("base_elri_preprocesada.rds")  |> 
  filter(modulo != "Salud mental" )

# variable <- unique(elri$variable_elegida)

# Lista de variables ------

modulo_demo <- tribble(~modulo, ~variable_elegida, ~enunciado,
                       "Sociodemográfico", "sexo", "Sexo",
                       "Sociodemográfico", "indigena_es", "Autoidentificación",
                       "Sociodemográfico", "edad", "Edad")


variables <- elri |> 
  select(variable_elegida, enunciado, modulo) 

variables <- bind_rows(modulo_demo,
                       variables)

variables_sociodemograficas <- variables |> filter(modulo == "Sociodemográfico") |> pull(variable_elegida)  

modulos <- variables |> pull(modulo) |> unique()


lista_variables <- variables |> 
  select(enunciado, variable_elegida) |> 
  distinct() |> 
  deframe()




# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "Encuesta ELRI", lang = "es",
  
  #use_googlefont("Oswald"),
  
  # tema de la app
  theme = bs_theme(
    bg = color_fondo,
    fg = color_texto,
    primary = color_principal,
    font_scale = 1.3,
    base_font = bslib::font_google("Open Sans")
  ),
  
  
  
  
  # fluidRow(
  #   column(width = 8, 
  #          h1 ("Prueba: Encuesta Longitudinal de Relaciones Interculturales"),
  #   ),
  #   column(width = 4, 
  #          # div(style = css(margin_left = "0px", margin_right = "auto"),
  #            tags$img(src = "logo.svg",
  #                     style = css(height = "200px")
  #                     )
  #          
  #   )
  # ),
  
  fluidRow(
    column(width = 12, 
           style = "margin-bottom: 50px; position: relative;",
           
           # Imágenes centradas y una al lado de la otra
           div(
             style = "text-align: center;",
             div(
               style = "display: inline-block; margin-top: 20px;",
               tags$img(src = "logo.svg", style = "height: 80px; margin-right: 10px; opacity: 0.8;"),
               tags$img(src = "udp.jpeg", style = "height: 80px; margin-right: 10px; opacity: 0.8;"),
               tags$img(src = "puc.png", style = "height: 80px; margin-right: 10px; opacity: 0.8;"),
               tags$img(src = "uahc.jpeg", style = "height: 80px; opacity: 0.8;"),
               tags$img(src = "anid.png", style = "height: 80px; margin-right: 10px; opacity: 0.8;")
               
             ), 
           
           
           
           
           # Título centrado con estilo
           div(style = "text-align: center; max-width: 90%; margin-top: 20px;",
               h1("Estudio Longitudinal de Relaciones Interculturales",
                  style = "font-family: 'Arial', sans-serif;
                font-size: 2.5em;
                font-weight: bold;
                background: linear-gradient(to right, #4660E0, #FFA07A);
                -webkit-background-clip: text;
                -webkit-text-fill-color: transparent;
                text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
                margin-bottom: 20px;"
               )
           ),
           
        #  # Texto adicional debajo del título
        #  div(style = "text-align: center; max-width: 90%; margin-top: 10px;",
        #      h3("La encuesta del cambio intercultural de Chile",
        #         style = "font-family: 'Arial', sans-serif;
        #       font-size: 1.4em;
        #       font-weight: bold;
        #       background: linear-gradient(to right, #C395AD, #000000);
        #       -webkit-background-clip: text;
        #       -webkit-text-fill-color: transparent;
        #       text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
        #       margin-bottom: 20px;"
        #      )
        #  ),
          
           )
    )
  )
  
  ,
  
  
  #luidRow(
  # column(width = 12,
  #        # style = "color: #4660E0;",
  #        # style = css(color = color_principal), # Formato R
  #        p ("Seleccione un módulo"),
  #        #strong ("Seleccione una variable"),
  #        #m ("Seleccione una variable"),
  #        selectInput(inputId = "modulo", "Seleccionar", choices = variable),
  # ), 
  
  
  ##intro ----
  fluidRow(
    column(12, style = "margin-top: -26px;",
           
           verbatimTextOutput("posicion_vertical"),
           
           markdown("El Estudio Longitudinal de Relaciones Interculturales (ELRI) es una medición que nace el año 2016 y 
           finaliza en 2023 con el objetivo de **dar cuenta y analizar las diversas relaciones interculturales entre aquellas personas que se identifican con alguna etnia dentro de los principales grupos indígenas del país y la población no indígena del país**.
"),
           
           markdown("Específicamente, se buscó levantar información de alta calidad respecto a cinco sub-temáticas principales: 
                    (1) Identificación étnica y relaciones familiares, (2) relaciones intergrupales (3) conflicto social, (4) políticas públicas, (5) salud física y mental."),
           
          markdown("El estudio ELRI fue creado y organizado por el Centro de Estudios Interculturales e Indígenas, CIIR."),
                  
           hr()
           
           
    )
  ),
  
  # 
  
 
  
  # Selectores -----
  
  fluidRow(
    column(width = 12,
           # style = "color: #4660E0;",
           # style = css(color = color_principal), # Formato R
           #p ("Seleccione una variable"),
           #strong ("Seleccione una variable"),
           #m ("Seleccione una variable"),
           selectInput(inputId = "modulo", "Seleccione un módulo", 
                       choices = modulos,
                       width = "100%" )
    )
  ),
  
  fluidRow(
    column(width = 6,
           # style = "color: #4660E0;",
           # style = css(color = color_principal), # Formato R
           #p ("Seleccione una variable"),
           #strong ("Seleccione una variable"),
           #m ("Seleccione una variable"),
           selectInput(inputId = "variable", "Seleccione una pregunta", 
                       choices = lista_variables,
                       width = "100%" )),
    column(width = 6,     
           selectInput(inputId = "grupo", "Filtro muestral", 
                       choices = c("Dato desagregado por autoidentificación indígena" = "muestra",
                                   "Dato desagregado por Sexo y Autoidentificación" = "muestra+sexo"
                       ),
                       width = "100%"
           ),
    )
  ), 
  
  fluidRow(
    column(width = 12, 
           
           
           div(style = css(margin_top = "20px",# Margen de arriba
                           margin_bottom = "20px",
                           opacity = "100%",
                           color =  "white"),
               h1(textOutput("titulo")),
           ),
           
           #div("Contraste indígena y no indígena",
           #    style = css(background = color_detalle,
           #                margin = "20px",
           #                padding = "12px",
           #                border_radius = "6px"
           #    )
           #  ),
           
           
           # grafico ----
           div(style = css (margin = "auto",
                            width = "700px"),
               
               plotOutput("plot")
           ),
           
           # tabla ----
           gt_output("tabla")
    )
  ),
  
  fluidRow(
    # Primera columna: Acceso a base de datos
    column(width = 3,  # 3 columnas para cada uno (12/4 = 3)
           div(style = "text-align: center; margin: 10px; padding: 15px; 
                    border: 1px solid #ccc; border-radius: 5px; 
                    background-color: #D7CDFF; min-height: 120px;",
               a("Base de datos", href = "https://www.google.com", target = "_blank",
                 style = "display: block; width: 100%; font-weight: bold; text-decoration: none; color: inherit;")
           )
    ),
    
    # Segunda columna: Acceso al manual metodológico
    column(width = 3,
           div(style = "text-align: center; margin: 10px; padding: 15px; 
                    border: 1px solid #ccc; border-radius: 5px; 
                    background-color: #D7CDFF; min-height: 120px;",
               a("Manual Metodológico", href = "https://www.google.com", target = "_blank",
                 style = "display: block; width: 100%; font-weight: bold; text-decoration: none; color: inherit;")
           )
    ),
    
    # Tercera columna: Acceso a libro de código
    column(width = 3,
           div(style = "text-align: center; margin: 10px; padding: 15px; 
                    border: 1px solid #ccc; border-radius: 5px; 
                    background-color: #D7CDFF; min-height: 120px;",
               a("Libro de códigos", href = "https://www.google.com", target = "_blank",
                 style = "display: block; width: 100%; font-weight: bold; text-decoration: none; color: inherit;")
           )
    ),
    
    # Cuarta columna: Aprobación ética
    column(width = 3,
           div(style = "text-align: center; margin: 10px; padding: 15px; 
                    border: 1px solid #ccc; border-radius: 5px; 
                    background-color: #D7CDFF; min-height: 120px;",
               a("Acta de aprobación ética", href = "https://www.google.com", target = "_blank",
                 style = "display: block; width: 100%; font-weight: bold; text-decoration: none; color: inherit;")
           )
    )
  ),
           
           
  
  fluidRow(
    column(12,
           
           hr(), 
           
           div(
             style = "font-size: 70%; opacity: 70%; text-align: center;",
             markdown("Este proyecto fue realizado por el **CIIR**
                      
                      Unidad de Estudios Cuantitativos 
                      
                      Equipo de Investigación:
                    *  Matías Deneken Uribe, Coordinador Técnico e Investigador ELRI
                    *  Bastián Olea Durán, Web Developer y Data Scientist"),
             
             # Imágenes una al lado de la otra
             div(
               style = "display: inline-block; margin-top: 20px;",
               tags$img(src = "logo.svg", style = "height: 70px; margin-right: 10px;"),
               tags$img(src = "udp.jpeg", style = "height: 80px; margin-right: 10px;"),
               tags$img(src = "puc.png", style = "height: 80px; margin-right: 10px;"),
               tags$img(src = "anid.png", style = "height: 80px; margin-right: 10px;"),
               tags$img(src = "uahc.jpeg", style = "height: 70px;")
             ),
             
             div(style = "margin-top: 20px; margin-bottom: 60px;",
                 markdown("Santiago, Chile. 2024.")
             )
           )
           
    )
  )
)  




# Define server logic ----
server <- function(input, output, session) {
  
  
  lista_variables <- reactive({
    variables |> 
      # filter(modulo == "Módulo Identidad") |> 
      filter(modulo == input$modulo) |>
      select(enunciado, variable_elegida) |> 
      distinct() |> 
      deframe()
  })
  
  observe({
    message("actualizando selector de variable")
    updateSelectInput(session,
                      "variable",
                      choices = lista_variables()
    )
  })
  
  
  # Dato por graficar------
  elri_variable <-  reactive({
    message("Variable elegida:", input$variable)
    dato <- elri |> filter(variable_elegida == input$variable) |> 
      # filter(grupo == "muestra")
      filter(grupo == input$grupo ) 
    
    #browser()
    print(dato)
    return(dato)
    
  })
  
  # Titular grafico ----
  
  output$titulo <- renderText({
    # browser()
    variables |> 
      filter(variable_elegida == input$variable) |> 
      pull(enunciado) |> 
      unique()
  })
  
  ## Outpout -----
  
  output$plot <- renderPlot({
    
    
    ### sociodemográfico ----
    if (input$variable %in% variables_sociodemograficas) {
      
      # if (input$variable == "sexo") 
      # lista_graficos()[[input$variable]]
      # browser()
      # elri |> 
      #   filter(grupo == "muestra") |>
      #   select(-variable_elegida) |> 
      #   rename(variable_elegida = input$variable) |> 
      #   ggplot(aes(as.factor(ano), porcentaje, fill = variable)) +
      #   geom_col()
      
      # plot <- elri |>
      #     filter(grupo == "muestra") |>
      #     select(-variable_elegida) |>
      #     rename(variable_elegida = input$variable) |>
      
      
      # habría que calcularlo en el script de precalcular
      
      plot <- ggplot()
      # ggplot(aes(as.factor(ano), porcentaje, col = variable, 
      #            group = variable)) +
      # geom_line() +
      # geom_text(aes(label = scales::percent(porcentaje,2)), 
      #           position = position_stack(vjust = 0.5), size = 4) +
      # # facet_wrap(~indigena_es + sexo, nrow = 1, scales = "free_x") +
      # labs(x = "Ola de medición",
      #      y = "Porcentaje de respuesta",
      #      fill = "Dimensión")
      
      
      ### gráfico de lineas ----
    } else if (input$variable %in% c("a4cod", "a5cod")) {
      
      # browser()
      plot <- elri_variable() |> 
        ggplot(aes(as.factor(ano), porcentaje, col = variable, 
                   group = variable)) +
        geom_line() +
        geom_text(aes(label = scales::percent(porcentaje,2)),
                  nudge_y = 0.02,
                  # position = position_stack(vjust = 0.5),
                  size = 4) +
        scale_y_continuous(limits = c(0, 1)) +
        facet_wrap(~indigena_es + sexo, nrow = 1, scales = "free_y") +
        labs(x = "Ola de medición",
             y = "Porcentaje de respuesta",
             fill = "Dimensión")
      
      # dev.new()  
      
    } else {
      
      ## gráfico de barras normal ----
      plot <- elri_variable() |> 
        ggplot(aes(as.factor(ano), porcentaje, fill = variable)) +
        geom_col() +
        geom_text(aes(label = scales::percent(porcentaje,2)), 
                  position = position_stack(vjust = 0.5), size = 4) +
        facet_wrap(~indigena_es + sexo, nrow = 1, scales = "free_x") +
        labs(x = "Ola de medición",
             y = "Porcentaje de respuesta",
             fill = "Dimensión")
    }
    
    return(plot)
  })
  
  # tabla ----
  output$tabla <- render_gt({
    message("Haciendo tabla")
    # elri_variable()
    
    # browser()
    
    tabla_1 <- elri_variable() |> 
      select(ano, grupo, porcentaje,
             variable, indigena_es)
    
    
    # ejecutar si hay missing
    if (is.na(tabla_1$porcentaje) |> any()) {
      tabla_1 <- tabla_1 |> 
        # relleno de NA
        group_by(variable) |>
        fill(porcentaje, .direction = "up")
      # print(n=Inf)
    }
    
    tabla_2 <- tabla_1 |> 
      ungroup() |> 
      arrange(indigena_es, variable, ano) |> 
      group_by(grupo, variable, indigena_es) |> 
      mutate(cambio = (porcentaje/lag(porcentaje)-1),
             cambio = ifelse(is.na(cambio), 0, cambio)) |> 
      mutate(flecha = case_when(cambio >= 0 ~ "arrow-up",
                                cambio < 0 ~ "arrow-down"))
    
    tabla_3 <-tabla_2 |> 
      select(-cambio) |> 
      pivot_wider(names_from = ano, 
                  values_from = c(flecha, porcentaje))
    
    # tabla_1
    
    # browser()
    
    tabla_out <- tabla_3 |> 
      ungroup() |> 
      select(-grupo) |> 
      select(-flecha_2016) |> 
      # relocate(flecha_2016, .before = porcentaje_2016) |>
      relocate(flecha_2018, .before = porcentaje_2018) |>
      relocate(flecha_2021, .before = porcentaje_2021) |>
      relocate(flecha_2023, .before = porcentaje_2023) |>
      rename_with(~str_remove(.x, "porcentaje_"), .cols = starts_with("porcentaje")) |> 
      gt() |> 
      fmt_icon(columns = starts_with("flecha")) |> 
      fmt_percent(columns = where(is.numeric),
                  decimals = 1) |> 
      cols_label(variable = "Dimensión",
                 indigena_es = "Identificación",
                 flecha_2018 = "",
                 flecha_2021 = "",
                 flecha_2023 = "") |> 
      data_color(columns = c(indigena_es), 
                 method = "factor", apply_to = "text",
                 palette = c("#357980", "#EE6A9B"),
                 na_color = color_texto) |> 
      tab_style(style = cell_text(weight = "bold"), 
                locations = cells_column_labels()) |> 
      # tab_style(locations = cells_body(columns = where(is.numeric)),
      # style = list(cell_borders(color = color_fondo, sides = "right", weight = px(24))
      # cell_fill(color = "red"),
      # cell_text(style = "italic"))
      # )) |> 
      tab_options(table.font.color = color_texto, 
                  table.background.color = color_fondo,
                  table.font.color.light = color_texto, 
                  table_body.hlines.color = color_detalle,
                  table_body.vlines.color = color_detalle,
                  column_labels.border.top.color = color_fondo, 
                  column_labels.border.bottom.color = color_detalle, 
                  table_body.border.bottom.color = color_detalle) 
    # table.font.names = "IBM Plex Mono")
    
    return(tabla_out)
  }) #Función reactiva, re-ejecutan con los input
  
  
  # output$titulo <- renderText({
  #   message("Haciendo texto")
  #   input$variable
  #   
  # }) 
  
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
