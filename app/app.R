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
library(ggplot2)
library(scales)
library(ggrepel)
library(viridis)


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

elri <- readRDS("base_elri_preprocesada.rds") |> 
  mutate(ano = as.numeric(ano)) |> 
  filter(modulo != "Salud mental" ) |> 
  filter(modulo != "Salud mental" )

# variable <- unique(elri$variable_elegida)

# Lista de variables ------

#modulo_demo <- tribble(~modulo, ~variable_elegida, ~enunciado,
#                       "Sociodemográfico", "sexo", "Sexo",
#                       "Sociodemográfico", "indigena_es", "Autoidentificación",
#                       "Sociodemográfico", "edad", "Edad")
#


identidad <- c("a4cod", "a5cod", "a6cod", "a7cod",
               "c1cod", "c2cod",
               "d1_1cod", "d1_2cod", "d1_3cod")






variables <- elri |> 
  select(variable_elegida, enunciado, modulo) 

#variables <- bind_rows(modulo_demo,
#                       variables)
#
#variables_sociodemograficas <- variables |> filter(modulo == "Sociodemográfico") |> pull(variable_elegida)  
#
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
               h1("Estudio Longitudinal de Relaciones Interculturales (ELRI)",
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
           
           div(
             style = "text-align: center; font-size: 100%;",
             HTML('El estudio ELRI fue creado y organizado por el 
       <a href="https://www.ciir.cl" target="_blank" style="color: #FF69B4; font-weight: bold;">
       Centro de Estudios Interculturales e Indígenas (CIIR)</a>, en alianza con el 
       <a href="https://www.coes.cl" target="_blank" style="color: #40485e; font-weight: bold;">
       Centro de Estudios de Conflicto y Cohesión Social (COES)</a> y el 
       <a href="https://midap.org" target="_blank" style="color: #bbc261; font-weight: bold;">
       Instituto Milenio para la Investigación en Depresión y Personalidad (MIDAP)</a>.')
           ),
           
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
    column(width = 8,
           # style = "color: #4660E0;",
           # style = css(color = color_principal), # Formato R
           #p ("Seleccione una variable"),
           #strong ("Seleccione una variable"),
           #m ("Seleccione una variable"),
           selectInput(inputId = "variable", "Seleccione una pregunta", 
                       choices = lista_variables,
                       width = "100%" )),
    column(width = 4,     
           selectInput(inputId = "grupo", "Filtro muestral", 
                       choices = c("Autoidentificación indígena" = "muestra"#,
                                  # "Dato desagregado por Sexo y Autoidentificación" = "muestra+sexo"
                       ),
                       width = "100%"
           ),
    )
  ), 
  
  fluidRow(
    column(width = 12, 
           
           
           div(style = css(margin_top = "22px",# Margen de arriba
                           margin_bottom = "22px",
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
                            width = "100%"),
               
               plotOutput("plot",
                          height = "600px")
           ),
           
        # texto ------
           
        div(
          style = css(
            width = "100%",              # Ocupar 100% del espacio disponible
            margin = "auto",             
            margin_top = "24px",
            font_size = "100%"
          ),
          htmlOutput("texto")  # El texto se renderiza en este div
        ),   
           
           
           # tabla ----
           gt_output("tabla")
    )
  ),
  
  # Cuadrados.  -----
  
  fluidRow(
    tags$head(
      tags$style(HTML("
      .link-box {
        text-align: center; 
        margin: 10px; 
        padding: 15px; 
        border: 1px solid #ccc; 
        border-radius: 5px; 
        background-color: #D7CDFF; 
        min-height: 120px;
        transition: background-color 0.3s ease;
      }
      .link-box a {
        display: block; 
        width: 100%; 
        font-weight: bold; 
        text-decoration: none; 
        color: inherit;
      }
      .link-box:hover {
        cursor: pointer;
        background-color: #C2B4F0;
      }
      .link-box a:hover {
        text-decoration: underline;
        color: #333;
      }
    "))
    ),
    
    column(width = 2,  
           div(class = "link-box",
               a("Base de datos", href = "https://drive.google.com/drive/folders/1umtNgQdbssiGyoESN53fFpXqxwWfk_Wk?usp=share_link", target = "_blank")
           )
    ),
    
    column(width = 2,
           div(class = "link-box",
               a("Cuestionarios", href = "https://drive.google.com/drive/folders/1252Bml2OAvRWPT2kkR08hDqssxUOuAxO?usp=share_link", target = "_blank")
           )
    ),
    
    column(width = 2,
           div(class = "link-box",
               a("Libro de códigos", href = "https://drive.google.com/drive/folders/1aRl8B2rmylmJvsLSVlICPKyVlY_T69Ul?usp=share_link", target = "_blank")
           )
    ),
    
    column(width = 2,
           div(class = "link-box",
               a("Manual Metodológico", href = "https://drive.google.com/file/d/14tLL1DpVX27_pEBEPyi-tJ2vkcNnItd4/view?usp=share_link", target = "_blank")
           )
    ),
    
    column(width = 2,
           div(class = "link-box",
               a("Consentimiento informado", href = "https://drive.google.com/file/d/1lSy8t0mVrOCkL5A9gDBHGMeJ60h7Jktg/view?usp=share_link", target = "_blank")
           )
    ),
    
    column(width = 2,
           div(class = "link-box",
               a("Acta de aprobación ética", href = "https://drive.google.com/file/d/1e0S8tZj2zZuOyeg4iMCzFTbiVDKbKeUl/view?usp=share_link", target = "_blank")
           )
    )
  ),
  
  
  fluidRow(
    column(12,
           
           hr(), 
           
           div(
             style = "font-size: 70%; opacity: 70%; text-align: center;",
             HTML('Este proyecto fue realizado por la Unidad de Estudios Cuantitativos del  
        <a href="https://www.ciir.cl" target="_blank" style="color: #FF69B4; font-weight: bold;">Centro de Estudios Interculturales e Indígenas</a>

        <br><br>
        Equipo de Investigación:<br>
        <ul style="list-style-type: disc; padding-left: 20px; text-align: left; display: inline-block;">
          <li>Matías Deneken Uribe, Investigador de la Unidad de Estudios Cuantitativos - CIIR   <a href="m.deneken@alumni.uc.cl" target="_blank" style="color: #0000FF; font-weight: bold;">(m.deneken@alumni.uc.cl)</a>&#128231;</li>
           <li>Roberto Cantillán, Coordinador Técnico ELRI </li>
          <li>Bastián Olea Herrera, Desarrollador Web </li>
        </ul>')
           )
           
           
           ,
           
             
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
             
             # Dirección
             div(style = "margin-top: 20px; margin-bottom: 60px;",
                 markdown("Av. Vicuña Mackenna 4860")
             ),
             
             # Año
             div(style = "margin-top: 2px; margin-bottom: 6px;",
                 markdown("2024")
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
    #if (input$variable %in% variables_sociodemograficas) {
      
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
      
   #   plot <- ggplot()
      # ggplot(aes(as.factor(ano), porcentaje, col = variable, 
      #            group = variable)) +
      # geom_line() +
      # geom_text(aes(label = scales::percent(porcentaje,2)), 
      #           position = position_stack(vjust = 0.5), size = 4) +
      # # facet_wrap(~indigena_es + sexo, nrow = 1, scales = "free_x") +
      # labs(x = "Ola de medición",
      #      y = "Porcentaje de respuesta",
      #      fill = "Dimensión")
      
      ##YA NO ME FILTRA BIEN ¿NO SÉ POR QUÉ?
    ## lineas ----
    #} else
      if (input$variable %in% identidad) {      
     
         cat("Gráfico de líneas")
        
        # browser()

        plot <- elri_variable() |> 
          ggplot(aes(x = ano, y = porcentaje, col = variable, group = variable)) +
          geom_line(lineend = "round", linewidth = 1.5) +  # Grosor de la línea
          geom_point(size = 2) +  # Puntos pequeños para marcar los años
          geom_text_repel(aes(label = scales::percent(porcentaje, accuracy = 1.1)),
                          nudge_y = 0.04,
                          size = 5,  # Tamaño del texto para los porcentajes
                          show.legend = FALSE,
                          direction = "y",
                          force = 3,
                          min.segment.length = 3) +
          scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                             breaks = unique(elri_variable()$ano)) +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          facet_wrap(~indigena_es , nrow = 1, scales = "free_y") +
          theme_classic(base_size = 24) +  # Tamaño base del texto
          theme(panel.grid.major.y = element_line(color = "grey90"),
                axis.title.x = element_text(size = 20, face = "bold"),  # Títulos en negrita
                axis.title.y = element_text(size = 20, face = "bold"),
                axis.text = element_text(size = 18),  # Texto de los ejes más grande
                strip.text = element_text(size = 22, face = "bold"),  # Títulos de facetas en negrita
                legend.position = "top",  # Leyenda arriba
                legend.text = element_text(size = 18),  # Texto de leyenda más grande
                plot.margin = margin(10, 10, 10, 10))  # Márgenes más amplios
        
        
      
      # dev.new()  
      
    } else {
      
      cat ("Gráfico de barra")
      
      ## gráfico de barras normal ----
      #browser()
      
      plot <- elri_variable() |> 
        ggplot(aes(as.factor(ano), porcentaje * 100, fill = variable)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = ifelse(porcentaje > 0.05, 
                                     scales::percent(porcentaje, accuracy = 1), 
                                     "")), 
                  position = position_stack(vjust = 0.5), 
                  size = 4, 
                  color = "white",    # Cambia el color de las letras a blanco
                  show.legend = FALSE) +
        facet_wrap(~indigena_es, nrow = 1, scales = "free_x") +
        scale_y_continuous(limits = c(0, 101), labels = scales::percent_format(scale = 1)) +
        theme_classic(base_size = 22)
      
    }
    
    plot <- plot +  
      guides(fill = guide_legend(position = "top"),
             color = guide_legend(position = "top")) + 
      labs(x = "Ola de medición",
           y = "Porcentaje de respuesta",
           fill = "Dimensión",
           color = "Dimensión") +
      theme(strip.background.x = element_rect(linewidth = 0),
            strip.text = element_text(size = 20)) +  # Eliminar bordes de facetas
      scale_fill_viridis_d(option = "viridis") +     # Paleta viridis
      scale_color_viridis_d(option = "viridis")      # Colores viridis para líneas
    
    
    return(plot)
  })
  
  # textos ----
  
  output$texto <- renderUI({ #Genera htlm.
    
    variable <- input$variable
    # variable <- "otras"
    
    archivo <- paste0("texto/", variable, ".md")
    
    archivo <- ifelse(file.exists(archivo) == FALSE, "texto/otras.md", archivo)
    
    includeMarkdown(archivo)
    
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
    
    tabla_2 <- tabla_1 |>  ## ¿Cómo le puedo añadir color??
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
