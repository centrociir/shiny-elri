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

color_base <- "#5460E0"
color_principal = color_base |> saturation(delta(-0.05)) |> as.character()
color_fondo = color_base |> brightness(delta(-0.65)) |> saturation(delta(-0.4)) |> as.character()
#color_fondo = "white"
color_detalle = color_base |> brightness(delta(-0.3)) |> saturation(delta(-0.5)) |> as.character()
color_texto = color_base |> chroma(70) |> lightness(95) |> as.character()

swatch(c(color_base, color_principal, color_fondo, color_detalle))


thematic_shiny(font = "auto",
               bg = color_fondo, fg = color_texto, 
               accent = color_detalle)

#elri <- readRDS("app/base_elri_preprocesada.rds") |> 
# filter(modulo != "Salud mental" )

elri <- readRDS("base_elri_preprocesada.rds")  |> 
  filter(modulo != "Salud mental")

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
  
  
  
  
  fluidRow(
    column(width = 12, 
           h1 ("Prueba: Encuesta Longitudinal de Relaciones Interculturales"))
  ),
  
  #luidRow(
  # column(width = 12,
  #        # style = "color: #4660E0;",
  #        # style = css(color = color_principal), # Formato R
  #        p ("Seleccione un módulo"),
  #        #strong ("Seleccione una variable"),
  #        #m ("Seleccione una variable"),
  #        selectInput(inputId = "modulo", "Seleccionar", choices = variable),
  # ), 
  
  # Selectores -----
  
  fluidRow(
    column(width = 12,
           # style = "color: #4660E0;",
           # style = css(color = color_principal), # Formato R
           #p ("Seleccione una variable"),
           #strong ("Seleccione una variable"),
           #m ("Seleccione una variable"),
           selectInput(inputId = "modulo", "Seleccionar", 
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
           selectInput(inputId = "variable", "Seleccionar", 
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
         tableOutput("tabla")
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
  
  
  output$tabla <- renderTable({
    message("Haciendo tabla")
    elri_variable()
    
  }) #Función reactiva, re-ejecutan con los input
  
  
  # output$titulo <- renderText({
  #   message("Haciendo texto")
  #   input$variable
  #   
  # }) 
  
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
