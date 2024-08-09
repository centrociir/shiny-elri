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

color_base <- "#5360E0"
color_principal = color_base |> saturation(delta(-0.05)) |> as.character()
color_fondo = color_base |> brightness(delta(-0.65)) |> saturation(delta(-0.4)) |> as.character()
color_detalle = color_base |> brightness(delta(-0.3)) |> saturation(delta(-0.5)) |> as.character()
color_texto = color_base |> chroma(70) |> lightness(95) |> as.character()


thematic_shiny(font = "auto",
               bg = color_fondo, fg = color_texto, 
               accent = color_detalle)

#elri <- readRDS("app/base_elri_preprocesada.rds") |> 
 # filter(modulo != "Salud mental" )

elri <- readRDS("base_elri_preprocesada.rds")  |> 
  filter(modulo != "Salud mental" )





# variable <- unique(elri$variable_elegida)

variable <- elri |> 
  select(variable_elegida, enunciado) |>
  mutate(etiqueta = paste("Pregunta", enunciado)) |>
  select(etiqueta, variable_elegida) |> 
  distinct() |> 
  deframe()


# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "Encuesta ELRI", lang = "es",
  
  # tema de la app
  theme = bs_theme(
    bg = color_fondo,
    fg = color_texto,
    primary = color_principal,
    font_scale = 1.3,
    base_font = bslib::font_google("Calibri")
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
  
  fluidRow(
    column(width = 12,
           # style = "color: #4660E0;",
           # style = css(color = color_principal), # Formato R
           p ("Seleccione una variable"),
           #strong ("Seleccione una variable"),
           #m ("Seleccione una variable"),
           selectInput(inputId = "variable", "Seleccionar", choices = variable),
    ), 
    
    column(width = 12, 
           
           
           div(style = css(margin_top = "20px",
                           margin_bottom = "20px",
                           opacity = "20%"),
               h1(textOutput("titulo")),
           ),
           
           div("Contraste indígena y no indígena",
               style = css(background = color_detalle,
                           margin = "20px",
                           padding = "12px",
                           border_radius = "6px"
               )
           ),
           
           
           
           div(style = css (margin = "auto",
                            width = "700px"),
               
               plotOutput("plot")
           ),
               
               
               tableOutput("tabla")
    )
  )  
  
  
  
  
)



# Define server logic ----
server <- function(input, output) {
  
  
  
  elri_variable <- reactive({
    message("Variable elegida:", input$variable)
    elri |> filter(variable_elegida == input$variable) |> 
      filter(grupo == "muestra")
    
  })
  ## Outpout -----
  
  output$plot <- renderPlot({
    elri_variable() |> 
      ggplot(aes(as.factor(ano), porcentaje, fill = variable)) +
      geom_col() +
      geom_text(aes(label = scales::percent(porcentaje,2)), 
                position = position_stack(vjust = 0.5), size = 4) +
      facet_wrap(~indigena_es + sexo, nrow = 1, scales = "free_x") +
      labs(x = "Ola de medición",
           y = "Porcentaje de respuesta",
           fill = "Dimensión")
    
    
  })
  
  
  output$tabla <- renderTable({
    message("Haciendo tabla")
    elri_variable()
    
  }) #Función reactiva, re-ejecutan con los input
  
  
  output$titulo <- renderText({
    message("Haciendo texto")
    input$variable
    
  }) 
  
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
