library(purrr)
library(ggplot2)
library(dplyr)
library(srvyr)
library(survey)


resultados <- readRDS("bbdd/base_elri_preprocesada.rds")

# para llegar a un dato específico, hay que filtrar el dataframe por la variable a graficar, y la desagregación (grupo)
resultados |> 
    filter(grupo == "muestra",
           variable_elegida == "d1_1cod")



# definir de antemano los parámetros para el filtro y gráfico
# .variable = variables[1]
.variable = variables[4]

.grupo = "muestra+sexo"
# .grupo = "muestra"



# revisar todas las variables

variables <- c("a4cod", "a5cod", "a6cod", "a7cod", 
               "c1cod", "c2cod", "c4cod", "c5cod", "c6_1cod", 
               "c6_2cod", "c6_3cod", "c6_4cod", "d1_1cod", "d1_2cod",
               "d1_3cod", "d3_1cod", "d3_2cod", "d4_2cod")


grupos <- c("muestra+sexo", "muestra")

# diagnóstioc automático
walk(variables, \(.variable) {
    walk(grupos, \(.grupo) {
        dato <- resultados |> 
            filter(grupo == .grupo,
                   variable_elegida == .variable)
        
        message("la variable ", .variable, " con la desagregación ", .grupo, " tiene ", nrow(dato), " filas")
    })
})



# query ----
# filtra en base a los parámetros especificados
dato <- resultados |> 
    filter(grupo == .grupo,
           variable_elegida == .variable)

# seleccionar tipo de gráfico ----

# if (variable_elegida %in% variables_grafico_de_barra) {

# en caso de error
if (nrow(dato) == 0) {
    plot <- ggplot() +
        annotate("text", label = "error", x = 1, y = 1) +
        theme_void()
}

plot <- dato |> 
  #filter(variable == 3) |> 
  ggplot(aes(as.factor(ano), porcentaje, fill = variable)) +
  geom_col() +
  geom_text(aes(label = scales::percent(porcentaje,2)), 
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~indigena_es + sexo, nrow = 1, scales = "free_x") +
  labs(x = "Ola de medición",
       y = "Porcentaje de respuesta",
       fill = "Dimensión")



plot <- dato |> 
  #filter(variable == 3) |> 
  ggplot(aes(as.factor(ano), porcentaje, color = variable, group = variable)) +
  geom_line() +
  geom_text(aes(label = scales::percent(porcentaje,2)), 
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~indigena_es + sexo, nrow = 1, scales = "free_x") +
  labs(x = "Ola de medición",
       y = "Porcentaje de respuesta",
       fill = "Dimensión")

plot

plot <- dato |>  
  ggplot(aes(y = porcentaje, x = ano, muestra, color = variable, group = variable,
                      label = as.character(scales::percent(porcentaje, accuracy = .1)))) +
  geom_line(size = 1) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  ylab(label = NULL) +
  xlab(label = NULL)


plot

# 
# } else {
# }

# selecionar paletas ----
if (variable %in% variables_semaforo) {
    plot + 
        scale_fill_manual(values = c("red", "green", "yellow"))
} else if (variable_elegida %in% variables_) {
    plot + 
        scale_fill_manual(values = c("red", "green", "yellow"))
} else {
}

