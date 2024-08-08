


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

variables <- c("d1_1cod",
               "d1_2cod", "a7cod")

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
    ggplot(aes(ano, porcentaje, fill = variable)) +
    geom_col() +
    facet_wrap(~indigena_es + sexo, nrow = 1)

plot

# 
# } else {
# }

# selecionar paletas ----
if (variable_elegida %in% variables_semaforo) {
    plot + 
        scale_fill_manual(values = c("red", "green", "yellow"))
} else if (variable_elegida %in% variables_) {
    plot + 
        scale_fill_manual(values = c("red", "green", "yellow"))
} else {
}

