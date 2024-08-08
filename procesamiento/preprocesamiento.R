library(purrr)
library(ggplot2)
library(dplyr)
library(srvyr)
library(survey)

elri_recodificada <- readRDS("bbdd/elri_recodificada.rds")

# vector de nombres de columnas que queremos incluir en el precalculo
variables <- c("a4cod", "a5cod", "a6cod", "a7cod", 
               "c1cod", "c2cod", "c4cod", "c5cod", "c6_1cod", 
               "c6_2cod", "c6_3cod", "c6_4cod", "d1_1cod", "d1_2cod",
               "d1_3cod", "d3_1cod", "d3_2cod", "d4_2cod")

# vectores de varibales que queremos usar para personalizar los gráficos
# vector de variables que son con barras
variables_barra <- c("d4_3cod")

# vector de variables que son con lineas

# vector de variables que son con torta

# vector de variables que tienen paleta de semaforo

# vector de variables que tienen paleta de semaforo inverso

# vector de variables que tienen paleta de


# # ejemplos de uso de purrr
# resultado <- purrr::map(variables, toupper)
# 
# resultado |> 
#     list_rbind()
# 
# resultado <- purrr::map(variables, ~{
#     print(.x)
# })
# 
# resultado <- purrr::map(variables, \(variable) {
#     print(variable)
# })


# loop de preprocesamiento ----

# el loop avanza por cada variable, y 

resultado <- purrr::map(variables |> set_names(), \(.variable) {
    message(.variable)
    # filtrar y renombrar
    # elige la variable a calcular, renombrándola por "variable"
    elri_codificada_2 <- elri_recodificada |> 
        rename(variable = .variable) |> 
        mutate(variable_elegida = .variable) #esta queda para caracterizar el dataframe
    
    # diseño
    elri_diseño <- srvyr::as_survey_design(
        elri_codificada_2,
        ids = NULL,
        strata = NULL,
        weights = pond)
    
    # calcular por indígena
    resultado_a <- svytable(~variable + ano + indigena_es, 
                            elri_diseño, round = F) |> 
        as_tibble() |> 
        group_by(ano, indigena_es) |> 
        mutate(porcentaje = n/sum(n)) |> 
        mutate(variable_elegida = .variable) |> 
        ungroup() |> 
        mutate(grupo = "muestra")
    
    # calcular por indígena y sexo
    resultado_b <- svytable(~variable + ano + indigena_es + sexo, 
                            elri_diseño, round = F) |> 
        as_tibble() |> 
        group_by(ano, indigena_es) |> 
        mutate(porcentaje = n/sum(n)) |> 
        mutate(variable_elegida = .variable) |> 
        ungroup() |> 
        mutate(grupo = "muestra+sexo") |> 
        try()
    if (.variable == "sexo") resultado_b <- NULL
    
    # unir los resultados en una lista
    resultado <- list("muestra" = resultado_a,
                      "muestra+sexo" = resultado_b)
})

# output del map
resultado

resultado |> length()
resultado[[1]]
str(resultado)

# transformar a dataframe
resultados <- resultado |> 
    # aplanar la lista para que pierda sus sublistas
    list_flatten() |> 
    # unir listas una arriba de la otra
    list_rbind()

# guardar ----
readr::write_rds(resultados, "bbdd/base_elri_preprocesada.rds")


