library(purrr)
library(ggplot2)
library(dplyr)
library(srvyr)
library(survey)

elri_recodificada <- readRDS("bbdd/elri_recodificada.rds")

# vector de nombres de columnas que queremos incluir en el precalculo
# Crear un vector con las variables

variables <- names(elri_recodificada)[!names(elri_recodificada) %in% c("ano", "urbano_rural", "pond" ,"a1","indigena_es" ,"sexo")]



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


#Nombres módulos

resultados$modulo <- resultados$variable_elegida

resultados$modulo <- ifelse(startsWith(resultados$variable_elegida, "a"), "Módulo Identidad", resultados$modulo)
resultados$modulo <- ifelse(startsWith(resultados$variable_elegida, "d"), "Módulo de Conflicto", resultados$modulo)
resultados$modulo <- ifelse(startsWith(resultados$variable_elegida, "c"), "Módulo de Relaciones Interculturales", resultados$modulo)
resultados$modulo <- ifelse(startsWith(resultados$variable_elegida, "g"), "Salud mental", resultados$modulo)
resultados$modulo <- ifelse(startsWith(resultados$variable_elegida, "e"), "Módulo Políticas Públicas", resultados$modulo)

resultados <- resultados |> filter(modulo != "Salud mental")


# Limpiar sexo
resultados$sexo[is.na(resultados$sexo)] <- " "


# Poner enunciado

resultados <- resultados |> 
  mutate(enunciado = case_when(
    startsWith(variable_elegida, "a4") ~ "¿Cuánto se identifica con los (% PUEBLO ORIGINARIO)?",
    startsWith(variable_elegida, "a5") ~ "¿Cuán importante es para usted ser parte de los (% PUEBLO ORIGINARIO)?",
    startsWith(variable_elegida, "a6") ~ "¿Cuánto se identifica con Chile?",
    startsWith(variable_elegida, "a7") ~ "¿Cuán importante es para usted ser chileno/a?",
    startsWith(variable_elegida, "c3_1") ~ "¿En qué medida considera que los chilenos no indígenas son amistosos? ",
    startsWith(variable_elegida, "c1") ~ "¿cómo le caen los chilenos no indígenas?",
    startsWith(variable_elegida, "c2") ~ "¿cuánto confía en los chilenos no indígenas?",
    startsWith(variable_elegida, "c4") ~ "¿cómo le caen los (% PUEBLO ORIGINARIO)?",
    startsWith(variable_elegida, "c5") ~ "¿cuánto confía en los (% PUEBLO ORIGINARIO)?",
    startsWith(variable_elegida, "c6_1") ~ "¿En qué medida considera que los (% PUEBLO ORIGINARIO) son amistosos?",
    startsWith(variable_elegida, "c6_2") ~ "¿En qué medida considera que los (% PUEBLO ORIGINARIO) son competentes?",
    startsWith(variable_elegida, "c7_1") ~ "Pensado con la gente que se relaciona ¿Cuántos de ellos son chilenos no-indígenas?",
    startsWith(variable_elegida, "c7_2") ~ "Pensado con la gente que se relaciona  ¿Con qué frecuencia conversa o interactúa con personas chilenas no-indígenas?",
    startsWith(variable_elegida, "c7_3") ~ "Cuando interactúa con personas chilenas no-indígenas, ¿Cuán amistosa ha sido esa experiencia de contacto?",
    startsWith(variable_elegida, "c12") ~ "Pensado con la gente que se relaciona ¿Cuántos de ellos diría usted que son (% PUEBLO ORIGINARIO)?",
    startsWith(variable_elegida, "c28_1") ~ "Los (% PUEBLO ORIGINARIO) deberían mantener sus costumbres y tradiciones",
    startsWith(variable_elegida, "c28_2") ~ "Los (% PUEBLO ORIGINARIO) deberían mantener su propia cultura.",
    startsWith(variable_elegida, "c28_3") ~ "Los (% PUEBLO ORIGINARIO) deberían adoptar costumbres y tradiciones chilenas",
    startsWith(variable_elegida, "c28_4") ~ "Los (% PUEBLO ORIGINARIO) deberían adquirir la cultura de los chilenos.",
    startsWith(variable_elegida, "c28_5") ~ "Los (% PUEBLO ORIGINARIO) deberían tener amistades con los chilenos no indígenas.",
    startsWith(variable_elegida, "c28_6") ~ "Que los (% PUEBLO ORIGINARIO) pasen su tiempo libre con chilenos no indígenas.",
    startsWith(variable_elegida, "d1_1") ~ "¿Cuánto conflicto diría usted que existe actualmente entre El Estado chileno y los pueblos originarios?",
    startsWith(variable_elegida, "d1_2") ~ "¿Cuánto conflicto diría usted que existe actualmente entre Indígenas y no-indígenas?",
    startsWith(variable_elegida, "d1_3") ~ "¿Cuánto conflicto diría usted que existe actualmente entre Empresas y los pueblos originarios?",
    startsWith(variable_elegida, "d3_1") ~ "¿En qué medida cree usted que se justificanel uso de la fuerza por parte de Carabineros para disolver protestas  de personas indígenas?",
    startsWith(variable_elegida, "d3_2") ~ "¿En qué medida cree usted que se justifican que agricultores usen armas para enfrentar a grupos de personas indígenas?",
    startsWith(variable_elegida, "d3_3") ~ "¿En qué medida cree usted que se justifican el uso de la fuerza por parte de Carabineros para allanar comunidades indígenas?",
    startsWith(variable_elegida, "d4_1") ~ "¿en qué medida cree usted que se justifican ataques incendiarios por parte de grupos de personas indígenas?",
    startsWith(variable_elegida, "d4_2") ~ "¿En qué medida cree usted que se justifican o no se justifica que grupos de personas indígenas se tomen terrenos que se consideran propios?",
    startsWith(variable_elegida, "d4_3") ~ "¿en qué medida cree usted que se justifican el bloqueo o corte de carreteras por parte de grupos de personas indígenas?",
    startsWith(variable_elegida, "d7_1") ~ "En vez de tanta preocupación por los derechos de las personas, lo que este país necesita es orden y mano dura",
    startsWith(variable_elegida, "d7_2") ~ "La obediencia y el respeto por la autoridad son los valores más importantes que los niños deberían aprender",
    startsWith(variable_elegida, "d7_3") ~ "Las parejas homosexuales estables debieran ser tratadas tal cual como se trata a las parejas casadas",
    
    TRUE ~ NA_character_ # Para casos donde no hay coincidencia, asignar NA
  )) |> drop_na(enunciado)


resultados |> count(variable_elegida) |> print(n= Inf)

# guardar ----
getwd()
readr::write_rds(resultados, "bbdd/base_elri_preprocesada.rds")
readr::write_rds(resultados, "app/base_elri_preprocesada.rds")



