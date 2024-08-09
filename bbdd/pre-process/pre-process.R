library(tidyverse)
library(haven)
library(sjmisc)


getwd()
setwd("/Users/matdknu/Dropbox/taller_shiny/shiny-elri")

elri <- read_dta("bbdd/BBDD_ELRI_LONG_4.0.dta")

# Solo quiero mantener al panel ----

folios_con_cuatro_anios <- elri %>%
  group_by(folio) %>%
  summarise(num_anios = n_distinct(ano), .groups = "drop") %>%
  filter(num_anios >= 4) %>%
  select(folio)

# Filter the original data to keep only 'folios' with at least 4 distinct years
elri <- elri %>%
  semi_join(folios_con_cuatro_anios, by = "folio") |> 
  filter(!folio %in% c(1310923808, 1510138508, 1510140305))

# Variables que me interesan ---

#Recode variable "g2" / rename "sexo".
elri $sexo <- factor(elri $g2,labels = c('Hombre', 'Mujer'))
elri $sexo <- sjlabelled::set_label(elri $sexo, label = c("Tipo de sexo"))#etiquetamos variable


elri_seleccionada <- elri  |> 
  select(
    ano, urbano_rural, a1, sexo, 
    # Identidad
    a4, a5, a6, a7, a8,
    # Relaciones intergrupales
    c1, c2, c4, c5, c6_1, c6_2, c7_1, c7_2, c7_3, c12, c28_1, c28_2, c28_3, c28_4, c28_5, c28_6,
    contains("c3_"), 
    # Conflicto
    contains("d1_"), d1_1, d1_2, d1_3, d3_1, d3_2, d3_3, d4_2, 
    contains("d4_"), contains("d7_"), contains("d8_"),
    # Apoyo a políticas públicas
    contains("e1_"), contains("e3_"),
    # Salud mental
    contains("g4"), g17, contains("g5_"), 
    pond) |> 
  mutate(indigena_es = case_when(
    a1 >= 10 ~ "No indígena",
    a1 <= 12   ~ "Indígena"
  ))



# Creación de funciones
# Función para recodificar valores
recodificar <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 1,
    x == 3 ~ 2,
    x == 4 ~ 2,
    x == 5 ~ 3,
    TRUE ~ NA_real_  # Manejo de valores que no sean 1 a 5
  )
}


# Ver el vector resultante
variables <- names(elri_seleccionada)[!names(elri_seleccionada) %in% c("ano","pond", "urbano_rural", "a1","indigena_es" ,"sexo")]



elri_recodificada <- elri_seleccionada |> 
  mutate(across(all_of(variables), recodificar, .names = "{.col}cod"))



elri_recodificada <-elri_recodificada |> select(ano, sexo, urbano_rural, indigena_es, ends_with("cod"), pond)

getwd()

write_rds(elri_recodificada, "bbdd/elri_recodificada.rds")

recodificar <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 2,
    x == 4 ~ 2,
    x == 5 ~ 3,
    TRUE ~ NA_real_  # Manejo de valores que no sean 1 a 5
  )
}
