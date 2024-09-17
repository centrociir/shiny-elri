library(tidyverse)
library(haven)
library(sjmisc)


# getwd()
# setwd("/Users/matdknu/Dropbox/taller_shiny/shiny-elri")

elri <- read_dta("bbdd/BBDD_ELRI_LONG_4.0.dta")

elri |> 
  select(a5) |> 
  count(a5) |> 
  mutate(a5b = recodificar(a5),
         a5c = as_factor(a5))

# elri |> 
#   pivot_longer(cols = c(everything())

elri |> 
  pivot_longer(cols = where(is.labelled))

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
    c1, c2, c4, c5, c6_1, c6_2, c7_1, c7_2, c7_3, c12, c8, c15, c16, c28_1, c28_2, c28_3, c28_4, c28_5, c28_6,
    c3_1, c3_2,
    # Conflicto
    contains("d1_"), d1_1, d1_2, d1_3, d3_1, 
    d3_2,
    d4_2, 
    d4_3,
    contains("d7_"), contains("d8_"),
    # Apoyo a políticas públicas
    contains("e1_"), contains("e3_"),
    # Salud mental
    contains("g4"), g17, contains("g5_"), 
    pond) |> 
  mutate(indigena_es = case_when(
    a1 >= 10 ~ "No indígena",
    a1 <= 12   ~ "Indígena"
  )) |> select(-e3_1, e3_1_o1)



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
  mutate(across(setdiff(variables, "a8"), recodificar, .names = "{.col}cod"))


elri_recodificada <-elri_recodificada |> select(ano, sexo, urbano_rural, indigena_es, ends_with("cod"), a8, pond)

elri_recodificada <- elri_recodificada %>%
  mutate(across(c(a4cod, a5cod, a6cod, a7cod), 
                ~ case_when(
                  . == 1 ~ "1. Poco",
                  . == 2 ~ "2. Algo",
                  . == 3 ~ "3. Bastante",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(c(c1cod, c4cod), 
                ~ case_when(
                  . == 1 ~ "1. Mal",
                  . == 2 ~ "2. Ni bien ni mal",
                  . == 3 ~ "3. Bien",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(c(c2cod, c5cod, c7_3cod, c3_1cod, c3_2cod), 
                ~ case_when(
                  . == 1 ~ "1. Nada",
                  . == 2 ~ "2. Ni mucho ni poco",
                  . == 3 ~ "3. Mucho",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(c(c7_1cod, c12cod, c16cod), 
                ~ case_when(
                  . == 1 ~ "1. Ninguno",
                  . == 2 ~ "2. Algunos",
                  . == 3 ~ "3. Mayoría",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(c(c15cod, c8cod, c7_2cod), 
                ~ case_when(
                  . == 1 ~ "1. Nada",
                  . == 2 ~ "2. Ni mucho ni poco",
                  . == 3 ~ "3. Mucho",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(matches("^c28|^d7|^d8|^e1"), 
                ~ case_when(
                  . == 1 ~ "1. En desacuerdo",
                  . == 2 ~ "2. Ni de acuerdo ni en desacuerdo",
                  . == 3 ~ "3. De acuerdo",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(starts_with("d1"), 
                ~ case_when(
                  . == 1 ~ "1. Nada de conflicto",
                  . == 2 ~ "2. Algo de conflicto",
                  . == 3 ~ "3. Mucho conflicto",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(c(d3_1cod, d3_2cod, d4_2cod, d4_3cod), 
                ~ case_when(
                  . == 1 ~ "1. Nunca se justifica",
                  . == 2 ~ "2. A veces se justifica",
                  . == 3 ~ "3. Siempre se justifica ",
                  TRUE ~ as.character(.)
                ))) |> 
  mutate(across(c(a8), 
                ~ case_when(
                  . == 1 ~ "Chileno ",
                  . == 2 ~ "Pueblo Originario",
                  . == 3 ~ "Chileno y Pueblo Originario",
                  . == 4 ~ "Originario primero y chileno después",
                  . == 5 ~ "Chileno primero y originario después",
                  TRUE ~ NA
                ))) 
  





getwd()

write_rds(elri_recodificada, "bbdd/elri_recodificada.rds")


