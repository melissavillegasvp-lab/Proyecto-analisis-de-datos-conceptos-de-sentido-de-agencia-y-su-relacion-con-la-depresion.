library(readxl)
library(dplyr)
library(ggplot2)
library(emmeans)
library(effectsize)
library(tidyverse)
library(ggsignif)
library(performance)
library(fitdistrplus)
library(MASS)
library(flexplot)
# install.packages("Rtsne")
library(Rtsne)
#install.packages("ordinal")
library(ordinal)

setwd("D:/Documentos/Agencia/Código")
#Leer base de datos
mis_datos <- read_xlsx("datos_sm_GRUPOS.xlsx")

##########################################################
### Estadística descriptiva participantes ################

# Tabla 1: estadística participantes
datos_unicos <- mis_datos %>%
  distinct(ID, .keep_all = TRUE) %>% dplyr::select(ID, GRUPO, GENERO, EDAD, PHQ9)

# Calcular estadísticas por grupo
Tabla_1_porgrupo <- datos_unicos %>%
  group_by(GRUPO) %>%
  summarise(
    n_personas = n_distinct(ID),
    Femenino = sum(GENERO == "Femenino"),
    Masculino = sum(GENERO == "Masculino"),
    Otro = sum(GENERO == "Otro"),
    Edad_media = mean(EDAD),
    Edad_de = sd(EDAD),
    Rango_edades = paste(min(EDAD), max(EDAD), sep = "-")
  )

# Calcular totales
Tabla_1_total <- datos_unicos %>%
  summarise(
    GRUPO = "Total",
    n_personas = n_distinct(ID),
    Femenino = sum(GENERO == "Femenino"),
    Masculino = sum(GENERO == "Masculino"),
    Otro = sum(GENERO == "Otro"),
    Edad_media = mean(EDAD),
    Edad_de = sd(EDAD),
    Rango_edades = paste(min(EDAD), max(EDAD), sep = "-")
  )

# Combinar dataframes
Tabla_1 <- bind_rows(Tabla_1_porgrupo, Tabla_1_total)

# Media y desviación estandar de PHQ9
sdphq9<-datos_unicos %>%
  summarise(
    n_personas = n_distinct(ID),
    Media = mean(PHQ9),
    DE = sd(PHQ9))

# _____________________Visualización de los puntajes_________________________ #

# Gráficas de PHQ-9 y puntajes en modalidades sensorimotoras
# 1. Formato largo
datos_largos <- mis_datos %>%
  pivot_longer(
    cols = -c(GRUPO, CONDICION, ID, EDAD, GENERO, PALABRA, PHQ9, VALENCIA),
    names_to = "Modalidad",
    values_to = "Puntaje"
  )

# 2. Puntajes PHQ-9

ggplot(datos_unicos, aes(x = factor(PHQ9))) +
  geom_bar(fill = "#B2DFEE", color= "#4A708B", width = 0.7)+
  labs(title = "Frecuencia de puntajes PHQ-9",
       x = "Puntaje en PHQ-9",
       y = "Frecuencia")+
  theme_classic()+
  theme(plot.title = element_text(
    hjust = 0.5,
    face = "bold",
    size = 16))

# 3. Puntajes sensorimotores, boxplot por grupo

ggplot(datos_largos, aes(x = Modalidad, y = Puntaje)) +
  geom_boxplot(fill = "#e3f0cd", color = "gray30") +
  facet_wrap(~GRUPO) +
  labs(title = "Puntajes sensorimotores por modalidad",
       x = "Modalidad",
       y = "Puntaje (0 a 5)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(min(datos_largos$Puntaje), max(datos_largos$Puntaje)))

# 4. Puntajes sensorimotores, boxplot por grupo y condición

ggplot(datos_largos, aes(x = Modalidad, y = Puntaje, fill = GRUPO)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.colour = "red") +
  facet_wrap(~ CONDICION) +
  labs(title = "Puntajes sensorimotores por modalidad y condición",
       x = "Modalidad",
       y = "Puntaje (0 a 5)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Pastel1")

######################################################################################################################################################
# Fuerza perceptual + Minkowski3 + Fuerza máxima y mínima + exclusividad + dominancia
resumen_sensorial <- mis_datos %>%
  group_by(PALABRA, ID, CONDICION, GRUPO, EDAD, GENERO) %>% 
  summarise(
    # Suma de las columnas perceptuales
    Sum_strength.perceptual = sum(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL, na.rm = TRUE),
    
    # Cálculo de Minkowski3 (raíz cúbica de la suma de cubos)
    Minkowski3.perceptual = (sum(AUDITIVO^3, GUSTATIVO^3, HAPTICO^3, INTEROCEPTIVO^3, OLFATIVO^3, VISUAL^3, na.rm = TRUE))^(1/3),
    
    # Guardar el valor máximo por columna
    AUDITIVO = mean(AUDITIVO, na.rm = TRUE),
    GUSTATIVO = mean(GUSTATIVO, na.rm = TRUE),
    HAPTICO = mean(HAPTICO, na.rm = TRUE),
    INTEROCEPTIVO = mean(INTEROCEPTIVO, na.rm = TRUE),
    OLFATIVO = mean(OLFATIVO, na.rm = TRUE),
    VISUAL = mean(VISUAL, na.rm = TRUE),
    VALENCIA = mean(VALENCIA, na.rm = TRUE),
    .groups = "drop"  # Para evitar advertencias con las nuevas versiones de dplyr
  ) %>%
  ungroup() %>%
  mutate(
    # Calcular la fuerza máxima
    Max_strength.perceptual = pmax(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL, na.rm = TRUE),
    
    # Calcular la fuerza mínima
    Min_strength.perceptual = pmin(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL, na.rm = TRUE),
    
    # Calcular la exclusividad
    Exclusivity.perceptual = (Max_strength.perceptual - Min_strength.perceptual) / Sum_strength.perceptual,
    
    # Obtener el perceptual dominante
    Dominant.perceptual = colnames(.[,c("AUDITIVO", "GUSTATIVO", "HAPTICO", "INTEROCEPTIVO", "OLFATIVO", "VISUAL")])
    [max.col(.[,c("AUDITIVO", "GUSTATIVO", "HAPTICO", "INTEROCEPTIVO", "OLFATIVO", "VISUAL")], ties.method = "random")]
  )

# Fuerza de acción + Minkowski3 + Fuerza máxima y mínima + exclusividad + dominancia
resumen_motor <- mis_datos %>%
  group_by(PALABRA, ID, CONDICION, GRUPO, EDAD, GENERO) %>% 
  summarise(
    # Suma de las medias
    Sum_strength.action = sum(PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO, na.rm = TRUE),
    
    # Cálculo de Minkowski3
    Minkowski3.action = (sum(PIE_PIERNA^3, MANO_BRAZO^3, CABEZA^3, BOCA_GARGANTA^3, TORSO^3, na.rm = TRUE))^(1/3),
    
    # Mantener los valores medios originales
    PIE_PIERNA = mean(PIE_PIERNA, na.rm = TRUE),
    MANO_BRAZO = mean(MANO_BRAZO, na.rm = TRUE),
    CABEZA = mean(CABEZA, na.rm = TRUE),
    BOCA_GARGANTA = mean(BOCA_GARGANTA, na.rm = TRUE),
    TORSO = mean(TORSO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    # Fuerza máxima de acción
    Max_strength.action = pmax(PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO, na.rm = TRUE),
    
    # Fuerza mínima de acción
    Min_strength.action = pmin(PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO, na.rm = TRUE),
    
    # Exclusividad de acción
    Exclusivity.action = (Max_strength.action - Min_strength.action) / Sum_strength.action,
    
    # Acción dominante
    Dominant.action = colnames(.[,c("PIE_PIERNA", "MANO_BRAZO", "CABEZA", "BOCA_GARGANTA", "TORSO")])
    [max.col(.[,c("PIE_PIERNA", "MANO_BRAZO", "CABEZA", "BOCA_GARGANTA", "TORSO")], ties.method = "random")]
  )

# Unir los dataframes manteniendo las columnas de CATEGORIA y GRUPO
sensorimotor_porparticipante <- cbind(
  resumen_sensorial, 
  resumen_motor[, !(names(resumen_motor) %in% c("PALABRA", "ID", "CONDICION", "GRUPO", "EDAD", "GENERO"))]
)

# Añadir cálculos adicionales para datos sensorimotores
sensorimotor_porparticipante <- sensorimotor_porparticipante %>%
  group_by(PALABRA, ID, CONDICION, GRUPO, EDAD, GENERO) %>%  # Incluir CATEGORIA y GRUPO en la agrupación
  mutate(
    # Minkowski3 sensorimotor
    Minkowski3.sensorimotor = sum(AUDITIVO^3, GUSTATIVO^3, HAPTICO^3, INTEROCEPTIVO^3, OLFATIVO^3, VISUAL^3, 
                                  PIE_PIERNA^3, MANO_BRAZO^3, CABEZA^3, BOCA_GARGANTA^3, TORSO^3, na.rm = TRUE)^(1/3),
    # Max strength
    Max_strength.sensorimotor = pmax(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL, 
                                     PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO, na.rm = TRUE),
    # Min strength
    Min_strength.sensorimotor = pmin(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL, 
                                     PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO, na.rm = TRUE),
    # Summed strength
    Sum_strength.sensorimotor = sum(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL, 
                                    PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO, na.rm = TRUE),
    # Exclusivity
    Exclusivity.sensorimotor = (Max_strength.sensorimotor - Min_strength.sensorimotor)/Sum_strength.sensorimotor
  ) %>%
  ungroup() %>%
  # Encontrar la dimensión dominante
  mutate(
    Dominant.sensorimotor = colnames(.[,c(9:14, 22:26)])[max.col(.[,c(9:14, 22:26)], ties.method = "random")],
    Dominant.sensorimotor = gsub(".mean", "", Dominant.sensorimotor, fixed = TRUE),
    PALABRA = gsub(".", " ", PALABRA, fixed = TRUE)
  )

# Reorganizar las columnas para mejorar la legibilidad
sensorimotor_porparticipante <- sensorimotor_porparticipante %>%
  dplyr::select(
    ID, PALABRA, CONDICION, GRUPO, EDAD, GENERO, VALENCIA,  # Añadido CATEGORIA y GRUPO aquí
    AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL,
    PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO,
    Max_strength.perceptual, Min_strength.perceptual, Minkowski3.perceptual, Exclusivity.perceptual, Dominant.perceptual,
    Max_strength.action, Min_strength.action, Minkowski3.action, Exclusivity.action, Dominant.action,
    Max_strength.sensorimotor, Minkowski3.sensorimotor, Exclusivity.sensorimotor, Dominant.sensorimotor
  )

# Crear un dataframe con medias y SD agrupadas por PALABRA y GRUPO
resumen_sensorial_medias <- sensorimotor_porparticipante %>%
  group_by(PALABRA, CONDICION, GRUPO) %>%  # Añadido CATEGORIA y GRUPO aquí
  summarise(
    # Medias
    Valencia.mean = mean(VALENCIA, na.rm = TRUE),
    Auditivo.mean = mean(AUDITIVO, na.rm = TRUE),
    Gustativo.mean = mean(GUSTATIVO, na.rm = TRUE),
    Haptico.mean = mean(HAPTICO, na.rm = TRUE),
    Interoceptivo.mean = mean(INTEROCEPTIVO, na.rm = TRUE),
    Olfatorio.mean = mean(OLFATIVO, na.rm = TRUE),
    Visual.mean = mean(VISUAL, na.rm = TRUE),
    # Desviaciones estándar
    Auditivo.SD = sd(AUDITIVO, na.rm = TRUE),
    Gustativo.SD = sd(GUSTATIVO, na.rm = TRUE),
    Haptico.SD = sd(HAPTICO, na.rm = TRUE),
    Interoceptivo.SD = sd(INTEROCEPTIVO, na.rm = TRUE),
    Olfatorio.SD = sd(OLFATIVO, na.rm = TRUE),
    Visual.SD = sd(VISUAL, na.rm = TRUE),
    # Summed strength
    Sum_strength.perceptual = sum(Auditivo.mean, Gustativo.mean, Haptico.mean, Interoceptivo.mean, Olfatorio.mean, Visual.mean),
    # Minkowski3
    Minkowski3.perceptual = (sum(Auditivo.mean^3, Gustativo.mean^3, Haptico.mean^3, Interoceptivo.mean^3, Olfatorio.mean^3, Visual.mean^3))^(1/3),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    # Max strength
    Max_strength.perceptual = pmax(Auditivo.mean, Gustativo.mean, Haptico.mean, Interoceptivo.mean, Olfatorio.mean, Visual.mean),
    # Min strength
    Min_strength.perceptual = pmin(Auditivo.mean, Gustativo.mean, Haptico.mean, Interoceptivo.mean, Olfatorio.mean, Visual.mean),
    # Exclusivity
    Exclusivity.perceptual = (Max_strength.perceptual - Min_strength.perceptual)/Sum_strength.perceptual,
    # Dominant perceptual
    Dominant.perceptual = colnames(.[,5:10])[max.col(.[,5:10], ties.method="random")],
    Dominant.perceptual = gsub(".mean", "", Dominant.perceptual, fixed = TRUE)
  )


# Resumen motor con medias y SD por PALABRA y GRUPO
resumen_motor_medias <- sensorimotor_porparticipante %>%
  group_by(PALABRA, CONDICION, GRUPO) %>% 
  summarise(
    # Medias
    Pie_pierna.mean = mean(PIE_PIERNA, na.rm = TRUE),
    Mano_brazo.mean = mean(MANO_BRAZO, na.rm = TRUE),
    Cabeza.mean = mean(CABEZA, na.rm = TRUE),
    Boca_garganta.mean = mean(BOCA_GARGANTA, na.rm = TRUE),
    Torso.mean = mean(TORSO, na.rm = TRUE),
    # Desviaciones estándar
    Pie_pierna.SD = sd(PIE_PIERNA, na.rm = TRUE),
    Mano_brazo.SD = sd(MANO_BRAZO, na.rm = TRUE),
    Cabeza.SD = sd(CABEZA, na.rm = TRUE),
    Boca_garganta.SD = sd(BOCA_GARGANTA, na.rm = TRUE),
    Torso.SD = sd(TORSO, na.rm = TRUE),
    Sum_strength.action = sum(Pie_pierna.mean, Mano_brazo.mean, Cabeza.mean, Boca_garganta.mean, Torso.mean),
    # Minkowski3
    Minkowski3.action = (sum(Pie_pierna.mean^3, Mano_brazo.mean^3, Cabeza.mean^3, Boca_garganta.mean^3, Torso.mean^3))^(1/3),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    # Max strength
    Max_strength.action = pmax(Pie_pierna.mean, Mano_brazo.mean, Cabeza.mean, Boca_garganta.mean, Torso.mean),
    # Min strength
    Min_strength.action = pmin(Pie_pierna.mean, Mano_brazo.mean, Cabeza.mean, Boca_garganta.mean, Torso.mean),
    # Exclusivity
    Exclusivity.action = (Max_strength.action - Min_strength.action)/Sum_strength.action,
    # Dominant action
    Dominant.action = colnames(.[,4:8])[max.col(.[,4:8], ties.method="random")],
    Dominant.action = gsub(".mean", "", Dominant.action, fixed = TRUE)
  )

# Unimos datos perceptual y datos action manteniendo CATEGORIA y GRUPO
normas <- cbind(
  resumen_sensorial_medias, 
  resumen_motor_medias[, !(names(resumen_motor_medias) %in% c("PALABRA", "CONDICION", "GRUPO"))]
)

# Agregamos el componente sensorimotor_____________________________________________________________
normas <- normas %>%
  group_by(PALABRA, CONDICION, GRUPO) %>%
  mutate(
    # Minkowski3
    Minkowski3.sensorimotor = sum(Auditivo.mean^3, Gustativo.mean^3, Haptico.mean^3, Interoceptivo.mean^3, Olfatorio.mean^3, Visual.mean^3, 
                                  Pie_pierna.mean^3, Mano_brazo.mean^3, Cabeza.mean^3, Boca_garganta.mean^3, Torso.mean^3)^(1/3),
    # Max strength
    Max_strength.sensorimotor = pmax(Auditivo.mean, Gustativo.mean, Haptico.mean, Interoceptivo.mean, Olfatorio.mean, Visual.mean, 
                                     Pie_pierna.mean, Mano_brazo.mean, Cabeza.mean, Boca_garganta.mean, Torso.mean),
    # Min strength
    Min_strength.sensorimotor = pmin(Auditivo.mean, Gustativo.mean, Haptico.mean, Interoceptivo.mean, Olfatorio.mean, Visual.mean, 
                                     Pie_pierna.mean, Mano_brazo.mean, Cabeza.mean, Boca_garganta.mean, Torso.mean),
    # Summed strength
    Sum_strength.sensorimotor = sum(Auditivo.mean, Gustativo.mean, Haptico.mean, Interoceptivo.mean, Olfatorio.mean, Visual.mean, 
                                    Pie_pierna.mean, Mano_brazo.mean, Cabeza.mean, Boca_garganta.mean, Torso.mean),
    # Exclusivity
    Exclusivity.sensorimotor = (Max_strength.sensorimotor - Min_strength.sensorimotor)/Sum_strength.sensorimotor
  ) %>%
  ungroup() %>%
  # Dominant sensorimotor
  mutate(
    Dominant.sensorimotor = colnames(.[,c(5:10, 23:27)])[max.col(.[,c(5:10, 23:27)], ties.method="random")],
    Dominant.sensorimotor = gsub(".mean", "", Dominant.sensorimotor, fixed = TRUE),
    PALABRA = gsub(".", " ", PALABRA, fixed = TRUE)
  )

# Reorganizar las columnas para mejorar la legibilidad
normas <- normas %>%
  dplyr::select(
    PALABRA, CONDICION, GRUPO,
    Auditivo.mean, Gustativo.mean, Haptico.mean, Interoceptivo.mean, Olfatorio.mean, Visual.mean,
    Pie_pierna.mean, Mano_brazo.mean, Cabeza.mean, Boca_garganta.mean, Torso.mean,
    Auditivo.SD, Gustativo.SD, Haptico.SD, Interoceptivo.SD, Olfatorio.SD, Visual.SD,
    Pie_pierna.SD, Mano_brazo.SD, Cabeza.SD, Boca_garganta.SD, Torso.SD,
    Max_strength.perceptual, Minkowski3.perceptual, Exclusivity.perceptual, Dominant.perceptual,
    Max_strength.action, Minkowski3.action, Exclusivity.action, Dominant.action,
    Max_strength.sensorimotor, Minkowski3.sensorimotor, Exclusivity.sensorimotor, Dominant.sensorimotor
  )

#____________________________Distancias euclidianas entre grupos (crear dataframe)___________________________ #

# Primero calculo la distancia en modalidades sensoriales

# Obtengo las medias
medias_perceptual <- mis_datos %>%
  group_by(PALABRA, GRUPO, CONDICION) %>%
  summarise(
    across(c(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL),
           ~ mean(., na.rm = TRUE),
           .names = "{.col}"),
    .groups = "drop"
  )

# Separo medias por grupo
grupos_wide_p <- medias_perceptual %>%
  pivot_wider(
    names_from = GRUPO,
    values_from = c(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL),
    names_sep = "_"
  )

# Distancia euclidiana entre grupos
euclidean_p <- grupos_wide_p %>%
  mutate(
    Euclidean.perceptual = sqrt(
      (AUDITIVO_Control - AUDITIVO_Depresion)^2 +
        (GUSTATIVO_Control - GUSTATIVO_Depresion)^2 +
        (HAPTICO_Control - HAPTICO_Depresion)^2 +
        (INTEROCEPTIVO_Control - INTEROCEPTIVO_Depresion)^2 +
        (OLFATIVO_Control - OLFATIVO_Depresion)^2 +
        (VISUAL_Control - VISUAL_Depresion)^2
    )
  )

# Distancia en modalidades motoras o de acción

# Obtengo las medias
medias_motor <- mis_datos %>%
  group_by(PALABRA, GRUPO, CONDICION) %>%
  summarise(
    across(c(PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO),
           ~ mean(., na.rm = TRUE),
           .names = "{.col}"),
    .groups = "drop"
  )

# Separo medias por grupo
grupos_wide_m <- medias_motor %>%
  pivot_wider(
    names_from = GRUPO,
    values_from = c(PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO),
    names_sep = "_"
  )

# Distancia euclidiana entre grupos
euclidean_m <- grupos_wide_m %>%
  mutate(
    Euclidean.motor = sqrt(
      (PIE_PIERNA_Control - PIE_PIERNA_Depresion)^2 +
        (MANO_BRAZO_Control - MANO_BRAZO_Depresion)^2 +
        (CABEZA_Control - CABEZA_Depresion)^2 +
        (BOCA_GARGANTA_Control - BOCA_GARGANTA_Depresion)^2 +
        (TORSO_Control - TORSO_Depresion)^2
    )
  )
# Distancia euclidiana entre grupos (sensorimotor)

# Obtengo las medias
medias_sm <- mis_datos %>%
  group_by(PALABRA, GRUPO, CONDICION) %>%
  summarise(
    across(c(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL,
             PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO),
           ~ mean(., na.rm = TRUE),
           .names = "{.col}"),
    .groups = "drop"
  )

# Separo medias por grupo
grupos_wide_sm <- medias_sm %>%
  pivot_wider(
    names_from = GRUPO,
    values_from = c(AUDITIVO, GUSTATIVO, HAPTICO, INTEROCEPTIVO, OLFATIVO, VISUAL,
                    PIE_PIERNA, MANO_BRAZO, CABEZA, BOCA_GARGANTA, TORSO),
    names_sep = "_"
  )

# Distancia euclidiana entre grupos
euclidean_sm <- grupos_wide_sm %>%
  mutate(
    Euclidean.sensorimotor = sqrt(
      (AUDITIVO_Control - AUDITIVO_Depresion)^2 +
        (GUSTATIVO_Control - GUSTATIVO_Depresion)^2 +
        (HAPTICO_Control - HAPTICO_Depresion)^2 +
        (INTEROCEPTIVO_Control - INTEROCEPTIVO_Depresion)^2 +
        (OLFATIVO_Control - OLFATIVO_Depresion)^2 +
        (VISUAL_Control - VISUAL_Depresion)^2 +
        (PIE_PIERNA_Control - PIE_PIERNA_Depresion)^2 +
        (MANO_BRAZO_Control - MANO_BRAZO_Depresion)^2 +
        (CABEZA_Control - CABEZA_Depresion)^2 +
        (BOCA_GARGANTA_Control - BOCA_GARGANTA_Depresion)^2 +
        (TORSO_Control - TORSO_Depresion)^2)
  )

# Hacer un df para la distancia euclidiana
distancia_combinada <- euclidean_sm %>%
  dplyr::select(PALABRA, CONDICION, Euclidean.sensorimotor) %>%
  left_join(
    euclidean_m %>% dplyr::select(PALABRA, Euclidean.motor),
    by = "PALABRA"
  ) %>%
  left_join(
    euclidean_p %>% dplyr::select(PALABRA, Euclidean.perceptual),
    by = "PALABRA"
  )

# Reorganizar columnas
distancia_combinada <- distancia_combinada %>%
  dplyr::select(
    PALABRA, CONDICION, Euclidean.perceptual, Euclidean.motor, Euclidean.sensorimotor)

# Dataframe con todas las distancias Euclidianas calculadas
view(distancia_combinada)

#-----------------------------------------------------------------------
# MODELOS PARA MEDIDAS DE FUERZA PERCEPTUAL Y MOTORA
#_______________________________________________________________________

# 1. Modelo con puntuaciones por participante (se conservan los datos de manera ordinal)
# Revisar distribución
hist(sensorimotor_porparticipante$Max_strength.action)
str(sensorimotor_porparticipante$Max_strength.action)

# Convertir a factor la variable predictora
sensorimotor_porparticipante$Max_strength.action <- as.factor(sensorimotor_porparticipante$Max_strength.action)

# Modelo ordinal para puntuaciones por participante
fuerza_motora <- clmm(Max_strength.action ~ GRUPO * CONDICION + (1|ID),
                      data = sensorimotor_porparticipante)
summary(fuerza_motora)

check_model(fuerza_motora) # Hay demasiada colinealidad, lo que puede alterar los coeficientes

# Pruebas con otros modelos

fuerza_motor <- polr(Max_strength.action ~ GRUPO * CONDICION,
                      data = sensorimotor_porparticipante)
summary(fuerza_motor)

check_model(fuerza_motor)

AIC(fuerza_motor, fuerza_motora)

#Chi cuadrada
clmm_nu <- clmm(Max_strength.action ~ 1 + (1|ID),
                      data = sensorimotor_porparticipante)

polr_nu <- polr(Max_strength.action ~ 1,
                     data = sensorimotor_porparticipante)

anova(clmm_nu, fuerza_motora, test = "Chisq")

anova(polr_nu, fuerza_motor, test = "Chisq")

# Revisar colinealidad ajustada (GVIF)

check_collinearity(fuerza_motora)

performance(fuerza_motora) #R²

#________________Análisis post hoc_________________
# Medias estimadas en la escala del modelo (log-odds) para CONDICIÓN | GRUPO
emm_cond <- emmeans(fuerza_motora, ~ CONDICION | GRUPO)

# Comparaciones por pares (Positiva vs. Negativa) dentro de cada grupo
pairs(emm_cond, adjust = "tukey")

# Resumen completo
dif_cond <- pairs(emm_cond, adjust = "tukey")

summary(dif_cond, infer = c(TRUE, TRUE))

# Ahora para fuerza perceptual____________________________________________________________
# Usaré directamente un clmm
hist(normas$Max_strength.perceptual)

# Convertir a factor la variable predictora
sensorimotor_porparticipante$Max_strength.perceptual <- as.factor(sensorimotor_porparticipante$Max_strength.perceptual)

# Modelo ordinal para puntuaciones por participante
fuerza_perceptual <- clmm(Max_strength.perceptual ~ GRUPO * CONDICION + (1|ID),
                      data = sensorimotor_porparticipante)
summary(fuerza_perceptual)

check_model(fuerza_perceptual)

# Chisq

clmm_nupe <- clmm(Max_strength.perceptual ~ 1 + (1|ID),
                data = sensorimotor_porparticipante)
anova(clmm_nupe, fuerza_perceptual, test = "Chisq" )

# Revisar colinealidad ajustada (GVIF)

check_collinearity(fuerza_perceptual)

performance(fuerza_perceptual) #R²

# Comparaciones para la condición
# Medias estimadas en la escala del modelo (log-odds) para CONDICIÓN | GRUPO
emm_cond2 <- emmeans(fuerza_perceptual, ~ CONDICION | GRUPO)

# Comparaciones por pares (Positiva vs. Negativa) dentro de cada grupo
pairs(emm_cond2, adjust = "tukey")

# Resumen completo
dif_cond2 <- pairs(emm_cond2, adjust = "tukey")

summary(dif_cond2, infer = c(TRUE, TRUE))

#-----------------------------------------------------------------------
# MODELOS para analizar la distancia Minkowski
#_______________________________________________________________________
# 1. Distancia Minkowski en función del grupo y la condición

hist(sensorimotor_porparticipante$Minkowski3.sensorimotor) # observar distribución

descdist(normas$Minkowski3.sensorimotor,
         discrete = FALSE,               # La distribución parece normal
         boot = 200)

minko <- lm(Minkowski3.sensorimotor ~ GRUPO * CONDICION,
             data = normas)

summary(minko)
check_model(minko)

hist(residuals(minko))
shapiro.test(residuals(minko)) # Los residuos siguen una distribución normal

# Comparaciones para la condición
posthoc_minko <- emmeans(minko, ~ CONDICION | GRUPO)

# Obtener contrastes con IC 95%
contrastes_min <- contrast(posthoc_minko, 
                           method = "pairwise",
                           adjust = "none",  # o "tukey" si prefieres
                           infer = c(TRUE, TRUE))  # IC y tests

# Ver con IC 95%
summary(contrastes_min, infer = TRUE)

#_______________________
# Modelo para analizar la relación entre la valencia y la distancia Minkowski

# Convierto la variable "VALENCIA" en factor (para modelo ordinal) y la distancia
# Minkowski sensorimotora (combina ambas modalidades) en numérica
sensorimotor_porparticipante$VALENCIA <- as.factor(sensorimotor_porparticipante$VALENCIA)

# Modelo mixto ordinal con efectos aleatorios
modelo_mixto <- clmm(VALENCIA ~ Minkowski3.sensorimotor * CONDICION + GRUPO + 
                       (1 | ID) + (1 | PALABRA),
                     data = sensorimotor_porparticipante)

summary(modelo_mixto)

check_model(modelo_mixto)
# Eligo este porque me da más información y la 
# colinealidad de la interacción es moderada

performance(modelo_mixto) #R²

# Modelo nulo___________
modelo0 <- clmm(VALENCIA ~ 1 + (1|ID) + (1|PALABRA),
                data = sensorimotor_porparticipante)
# Chisq
anova(modelo_mixto, modelo0, test = "Chisq")

# Calcular pendiente de Minkowski3 en cada condición
simple_slopes <- emtrends(modelo_mixto, 
                          ~ CONDICION, 
                          var = "Minkowski3.sensorimotor",
                          mode = "latent")  # para CLMM

print(simple_slopes)

# Test si cada pendiente es diferente de 0
test_slopes <- test(simple_slopes, null = 0)
print(test_slopes)

#__________________________________________________________________________________________________________________________________________________________
# Gráficas
######################################################################################
# Agrupaciones para palabras_________________________________________________________
######################################################################################
## ADAPTADO DEL CÓDIGO DE LYNOTT et al. (2020) (figura 6)##
# En el código original hacen dos gráficas, una para cada tipo de modalidad (perceptual
# y motora). En mi caso, debido a que no tengo tantas palabras, decidí combinar ambos
# tipos de modalidades en una sola gráfica.

# Crear data frame con la palabra, promedios de cada dimensión
# y la dimensión dominante
set.seed(123)
control_norms_tsne <- normas %>%
  dplyr::select(GRUPO, PALABRA, Auditivo.mean:Visual.mean,
                Pie_pierna.mean:Torso.mean, Dominant.sensorimotor)

# Crear una nueva columna que combine grupo y modalidad
perceptual_norms_tsne <- control_norms_tsne %>%
  mutate(grupo_modalidad = paste(GRUPO, Dominant.sensorimotor, sep = "_"))

# Implementación t-SNE
tsne.sensorimotor <- Rtsne(perceptual_norms_tsne[,3:13],
                           check_duplicates = FALSE, perplexity = 2)

# Se combinan los valores derivados de t-SNE (llamados V1 y V2, que en
# "tsne.sensorimotor" son la variable Y) con el data frame anterior
perceptual_norms_tsne <- cbind(perceptual_norms_tsne, 
                               as.data.frame(tsne.sensorimotor$Y))

# La dimensión dominante se cambia a solo la primera letra para la gráfica de adenlante
perceptual_norms_tsne$Dominant.sensorimotor <- substr(perceptual_norms_tsne$Dominant.sensorimotor, 1, 1)

################################################################################################################################################

# Gráfica sensorimotora

tsne_perceptual_plot <- ggplot(perceptual_norms_tsne) +
  # Las siguientes lineas geom_point y geom_text requieren que cada punto de datos se represente de una manera específica,
  # representando la dimensión dominante del punto de datos mediante la letra correspondiente. En este caso coloqué únicamente
  # las letras de las modalidades dominantes que se presentan en la base de datos hecha anteriormente, esto debido a que 
  # tengo pocas palabras y no todas las modalidades sensorimotoras están presentes como en el artículo de Lynott (2020).
  geom_point(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "B"),
             aes(x = V1, y = V2, color = grupo_modalidad),size = 0, stroke = 0) +
  geom_point(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "C"),
             aes(x = V1, y = V2, color = grupo_modalidad),size = 0, stroke = 0) +
  geom_point(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "V"),
             aes(x = V1, y = V2, color = grupo_modalidad),size = 0, stroke = 0) +
  geom_point(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "I"),
             aes(x = V1, y = V2, color = grupo_modalidad),size = 0, stroke = 0) +
  geom_point(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "H"),
             aes(x = V1, y = V2, color = grupo_modalidad),size = 0, stroke = 0) +
  geom_point(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "A"),
             aes(x = V1, y = V2, color = grupo_modalidad),size = 0, stroke = 0) +
  
  geom_text(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "B"),
            aes(x = V1, y = V2, color = grupo_modalidad, label = PALABRA), size = 4, show.legend = FALSE) +
  geom_text(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "C"),
            aes(x = V1, y = V2, color = grupo_modalidad, label = PALABRA), size = 4, show.legend = FALSE) +
  geom_text(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "V"),
            aes(x = V1, y = V2, color = grupo_modalidad, label = PALABRA), size = 4, show.legend = FALSE) +
  geom_text(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "I"),
            aes(x = V1, y = V2, color = grupo_modalidad, label = PALABRA), size = 4, show.legend = FALSE) +
  geom_text(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "H"),
            aes(x = V1, y = V2, color = grupo_modalidad, label = PALABRA), size = 4, show.legend = FALSE) +
  geom_text(data = subset(perceptual_norms_tsne, perceptual_norms_tsne$Dominant.sensorimotor == "A"),
            aes(x = V1, y = V2, color = grupo_modalidad, label = PALABRA), size = 4, show.legend = FALSE) +
  # quita la leyenda para la transparencia y el tamaño de las variables
  scale_alpha_continuous(guide = 'none') +
  scale_size_continuous(guide = 'none') +
  # modifica la leyenda para que tenga las letras representativas, no solo el color como un punto o la lentra en minúsculas (como se tiene por default).
  guides(colour = guide_legend(override.aes = list(size = 6, shape = c(utf8ToInt("B"), utf8ToInt("C"), utf8ToInt("V"), utf8ToInt("I"), utf8ToInt("H"),
                                                                       utf8ToInt("I"), utf8ToInt("C"), utf8ToInt("H"), utf8ToInt("V"), utf8ToInt("A"))))) +
  # nombrar los ejes "x" y "y"
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  # quitar el título
  ggtitle("") +
  # poner colores y caraterísticas de la leyenda
  scale_colour_manual(name = "Modalidad", breaks = c("Control_Boca_garganta", "Control_Cabeza", "Control_Visual",
                                                     "Control_Interoceptivo", "Control_Haptico", 
                                                     "Depresion_Interoceptivo", "Depresion_Cabeza", "Depresion_Haptico",
                                                     "Depresion_Visual", "Depresion_Auditivo"),
                      labels = c("Control_Boca_garganta", "Control_Cabeza", "Control_Visual",
                                 "Control_Interoceptivo", "Control_Haptico",
                                 "Depresion_Interoceptivo", "Depresion_Cabeza", "Depresion_Haptico",
                                 "Depresion_Visual", "Depresion_Auditivo"),
                      values = c(# Grupo1 - escala rosas
                        "Control_Boca_garganta" = "#CD96CD", "Control_Cabeza" = "#FF69B4", "Control_Visual" = "#8B0A50",
                        "Control_Interoceptivo" = "#DC143C", "Control_Haptico" = "#FF6A6A",
                        # Grupo2 - escala azules
                        "Depresion_Interoceptivo" = "#6C7B8B", "Depresion_Cabeza" = "#1E90FF", "Depresion_Haptico" = "#0000FF",
                        "Depresion_Visual" = "#008B8B", "Depresion_Auditivo" = "palegreen4"
                      )) +
  theme_classic(base_size=10) +
  # posición de la leyenda y el tamaño del texto
  theme(legend.position = c(0.99, 0.55), legend.justification=c(1,0), legend.title=element_text(size=18),
        legend.text = element_text(size=12), axis.text=element_text(size=15), axis.title=element_text(size=15,face="bold"))
#PLOT
tsne_perceptual_plot

# Gráfico de fuerza motora y perceptual

fuerza_mo_pl<- ggplot(sensorimotor_porparticipante,
                      aes(x = GRUPO, y = as.numeric(Max_strength.action), 
                          fill = CONDICION)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(0.9), width = 0.2) +
  labs(title = "Fuerza motora",
       y = "Fuerza máxima (promedio)" +
         theme_classic()
  )+
  # Barras de significancia PARA CADA GRUPO
  geom_signif(
    # Comparaciones dentro del grupo Control
    comparisons = list(c("Control", "Control")),  # Mismo grupo
    map_signif_level = TRUE,
    annotations = "*",
    y_position = 4.8,
    tip_length = 0.02
  ) +
  geom_signif(
    # Comparaciones dentro del grupo Depresión
    comparisons = list(c("Depresion", "Depresion")),
    map_signif_level = TRUE,
    annotations = "*",  
    y_position = 4.8,
    tip_length = 0.02
  )

# Fuerza perceptual
fuerza_pe_pl<- ggplot(sensorimotor_porparticipante,
                      aes(x = GRUPO, y = as.numeric(Max_strength.perceptual), 
                          fill = CONDICION)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(0.9), width = 0.2) +
  labs(title = "Fuerza perceptual",
       y = "Fuerza máxima (promedio)" +
         theme_classic()
  )+
  # Barras de significancia PARA CADA GRUPO
  geom_signif(
    # Comparaciones dentro del grupo Control
    comparisons = list(c("Control", "Control")),  # Mismo grupo
    map_signif_level = TRUE,
    annotations = "*",
    y_position = 4.8,
    tip_length = 0.02
  )

fuerza_pe_pl

# library(cowplot) # Esto es por motivos de visualización (unir dos gráficas), es opcional.
plot_grid(fuerza_mo_pl, fuerza_pe_pl,
          labels = c("A", "B"),
          label_size = 15,
          nrow = 2, 
          align = "v",
          rel_widths = c(1, 1.2))

# Grafica valencia

ggplot(sensorimotor_porparticipante, 
       aes(x = Minkowski3.sensorimotor, 
           y = as.numeric(VALENCIA), 
           color = CONDICION)) +
  geom_smooth(method = "lm", se = TRUE) +  # Líneas con bandas de error
  geom_point(alpha = 0.3) +  # Puntos semitransparentes
  labs(x = "Minkowski3", y = "Valencia", color = "Condición") +
  theme_minimal()
