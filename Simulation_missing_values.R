
rm(list = ls())

library(kableExtra)
library(ez)
library(lavaan)
library(mice)
library(reshape)
library(mitml)
library(lme4)
library(ggplot2)
library(matlib)

###################################################################
###################################################################
###################################################################
###################################################################

                      # Perdida de datos Aleatoria

porcentajes <- c(0, 0.05, 0.1, 0.15, 0.20, 0.25)

Prueba_Clásica_p <- as.numeric(sample(NA,6, replace = T))
Prueba_I_M_Mice_p <- as.numeric(sample(NA,6, replace = T))
Prueba_MV_Lavaan_p <- as.numeric(sample(NA,6, replace = T))

for (i in 1:length(porcentajes)) {
  
  set.seed(123)  # Para reproducibilidad
  
  Grupo <- c(rep(0, 150), rep(1, 150))
  Comprension_lectora <- c(rnorm(150, 20, 1), rnorm(150, 19.65, 1))
  Notamedia <- sample(2:17, 300, replace = TRUE)
  
  datos <- data.frame(Grupo, Comprension_lectora, Notamedia)
  
  indices_na <- sample(1:nrow(datos), porcentajes[i]*300, replace = FALSE) # De manera aleatoria
  
  datos$Comprension_lectora[indices_na] <- NA
  
  # Prueba Clásica
  
  P_C <- t.test(Comprension_lectora ~ Grupo,conf.level = 0.95, data = datos)
  
  Prueba_Clásica_p[i] <- P_C$p.value
  
  # Imputación Múltiple con Mice
  
  imp1 <- mice(datos,method="norm", m=50, seed = 20000)
  print(imp1)
  complete(imp1,1)
  fitindep <- with(imp1, lm(Comprension_lectora ~ Grupo))
  summary <- summary(pool(fitindep))
  summary[,2:6] <- round(summary[,2:6], 3); summary
  
  Prueba_I_M_Mice_p[i] <- summary$p.value[2]
  
  # Máxima Verosimilitud Lavaan
  
  HS.tind <- '
Comprension_lectora~~Comprension_lectora
Notamedia~~Notamedia
Notamedia~~Comprension_lectora
#medias 
Comprension_lectora ~ c(m0,m1)*1 #se restringen que sean iguales poniendo c(m0,m0)*1
difm := m0-m1 #wald test 
'
  fitindml <- cfa(HS.tind,data = datos,group = "Grupo",missing = "ML")
  sum_MV <- summary(fitindml,fit.measures=TRUE,standardized=TRUE) 
  
  Prueba_MV_Lavaan_p[i] <- sum_MV$pe$pvalue[11]
  
}


Datos_NAs <- c("Sin_NA", "5%", "10%", "15%", "20%", "25%")

Prueba_Clásica_p <- round(Prueba_Clásica_p,3)

Prueba_MV_Lavaan_p <- round(Prueba_MV_Lavaan_p,3)

Tabla_Final <- cbind(Datos_NAs, Prueba_Clásica_p, Prueba_I_M_Mice_p, Prueba_MV_Lavaan_p)

Tabla_Final %>%
  kbl(caption = "Tabla Comparaciones NAs aleatorios") %>%
  kable_classic(full_width = F, html_font = "Cambria")



###################################################################
###################################################################
###################################################################
###################################################################

                       # Perdida de datos MAR

rm(list = ls())

library(kableExtra)
library(ez)
library(lavaan)
library(mice)
library(reshape)
library(mitml)
library(lme4)
library(ggplot2)
library(matlib)

porcentajes <- c(0, 0.05, 0.1, 0.15, 0.20, 0.25)

Prueba_Clásica_p <- as.numeric(sample(NA,6, replace = T))
Prueba_I_M_Mice_p <- as.numeric(sample(NA,6, replace = T))
Prueba_MV_Lavaan_p <- as.numeric(sample(NA,6, replace = T))

for (i in 1:length(porcentajes)) {

  set.seed(123)  # Para reproducibilidad
  
  Grupo <- c(rep(0, 150), rep(1, 150))
  Comprension_lectora <- c(rnorm(150, 20, 1), rnorm(150, 19.65, 1))
  Notamedia <- c(rnorm(150, 15, 1), rnorm(150, 14, 1))  # Los que tengan mayor comprensión tendrán mayor Notamedia
  hist(Notamedia)
  datos <- data.frame(Grupo, Comprension_lectora, Notamedia)

  # Calculamos los cuantiles de Notamedia para definir "alto" y "bajo"
  high_quantile <- quantile(datos$Notamedia, 1-porcentajes[i])
  
  # Seleccionamos índices donde Notamedia es alta o baja
  high_indices <- which(datos$Notamedia >= high_quantile)
  
  # Combinamos los índices
  selected_indices <- high_indices

  # Sacamos el 5% de estos índices para asignar NA
  na_indices <- sample(selected_indices, size = round(porcentajes[i] * nrow(datos)))
  
  # Por último, asignamos los NAs
  datos$Comprension_lectora[na_indices] <- NA
  
  # Prueba Clásica
  
  P_C <- t.test(Comprension_lectora ~ Grupo,conf.level = 0.95, data = datos)
  
  Prueba_Clásica_p[i] <- P_C$p.value
  
  # Imputación Múltiple con Mice
  
  imp1 <- mice(datos,method="norm", m=50, seed = 20000)
  print(imp1)
  complete(imp1,1)
  fitindep <- with(imp1, lm(Comprension_lectora ~ Grupo))
  summary <- summary(pool(fitindep))
  summary[,2:6] <- round(summary[,2:6], 3); summary
  
  Prueba_I_M_Mice_p[i] <- summary$p.value[2]
  
  # Máxima Verosimilitud Lavaan
  
  HS.tind <- '
Comprension_lectora~~Comprension_lectora
Notamedia~~Notamedia
Notamedia~~Comprension_lectora
#medias 
Comprension_lectora ~ c(m0,m1)*1 #se restringen que sean iguales poniendo c(m0,m0)*1
difm := m0-m1 #wald test 
'
  fitindml <- cfa(HS.tind,data = datos,group = "Grupo",missing = "ML")
  sum_MV <- summary(fitindml,fit.measures=TRUE,standardized=TRUE) 
  
  Prueba_MV_Lavaan_p[i] <- sum_MV$pe$pvalue[11]

}


Datos_NAs <- c("Sin_NA", "5%", "10%", "15%", "20%", "25%")

Prueba_Clásica_p <- round(Prueba_Clásica_p,3)

Prueba_MV_Lavaan_p <- round(Prueba_MV_Lavaan_p,3)

Tabla_Final <- cbind(Datos_NAs, Prueba_Clásica_p, Prueba_I_M_Mice_p, Prueba_MV_Lavaan_p)

Tabla_Final %>%
  kbl(caption = "Tabla Comparaciones MAR") %>%
  kable_classic(full_width = F, html_font = "Cambria")








