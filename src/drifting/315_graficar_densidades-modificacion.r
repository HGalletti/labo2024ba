# Script para encontrar visualmente el data drifting
# Focalizado solo en los campos de un buen árbol de decisión

# Limpio la memoria
rm(list = ls()) # Remove all objects
gc() # Garbage collection

require("data.table")
require("rpart")
require("yaml")

kmes0 <- 202107
kmes1 <- 202109

#------------------------------------------------------------------------------
# Función para calcular el ancho de banda específico para cada columna
calcular_ancho_banda <- function(campo) {
  datos_columna <- dataset[foto_mes %in% c(kmes0, kmes1), get(campo)]
  
  # Excluir los valores extremos según el rango intercuartílico
  q <- quantile(datos_columna, probs = c(0.05, 0.95), na.rm = TRUE)
  datos_filtrados <- datos_columna[datos_columna >= q[1] & datos_columna <= q[2]]
  
  # Eliminar valores faltantes
  datos_filtrados <- na.omit(datos_filtrados)
  
  # Verificar si hay datos suficientes después de la limpieza
  if (length(datos_filtrados) < 2) {
    return(NA) # Retornar NA si no hay suficientes datos para calcular la densidad
  }
  
  # Calcular el ancho de banda usando el rango intercuartílico filtrado
  density(datos_filtrados, kernel = "gaussian")$bw
}

# Función para graficar la densidad de un campo
graficar_campo <- function(campo) {
  ancho_banda <- calcular_ancho_banda(campo)
  
  # Si el ancho de banda no se puede calcular, saltar el campo
  if (is.na(ancho_banda)) {
    cat("No se puede calcular el ancho de banda para", campo, "\n")
    return()
  }
  
  # Quitar de gráfico las colas del 5% de las densidades
  qA <- quantile(dataset[foto_mes == kmes0, get(campo)],
                 prob = c(0.05, 0.95), na.rm = TRUE
  )
  
  qB <- quantile(dataset[foto_mes == kmes1, get(campo)],
                 prob = c(0.05, 0.95), na.rm = TRUE
  )
  
  xxmin <- pmin(qA[[1]], qB[[1]])
  xxmax <- pmax(qA[[2]], qB[[2]])
  
  densidad_A <- density(dataset[foto_mes == kmes0, get(campo)],
                        kernel = "gaussian", bw = ancho_banda, na.rm = TRUE
  )
  
  densidad_B <- density(dataset[foto_mes == kmes1, get(campo)],
                        kernel = "gaussian", bw = ancho_banda, na.rm = TRUE
  )
  
  plot(densidad_A,
       col = "blue",
       xlim = c(xxmin, xxmax),
       ylim = c(0, pmax(max(densidad_A$y), max(densidad_B$y))),
       main = campo
  )
  
  lines(densidad_B, col = "red", lty = 2)
  
  legend("topright",
         legend = c(kmes0, kmes1),
         col = c("blue", "red"), lty = c(1, 2)
  )
}

#------------------------------------------------------------------------------
# Aquí comienza el programa
setwd("~/buckets/b1/") # Establezco el Working Directory

# Cargo miAmbiente
miAmbiente <- read_yaml("~/buckets/b1/miAmbiente.yml")

# Cargo dataset
dataset <- fread(miAmbiente$dataset_pequeno)

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/DR3150/", showWarnings = FALSE)
setwd("./exp/DR3150/")

dataset <- dataset[foto_mes %in% c(kmes0, kmes1)]

# Creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[
  foto_mes == kmes0,
  clase_binaria := ifelse(clase_ternaria == "CONTINUA", "NEG", "POS")
]

# Entreno el modelo
# Utilizo los mejores hiperparámetros encontrados
# en una Bayesian Optimization con 5-fold Cross Validation
modelo <- rpart(
  formula = "clase_binaria ~ . -clase_ternaria",
  data = dataset[foto_mes == kmes0], # Los datos donde voy a entrenar
  xval = 0,
  cp = -1,
  minsplit = 1144,
  minbucket = 539,
  maxdepth = 8
)

campos_modelo <- names(modelo$variable.importance)
campos_buenos <- c(campos_modelo, setdiff(colnames(dataset), campos_modelo))
campos_buenos <- setdiff(
  campos_buenos,
  c("foto_mes", "clase_ternaria", "clase_binaria")
)

# Genero los gráficos en un archivo
pdf(paste0("densidades_con_mismo_bw_", kmes0, "_", kmes1, ".pdf"))

for (campo in campos_buenos) {
  cat(campo, "  ")
  graficar_campo(campo)
}

dev.off()

# Copio al bucket para Modalidad Conceptual
system("~/install/repobrutalcopy.sh")

