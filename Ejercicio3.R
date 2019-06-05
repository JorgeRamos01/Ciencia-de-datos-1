

################################### Ejercicio 3 ############################################


## Función que imputa datos haciendo uso de la proporción que representa la variable
ImputeData <- function(df, vars, total.var){
  
  new <- df
  
  for(i in vars){
    pob.sel <- df[, i]
    filter1 <- df[, i] >= 0  # Solo se consideran los valores positivos
    # Calcula de la proporción
    val_mean <- mean(df[filter1, total.var])  # media del total de las filas seleccionadas
    prop.sel <- mean(df[filter1, i]) / ifelse(isTRUE(all.equal(val_mean, 0)), 1, val_mean)
    
    pob.sel[!filter1] <- prop.sel * df[!filter1, total.var]  # Imputamos los nuevos datos
    new[, i] <- pob.sel
  }
  
  new
}


# Construcción de los indicadores
indicador1 <- function(df){
  
  sel.var <- "POB20"
  pob.15mas <- df[, sel.var]  # población con 15 años o más
  neg.filter <- df[, sel.var] >= 0  # positive quantities
  prop.15mas <- mean(df[neg.filter, sel.var]) / mean(df[neg.filter, "POB1"])
  
  pob.15mas[!neg.filter] <- prop.15mas * df[!neg.filter, "POB1"]
  
  
  sel.var <- "EDU28" 
  pob.anal <- df[, sel.var]  # analfabeta
  neg.filter <- df[, sel.var] >= 0  # cantidades positivas
  prop.anal <- mean(df[neg.filter, sel.var]) / mean(df[neg.filter, "POB1"])
  
  pob.anal[!neg.filter] <- prop.anal * df[!neg.filter, "POB1"]
  
  (pob.anal / pob.15mas)*100
}


indicador2 <- function(df){
  
  vars <- c("EDU31", "P15PRI_IN", "P15SEC_IN", "P15PRI_CO", "P15SEC_CO",
            "P18YM_PB")  
  total.var <- "POB1" 
  
  newdata <- ImputeData(df, vars = vars, total.var = total.var) 

  numd <- newdata[, "EDU31"] + newdata[, "P15PRI_IN"]
  denom <- apply(newdata[, vars], 1, sum) 
  
  (numd / denom)*100 
}


indicador3 <- function(df){
  vars <- c("VIV19")
  total.var <- "VIV2"
  
  newdata <- ImputeData(df, vars = vars, total.var = total.var)
  
  denom <- newdata[, "VIV2"]
  numd <- denom - newdata[, "VIV19"]
  
  zeros <- sapply(denom, function(x) isTRUE(all.equal(x, 0)))
  
  mapply(function(denom, numd, zeros) ifelse(zeros, denom, numd / denom), denom, numd, zeros)
}

indicador4 <- function(df){
  
  vars <- c("VIV15", "VIV14")
  total.var <- "VIV2"
  
  newdata <- ImputeData(df, vars, total.var)
  
  numd <- newdata[, "VIV15"]
  denom <- numd + newdata[, "VIV14"]
  
  mapply(function(y, x) ifelse(isTRUE(all.equal(x, 0)), x, y / x), numd, denom)
}


indicador5 <- function(df){
  vars <- c("VIV17", "VIV16")
  total.var <- "VIV2"
  
  newdata <- ImputeData(df, vars, total.var)
  
  numd <- newdata[, "VIV17"]
  denom <- numd + newdata[, "VIV16"]
  
  mapply(function(y, x) ifelse(isTRUE(all.equal(x, 0)), x, y / x), numd, denom)
}

indicador6 <- function(df){
  
  vars <- c("PROM_OCUP")
  total.var <- "POB1"
  
  newdata <- ImputeData(df, vars, total.var)  
  
  newdata[, "PROM_OCUP"]
}


indicador7 <- function(df){
  
  vars <- c("VIV6", "VPH_PISODT")
  total.var <- "VIV2"
  
  newdata <- ImputeData(df, vars, total.var = total.var)
  
  numd <- newdata[, "VIV6"]
  denom <- numd + newdata[ ,"VPH_PISODT"]
  
  mapply(function(y, x) ifelse(isTRUE(all.equal(x, 0)), x, y / x), numd, denom)
}


indicador8 <- function(df){
  
  vars <- "VIV26"
  total.var <- "VIV2"
  
  newdata <- ImputeData(df, vars, total.var)
  
  numd <- newdata[, "VIV2"] - newdata[, "VIV26"]
  denom <- newdata[, "VIV2"] 
  
  mapply(function(y, x) ifelse(isTRUE(all.equal(x, 0)), x, y / x), numd, denom)
}
  


#Comenzamos a generar el indicador
rawdata <- read.csv("censo_nl.csv")

## Variables faltantes

rawdata.extra <- read.csv("censo_nl_viviendas.txt", 
                          na.strings = c("*", "N/D", "N/A"))

### Limpieza de los datos con las variables faltantes

rawdata.extra2 <- rawdata.extra

for(i in 1:ncol(rawdata.extra2)){
  
  colclass <- class(rawdata.extra2[, i]) 
  if(colclass == "integer" | colclass == "numeric")
    rawdata.extra2[is.na(rawdata.extra[, i]), i] <- -1  
}

rawdata.extra3 <- rawdata.extra2
keys1 <- c("ENTIDAD", "MUN", "LOC")  
rawdata.extra3[, "CVEGEO"] <- paste0(formatC(rawdata.extra2[, keys1[1]],width = 2, format = "d", flag = "0"),
                                     formatC(rawdata.extra2[, keys1[2]], width = 3, format = "d", flag = "0"),
                                     formatC(rawdata.extra2[, keys1[3]], width = 4, format = "d", flag = "0"))

extra.vars <- c("CVEGEO", "VPH_PISODT", "VPH_PISOTI", "PROM_OCUP", "P_TOTAL",
                "P15PRI_IN", "P15PRI_CO", "P15SEC_IN", "P15SEC_CO", "P18YM_PB")

rawdata.comp <- merge(rawdata, rawdata.extra3[, extra.vars], 
                      by = "CVEGEO", sort = F)


ind.mat <- (sapply(1:8, function(i) get(paste0("indicador", i))(rawdata.comp))) * 100
colnames(ind.mat) <- c("15.analfabeta", "15.sin.primaria", "viv.sin.excusado",
                       "viv.sin.energia", "viv.sin.agua.entub", "prom.ocup", 
                       "viv.piso.tierra", "viv.sin.refrig")


mat.z <-scale(ind.mat)  # Matriz con los valores de los indicadores normalizados

mat.cor <- cor(mat.z)  # cMatriz de correlación

## Aplicación de PCA a la matriz 
pca.model <- prcomp(mat.z, scale. = T)

loads <- pca.model$rotation

loads[, 1] #Obtenemos los pesos del indicador

