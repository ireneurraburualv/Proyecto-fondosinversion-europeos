#CARGO LAS LIBRERIAS
library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(rpart)
library(pls)
library(e1071)
library(caret)
library(randomForestSRC)
library(ranger)
library(tidyr)


#SI ALGUNA VEZ NO VAN LOS PAQUETES
#chooseCRANmirror()

#CREO LOS DATAFRAMES
df<-read.xlsx("FI_Europa.xlsx",sheet = 1)



##################PREPROCESAMIENTO DE DATOS###########################################################

#ELIMINAR LOS PARENTESIS y GUIONES DE LOS NOMBRES DE LAS VARIABLES
nombres_sin_parentesis<-gsub("[()#%-]","",names(df))
names(df)<-nombres_sin_parentesis

#CAMBIOS DIVISA
numeros_asignados<-c(0.93,1.17,1,0.69,0.61,1.02,0.084,0.006,0.12,0.13,0.12908,
                     0.55,0.68,0.086,0.25,0.13,0.25,0.23,0.0067,0.04,0.05,0.055,
                     0.0026,0.18,0.51,0.2,NA,0.1956,0.26,0.01,0.029,0.0034,0.0021)


#FUNCION PARA CREAR LA COLUMNA TIPO DE CAMBIO
asignar_numeros_personalizados <- function(df, base_currency_column, numeros_asignados) {
  
  # Obtener los nombres únicos de la columna 'base currency'
  monedas_unicas <- unique(df[["Base.Currency"]])
  
  # Crear un data frame auxiliar con las correspondencias entre monedas y números personalizados
  correspondencias <- data.frame(
    Base.Currency = monedas_unicas,
    Tipo.Cambio = numeros_asignados)
  
  # Unir el dataframe original con las correspondencias
   df <- df %>%
    left_join(correspondencias, by = "Base.Currency")
  
  return(df)
}

df<-asignar_numeros_personalizados(df,"Base.Currency",numeros_asignados)


#TRANSFORMACIÓN DE LAS COLUMNAS 
df$Minimum.Investment.Base.Currency <- df$Minimum.Investment.Base.Currency * df$Tipo.Cambio
df$Fund.Size.Base.Currency <- df$Fund.Size.Base.Currency * df$Tipo.Cambio
df$Net.Assets..Share.Class.Base.Currency <- df$Net.Assets..Share.Class.Base.Currency * df$Tipo.Cambio
df$Est.FundLevel.Net.Flow.3.Yr.MoEnd.Base.Currency <- df$Est.FundLevel.Net.Flow.3.Yr.MoEnd.Base.Currency * df$Tipo.Cambio
df$Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency <- df$Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency * df$Tipo.Cambio


#FUNCIÓN PARA CALCULAR EL TAMAÑO DE LA GESTORA
calcular_tamano_gestora <- function(df, firm_name_id_column, assets_column, nueva_columna = "Tamaño.de.Gestora") {
  # Agrupar por Firm.Name.ID y calcular la suma de los activos (patrimonios) de cada gestora
  df <- df %>%
    group_by({{ firm_name_id_column }}) %>%
    mutate({{ nueva_columna }} := sum({{ assets_column }}, na.rm = TRUE)) %>%
    ungroup()  # Desagrupar el dataframe
  
  return(df)
}

df <- calcular_tamano_gestora(df, Firm.Name.ID, Net.Assets..Share.Class.Base.Currency)

#FUNCION PARA CALCULAR MATURITY
# Calcula los valores intermedios para cada intervalo de maturidad
valores_intermedios <- c(2,4, 6, 8.5, 12.5, 17.5, 25)  # Valores intermedios de los intervalos

# Multiplica cada columna por su valor intermedio y luego suma
suma_numerador <- df$Maturity.13.Yr..Long * valores_intermedios[1] +
  df$Maturity.35.Yr..Long * valores_intermedios[2] +
  df$Maturity.57.Yr..Long * valores_intermedios[3] +
  df$Maturity.710.Yr..Long * valores_intermedios[4] +
  df$Maturity.1015.Yr..Long * valores_intermedios[5] +
  df$Maturity.1520.Yr..Long * valores_intermedios[6] + 
  df$Maturity.2030.Yr..Long * valores_intermedios[7]

# Calcula la suma de las columnas originales antes de la multiplicación
suma_denominador <- rowSums(df[, c("Maturity.13.Yr..Long","Maturity.35.Yr..Long", "Maturity.57.Yr..Long", "Maturity.710.Yr..Long",
                                      "Maturity.1015.Yr..Long", "Maturity.1520.Yr..Long", "Maturity.2030.Yr..Long")])

# Crea la nueva columna en el dataframe con el resultado final
df$Maturity_Pond <- suma_numerador / suma_denominador


#RELLENO VALORES AUSENTES 
rellenar_ausentes<-function(df){
  df$FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI<-ifelse(is.na(df$FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI),df$Average.Eff.Duration,df$FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI)
  df$FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI<-ifelse(is.na(df$FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI),df$Maturity_Pond,df$FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI)
  return(df)
}
df<-rellenar_ausentes(df)

write.xlsx(df,"FI Europa(2).xlsx",colnames=TRUE)





