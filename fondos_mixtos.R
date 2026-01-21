#CARGO LAS LIBRERIAS
install.packages("readxl")
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
library(rpart.plot)
library(randomForest)
library(DMwR2)
library(imputeR)
library(mice)
library(VIM)
library(imputeTS)
library(missForest)
library(class)
library(ggcorrplot)
library(plotly)

#CREO LOS DATAFRAMES
df_mixtos<-read.xlsx("fondos_mixtos.xlsx",sheet = 1)


#COBERTURA DE DATOS
cobertura_funcion<-function(df){
  cobertura_datos<-colMeans(!is.na(df))*100
  cobertura<-data.frame(cobertura_datos)
  return(cobertura_datos)}

cobertura_mixtos<-data.frame(cobertura_funcion(df_mixtos))

write.xlsx(cobertura_mixtos,"cobertura_mixtos.xlsx",colnames=TRUE)


#FUNCION QUE ELIMINA COLUMNAS CON BAJA COBERTURA DE DATOS (menor de 70%)
eliminar_columnas_baja_cobertura<-function(df,umbral_cobertura){
  cobertura_por_columna<-colMeans(!is.na(df))*100
  columnas_a_mantener<-cobertura_por_columna>=umbral_cobertura
  df_sin_baja_cobertura<-df[,columnas_a_mantener,drop=FALSE]
  return(df_sin_baja_cobertura)
}
datos_modelo<-eliminar_columnas_baja_cobertura(df_mixtos,70)

#VARIABLES PARA EL MODELO
variables_modelo<-c("Share.Class.without.Retrocession",
                    "Firm.Country",
                    "Number.of.Funds.in.Global.Category..Sustainability",
                    "Fund.of.Funds",
                    "Investment.Area",
                    "EU.SFDR.Fund.type.Article.8.or.Article.9",
                    "UCITS",
                    "Region.of.Sale",
                    "Investor.Type..Retail",
                    "Investor.Type..Professional",
                    "Investor.Type..Eligible.Counterparty",
                    "Institutional",
                    "Minimum.Investment.Base.Currency",
                    "Fund.Size.Base.Currency",
                    "Net.Assets..Share.Class.Base.Currency",
                    "Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency",
                    "Asset.Alloc.Bond..Net",
                    "Asset.Alloc.Equity..Net",
                    "Asset.Alloc.Cash..Net",
                    "Asset.Alloc.Other..Net",
                    ".of.Bond.Holdings.Long",
                    ".of.Holdings.Long",
                    ".of.Stock.Holdings.Long",
                    ".of.Other.Holdings.Long",
                    ".Asset.in.Top.10.Holdings",
                    "PRIIPS.KID.Summary.Risk.Indicator",
                    "Management.Fee.Starting",
                    "PRIIPS.KID.Ongoing.Costs.Other.Costs",
                    "PRIIPS.KID.Ongoing.Costs.Transaction.Cost",
                    "Entry.Fee",
                    "Exit.Fee",
                    "Performance.Fee.Actual",
                    "Std.Dev.3.Yr.MoEnd.Risk.Currency",
                    "Gain.Std.Dev.20210401.to.20240331.Base.Currency" ,  
                    "Loss.Std.Dev.20210201.to.20240331.Base.Currency",
                    "Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency",
                    "Dividend.Yield.Long",
                    "Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency",             
                     "Emerging.Market.Bond..Long",                      
                     "Emerging.Market.Stock..Long",                      
                     "Equity.Style.Small.Cap..Long",                      
                     "Equity.Style.Mid.Cap..Long",                      
                     "Equity.Style.Large.Cap..Long",                     
                     "Equity.Style.Core..Long",                           
                    "Equity.Style.Value..Long",                          
                     "Equity.Style.Growth..Long",                         
                    "Equity.Style.Factor.BV.Growth.Long",                
                     "Equity.Style.Box.Long",                             
                     "ValueGrowth.Score.Long",                            
                     "Equity.Econ.Super.Sector.Cyclical..Long",           
                     "Equity.Econ.Super.Sector.Defensive..Long",          
                     "Equity.Econ.Super.Sector.Sensitive..Long",          
                     "Market.Cap.Giant..Long",                            
                     "Market.Cap.Micro..Long", 
                    "12.Mo.Yield",
                    "FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI",
                    "Investment.Grade",
                    "High.Yield",
                    "FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI",
                    "Tamaño.de.Gestora"
)

# Filtrar el conjunto de datos con las variables seleccionadas
datos_modelo<- datos_modelo[,variables_modelo]

#RESUMEN ESTADISTICO DE LAS VARIABLES DEL MODELO
resumen1<-summary(datos_modelo)


#IMPUTACION VALORES AUSENTES
datos_imputados<-kNN(datos_modelo,variable = "Number.of.Funds.in.Global.Category..Sustainability",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Minimum.Investment.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Fund.Size.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Asset.Alloc.Bond..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Asset.Alloc.Equity..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Asset.Alloc.Cash..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Asset.Alloc.Other..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".of.Bond.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".of.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".of.Stock.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".of.Other.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".Asset.in.Top.10.Holdings",k=3)
datos_imputados<-kNN(datos_imputados,variable = "PRIIPS.KID.Summary.Risk.Indicator",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Management.Fee.Starting",k=3)
datos_imputados<-kNN(datos_imputados,variable = "PRIIPS.KID.Ongoing.Costs.Other.Costs",k=3)
datos_imputados<-kNN(datos_imputados,variable = "PRIIPS.KID.Ongoing.Costs.Transaction.Cost",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Entry.Fee",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Exit.Fee",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Std.Dev.3.Yr.MoEnd.Risk.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Loss.Std.Dev.20210201.to.20240331.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Gain.Std.Dev.20210401.to.20240331.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Dividend.Yield.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Emerging.Market.Stock..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Style.Small.Cap..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Style.Mid.Cap..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Style.Large.Cap..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Style.Core..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Style.Value..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Style.Growth..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Style.Factor.BV.Growth.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Style.Box.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Econ.Super.Sector.Cyclical..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Econ.Super.Sector.Defensive..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Equity.Econ.Super.Sector.Sensitive..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Market.Cap.Giant..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Market.Cap.Micro..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "12.Mo.Yield",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Investment.Grade",k=3)
datos_imputados<-kNN(datos_imputados,variable = "High.Yield",k=3)
datos_imputados<-kNN(datos_imputados,variable = "FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI",k=3)
datos_imputados<-kNN(datos_imputados,variable = "FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI",k=3)

write.xlsx(datos_imputados,"datos_imputados_mixtos.xlsx")



datos_imputados<-read.xlsx("datos_imputados_mixtos.xlsx",sheet = 1)


#ELIMINO TODAS LAS VARIABLES NUEVAS CREADAS
eliminar_columnas <- function(df) {
  
  cols_to_keep <- grep("_imp$", names(df), value = TRUE, invert = TRUE)
  df <- df[, cols_to_keep]
  
  return(df)
}
datos_modelo<-eliminar_columnas(datos_imputados)
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "PRIIPS.KID.Ongoing.Costs.Other.Costs"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "12.Mo.Yield"]

#RESUMEN ESTADISTICO DE LAS VARIABLES DEL MODELO
summary(datos_modelo)


#TRANSFORMO LAS VARIABLES CATEGORICAS EN FACTOR
datos_modelo$Share.Class.without.Retrocession<-as.factor(datos_modelo$Share.Class.without.Retrocession)
datos_modelo$Firm.Country<-as.factor(datos_modelo$Firm.Country)
datos_modelo$Fund.of.Funds<-as.factor(datos_modelo$Fund.of.Funds)
datos_modelo$Investment.Area<-as.factor(datos_modelo$Investment.Area)
datos_modelo$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(datos_modelo$EU.SFDR.Fund.type.Article.8.or.Article.9)
datos_modelo$UCITS<-as.factor(datos_modelo$UCITS)
datos_modelo$Region.of.Sale<-as.factor(datos_modelo$Region.of.Sale)
datos_modelo$Investor.Type..Retail<-as.factor(datos_modelo$Investor.Type..Retail)
datos_modelo$Investor.Type..Professional<-as.factor(datos_modelo$Investor.Type..Professional)
datos_modelo$Investor.Type..Eligible.Counterparty<-as.factor(datos_modelo$Investor.Type..Eligible.Counterparty)
datos_modelo$Institutional<-as.factor(datos_modelo$Institutional)
datos_modelo$Equity.Style.Box.Long<-as.factor(datos_modelo$Equity.Style.Box.Long)

#TRANSFORMO LA VARIABLE TOTAL RET ANNALZ
datos_modelo$Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency<-datos_modelo$Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency+datos_modelo$Management.Fee.Starting



###################################CORRELACIONES###########################################################
columnas_numericas<-na.omit(datos_modelo[,sapply(datos_modelo,is.numeric)])
columnas_numericas_sin_constantes<-scale(na.omit(columnas_numericas[,apply(columnas_numericas,2,function(x) length(unique(x))>1)]))
correlacion_mixtos<-data.frame(cor(columnas_numericas,method="spearman"))
cor_objetivo<- correlacion_mixtos["Management.Fee.Starting", -which(names(correlacion_mixtos) == "Management.Fee.Starting")]
cor_objetivo_long <- gather(cor_objetivo, key = "Variable", value = "Correlacion")


ggplot(cor_objetivo_long, aes(x = reorder(Variable, -Correlacion), y = Correlacion, fill = NULL)) +
  geom_bar(stat = "identity", fill = "#99CCFF") +  # Todas las barras en azul claro
  geom_text(aes(label = round(Correlacion, 2), y = Correlacion), hjust = -0.2, size = 3.5, color = "black") + # Aumentar tamaño de las etiquetas de los valores
  coord_flip() + 
  labs(title = "Correlación con Management Fee Starting", x = "Variable") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Ocultar la leyenda
    axis.text.y = element_text(size = 9),  # Aumentar tamaño de los nombres de las variables
    plot.title = element_text(size = 14)  # Aumentar tamaño del título
  )


write.xlsx(correlacion_rf,"correlacion_mixtos.xlsx")

#CORRELACION DE LA VARIABLE ENTRY FEE
correlacion_entryfee<-data.frame(cor(columnas_numericas,method="spearman"))
cor_objetivo<- correlacion_mixtos["Entry.Fee", -which(names(correlacion_entryfee) == "Entry.Fee")]
cor_objetivo_long <- gather(cor_objetivo, key = "Variable", value = "Correlacion")


ggplot(cor_objetivo_long, aes(x = reorder(Variable, -Correlacion), y = Correlacion, fill = NULL)) +
  geom_bar(stat = "identity", fill = "#99CCFF") +  # Todas las barras en azul claro
  geom_text(aes(label = round(Correlacion, 2), y = Correlacion), hjust = -0.2, size = 3.5, color = "black") + # Aumentar tamaño de las etiquetas de los valores
  coord_flip() + 
  labs(title = "Correlación con Entry Fee", x = "Variable") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Ocultar la leyenda
    axis.text.y = element_text(size = 12),  # Aumentar tamaño de los nombres de las variables
    plot.title = element_text(size = 14)  # Aumentar tamaño del título
  )

#ELIMINAR VARIABLE CON ALTA CORRELACION
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% ".of.Bond.Holdings.Long"]



##############################OUTLIERS#################################################################

#FUNCION VISUALIZACION OUTLIERS
visualizar_outliers <- function(dataframe) {
  num_cols <- sapply(dataframe, is.numeric)
  dataframe_num <- dataframe[, num_cols][45:46]
  
  par(mar = c(4, 4, 2, 1))
  par(mfrow = c(ceiling(sqrt(ncol(dataframe_num))), ceiling(sqrt(ncol(dataframe_num)))))
  
  for (i in 1:ncol(dataframe_num)) {
    boxplot(dataframe_num[, i], main = names(dataframe_num)[i], col = "skyblue", border = "black", cex.main = 2.25)
    # Ajusta el valor de cex.main según tus preferencias para controlar el tamaño del título
  }
  
  par(mfrow = c(1, 1))
}

quartz(width = 15, height = 10)
visualizar_outliers(datos_modelo)



#ELIMINAR OUTLIERS POR PERCENTILES
eliminar_outliers<-function(df,percentil_inferior,percentil_superior){
  datos_sin_outliers<-df
  for (col in names(df)){
    if (is.numeric(df[[col]])){
      datos_numericos<-df[[col]][!is.na(df[[col]])]
      q_inf<-quantile(datos_numericos,percentil_inferior)
      q_sup<-quantile(datos_numericos,percentil_superior)
      datos_sin_outliers<-datos_sin_outliers[is.na(datos_sin_outliers[[col]]) |
                                               datos_sin_outliers[[col]]>=q_inf&
                                               datos_sin_outliers[[col]]<=q_sup,]
    }
  }
  return(datos_sin_outliers)
}

datos_modelo<-eliminar_outliers(datos_modelo,0.01,0.99)



#############################MODELOS###################################################################

#DIVIDO LOS DATOS EN ENTRENAMIENTO Y PRUEBA
set.seed(123)
indice_entrenamiento<-sample(seq_len(nrow(datos_modelo)),size=0.8*nrow(datos_modelo))
datos_entrenamiento<-na.omit(datos_modelo[indice_entrenamiento, ]) #80%datos
datos_prueba<-na.omit(datos_modelo[-indice_entrenamiento, ]) #20% datos


cambiar_nombres<-gsub("[#%]","",colnames(datos_entrenamiento)) #quito hastags y porcentajes de los nombres
colnames(datos_entrenamiento)<-cambiar_nombres
cambiar_nombres2<-gsub("[#%]","",colnames(datos_prueba)) #quito hastags y porcentajes de los nombres
colnames(datos_prueba)<-cambiar_nombres2



############################ARBOL####################################################################################
#Modelo de arbol mediante validación cruzada para encontrar el mejor cp y max_depth
cp_grid <- expand.grid(cp = seq(0.01, 0.2, by = 0.01))
max_depth_values <- c(3, 5, 7, 10)

# Lista para almacenar los resultados de la validación cruzada
cv_results <- list()

# Iterar sobre cada valor de cp y max_depth
for (cp in cp_grid$cp) {
  for (depth in max_depth_values) {
    
    modelo_cv <- rpart(
      Management.Fee.Starting ~ ., 
      data = datos_entrenamiento,
      method = "anova",
      control = rpart.control(cp = cp, maxdepth = depth)
    )
    #Realizar predicciones en el conjunto de prueba
    predicciones <- predict(modelo_cv, newdata = datos_prueba)
    #Calcular el ECM
    ecm <- mean((predicciones - datos_prueba$Management.Fee.Starting)^2)
    #Almacenar los resultados en la lista
    cv_results <- c(cv_results, list(list(cp = cp, max_depth = depth, ecm = ecm)))
  }
}
#Convertir los resultados de la validación cruzada en un dataframe
cv_results_df <- do.call(rbind, lapply(cv_results, as.data.frame))

#Encontrar la fila que contiene el mejor valor de cp
fila_mejor_cp <- cv_results_df[which.min(cv_results_df$ecm), ]

#Extraer el mejor valor de cp y de max_depth
best_cp <- fila_mejor_cp$cp
best_max_depth <- fila_mejor_cp$max_depth

# Reentrenar el modelo con los mejores hiperparámetros
mejor_modelo_arbol <- rpart(
  Management.Fee.Starting ~ ., 
  data = datos_entrenamiento,
  method = "anova",
  control = rpart.control(cp = best_cp, maxdepth = best_max_depth)
)

# Resumen del mejor modelo
print(mejor_modelo_arbol)

# Realizar predicciones en el conjunto de prueba
predicciones <- predict(mejor_modelo_arbol, newdata = datos_prueba)

# Calcular el nuevo error cuadrático medio (ECM)
ecm_nuevo <- mean((predicciones - datos_prueba$Management.Fee.Starting)^2)

# Imprimir el ECM con los nuevos hiperparámetros
print(ecm_nuevo)

#Visualizar el árbol
prp(mejor_modelo_arbol, split.col = 2, cex = 0.8)


#Comparamos las predicciones
resultados <- data.frame(
  Predicciones = predicciones,
  Comision_de_gestion = datos_prueba$Management.Fee.Starting
)

#Estadisticas del arbol
printcp(mejor_modelo_arbol)





#############################RANDOM FOREST####################################################################################################
#Modelo de random forest con validacion cruzada para encontrar el mejor num_trees y mtry
mtry_values <- c(12,13,14,15)  # Valores para mtry
num_trees_values <- c(100, 200, 300,400)  # Valores para num.trees

# Lista para almacenar los resultados de la validación cruzada
cv_results <- list()

# Iterar sobre cada valor de mtry y num.trees
for (mtry in mtry_values) {
  for (num_trees in num_trees_values) {
    
    # Entrenar el modelo con ranger
    modelo_cv <- ranger(
      Management.Fee.Starting ~ ., 
      data = datos_entrenamiento,
      num.trees = num_trees,
      mtry = mtry,
      seed = 42,  # Semilla para reproducibilidad
      importance = "impurity" 
    )
    
    # Realizar predicciones en el conjunto de prueba
    predicciones <- predict(modelo_cv, data = datos_prueba)$predictions
    
    # Calcular el ECM
    ecm <- mean((predicciones - datos_prueba$Management.Fee.Starting)^2)
    
    # Almacenar los resultados en la lista
    cv_results <- c(cv_results, list(list(mtry = mtry, num_trees = num_trees, ecm = ecm)))
  }
}
#Convertir los resultados de la validación cruzada en un dataframe
cv_results_df <- do.call(rbind, lapply(cv_results, as.data.frame))

#Encontrar la fila que contiene el mejor valor de cp
fila_mejor <- cv_results_df[which.min(cv_results_df$ecm), ]

#Extraer el mejor valor de cp y de max_depth
best_mtry <- fila_mejor$mtry
best_num_trees <- fila_mejor$num_trees


#Reentrenamos el modelo con los mejores hiperparametros
modelo_rf <- ranger(
  Management.Fee.Starting ~ .,  
  data = datos_entrenamiento,
  num.trees = best_num_trees,  # Número de árboles en el bosque 
  mtry=best_mtry,
  importance = "impurity"  
)

#PREDICCIONES
predicciones_rf <- predict(modelo_rf, data = datos_prueba)$predictions

#ERROR CUADRATICO MEDIO
ecm_rf <- mean((predicciones_rf - datos_prueba$Management.Fee.Starting)^2)
ecm_rf



#IMPORTANCIA VARIABLES
importancia_rf <- modelo_rf$variable.importance

importancia_df <- data.frame(Variable = names(importancia_rf), Importance = importancia_rf)
importancia_df <- importancia_df[order(importancia_df$Importance, decreasing = TRUE), ]
importancia_df_filtrado <- importancia_df[importancia_df$Importance > 70, ]

# Crear el gráfico de barras con ggplot2
ggplot(importancia_df_filtrado, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(Importance, 2)), hjust = -0.1, size = 3.5) +
  coord_flip() +  # Voltear el gráfico para que las barras sean horizontales
  labs(
    title = "Importancia de las Variables",
    x = "Variables",
    y = "Importancia"
  ) +
  theme_minimal()







#MODELO RF LIMITANDO LAS VARIABLES EN FUNCION DEL UMBRAL DE LA IMPORTANCIA
umbral_importancia <- 70  # Ejemplo de umbral de importancia
variables_no_importantes <- rownames(importancia_df[importancia_df$Importance <= umbral_importancia, ])

datos_entrenamiento_reducido <- datos_entrenamiento[, c("Minimum.Investment.Base.Currency",
                                                        "Tamaño.de.Gestora",
                                                        "Entry.Fee",
                                                        "Share.Class.without.Retrocession",
                                                        "FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI",
                                                       "Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency",
                                                       "Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency",
                                                        "Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency",
                                                        "Management.Fee.Starting",
                                                        "Asset.Alloc.Bond..Net")]


mtry_values <- c(2,3,4,5)  # Valores para mtry
num_trees_values <- c(100, 200, 300,400)  # Valores para num.trees

# Lista para almacenar los resultados de la validación cruzada
cv_results <- list()

# Iterar sobre cada valor de mtry y num.trees
for (mtry in mtry_values) {
  for (num_trees in num_trees_values) {
    
    # Entrenar el modelo con ranger
    modelo_cv <- ranger(
      Management.Fee.Starting ~ ., 
      data = datos_entrenamiento_reducido,
      num.trees = num_trees,
      mtry = mtry,
      seed = 42,  # Semilla para reproducibilidad
      importance = "impurity" 
    )
    
    # Realizar predicciones en el conjunto de prueba
    predicciones <- predict(modelo_cv, data = datos_prueba)$predictions
    
    # Calcular el ECM
    ecm <- mean((predicciones - datos_prueba$Management.Fee.Starting)^2)
    
    # Almacenar los resultados en la lista
    cv_results <- c(cv_results, list(list(mtry = mtry, num_trees = num_trees, ecm = ecm)))
  }
}
#Convertir los resultados de la validación cruzada en un dataframe
cv_results_df <- do.call(rbind, lapply(cv_results, as.data.frame))

#Encontrar la fila que contiene el mejor valor de cp
fila_mejor <- cv_results_df[which.min(cv_results_df$ecm), ]

#Extraer el mejor valor de cp y de max_depth
best_mtry <- fila_mejor$mtry
best_num_trees <- fila_mejor$num_trees


#Reentrenamos el modelo con los mejores hiperparametros
modelo_rf_reducido <- ranger(
  Management.Fee.Starting ~ .,  
  data = datos_entrenamiento_reducido,
  num.trees = best_num_trees,  # Número de árboles en el bosque 
  mtry=best_mtry,
  importance = "impurity",
  write.forest = TRUE
)

#PREDICCIONES
predicciones_rf_reducido <- predict(modelo_rf_reducido, data = datos_prueba)$predictions

#ERROR CUADRATICO MEDIO
ecm_rf_reducido <- mean((predicciones_rf_reducido - datos_prueba$Management.Fee.Starting)^2)
ecm_rf_reducido



#########EJEMPLO KUTXABANK GESTIÓN ACTIVA INV ESTÁNDAR FI#######################
nuevos_datos<-data.frame(Share.Class.without.Retrocession="No",                  
                          Firm.Country="Spain",                                    
                          Number.of.Funds.in.Global.Category..Sustainability=7075,
                          Fund.of.Funds="Yes",                                    
                          Investment.Area="Global",                                  
                          EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                          UCITS="Yes",                                           
                          Region.of.Sale="Spain",                                    
                          Investor.Type..Retail="Yes",                            
                          Investor.Type..Professional="Yes",                       
                          Investor.Type..Eligible.Counterparty="Yes",              
                          Institutional="No",                                    
                          Minimum.Investment.Base.Currency=1,                 
                          Fund.Size.Base.Currency=7220485030,                          
                          Net.Assets..Share.Class.Base.Currency=124999387,             
                          Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=43602248.089, 
                          Asset.Alloc.Bond..Net=19.36944,                           
                          Asset.Alloc.Equity..Net=74.12918,                        
                          Asset.Alloc.Cash..Net=6.17775,                            
                          Asset.Alloc.Other..Net=0.32361,                            
                          .of.Bond.Holdings.Long=0,                            
                          .of.Holdings.Long=34,                                
                          .of.Stock.Holdings.Long=0,                          
                          .of.Other.Holdings.Long=34,                           
                          .Asset.in.Top.10.Holdings=57.34072,                        
                          PRIIPS.KID.Summary.Risk.Indicator=4,              
                          PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,         
                          Entry.Fee=0,                                       
                          Exit.Fee=0,                                         
                          Performance.Fee.Actual=0,                           
                          Std.Dev.3.Yr.MoEnd.Risk.Currency=12.478,                
                          Gain.Std.Dev.20210401.to.20240331.Base.Currency=6.98737203,   
                          Loss.Std.Dev.20210201.to.20240331.Base.Currency=1.28006459,   
                          Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=4.52,      
                          Dividend.Yield.Long=0.02051,                         
                          Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=0.187,           
                          Emerging.Market.Bond..Long=0.7717,                    
                          Emerging.Market.Stock..Long=1.85449,                    
                          Equity.Style.Small.Cap..Long=3.93654,                   
                          Equity.Style.Mid.Cap..Long=17.29579,                     
                          Equity.Style.Large.Cap..Long=43.55132,                    
                          Equity.Style.Core..Long=24.69456,                        
                          Equity.Style.Value..Long=13.18037,                       
                          Equity.Style.Growth..Long=26.90872,                        
                          Equity.Style.Factor.BV.Growth.Long=4.43019,             
                          Equity.Style.Box.Long="Large Blend",                      
                          ValueGrowth.Score.Long=199.871,                       
                          Equity.Econ.Super.Sector.Cyclical..Long=20.80443,         
                          Equity.Econ.Super.Sector.Defensive..Long=12.48006,        
                          Equity.Econ.Super.Sector.Sensitive..Long=32.02034,        
                          Market.Cap.Giant..Long=21.60333,                  
                          Market.Cap.Micro..Long=0.21966,                     
                          FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=3.32653652,             
                          Investment.Grade=84.48742,                             
                          High.Yield=4.69589,                                     
                          FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=10.81669,         
                          Tamaño.de.Gestora=23689035497)



#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)





#########EJEMPLO KUTXABANK GESTIÓN ACTIVA INV EXTRA FI#######################
nuevos_datos<-data.frame( Share.Class.without.Retrocession="No",                  
                          Firm.Country="Spain",                                    
                          Number.of.Funds.in.Global.Category..Sustainability=7075,
                          Fund.of.Funds="Yes",                                    
                          Investment.Area="Global",                                  
                          EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                          UCITS="Yes",                                           
                          Region.of.Sale="Spain",                                    
                          Investor.Type..Retail="Yes",                            
                          Investor.Type..Professional="Yes",                       
                          Investor.Type..Eligible.Counterparty="Yes",              
                          Institutional="No",                                    
                          Minimum.Investment.Base.Currency=30000,                 
                          Fund.Size.Base.Currency=218275438,                          
                          Net.Assets..Share.Class.Base.Currency=35036755,             
                          Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=17269405.897, 
                          Asset.Alloc.Bond..Net=19.36944,                           
                          Asset.Alloc.Equity..Net=74.12918,                        
                          Asset.Alloc.Cash..Net=6.17775,                            
                          Asset.Alloc.Other..Net=0.32361,                            
                          .of.Bond.Holdings.Long=0,                            
                          .of.Holdings.Long=34,                                
                          .of.Stock.Holdings.Long=0,                          
                          .of.Other.Holdings.Long=34,                           
                          .Asset.in.Top.10.Holdings=57.34072,                        
                          PRIIPS.KID.Summary.Risk.Indicator=4,              
                          PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,         
                          Entry.Fee=0,                                       
                          Exit.Fee=0,                                         
                          Performance.Fee.Actual=0,                           
                          Std.Dev.3.Yr.MoEnd.Risk.Currency=12.478,                
                          Gain.Std.Dev.20210401.to.20240331.Base.Currency=6.98737203,   
                          Loss.Std.Dev.20210201.to.20240331.Base.Currency=1.28006459,   
                          Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=3.892,      
                          Dividend.Yield.Long=0.02051,                         
                          Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=0.199,           
                          Emerging.Market.Bond..Long=0.7717,                    
                          Emerging.Market.Stock..Long=1.85449,                    
                          Equity.Style.Small.Cap..Long=3.93654,                   
                          Equity.Style.Mid.Cap..Long=17.29579,                     
                          Equity.Style.Large.Cap..Long=43.55132,                    
                          Equity.Style.Core..Long=24.69456,                        
                          Equity.Style.Value..Long=13.18037,                       
                          Equity.Style.Growth..Long=26.90872,                        
                          Equity.Style.Factor.BV.Growth.Long=4.43019,             
                          Equity.Style.Box.Long="Large Blend",                      
                          ValueGrowth.Score.Long=199.871,                       
                          Equity.Econ.Super.Sector.Cyclical..Long=20.80443,         
                          Equity.Econ.Super.Sector.Defensive..Long=12.48006,        
                          Equity.Econ.Super.Sector.Sensitive..Long=32.02034,        
                          Market.Cap.Giant..Long=21.60333,                  
                          Market.Cap.Micro..Long=0.21966,                     
                          FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=3.32653652,             
                          Investment.Grade=84.48742,                             
                          High.Yield=4.69589,                                     
                          FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=10.81669,         
                          Tamaño.de.Gestora=23689035497)



#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)



#########EJEMPLO KUTXABANK GESTIÓN ACTIVA INV PLUS FI#######################
nuevos_datos<-data.frame(Share.Class.without.Retrocession="No",                  
                          Firm.Country="Spain",                                    
                          Number.of.Funds.in.Global.Category..Sustainability=7075,
                          Fund.of.Funds="Yes",                                    
                          Investment.Area="Global",                                  
                          EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                          UCITS="Yes",                                           
                          Region.of.Sale="Spain",                                    
                          Investor.Type..Retail="Yes",                            
                          Investor.Type..Professional="Yes",                       
                          Investor.Type..Eligible.Counterparty="Yes",              
                          Institutional="No",                                    
                          Minimum.Investment.Base.Currency=60000,                 
                          Fund.Size.Base.Currency=218275438,                          
                          Net.Assets..Share.Class.Base.Currency=60448888,             
                          Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=19014752.476, 
                          Asset.Alloc.Bond..Net=19.36944,                           
                          Asset.Alloc.Equity..Net=74.12918,                        
                          Asset.Alloc.Cash..Net=6.17775,                            
                          Asset.Alloc.Other..Net=0.32361,                            
                          .of.Bond.Holdings.Long=0,                            
                          .of.Holdings.Long=34,                                
                          .of.Stock.Holdings.Long=0,                          
                          .of.Other.Holdings.Long=34,                           
                          .Asset.in.Top.10.Holdings=57.34072,                        
                          PRIIPS.KID.Summary.Risk.Indicator=4,              
                          PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,         
                          Entry.Fee=0,                                       
                          Exit.Fee=0,                                         
                          Performance.Fee.Actual=0,                           
                          Std.Dev.3.Yr.MoEnd.Risk.Currency=12.482,                
                          Gain.Std.Dev.20210401.to.20240331.Base.Currency=6.98737203,   
                          Loss.Std.Dev.20210201.to.20240331.Base.Currency=1.28006459,   
                          Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=4.53,      
                          Dividend.Yield.Long=0.02051,                         
                          Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=0.223,           
                          Emerging.Market.Bond..Long=0.7717,                    
                          Emerging.Market.Stock..Long=1.85449,                    
                          Equity.Style.Small.Cap..Long=3.93654,                   
                          Equity.Style.Mid.Cap..Long=17.29579,                     
                          Equity.Style.Large.Cap..Long=43.55132,                    
                          Equity.Style.Core..Long=24.69456,                        
                          Equity.Style.Value..Long=13.18037,                       
                          Equity.Style.Growth..Long=26.90872,                        
                          Equity.Style.Factor.BV.Growth.Long=4.43019,             
                          Equity.Style.Box.Long="Large Blend",                      
                          ValueGrowth.Score.Long=199.871,                       
                          Equity.Econ.Super.Sector.Cyclical..Long=20.80443,         
                          Equity.Econ.Super.Sector.Defensive..Long=12.48006,        
                          Equity.Econ.Super.Sector.Sensitive..Long=32.02034,        
                          Market.Cap.Giant..Long=21.60333,                  
                          Market.Cap.Micro..Long=0.21966,                     
                          FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=3.32653652,             
                          Investment.Grade=84.48742,                             
                          High.Yield=4.69589,                                     
                          FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=10.81669,         
                          Tamaño.de.Gestora=23689035497)



#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)



#########EJEMPLO KUTXABANK GESTIÓN ACTIVA PATRI ESTÁNDAR FI#######################
nuevos_datos<-data.frame( Share.Class.without.Retrocession="No",                  
                          Firm.Country="Spain",                                    
                          Number.of.Funds.in.Global.Category..Sustainability=5997,
                          Fund.of.Funds="Yes",                                    
                          Investment.Area="Global",                                  
                          EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                          UCITS="Yes",                                           
                          Region.of.Sale="Spain",                                    
                          Investor.Type..Retail="Yes",                            
                          Investor.Type..Professional="Yes",                       
                          Investor.Type..Eligible.Counterparty="Yes",              
                          Institutional="No",                                    
                          Minimum.Investment.Base.Currency=1,                 
                          Fund.Size.Base.Currency=877468359,                          
                          Net.Assets..Share.Class.Base.Currency=365189883,             
                          Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=38636906.929, 
                          Asset.Alloc.Bond..Net=67.56386,                           
                          Asset.Alloc.Equity..Net=14.37363,                        
                          Asset.Alloc.Cash..Net=17.05543,                            
                          Asset.Alloc.Other..Net=1.00708,                            
                          .of.Bond.Holdings.Long=0,                            
                          .of.Holdings.Long=32,                                
                          .of.Stock.Holdings.Long=0,                          
                          .of.Other.Holdings.Long=32,                           
                          .Asset.in.Top.10.Holdings=76.93419,                        
                          PRIIPS.KID.Summary.Risk.Indicator=2,              
                          PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,         
                          Entry.Fee=0,                                       
                          Exit.Fee=0,                                         
                          Performance.Fee.Actual=0,                           
                          Std.Dev.3.Yr.MoEnd.Risk.Currency=4.092,                
                          Gain.Std.Dev.20210401.to.20240331.Base.Currency=2.60189774599563,   
                          Loss.Std.Dev.20210201.to.20240331.Base.Currency=2.51371651998277,   
                          Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=0.855,      
                          Dividend.Yield.Long=0.02083,                         
                          Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0.404,           
                          Emerging.Market.Bond..Long=2.53339,                    
                          Emerging.Market.Stock..Long=0.38332,                    
                          Equity.Style.Small.Cap..Long=0.82121,                   
                          Equity.Style.Mid.Cap..Long=3.92533,                     
                          Equity.Style.Large.Cap..Long=9.74396,                    
                          Equity.Style.Core..Long=5.49744,                        
                          Equity.Style.Value..Long=3.23103,                       
                          Equity.Style.Growth..Long=5.76203,                        
                          Equity.Style.Factor.BV.Growth.Long=4.64652,             
                          Equity.Style.Box.Long="Large Blend",                      
                          ValueGrowth.Score.Long=192.187,                       
                          Equity.Econ.Super.Sector.Cyclical..Long=4.69124,         
                          Equity.Econ.Super.Sector.Defensive..Long=2.69158,        
                          Equity.Econ.Super.Sector.Sensitive..Long=7.22908,        
                          Market.Cap.Giant..Long=4.94624,                  
                          Market.Cap.Micro..Long=0.03809,                     
                          FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=5.89799801010666,             
                          Investment.Grade=86.46148,                             
                          High.Yield=4.00394,                                     
                          FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=9.53458,         
                          Tamaño.de.Gestora=23689035497)



#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)



#########EJEMPLO KUTXABANK GESTIÓN ACTIVA PATRI EXTRA FI#######################
nuevos_datos<-data.frame( Share.Class.without.Retrocession="No",                  
                          Firm.Country="Spain",                                    
                          Number.of.Funds.in.Global.Category..Sustainability=5997,
                          Fund.of.Funds="Yes",                                    
                          Investment.Area="Global",                                  
                          EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                          UCITS="Yes",                                           
                          Region.of.Sale="Spain",                                    
                          Investor.Type..Retail="Yes",                            
                          Investor.Type..Professional="Yes",                       
                          Investor.Type..Eligible.Counterparty="Yes",              
                          Institutional="No",                                    
                          Minimum.Investment.Base.Currency=30000,                 
                          Fund.Size.Base.Currency=877468359,                          
                          Net.Assets..Share.Class.Base.Currency=183225472,             
                          Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=38636906.929, 
                          Asset.Alloc.Bond..Net=67.56386,                           
                          Asset.Alloc.Equity..Net=14.37363,                        
                          Asset.Alloc.Cash..Net=17.05543,                            
                          Asset.Alloc.Other..Net=1.00708,                            
                          .of.Bond.Holdings.Long=0,                            
                          .of.Holdings.Long=32,                                
                          .of.Stock.Holdings.Long=0,                          
                          .of.Other.Holdings.Long=32,                           
                          .Asset.in.Top.10.Holdings=76.93419,                        
                          PRIIPS.KID.Summary.Risk.Indicator=2,              
                          PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,         
                          Entry.Fee=0,                                       
                          Exit.Fee=0,                                         
                          Performance.Fee.Actual=0,                           
                          Std.Dev.3.Yr.MoEnd.Risk.Currency=4.092,                
                          Gain.Std.Dev.20210401.to.20240331.Base.Currency=2.60189774599563,   
                          Loss.Std.Dev.20210201.to.20240331.Base.Currency=2.51371651998277,   
                          Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=0.854,      
                          Dividend.Yield.Long=0.02083,                         
                          Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0.367,           
                          Emerging.Market.Bond..Long=2.53339,                    
                          Emerging.Market.Stock..Long=0.38332,                    
                          Equity.Style.Small.Cap..Long=0.82121,                   
                          Equity.Style.Mid.Cap..Long=3.92533,                     
                          Equity.Style.Large.Cap..Long=9.74396,                    
                          Equity.Style.Core..Long=5.49744,                        
                          Equity.Style.Value..Long=3.23103,                       
                          Equity.Style.Growth..Long=5.76203,                        
                          Equity.Style.Factor.BV.Growth.Long=4.64652,             
                          Equity.Style.Box.Long="Large Blend",                      
                          ValueGrowth.Score.Long=192.187,                       
                          Equity.Econ.Super.Sector.Cyclical..Long=4.69124,         
                          Equity.Econ.Super.Sector.Defensive..Long=2.69158,        
                          Equity.Econ.Super.Sector.Sensitive..Long=7.22908,        
                          Market.Cap.Giant..Long=4.94624,                  
                          Market.Cap.Micro..Long=0.03809,                     
                          FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=5.89799801010666,             
                          Investment.Grade=86.46148,                             
                          High.Yield=4.00394,                                     
                          FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=9.53458,         
                          Tamaño.de.Gestora=23689035497)



#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)


#########EJEMPLO KUTXABANK GESTIÓN ACTIVA PATRI PLUS FI#######################
nuevos_datos<-data.frame( Share.Class.without.Retrocession="No",                  
                          Firm.Country="Spain",                                    
                          Number.of.Funds.in.Global.Category..Sustainability=5997,
                          Fund.of.Funds="Yes",                                    
                          Investment.Area="Global",                                  
                          EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                          UCITS="Yes",                                           
                          Region.of.Sale="Spain",                                    
                          Investor.Type..Retail="Yes",                            
                          Investor.Type..Professional="Yes",                       
                          Investor.Type..Eligible.Counterparty="Yes",              
                          Institutional="No",                                    
                          Minimum.Investment.Base.Currency=60000,                 
                          Fund.Size.Base.Currency=877468359,                          
                          Net.Assets..Share.Class.Base.Currency=329053004,             
                          Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=134132772.282, 
                          Asset.Alloc.Bond..Net=67.56386,                           
                          Asset.Alloc.Equity..Net=14.37363,                        
                          Asset.Alloc.Cash..Net=17.05543,                            
                          Asset.Alloc.Other..Net=1.00708,                            
                          .of.Bond.Holdings.Long=0,                            
                          .of.Holdings.Long=32,                                
                          .of.Stock.Holdings.Long=0,                          
                          .of.Other.Holdings.Long=32,                           
                          .Asset.in.Top.10.Holdings=76.93419,                        
                          PRIIPS.KID.Summary.Risk.Indicator=2,              
                          PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,         
                          Entry.Fee=0,                                       
                          Exit.Fee=0,                                         
                          Performance.Fee.Actual=0,                           
                          Std.Dev.3.Yr.MoEnd.Risk.Currency=4.092,                
                          Gain.Std.Dev.20210401.to.20240331.Base.Currency=2.60189774599563,   
                          Loss.Std.Dev.20210201.to.20240331.Base.Currency=2.51371651998277,   
                          Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=0.852,      
                          Dividend.Yield.Long=0.02083,                         
                          Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0.316,           
                          Emerging.Market.Bond..Long=2.53339,                    
                          Emerging.Market.Stock..Long=0.38332,                    
                          Equity.Style.Small.Cap..Long=0.82121,                   
                          Equity.Style.Mid.Cap..Long=3.92533,                     
                          Equity.Style.Large.Cap..Long=9.74396,                    
                          Equity.Style.Core..Long=5.49744,                        
                          Equity.Style.Value..Long=3.23103,                       
                          Equity.Style.Growth..Long=5.76203,                        
                          Equity.Style.Factor.BV.Growth.Long=4.64652,             
                          Equity.Style.Box.Long="Large Blend",                      
                          ValueGrowth.Score.Long=192.187,                       
                          Equity.Econ.Super.Sector.Cyclical..Long=4.69124,         
                          Equity.Econ.Super.Sector.Defensive..Long=2.69158,        
                          Equity.Econ.Super.Sector.Sensitive..Long=7.22908,        
                          Market.Cap.Giant..Long=4.94624,                  
                          Market.Cap.Micro..Long=0.03809,                     
                          FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=5.89799801010666,             
                          Investment.Grade=86.46148,                             
                          High.Yield=4.00394,                                     
                          FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=9.53458,         
                          Tamaño.de.Gestora=23689035497)



#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)


#########EJEMPLO KUTXABANK GESTIÓN ACTIVA REND ESTÁNDAR FI#######################
nuevos_datos<-data.frame( Share.Class.without.Retrocession="No",                  
                          Firm.Country="Spain",                                    
                          Number.of.Funds.in.Global.Category..Sustainability=5766,
                          Fund.of.Funds="Yes",                                    
                          Investment.Area="Global",                                  
                          EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                          UCITS="Yes",                                           
                          Region.of.Sale="Spain",                                    
                          Investor.Type..Retail="Yes",                            
                          Investor.Type..Professional="Yes",                       
                          Investor.Type..Eligible.Counterparty="Yes",              
                          Institutional="No",                                    
                          Minimum.Investment.Base.Currency=1,                 
                          Fund.Size.Base.Currency=1153754489,                          
                          Net.Assets..Share.Class.Base.Currency=568661624,             
                          Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=134132772.282, 
                          Asset.Alloc.Bond..Net=43.5997,                           
                          Asset.Alloc.Equity..Net=45.87869,                        
                          Asset.Alloc.Cash..Net=9.84275,                            
                          Asset.Alloc.Other..Net=0.67886,                            
                          .of.Bond.Holdings.Long=2,                            
                          .of.Holdings.Long=36,                                
                          .of.Stock.Holdings.Long=0,                          
                          .of.Other.Holdings.Long=34,                           
                          .Asset.in.Top.10.Holdings=55.74866,                        
                          PRIIPS.KID.Summary.Risk.Indicator=3,              
                          PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,         
                          Entry.Fee=0,                                       
                          Exit.Fee=0,                                         
                          Performance.Fee.Actual=0,                           
                          Std.Dev.3.Yr.MoEnd.Risk.Currency=8.474,                
                          Gain.Std.Dev.20210401.to.20240331.Base.Currency=4.83217109084197,   
                          Loss.Std.Dev.20210201.to.20240331.Base.Currency=4.38514331504822,   
                          Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=1.867,      
                          Dividend.Yield.Long=0.0202,                         
                          Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0.068,           
                          Emerging.Market.Bond..Long=2.52553,                    
                          Emerging.Market.Stock..Long=1.2105,                    
                          Equity.Style.Small.Cap..Long=2.44703,                   
                          Equity.Style.Mid.Cap..Long=11.85231,                     
                          Equity.Style.Large.Cap..Long=31.99456,                    
                          Equity.Style.Core..Long=17.36458,                        
                          Equity.Style.Value..Long=9.814,                       
                          Equity.Style.Growth..Long=19.11532,                        
                          Equity.Style.Factor.BV.Growth.Long=4.75543,             
                          Equity.Style.Box.Long="Large Blend",                      
                          ValueGrowth.Score.Long=199.252,                       
                          Equity.Econ.Super.Sector.Cyclical..Long=14.83906,         
                          Equity.Econ.Super.Sector.Defensive..Long=8.60767,        
                          Equity.Econ.Super.Sector.Sensitive..Long=23.19456,        
                          Market.Cap.Giant..Long=16.7394,                  
                          Market.Cap.Micro..Long=0.10791,                     
                          FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=7.30717617208187,             
                          Investment.Grade=85.10109,                             
                          High.Yield=6.38151,                                     
                          FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=8.51739,         
                          Tamaño.de.Gestora=23689035497)



#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)



#########EJEMPLO KUTXABANK GESTIÓN ACTIVA REND EXTRA FI#######################
nuevos_datos<-data.frame( Share.Class.without.Retrocession="No",                  
                          Firm.Country="Spain",                                    
                          Number.of.Funds.in.Global.Category..Sustainability=5766,
                          Fund.of.Funds="Yes",                                    
                          Investment.Area="Global",                                  
                          EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                          UCITS="Yes",                                           
                          Region.of.Sale="Spain",                                    
                          Investor.Type..Retail="Yes",                            
                          Investor.Type..Professional="Yes",                       
                          Investor.Type..Eligible.Counterparty="Yes",              
                          Institutional="No",                                    
                          Minimum.Investment.Base.Currency=30000,                 
                          Fund.Size.Base.Currency=1153754489,                          
                          Net.Assets..Share.Class.Base.Currency=235928801,             
                          Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=102060292.629, 
                          Asset.Alloc.Bond..Net=43.5997,                           
                          Asset.Alloc.Equity..Net=45.87869,                        
                          Asset.Alloc.Cash..Net=9.84275,                            
                          Asset.Alloc.Other..Net=0.67886,                            
                          .of.Bond.Holdings.Long=2,                            
                          .of.Holdings.Long=36,                                
                          .of.Stock.Holdings.Long=0,                          
                          .of.Other.Holdings.Long=34,                           
                          .Asset.in.Top.10.Holdings=55.74866,                        
                          PRIIPS.KID.Summary.Risk.Indicator=3,              
                          PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,         
                          Entry.Fee=0,                                       
                          Exit.Fee=0,                                         
                          Performance.Fee.Actual=0,                           
                          Std.Dev.3.Yr.MoEnd.Risk.Currency=8.474,                
                          Gain.Std.Dev.20210401.to.20240331.Base.Currency=4.83217109084197,   
                          Loss.Std.Dev.20210201.to.20240331.Base.Currency=4.38514331504822,   
                          Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=1.866,      
                          Dividend.Yield.Long=0.0202,                         
                          Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0.044,           
                          Emerging.Market.Bond..Long=2.52553,                    
                          Emerging.Market.Stock..Long=1.2105,                    
                          Equity.Style.Small.Cap..Long=2.44703,                   
                          Equity.Style.Mid.Cap..Long=11.85231,                     
                          Equity.Style.Large.Cap..Long=31.99456,                    
                          Equity.Style.Core..Long=17.36458,                        
                          Equity.Style.Value..Long=9.814,                       
                          Equity.Style.Growth..Long=19.11532,                        
                          Equity.Style.Factor.BV.Growth.Long=4.75543,             
                          Equity.Style.Box.Long="Large Blend",                      
                          ValueGrowth.Score.Long=199.252,                       
                          Equity.Econ.Super.Sector.Cyclical..Long=14.83906,         
                          Equity.Econ.Super.Sector.Defensive..Long=8.60767,        
                          Equity.Econ.Super.Sector.Sensitive..Long=23.19456,        
                          Market.Cap.Giant..Long=16.7394,                  
                          Market.Cap.Micro..Long=0.10791,                     
                          FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=7.30717617208187,             
                          Investment.Grade=85.10109,                             
                          High.Yield=6.38151,                                     
                          FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=8.51739,         
                          Tamaño.de.Gestora=23689035497)



#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)


#########EJEMPLO KUTXABANK GESTIÓN ACTIVA REND PLUS FI#######################
nuevos_datos<-data.frame( Share.Class.without.Retrocession="No",                  
                          Firm.Country="Spain",                                    
                          Number.of.Funds.in.Global.Category..Sustainability=5766,
                          Fund.of.Funds="Yes",                                    
                          Investment.Area="Global",                                  
                          EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                          UCITS="Yes",                                           
                          Region.of.Sale="Spain",                                    
                          Investor.Type..Retail="Yes",                            
                          Investor.Type..Professional="Yes",                       
                          Investor.Type..Eligible.Counterparty="Yes",              
                          Institutional="No",                                    
                          Minimum.Investment.Base.Currency=60000,                 
                          Fund.Size.Base.Currency=1153754489,                          
                          Net.Assets..Share.Class.Base.Currency=349164064,             
                          Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=133418327.493, 
                          Asset.Alloc.Bond..Net=43.5997,                           
                          Asset.Alloc.Equity..Net=45.87869,                        
                          Asset.Alloc.Cash..Net=9.84275,                            
                          Asset.Alloc.Other..Net=0.67886,                            
                          .of.Bond.Holdings.Long=2,                            
                          .of.Holdings.Long=36,                                
                          .of.Stock.Holdings.Long=0,                          
                          .of.Other.Holdings.Long=34,                           
                          .Asset.in.Top.10.Holdings=55.74866,                        
                          PRIIPS.KID.Summary.Risk.Indicator=3,              
                          PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,         
                          Entry.Fee=0,                                       
                          Exit.Fee=0,                                         
                          Performance.Fee.Actual=0,                           
                          Std.Dev.3.Yr.MoEnd.Risk.Currency=8.474,                
                          Gain.Std.Dev.20210401.to.20240331.Base.Currency=4.83217109084197,   
                          Loss.Std.Dev.20210201.to.20240331.Base.Currency=4.38514331504822,   
                          Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=1.866,      
                          Dividend.Yield.Long=0.0202,                         
                          Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0.02,           
                          Emerging.Market.Bond..Long=2.52553,                    
                          Emerging.Market.Stock..Long=1.2105,                    
                          Equity.Style.Small.Cap..Long=2.44703,                   
                          Equity.Style.Mid.Cap..Long=11.85231,                     
                          Equity.Style.Large.Cap..Long=31.99456,                    
                          Equity.Style.Core..Long=17.36458,                        
                          Equity.Style.Value..Long=9.814,                       
                          Equity.Style.Growth..Long=19.11532,                        
                          Equity.Style.Factor.BV.Growth.Long=4.75543,             
                          Equity.Style.Box.Long="Large Blend",                      
                          ValueGrowth.Score.Long=199.252,                       
                          Equity.Econ.Super.Sector.Cyclical..Long=14.83906,         
                          Equity.Econ.Super.Sector.Defensive..Long=8.60767,        
                          Equity.Econ.Super.Sector.Sensitive..Long=23.19456,        
                          Market.Cap.Giant..Long=16.7394,                  
                          Market.Cap.Micro..Long=0.10791,                     
                          FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=7.30717617208187,             
                          Investment.Grade=85.10109,                             
                          High.Yield=6.38151,                                     
                          FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=8.51739,         
                          Tamaño.de.Gestora=23689035497)



#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)









# Obtener información sobre la primera división del primer árbol
# Supongamos que el modelo se llama `modelo_rf`
primer_arbol <- modelo_rf$forest[[1]]  # Accede al primer árbol del modelo

# Obtenemos el número de nodos en el árbol
num_nodos <- length(primer_arbol$split.varIDs)

# Iteramos sobre cada nodo del árbol
for (i in 1:num_nodos) {
  # Obtén el nombre de la variable utilizada para la división
  var_nombre <- modelo_rf$forest[[1]]$split.varIDs[i]
  cat("Variable utilizada para la división en el nodo", i, ":", var_nombre, "\n")
  
  # Obtén el valor de la división
  split_valor <- modelo_rf$forest[[1]]$split.values[i]
  cat("Valor de la división en el nodo", i, ":", split_valor, "\n")
  
  # Obtén los nodos hijos
  left_child <- modelo_rf$forest[[1]]$leftDaughter[i]  # Nodo hijo izquierdo
  right_child <- modelo_rf$forest[[1]]$rightDaughter[i]  # Nodo hijo derecho
  cat("Nodo hijo izquierdo del nodo", i, ":", left_child, "\n")
  cat("Nodo hijo derecho del nodo", i, ":", right_child, "\n")
}