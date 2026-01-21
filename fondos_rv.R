#INSTALAR TODAS LAS LIBRERIAS POSTERIORES
install.packages("")


#CARGO LAS LIBRERIAS
library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(rpart)
library(pls)
library(e1071)
library(dplyr)
library(caret)
library(randomForestSRC)
library(ranger)
library(tidyr)
library(tibble)
library(tidyr)
library(rpart.plot)
library(randomForest)
library(DMwR2)
library(imputeR)
library(mice)
library(VIM)
library(imputeTS)
library(missForest)
library(tree)



#CREO LOS DATAFRAMES
df_rv<-read.xlsx("fondos_rv.xlsx",sheet = 1)

##############################################PROCESAMIENTO####################################################################################


#COBERTURA DE DATOS (funcion que crea un dataframe con el nombre de las variables y la cobertura de datos que tiene cada una)
cobertura_funcion<-function(df){
  cobertura_datos<-colMeans(!is.na(df))*100
  cobertura<-data.frame(cobertura_datos)
  return(cobertura_datos)}

cobertura_rv<-data.frame(cobertura_funcion(df_rv))


#FUNCION QUE ELIMINA COLUMNAS CON BAJA COBERTURA DE DATOS (menor de 70%)
eliminar_columnas_baja_cobertura<-function(df,umbral_cobertura){
  cobertura_por_columna<-colMeans(!is.na(df))*100
  columnas_a_mantener<-cobertura_por_columna>=umbral_cobertura
  df_sin_baja_cobertura<-df[,columnas_a_mantener,drop=FALSE]
  return(df_sin_baja_cobertura)
}
datos_modelo<-eliminar_columnas_baja_cobertura(df_rv,70) #aqui se puede poner el umbral deseado

#VARIABLES PARA EL MODELO (son los nombres de las variables seleccionadas para los modelos, si se desean mas, añadir de la misma forma)
variables_modelo <- c("Share.Class.without.Retrocession",
                      "Firm.Country",
                      "Domicile",
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
                      "Morningstar.Medalist.Rating",
                      "PRIIPS.KID.Summary.Risk.Indicator",
                      "Growth.Grade.Long",
                      "Management.Fee.Starting",
                      "PRIIPS.KID.Ongoing.Costs.Other.Costs",
                      "PRIIPS.KID.Ongoing.Costs.Transaction.Cost",
                      "Entry.Fee",
                      "Exit.Fee",
                      "Performance.Fee.Actual",
                      "Gain.Std.Dev.20210401.to.20240331.Base.Currency",
                      "Std.Dev.3.Yr.MoEnd.Risk.Currency",
                      "Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency",
                      "Total.Return.Abs.Rank.Cat.3.Yr.MoEnd",
                      "Dividend.Yield.Long",
                      "Tracking.Error.3.Yr.MoEnd.Risk.Currency",
                      "Downside.Capture.Ratio.3.Yr.MoEnd.Risk.Currency",
                      "Upside.Capture.Ratio.3.Yr.MoEnd.Risk.Currency",
                      "Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency",
                      "Alpha.nonexcess.return.20210401.to.20240331.Base.Currency",
                      "Beta.nonexcess.return.20210401.to.20240331.Base.Currency",
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
                      "Factor.Profile..Liquidity",
                      "Factor.Profile..Momentum",
                      "Factor.Profile..Quality",
                      "Factor.Profile..Size",
                      "Factor.Profile..Style",
                      "Factor.Profile..Volatility",
                      "Tamaño.de.Gestora")

#FILTRAR CONJUNTO DE DATOS CON LAS VARIABLES
datos_modelo<- datos_modelo[,variables_modelo]

#RESUMEN ESTADÍSTICO VARIABLES
resumen1<-summary(datos_modelo)


#IMPUTACIÓN VALORES AUSENTES CON KNN MEDIANTE EL ALGORTIMO KNN (variable a variable)
datos_imputados<-kNN(datos_modelo,variable="Number.of.Funds.in.Global.Category..Sustainability",k=3)
datos_imputados<-kNN(datos_imputados,variable="Minimum.Investment.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable="Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable="Asset.Alloc.Bond..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable="Asset.Alloc.Equity..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable="Asset.Alloc.Cash..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable="Asset.Alloc.Other..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable=".of.Bond.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable=".of.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable=".of.Stock.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable=".of.Other.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable=".Asset.in.Top.10.Holdings",k=3)
datos_imputados<-kNN(datos_imputados,variable="PRIIPS.KID.Summary.Risk.Indicator",k=3)
datos_imputados<-kNN(datos_imputados,variable="Management.Fee.Starting",k=3)
datos_imputados<-kNN(datos_imputados,variable="PRIIPS.KID.Ongoing.Costs.Other.Costs",k=3)
datos_imputados<-kNN(datos_imputados,variable="PRIIPS.KID.Ongoing.Costs.Transaction.Cost",k=3)
datos_imputados<-kNN(datos_imputados,variable="Entry.Fee",k=3)
datos_imputados<-kNN(datos_imputados,variable="Exit.Fee",k=3)
datos_imputados<-kNN(datos_imputados,variable="Performance.Fee.Actual",k=3)
datos_imputados<-kNN(datos_imputados,variable="Gain.Std.Dev.20210401.to.20240331.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable="Std.Dev.3.Yr.MoEnd.Risk.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable="Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable="Total.Return.Abs.Rank.Cat.3.Yr.MoEnd",k=3)
datos_imputados<-kNN(datos_imputados,variable="Dividend.Yield.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable="Emerging.Market.Stock..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Style.Small.Cap..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Style.Mid.Cap..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Style.Large.Cap..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Style.Core..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Style.Value..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Style.Growth..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Style.Factor.BV.Growth.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Style.Box.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Econ.Super.Sector.Cyclical..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Econ.Super.Sector.Defensive..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Equity.Econ.Super.Sector.Sensitive..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Market.Cap.Giant..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Market.Cap.Micro..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable="Factor.Profile..Liquidity",k=3)
datos_imputados<-kNN(datos_imputados,variable="Factor.Profile..Momentum",k=3)
datos_imputados<-kNN(datos_imputados,variable="Factor.Profile..Quality",k=3)
datos_imputados<-kNN(datos_imputados,variable="Factor.Profile..Size",k=3)
datos_imputados<-kNN(datos_imputados,variable="Factor.Profile..Style",k=3)
datos_imputados<-kNN(datos_imputados,variable="Factor.Profile..Volatility",k=3)
datos_imputados<-kNN(datos_imputados,variable="Tamaño.de.Gestora",k=3)
datos_imputados<-kNN(datos_imputados,variable="ValueGrowth.Score.Long",k=3)

#GUARDAR DATOS IMPUTADOS
write.xlsx(datos_imputados,"datos_imputados_rv.xlsx") #(IMPORTANTE)

#CARGAR DATOS IMPUTADOS
datos_imputados<-read.xlsx("datos_imputados_rv.xlsx",sheet = 1)


#RESUMEN ESTADISTICO DE LAS VARIABLES DEL MODELO
summary(datos_imputados)


#Al hacer el algortimo KNN se crean nuevas columnas con nombres acabados en _imp que son variables binarias indicando si el valor
#ha sido completado o no. La siguiente funcion es para eliminar todas ellas

#ELIMINO TODAS LAS VARIABLES NUEVAS CREADAS
eliminar_columnas <- function(df) {
  cols_to_keep <- grep("_imp$", names(df), value = TRUE, invert = TRUE)
  df <- df[, cols_to_keep]
  
  return(df)
}
datos_modelo<-eliminar_columnas(datos_imputados)


#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR (IMPORTANTE) si hay mas variables categoricas hay que añadirlas
datos_modelo$Share.Class.without.Retrocession<-as.factor(datos_modelo$Share.Class.without.Retrocession)
datos_modelo$Firm.Country<-as.factor(datos_modelo$Firm.Country)
datos_modelo$Domicile<-as.factor(datos_modelo$Domicile)
datos_modelo$Fund.of.Funds<-as.factor(datos_modelo$Fund.of.Funds)
datos_modelo$Investment.Area<-as.factor(datos_modelo$Investment.Area)
datos_modelo$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(datos_modelo$EU.SFDR.Fund.type.Article.8.or.Article.9)
datos_modelo$UCITS<-as.factor(datos_modelo$UCITS)
datos_modelo$Region.of.Sale <-as.factor(datos_modelo$Region.of.Sale)
datos_modelo$Investor.Type..Retail<-as.factor(datos_modelo$Investor.Type..Retail)
datos_modelo$Investor.Type..Professional<-as.factor(datos_modelo$Investor.Type..Professional)
datos_modelo$Investor.Type..Eligible.Counterparty<-as.factor(datos_modelo$Investor.Type..Eligible.Counterparty)
datos_modelo$Institutional<-as.factor(datos_modelo$Institutional)
datos_modelo$Morningstar.Medalist.Rating<-as.factor(datos_modelo$Morningstar.Medalist.Rating)
datos_modelo$Growth.Grade.Long<-as.factor(datos_modelo$Growth.Grade.Long)
datos_modelo$Equity.Style.Box.Long<-as.factor(datos_modelo$Equity.Style.Box.Long)



#ELIMINACION VARIABLES NO DESEADAS
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "Loss.Std.Dev.20210201.to.20240331.Base.Currency"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "Down.Capture.Return.20210401.to.20240331.Base.Currency"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "Excess.Return.20210401.to.20240331.Base.Currency"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "Tracking.Error.3.Yr.MoEnd.Risk.Currency"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "Downside.Capture.Ratio.3.Yr.MoEnd.Risk.Currency"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "Upside.Capture.Ratio.3.Yr.MoEnd.Risk.Currency"]
datos_modelo<- datos_modelo[, !names(datos_modelo) %in% "Alpha.nonexcess.return.20210401.to.20240331.Base.Currency"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "Beta.nonexcess.return.20210401.to.20240331.Base.Currency"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "PRIIPS.KID.Ongoing.Costs.Other.Costs"]


#TRANSFORMO LA VARIABLE TOTAL RET ANNALZ
datos_modelo$Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency<-datos_modelo$Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency+datos_modelo$Management.Fee.Starting





###################################CORRELACIONES###########################################################
columnas_numericas<-na.omit(datos_modelo[,sapply(datos_modelo,is.numeric)])                                                            #nos quedamos unicamente con las variables numericas 
columnas_numericas_sin_constantes<-scale(na.omit(columnas_numericas[,apply(columnas_numericas,2,function(x) length(unique(x))>1)]))   #desechamos las que son constantes
correlacion_rv<-data.frame(cor(columnas_numericas,method = "spearman"))                                                               #dataframe de todas las correlaciones
cor_objetivo<- correlacion_rv["Management.Fee.Starting", -which(names(correlacion_rv) == "Management.Fee.Starting")]
cor_objetivo_long <- gather(cor_objetivo, key = "Variable", value = "Correlacion")


#GRÁFICO DE CORRELACIONES
ggplot(cor_objetivo_long, aes(x = reorder(Variable, -Correlacion), y = Correlacion, fill = NULL)) +
  geom_bar(stat = "identity", fill = "#99CCFF") +  # Todas las barras en azul claro
  geom_text(aes(label = round(Correlacion, 2), y = Correlacion), hjust = -0.2, size = 3, color = "black") + 
  coord_flip() + 
  labs(title = "Correlación con Management Fee Starting", x = "Variable") +
  theme_minimal() +
  theme(legend.position = "none")  


#GUARDAR LAS CORRELACIONES
write.xlsx(correlacion_rv,"correlacion_rv.xlsx")



#CORRELACIONES CON LA VARIABLE ENTRY FEE
correlacion_entryfee<-data.frame(cor(columnas_numericas,method="spearman"))
cor_objetivo<- correlacion_rv["Entry.Fee", -which(names(correlacion_entryfee) == "Entry.Fee")]
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

#ELIMINO LAS VARIABLES NUMERICAS CON ALTA CORRELACIÓN
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% ".of.Holdings.Long"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "Factor.Profile.Style"]
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "Factor.Profile.Size"]



######################################OUTLIERS################################################################
#FUNCION VISUALIZACION OUTLIERS
visualizar_outliers<-function(dataframe){
  num_cols<-sapply(dataframe,is.numeric)
  dataframe_num <- dataframe[, num_cols][1:4] #cada vez que se ejecute una vez, cambiar en los corchetes los valores tal que los siguientes 
                                              #sean [5:8],[9:12],... y asi hasta el numero total de variables numericas
  par(mar=c(4,4,2,1))
  par(mfrow=c(ceiling(sqrt(ncol(dataframe_num))),ceiling(sqrt(ncol(dataframe_num)))))
  for (i in 1:ncol(dataframe_num)) {
    boxplot(dataframe_num[,i],main=names(dataframe_num)[i],col="skyblue",border="black",cex.main = 2.25)
  }
  par(mfrow=c(1,1))
}

quartz(width=15,height = 10)
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

datos_modelo<-eliminar_outliers(datos_modelo,0.01,0.99)  #los umbrales de los percentiles pueden ser otros







#############################MODELOS###################################################################
#DIVIDO LOS DATOS EN ENTRENAMIENTO Y PRUEBA
set.seed(123)
indice_entrenamiento<-sample(seq_len(nrow(datos_modelo)),size=0.8*nrow(datos_modelo)) #establezco un indice
datos_entrenamiento<-na.omit(datos_modelo[indice_entrenamiento, ]) #80%datos
cambiar_nombres<-gsub("[#%]","",colnames(datos_entrenamiento)) #quito hastags y porcentajes de los nombres
colnames(datos_entrenamiento)<-cambiar_nombres

datos_prueba<-na.omit(datos_modelo[-indice_entrenamiento, ]) #20% datos
cambiar_nombres2<-gsub("[#%]","",colnames(datos_prueba)) #quito hastags y porcentajes de los nombres
colnames(datos_prueba)<-cambiar_nombres2



###############################ARBOL###################################################################

#Vectores con diferentes valores para los hiperparametros para que el bucle vaya probando y que encuentre los mejores valores
cp_grid <- expand.grid(cp = seq(0.01, 0.2, by = 0.01)) #Numero que mide la complejidad el arbol
max_depth_values <- c(3, 5, 7, 10) #Altura maxima del arbol

#LISTA ALMACENAMIENTO RESULTADOS
cv_results <- list()

#Bucle donde se entrena el modelo con todos los valores de los hiperparámetros posibles y encuentra los mejores valores
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

#PREDICCIONES
predicciones <- predict(mejor_modelo_arbol, newdata = datos_prueba)

#ECM
ecm_nuevo <- mean((predicciones - datos_prueba$Management.Fee.Starting)^2)
print(ecm_nuevo)




#VISUALIZACION ARBOL
#1ERA FORMA
par(mfrow = c(1, 1), mar = c(8, 0, 12, 0))  # Ajustar los márgenes para hacer espacio para el título
rpart.plot(mejor_modelo_arbol, branch = 1)  # Graficar el árbol

#2DA FORMA
prp(mejor_modelo_arbol, split.col = 2, cex = 0.8)


#COMPARACION PREDICCIONES
resultados <- data.frame(
  Predicciones = predicciones,
  Comision_de_gestion = datos_prueba$Management.Fee.Starting
)

#ESTADISTICAS DEL MODELO DE ARBOL
printcp(mejor_modelo_arbol)

#IMPORTANCIA VARIABLES
mejor_modelo_arbol$variable.importance









#############################RANDOM FOREST##################################################################

#Vectores con diferentes valores para los hiperparametros para que el bucle vaya probando y que encuentre los mejores valores
mtry_values <- c(15,17,19,20,22)  #Numero de variables que coge cada arbol para predecir
num_trees_values <- c(100, 200, 300,400)  #Numero total de arboles en el bosque

# Lista para almacenar los resultados de la validación cruzada
cv_results <- list()

#Bucle donde se entrena el modelo con todos los valores de los hiperparámetros posibles y encuentra los mejores valores
for (mtry in mtry_values) {
  for (num_trees in num_trees_values) {
    
    # Entrenar el modelo con ranger
    modelo_cv <- ranger(
      Management.Fee.Starting ~ ., 
      data = datos_entrenamiento,
      num.trees = num_trees,
      mtry = mtry,
      seed = 42,  # Semilla para reproducibilidad
      importance = "impurity",
      keep.inbag = TRUE
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
  importance = "impurity",
  keep.inbag = TRUE
)

#PREDICCIONES
predicciones_rf <- predict(modelo_rf, data = datos_prueba)$predictions

#ERROR CUADRATICO MEDIO
ecm_rf <- mean((predicciones_rf - datos_prueba$Management.Fee.Starting)^2)
ecm_rf

#COMPARACION PREDICCIONES CON DATOS PRUEBA
resultados <- data.frame(
  Predicciones = predicciones_rf,
  Comision_de_gestion = datos_prueba$Management.Fee.Starting
)



#IMPORTANCIA VARIABLES EN LA CONSTRUCCION DEL MODELO
importancia_rf <- modelo_rf$variable.importance

importancia_df <- data.frame(Variable = names(importancia_rf), Importance = importancia_rf)
importancia_df <- importancia_df[order(importancia_df$Importance, decreasing = TRUE), ] #orden de la importancia de forma decreciente
importancia_df_filtrado <- importancia_df[importancia_df$Importance > 125, ] #umbral de 125

# Crear el gráfico de barras con ggplot2
ggplot(importancia_df_filtrado, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(Importance, 2)), hjust = -0.1, size = 4) +
  coord_flip() +  
  labs(
    title = "Importancia de las Variables",
    x = "Variables",
    y = "Importancia"
  ) +
  theme_minimal()








#####################EJEMPLO DE KUTXABANK: KUTXABANK BOLSA CARTERA FI###################################################
nuevos_datos<-data.frame(Share.Class.without.Retrocession ="Yes",                 
                         Firm.Country="Spain",                                      
                         Domicile="Spain",                                        
                         Number.of.Funds.in.Global.Category..Sustainability=2585,
                         Fund.of.Funds="No",                                    
                         Investment.Area="Spain",                                  
                         EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                         UCITS="Yes",                                           
                         Region.of.Sale="Spain",                                   
                         Investor.Type..Retail="Yes",                            
                         Investor.Type..Professional="Yes",                       
                         Investor.Type..Eligible.Counterparty="Yes",             
                         Institutional="No",                                   
                         Minimum.Investment.Base.Currency=1,                
                         Net.Assets..Share.Class.Base.Currency=77646470,            
                         Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=-17801114, 
                         Asset.Alloc.Bond..Net=0,                           
                         Asset.Alloc.Equity..Net=97.70877,                         
                         Asset.Alloc.Cash..Net=2.29123,                           
                         Asset.Alloc.Other..Net=0,                           
                         .of.Bond.Holdings.Long=0,                            
                         .of.Stock.Holdings.Long=43,                           
                         .of.Other.Holdings.Long=6,                         
                         .Asset.in.Top.10.Holdings=64.29257,                        
                         Morningstar.Medalist.Rating="Neutral",                       
                         PRIIPS.KID.Summary.Risk.Indicator=4,                
                         Growth.Grade.Long="C",  
                         Management.Fee.Starting=NA,
                         PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.2,        
                         Entry.Fee=0,                                     
                         Exit.Fee=0,                                        
                         Performance.Fee.Actual=0,                           
                         Gain.Std.Dev.20210401.to.20240331.Base.Currency=11.5540226,   
                         Std.Dev.3.Yr.MoEnd.Risk.Currency=15.243,                  
                         Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=11.085,         
                         Total.Return.Abs.Rank.Cat.3.Yr.MoEnd=42,              
                         Dividend.Yield.Long=0.037,                            
                         Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=0.647,             
                         Emerging.Market.Stock..Long=0,                      
                         Equity.Style.Small.Cap..Long=7.68926,                      
                         Equity.Style.Mid.Cap..Long=16.47142,                       
                         Equity.Style.Large.Cap..Long=65.74297,                      
                         Equity.Style.Core..Long=26.38574,                      
                         Equity.Style.Value..Long=34.20954,                          
                         Equity.Style.Growth..Long=29.30837,                         
                         Equity.Style.Factor.BV.Growth.Long=6.75896,               
                         Equity.Style.Box.Long="Large Blend",                             
                         ValueGrowth.Score.Long=132.78,                           
                         Equity.Econ.Super.Sector.Cyclical..Long=49.20971,           
                         Equity.Econ.Super.Sector.Defensive..Long=19.84271,         
                         Equity.Econ.Super.Sector.Sensitive..Long=20.87589,         
                         Market.Cap.Giant..Long=18.95532,                          
                         Market.Cap.Micro..Long=2.06988,                           
                         Factor.Profile..Liquidity=50.787275,                         
                         Factor.Profile..Momentum=40.038354,                         
                         Factor.Profile..Quality=86.5123,                           
                         Factor.Profile..Size=42.854067,                              
                         Factor.Profile..Style=62.623109,                             
                         Factor.Profile..Volatility=71.903042,                        
                         Tamaño.de.Gestora=23689035497)

#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Domicile<-as.factor(nuevos_datos$Domicile)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Morningstar.Medalist.Rating<-as.factor(nuevos_datos$Morningstar.Medalist.Rating)
nuevos_datos$Growth.Grade.Long<-as.factor(nuevos_datos$Growth.Grade.Long)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)


#####################EJEMPLO DE KUTXABANK: KUTXABANK BOLSA ESTANDAR FI###################################################
nuevos_datos<-data.frame(Share.Class.without.Retrocession="No",
                         Firm.Country="Spain",                                      
                         Domicile="Spain",                                        
                         Number.of.Funds.in.Global.Category..Sustainability=2585,
                         Fund.of.Funds="No",                                    
                         Investment.Area="Spain",                                  
                         EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",         
                         UCITS="Yes",                                           
                         Region.of.Sale="Spain",                                   
                         Investor.Type..Retail="Yes",                            
                         Investor.Type..Professional="Yes",                       
                         Investor.Type..Eligible.Counterparty="Yes",             
                         Institutional="No",                                   
                         Minimum.Investment.Base.Currency=1,                
                         Net.Assets..Share.Class.Base.Currency=31367190,            
                         Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=-14926857.59, 
                         Asset.Alloc.Bond..Net=0,                           
                         Asset.Alloc.Equity..Net=97.70877,                         
                         Asset.Alloc.Cash..Net=2.29123,                           
                         Asset.Alloc.Other..Net=0,                           
                         .of.Bond.Holdings.Long=0,                            
                         .of.Stock.Holdings.Long=43,                           
                         .of.Other.Holdings.Long=6,                         
                         .Asset.in.Top.10.Holdings=64.29257,                        
                         Morningstar.Medalist.Rating="Neutral",                       
                         PRIIPS.KID.Summary.Risk.Indicator=4,                
                         Growth.Grade.Long="C",  
                         Management.Fee.Starting=NA,
                         PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.2,        
                         Entry.Fee=0,                                     
                         Exit.Fee=0,                                        
                         Performance.Fee.Actual=0,                           
                         Gain.Std.Dev.20210401.to.20240331.Base.Currency=11.3568569760241,   
                         Std.Dev.3.Yr.MoEnd.Risk.Currency=15.231,                  
                         Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=10.98,         
                         Total.Return.Abs.Rank.Cat.3.Yr.MoEnd=67,              
                         Dividend.Yield.Long=0.037,                            
                         Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=0.572,             
                         Emerging.Market.Stock..Long=0,                      
                         Equity.Style.Small.Cap..Long=7.68926,                      
                         Equity.Style.Mid.Cap..Long=16.47142,                       
                         Equity.Style.Large.Cap..Long=65.74297,                      
                         Equity.Style.Core..Long=26.38574,                      
                         Equity.Style.Value..Long=34.20954,                          
                         Equity.Style.Growth..Long=29.30837,                         
                         Equity.Style.Factor.BV.Growth.Long=6.75896,               
                         Equity.Style.Box.Long="Large Blend",                             
                         ValueGrowth.Score.Long=132.78,                           
                         Equity.Econ.Super.Sector.Cyclical..Long=49.20971,           
                         Equity.Econ.Super.Sector.Defensive..Long=19.84271,         
                         Equity.Econ.Super.Sector.Sensitive..Long=20.87589,         
                         Market.Cap.Giant..Long=18.95532,                          
                         Market.Cap.Micro..Long=2.06988,                           
                         Factor.Profile..Liquidity=50.787275,                         
                         Factor.Profile..Momentum=42.038354,                         
                         Factor.Profile..Quality=86.5123,                           
                         Factor.Profile..Size=42.854067,                              
                         Factor.Profile..Style=62.623109,                             
                         Factor.Profile..Volatility=71.903042,                        
                         Tamaño.de.Gestora=23689035497)     
                                                        

#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Domicile<-as.factor(nuevos_datos$Domicile)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale <-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)
nuevos_datos$Morningstar.Medalist.Rating<-as.factor(nuevos_datos$Morningstar.Medalist.Rating)
nuevos_datos$Growth.Grade.Long<-as.factor(nuevos_datos$Growth.Grade.Long)
nuevos_datos$Equity.Style.Box.Long<-as.factor(nuevos_datos$Equity.Style.Box.Long)

#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)





