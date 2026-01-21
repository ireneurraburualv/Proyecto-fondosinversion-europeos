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
df_rf<-read.xlsx("fondos_rf.xlsx",sheet = 1)


##############################################PROCESAMIENTO####################################################################################


#COBERTURA DE DATOS (funcion que crea un dataframe con el nombre de las variables y la cobertura de datos que tiene cada una)
cobertura_funcion<-function(df){
  cobertura_datos<-colMeans(!is.na(df))*100
  cobertura<-data.frame(cobertura_datos)
  return(cobertura_datos)}

cobertura<-data.frame(cobertura_funcion(df_rf))


#FUNCION QUE ELIMINA COLUMNAS CON BAJA COBERTURA DE DATOS (menor de 70%)
eliminar_columnas_baja_cobertura<-function(df,umbral_cobertura){
  cobertura_por_columna<-colMeans(!is.na(df))*100
  columnas_a_mantener<-cobertura_por_columna>=umbral_cobertura
  df_sin_baja_cobertura<-df[,columnas_a_mantener,drop=FALSE]
  return(df_sin_baja_cobertura)
}
datos_modelo<-eliminar_columnas_baja_cobertura(df_rf,70) #aqui se puede poner el umbral deseado

#VARIABLES PARA EL MODELO (son los nombres de las variables seleccionadas para los modelos, si se desean mas, añadir de la misma forma)
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
                    "Morningstar.Sustainability.Rating™",
                    "PRIIPS.KID.Summary.Risk.Indicator",
                    "Management.Fee.Starting",
                    "PRIIPS.KID.Ongoing.Costs.Other.Costs",
                    "PRIIPS.KID.Ongoing.Costs.Transaction.Cost",
                    "Entry.Fee",
                    "Exit.Fee",
                    "Performance.Fee.Actual",
                    "Gain.Std.Dev.20210401.to.20240331.Base.Currency",
                    "Loss.Std.Dev.20210201.to.20240331.Base.Currency",
                    "Std.Dev.3.Yr.MoEnd.Risk.Currency",
                    "Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency",
                    "Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency",
                    "Emerging.Market.Bond..Long",
                    "12.Mo.Yield",
                    "FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI",
                    "Investment.Grade",
                    "High.Yield",
                    "FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI",
                    "Tamaño.de.Gestora"
)

# Filtrar el conjunto de datos con las variables seleccionadas
datos_modelo<- datos_modelo[,variables_modelo]

#RESUMEN ESTADISTICO DE LAS VARIABLES DEL MODELO (resumen para ver minimo,media,maximo,cuartiles... de cada variable)
resumen1<-summary(datos_modelo)


#IMPUTACION VALORES AUSENTES MEDIANTE EL ALGORTIMO KNN (variable a variable)

datos_imputados<-kNN(datos_modelo,variable = "Fund.Size.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Number.of.Funds.in.Global.Category..Sustainability",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Asset.Alloc.Bond..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Entry.Fee",k=3)
datos_imputados<-kNN(datos_imputados,variable = "PRIIPS.KID.Ongoing.Costs.Other.Costs",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Asset.Alloc.Equity..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Asset.Alloc.Cash..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Asset.Alloc.Other..Net",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".of.Bond.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".of.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".of.Stock.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".of.Other.Holdings.Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Exit.Fee",k=3)
datos_imputados<-kNN(datos_imputados,variable = "PRIIPS.KID.Ongoing.Costs.Transaction.Cost",k=3)
datos_imputados<-kNN(datos_imputados,variable = ".Asset.in.Top.10.Holdings",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Investment.Grade",k=3)
datos_imputados<-kNN(datos_imputados,variable = "High.Yield",k=3)
datos_imputados<-kNN(datos_imputados,variable = "FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI",k=3)
datos_imputados<-kNN(datos_imputados,variable = "FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Std.Dev.3.Yr.MoEnd.Risk.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Loss.Std.Dev.20210201.to.20240331.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Gain.Std.Dev.20210401.to.20240331.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Minimum.Investment.Base.Currency",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Management.Fee.Starting",k=3)
datos_imputados<-kNN(datos_imputados,variable = "PRIIPS.KID.Summary.Risk.Indicator",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Emerging.Market.Bond..Long",k=3)
datos_imputados<-kNN(datos_imputados,variable = "12.Mo.Yield",k=3)
datos_imputados<-kNN(datos_imputados,variable = "Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency",k=3)


write.xlsx(datos_imputados,"datos_imputados_rf.xlsx") #guardar la nueva base de datos (IMPORTANTE)

#CARGO LOS NUEVOS DATOS
datos_imputados<-read.xlsx("datos_imputados_rf.xlsx",sheet = 1)


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

#TRANSFORMO LAS VARIABLES CATEGORICAS EN FACTOR (IMPORTANTE) si hay mas variables categoricas hay que añadirlas

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



#ELIMINO ESTA VARIABLE POR ESTAR MUY RELACIONADA CON LA VARIABLE OBJETIVO
datos_modelo <- datos_modelo[, !names(datos_modelo) %in% "PRIIPS.KID.Ongoing.Costs.Other.Costs"]

#TRANSFORMO LA VARIABLE TOTAL RET ANNALZ
datos_modelo$Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency<-datos_modelo$Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency+datos_modelo$Management.Fee.Starting




###################################CORRELACIONES###########################################################
columnas_numericas<-na.omit(datos_modelo[,sapply(datos_modelo,is.numeric)])                                                         #nos quedamos unicamente con las variables numericas 
columnas_numericas_sin_constantes<-scale(na.omit(columnas_numericas[,apply(columnas_numericas,2,function(x) length(unique(x))>1)])) #desechamos las que son constantes
correlacion_rf<-data.frame(cor(columnas_numericas,method="spearman"))                                                               #dataframe de todas las correlaciones
cor_objetivo<- correlacion_rf["Management.Fee.Starting", -which(names(correlacion_rf) == "Management.Fee.Starting")]
cor_objetivo_long <- gather(cor_objetivo, key = "Variable", value = "Correlacion")

#VISUALIZACION DE LAS CORRELACIONES CON LA VARIABLE OBJETIVO
ggplot(cor_objetivo_long, aes(x = reorder(Variable, -Correlacion), y = Correlacion, fill = NULL)) +
  geom_bar(stat = "identity", fill = "#99CCFF") +  
  geom_text(aes(label = round(Correlacion, 2), y = Correlacion), hjust = -0.2, size = 3.5, color = "black") + 
  coord_flip() + 
  labs(title = "Correlación con Management Fee Starting", x = "Variable") +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.y = element_text(size = 12), 
    plot.title = element_text(size = 14)  
  )


write.xlsx(correlacion_rf,"correlacion_rf.xlsx") #guardar las correlaciones


#CORRELACION DE LA VARIABLE ENTRY FEE
correlacion_entryfee<-data.frame(cor(columnas_numericas,method="spearman"))
cor_objetivo<- correlacion_rf["Entry.Fee", -which(names(correlacion_entryfee) == "Entry.Fee")]
cor_objetivo_long <- gather(cor_objetivo, key = "Variable", value = "Correlacion")

#VISUALIZACION CORRELACION CON VARIABLE ENTRY FEE
ggplot(cor_objetivo_long, aes(x = reorder(Variable, -Correlacion), y = Correlacion, fill = NULL)) +
  geom_bar(stat = "identity", fill = "#99CCFF") +  
  geom_text(aes(label = round(Correlacion, 2), y = Correlacion), hjust = -0.2, size = 3.5, color = "black") + 
  coord_flip() + 
  labs(title = "Correlación con Entry Fee", x = "Variable") +
  theme_minimal() +
  theme(
    legend.position = "none",  
    axis.text.y = element_text(size = 12),  
    plot.title = element_text(size = 14)  
  )








##############################OUTLIERS#################################################################

#FUNCION VISUALIZACION OUTLIERS MEDIANTE DIAGRAMAS DE CAJAS
visualizar_outliers <- function(dataframe) {
  num_cols <- sapply(dataframe, is.numeric)
  dataframe_num <- dataframe[, num_cols][1:4] #cada vez que se ejecute una vez cambiar en los corchetes los valores tal que los siguientes 
                                              #sean [5:8],[9:12],... y asi hasta el numero total de variables numericas
  
  par(mar = c(4, 4, 2, 1))
  par(mfrow = c(ceiling(sqrt(ncol(dataframe_num))), ceiling(sqrt(ncol(dataframe_num)))))
  
  for (i in 1:ncol(dataframe_num)) {
    boxplot(dataframe_num[, i], main = names(dataframe_num)[i], col = "skyblue", border = "black", cex.main = 2.25)
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

datos_modelo<-eliminar_outliers(datos_modelo,0.01,0.99) #los umbrales de los percentiles pueden ser otros



#############################MODELOS###################################################################

#DIVIDO LOS DATOS EN ENTRENAMIENTO Y PRUEBA
set.seed(123)
indice_entrenamiento<-sample(seq_len(nrow(datos_modelo)),size=0.8*nrow(datos_modelo)) #indice que me establece el numero de datos
datos_entrenamiento<-na.omit(datos_modelo[indice_entrenamiento, ]) #80%datos
datos_prueba<-na.omit(datos_modelo[-indice_entrenamiento, ]) #20% datos

#ELEIMINAR PORCENTAJES Y HAGSTAGS DE LOS NOMBRES
cambiar_nombres<-gsub("[#%]","",colnames(datos_entrenamiento)) #quito hastags y porcentajes de los nombres
colnames(datos_entrenamiento)<-cambiar_nombres
cambiar_nombres2<-gsub("[#%]","",colnames(datos_prueba)) #quito hastags y porcentajes de los nombres
colnames(datos_prueba)<-cambiar_nombres2



############################ARBOL####################################################################################

#Vectores con diferentes valores para los hiperparametros para que el bucle vaya probando y que encuentre los mejores valores
cp_grid <- expand.grid(cp = seq(0.01, 0.2, by = 0.01)) #Numero que mide la complejidad el arbol
max_depth_values <- c(3, 5, 7, 10) #Altura maxima del arbol

# Lista para almacenar los resultados de la validación cruzada
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

#Encontrar la fila que contiene el menor valor de ecm
fila_menor_ecm <- cv_results_df[which.min(cv_results_df$ecm), ]

#Extraer el mejor valor de cp y de max_depth
best_cp <- fila_menor_ecm$cp
best_max_depth <- fila_menor_ecm$max_depth

#Reentrenar el modelo con los mejores hiperparámetros
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

# Imprimir el ECM
print(ecm_nuevo)





#Visualizamos el arbol
#1ERA FORMA
par(mfrow = c(1, 1), mar = c(8, 0, 12, 0))  # Ajustar los márgenes para hacer espacio para el título
rpart.plot(mejor_modelo_arbol, branch = 1)  # Graficar el árbol

#2DA FORMA
prp(mejor_modelo_arbol, split.col = 2, cex = 0.8)


#Comparamos las predicciones con el dato real
resultados <- data.frame(
  Predicciones = predicciones,
  Comision_de_gestion = datos_prueba$Management.Fee.Starting
)

#Estadisticas del arbol
printcp(mejor_modelo_arbol)





#############################RANDOM FOREST####################################################################################################

#Vectores con diferentes valores para los hiperparametros para que el bucle vaya probando y que encuentre los mejores valores
mtry_values <- c(12,13,14,15)  #Numero de variables que coge cada arbol para predecir
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



#COMPARACION PREDICCIONES CON DATOS PRUEBA
resultados <- data.frame(
  Predicciones = predicciones_rf,
  Comision_de_gestion = datos_prueba$Management.Fee.Starting
)


#IMPORTANCIA VARIABLES EN LA CONSTRUCCION DEL BOSQUE
importancia_rf <- modelo_rf$variable.importance

importancia_df <- data.frame(Variable = names(importancia_rf), Importance = importancia_rf)
importancia_df <- importancia_df[order(importancia_df$Importance, decreasing = TRUE), ] #Ordenar los valores de forma decreciente
importancia_df_filtrado <- importancia_df[importancia_df$Importance > 200, ] #Se filtra por un umbral(200)

#Este grafico nom muestra valores con la importancia que tienen las variables en la construccion del bosque
ggplot(importancia_df_filtrado, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(Importance, 2)), hjust = -0.1, size = 3.5) +
  coord_flip() +  
  labs(
    title = "Importancia de las Variables",
    x = "Variables",
    y = "Importancia"
  ) +
  theme_minimal()






#######################EJEMPLO KUTXABANK BONO CARTERA FI##################################
nuevos_datos<-data.frame(Share.Class.without.Retrocession="Yes",
                         Number.of.Funds.in.Global.Category..Sustainability=2474,
                         Fund.of.Funds="Yes",
                         Investment.Area="Euroland",
                         EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",
                         UCITS="Yes",
                         Region.of.Sale="Spain",
                         Investor.Type..Retail="Yes",
                         Investor.Type..Professional="Yes",
                         Investor.Type..Eligible.Counterparty="Yes",
                         Institutional="No",
                         Net.Assets..Share.Class.Base.Currency=1336667634,
                         Fund.Size.Base.Currency=1793342214,
                         .of.Stock.Holdings.Long=0,
                         Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=137333922,
                         .Asset.in.Top.10.Holdings=27.83135,
                         Performance.Fee.Actual=0,
                         Loss.Std.Dev.20210201.to.20240331.Base.Currency=1.47490249,
                         Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=0.15442,
                         FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=9.17694,
                         PRIIPS.KID.Summary.Risk.Indicator=2,
                         PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,
                         Exit.Fee=0,
                         Std.Dev.3.Yr.MoEnd.Risk.Currency=2.194,
                         Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0,501,
                         FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=3.21963034,
                         Gain.Std.Dev.20210401.to.20240331.Base.Currency=1.31262773,
                         Asset.Alloc.Bond..Net=81.00713,
                         Asset.Alloc.Equity..Net=0,
                         Asset.Alloc.Cash..Net=18.93855,
                         Asset.Alloc.Other..Net=0.05432,
                         .of.Bond.Holdings.Long=113,
                         .of.Holdings.Long=122,
                         .of.Other.Holdings.Long=9,
                         Emerging.Market.Bond..Long=1.24453,
                         Firm.Country="Spain",
                         Minimum.Investment.Base.Currency=1,
                         Investment.Grade=90.81875,
                         High.Yield=0.00431,
                         Entry.Fee=0,
                         Tamaño.de.Gestora=23689035497)


#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR

nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale<-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)


#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)


#######################EJEMPLO KUTXABANK BONO ESTANDAR FI##################################
nuevos_datos<-data.frame(Share.Class.without.Retrocession="No",
                         Number.of.Funds.in.Global.Category..Sustainability=2474,
                         Fund.of.Funds="No",
                         Investment.Area="Euroland",
                         EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",
                         UCITS="Yes",
                         Region.of.Sale="Spain",
                         Investor.Type..Retail="Yes",
                         Investor.Type..Professional="Yes",
                         Investor.Type..Eligible.Counterparty="Yes",
                         Institutional="No",
                         Net.Assets..Share.Class.Base.Currency=456674580,
                         Fund.Size.Base.Currency=1793342214,
                         .of.Stock.Holdings.Long=0,
                         Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=198002311.357,
                         .Asset.in.Top.10.Holdings=27.83135,
                         Performance.Fee.Actual=0,
                         Loss.Std.Dev.20210201.to.20240331.Base.Currency=1.47387097811101,
                         Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=-1.28295,
                         FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=9.17694,
                         PRIIPS.KID.Summary.Risk.Indicator=2,
                         PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,
                         Exit.Fee=0,
                         Std.Dev.3.Yr.MoEnd.Risk.Currency=2.193,
                         Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0.77,
                         FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=3.21963033808481,
                         Gain.Std.Dev.20210401.to.20240331.Base.Currency=1.31410172090493,
                         Asset.Alloc.Bond..Net=81.00713,
                         Asset.Alloc.Equity..Net=0,
                         Asset.Alloc.Cash..Net=18.93855,
                         Asset.Alloc.Other..Net=0.05432,
                         .of.Bond.Holdings.Long=113,
                         .of.Holdings.Long=122,
                         .of.Other.Holdings.Long=9,
                         Emerging.Market.Bond..Long=1.24453,
                         Firm.Country="Spain",
                         Minimum.Investment.Base.Currency=1,
                         Investment.Grade=90.81875,
                         High.Yield=0.00431,
                         Entry.Fee=0,
                         Tamaño.de.Gestora=23689035497)


#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale<-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)


#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)



#######################EJEMPLO KUTXABANK RENTA FIJA LP CARTERA FI##################################
nuevos_datos<-data.frame(Share.Class.without.Retrocession="Yes",
                         Number.of.Funds.in.Global.Category..Sustainability=2474,
                         Fund.of.Funds="No",
                         Investment.Area="Euroland",
                         Firm.Country="Spain",
                         EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",
                         UCITS="Yes",
                         Region.of.Sale="Spain",
                         Investor.Type..Retail="Yes",
                         Investor.Type..Professional="Yes",
                         Investor.Type..Eligible.Counterparty="Yes",
                         Institutional="No",
                         Minimum.Investment.Base.Currency=1,
                         Net.Assets..Share.Class.Base.Currency=1625056248,
                         Fund.Size.Base.Currency=1670081760,
                         Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=551974537.006,
                         Asset.Alloc.Bond..Net=96.789,
                         Asset.Alloc.Equity..Net=0,
                         Asset.Alloc.Cash..Net=-15,62072,
                         Asset.Alloc.Other..Net=18,83172,
                         .of.Bond.Holdings.Long=257,
                         .of.Holdings.Long=279,
                         .of.Other.Holdings.Long=0,
                         .of.Stock.Holdings.Long=22,
                         .Asset.in.Top.10.Holdings=41.09928,
                         Performance.Fee.Actual=0,
                         PRIIPS.KID.Summary.Risk.Indicator=2,
                         Entry.Fee=0,
                         Exit.Fee=0,
                         Loss.Std.Dev.20210201.to.20240331.Base.Currency=3.18063002566473,
                         Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=-0.57862,
                         FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=9.12647,
                         PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,
                         Std.Dev.3.Yr.MoEnd.Risk.Currency=4.732,
                         Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0.486,
                         FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=4.72145,
                         Gain.Std.Dev.20210401.to.20240331.Base.Currency=3.18592509932035,
                         Emerging.Market.Bond..Long=2.70994,
                         Investment.Grade=82.52434,
                         High.Yield=8.34919,
                         Tamaño.de.Gestora=23689035497)


#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale<-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)


#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)



#######################EJEMPLO KUTXABANK RENTA FIJA LP ESTANDAR FI##################################
nuevos_datos<-data.frame(Share.Class.without.Retrocession="No",
                         Number.of.Funds.in.Global.Category..Sustainability=2474,
                         Fund.of.Funds="No",
                         Investment.Area="Euroland",
                         Firm.Country="Spain",
                         EU.SFDR.Fund.type.Article.8.or.Article.9="Article 8",
                         UCITS="Yes",
                         Region.of.Sale="Spain",
                         Investor.Type..Retail="Yes",
                         Investor.Type..Professional="Yes",
                         Investor.Type..Eligible.Counterparty="Yes",
                         Institutional="No",
                         Minimum.Investment.Base.Currency=1,
                         Net.Assets..Share.Class.Base.Currency=45025512,
                         Fund.Size.Base.Currency=1670081760,
                         Est.Share.Class.Net.Flow.3.Yr.MoEnd.Base.Currency=-6113048.314,
                         Asset.Alloc.Bond..Net=96.789,
                         Asset.Alloc.Equity..Net=0,
                         Asset.Alloc.Cash..Net=-15,62072,
                         Asset.Alloc.Other..Net=18,83172,
                         .of.Bond.Holdings.Long=257,
                         .of.Holdings.Long=279,
                         .of.Other.Holdings.Long=0,
                         .of.Stock.Holdings.Long=22,
                         .Asset.in.Top.10.Holdings=41.09928,
                         Performance.Fee.Actual=0,
                         PRIIPS.KID.Summary.Risk.Indicator=2,
                         Entry.Fee=0,
                         Exit.Fee=0,
                         Loss.Std.Dev.20210201.to.20240331.Base.Currency=3.1777028536875,
                         Total.Ret.Annlzd.3.Yr.MoEnd.Base.Currency=-0.56,
                         FixdInc.Credit.Rtg..Brkdwn.NR.Calc.Net.FI=9.12647,
                         PRIIPS.KID.Ongoing.Costs.Transaction.Cost=0.1,
                         Std.Dev.3.Yr.MoEnd.Risk.Currency=4.732,
                         Sharpe.Ratio.3.Yr.MoEnd.Risk.Currency=-0.67,
                         FixdInc.Eff.Dur..Avg.yrs.Calc.Net.FI=4.72145,
                         Gain.Std.Dev.20210401.to.20240331.Base.Currency=3.18592509932035,
                         Emerging.Market.Bond..Long=2.70994,
                         Investment.Grade=82.52434,
                         High.Yield=8.34919,
                         Tamaño.de.Gestora=23689035497)

#TRANSFORMO LAS VARIABLES CATEGÓRICAS EN FACTOR
nuevos_datos$Share.Class.without.Retrocession<-as.factor(nuevos_datos$Share.Class.without.Retrocession)
nuevos_datos$Firm.Country<-as.factor(nuevos_datos$Firm.Country)
nuevos_datos$Fund.of.Funds<-as.factor(nuevos_datos$Fund.of.Funds)
nuevos_datos$Investment.Area<-as.factor(nuevos_datos$Investment.Area)
nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9<-as.factor(nuevos_datos$EU.SFDR.Fund.type.Article.8.or.Article.9)
nuevos_datos$UCITS<-as.factor(nuevos_datos$UCITS)
nuevos_datos$Region.of.Sale<-as.factor(nuevos_datos$Region.of.Sale)
nuevos_datos$Investor.Type..Retail<-as.factor(nuevos_datos$Investor.Type..Retail)
nuevos_datos$Investor.Type..Professional<-as.factor(nuevos_datos$Investor.Type..Professional)
nuevos_datos$Investor.Type..Eligible.Counterparty<-as.factor(nuevos_datos$Investor.Type..Eligible.Counterparty)
nuevos_datos$Institutional<-as.factor(nuevos_datos$Institutional)


#PREDICCION
predict(modelo_rf,nuevos_datos)$predictions
predict(mejor_modelo_arbol,nuevos_datos)







