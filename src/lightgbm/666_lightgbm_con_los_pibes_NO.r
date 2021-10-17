#Este LightGBM fue construido  para destronar a quienes desde el inicio utilizaron XGBoost y  LightGBM
#mientras sus compañeros luchaban por correr un rpart

#Con los pibes NO

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

setwd("D:/Cursos/Maestria en Big Data/MBD 2021/DM-Economia-Finanzas")

#cargo el dataset
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#Quito el Data Drifting de  "ccajas_transacciones"  "Master_mpagominimo"
#campos_buenos  <- setdiff( colnames(dataset),
#                           c("clase_ternaria", "clase01", "ccajas_transacciones", "Master_mpagominimo" ) )

#Quito el Data Drifting de  "internet"  "tmobile_app" "cmobile_app_trx" "Master_Finiciomora" "Master_madelantodolares" "Visa_mpagado"
campos_buenos  <- setdiff( colnames(dataset),
                           c("clase_ternaria", "clase01", "internet", "tmobile_app",
                             "cmobile_app_trx","Master_Finiciomora","Master_madelantodolares","Visa_mpagado") )


#genero el formato requerido por LightGBM
dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset[ , clase01]
                      )

#Solo uso DOS hiperparametros,  max_bin  y min_data_in_leaf
#Dadme un punto de apoyo y movere el mundo, Arquimedes
modelo  <- lightgbm( data= dtrain,
                     params= list( objective= "binary",
                                   max_bin= 15,
                                   min_data_in_leaf= 4000,
                                   learning_rate= 0.049,
                                   num_iterations = 100)  )


#cargo el dataset donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

#calculo la importancia de variables
kscript           <- "666_lightgbm2"
kimp        <- paste0("./work/E", 1000, "_", kscript, "_" )
tb_importancia  <- lgb.importance( model= modelo )
fwrite( tb_importancia, 
        file= paste0(kimp, "imp_", "lightgbm2", ".txt"),
        sep="\t" )


#aplico el modelo a los datos nuevos, dapply
prediccion  <- predict( modelo,  data.matrix( dapply[  , campos_buenos, with=FALSE]))

#la probabilidad de corte ya no es 0.025,  sino que 0.031
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > 0.031) ) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/lightgbm_con_los_pibes_NO_7v3.csv",
        sep=  "," )




