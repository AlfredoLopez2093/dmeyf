#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("D:/Cursos/Maestria en Big Data/MBD 2021/DM-Economia-Finanzas")  #Establezco el Working Directory

#cargo los datos donde entreno
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")

#cargar aqui los parametros
#parametros  <-  list( "cp"=-1, "minsplit"=750,  "minbucket"=375, "maxdepth"=5 )
#parametros  <-  list( "cp"=0, "minsplit"=50,  "minbucket"=5, "maxdepth"=14 )
parametros  <-  list( "cp"=-0.803613435182936, "minsplit"=1943,  "minbucket"=452, "maxdepth"=20 )

modelo  <- rpart("clase_ternaria ~ .",
                 data= dtrain,
                 xval= 0,
                 control= parametros )

#cargo los datos donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

prediccion  <- predict( modelo, dapply , type= "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

entrega  <- dapply[  , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
#fwrite( entrega, 
#        file= paste0( "./kaggle/arbol_aplicado.csv"), 
#        sep= "," )

fwrite( entrega, 
        file= paste0( "./kaggle/arbol_aplicado_mejor_param160921_0115.csv"), 
        sep= "," )
