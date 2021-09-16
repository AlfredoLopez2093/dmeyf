#Ensemble de arboles de decision

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("D:/Cursos/Maestria en Big Data/MBD 2021/DM-Economia-Finanzas")

#cargo los datos donde entreno
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")

#cargo los datos donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")


#Establezco cuales son los campos que puedo usar para la prediccion
#campos_buenos  <- setdiff(  colnames(dtrain) ,  c("clase_ternaria") )
campos_buenos  <- setdiff(  colnames(dtrain) ,  c("clase_ternaria","internet","mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos","matm_other","tmobile_app","cmobile_app_trx","Master_Finiciomora","Master_madelantopesos","Master_madelantodolares","Visa_Finiciomora","Visa_mpagado") ) #azarosos008


#parametros  <-  list( "cp"=-1, "minsplit"=900,  "minbucket"=440, "maxdepth"=5 )
#parametros  <-  list( "cp"=-0.803613435182936, "minsplit"=1943,  "minbucket"=452, "maxdepth"=20 ) #azarosos005
#parametros  <-  list( "cp"=-0.0975526127480573, "minsplit"=2032,  "minbucket"=473, "maxdepth"=11 ) #azarosos006
parametros  <-  list( "cp"=-0.803082571775553, "minsplit"=1050,  "minbucket"=471, "maxdepth"=18 ) #azarosos007

num_trees         <-  200    #voy a generar 10 arboles
feature_fraction  <-   0.5  #entreno cada arbol con solo 50% de las variables variables

set.seed(946669) #Establezco la semilla aleatoria

#inicializo en CERO el vector de las probabilidades en dapply
#Aqui es donde voy acumulando, sumando, las probabilidades
probabilidad_ensemble  <- rep( 0, nrow(dapply) )

for(  i in  1:num_trees ) #genero  num_trees arboles
{
  qty_campos_a_utilizar  <- as.integer( length(campos_buenos)* feature_fraction )
  campos_random  <- sample( campos_buenos, qty_campos_a_utilizar )
  
  #paso de un vector a un string con los elementos separados por un signo de "+"
  #este hace falta para la formula
  campos_random  <- paste( campos_random, collapse=" + ")

  #genero el modelo
  formulita  <- paste0( "clase_ternaria ~ ", campos_random )

  modelo  <- rpart( formulita,
                    data= dtrain,
                    xval= 0,
                    control= parametros )

  #aplico el modelo a los datos que no tienen clase
  prediccion  <- predict( modelo, dapply , type = "prob")

  #voy acumulando la probabilidad
  probabilidad_ensemble  <- probabilidad_ensemble +  prediccion[, "BAJA+2"]
}

#fue sumando las probabilidades, ahora hago el cociente por la cantidad de arboles
#o sea, calculo el promedio
probabilidad_ensemble  <- probabilidad_ensemble / num_trees

#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(probabilidad_ensemble > 0.025) ) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file="./kaggle/arboles_azarosos_010.csv", 
        sep="," )
