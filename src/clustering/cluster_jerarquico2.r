##Prueba considerando dataset original y las 20 vbles mas importantes

require("data.table")
require("randomForest")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasetsOri/paquete_premium.csv.gz", stringsAsFactors= TRUE)
gc()

#achico el dataset
dataset[  ,  azar := runif( nrow(dataset) ) ]
dataset  <-  dataset[  clase_ternaria =="BAJA+1"  & foto_mes>=202001  & foto_mes<=202011, ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )
gc()


#campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
#                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
#                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
#                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
#                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
#                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
#                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
#                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
#                     "mpagomiscuentas")

campos_buenos  <- c( "Visa_status", "ctrx_quarter", "mdescubierto_preacordado", "mcuentas_saldo", "mcuenta_corriente",
                     "Visa_Finiciomora", "mprestamos_personales", "mrentabilidad", "cpayroll_trx", "mcaja_ahorro",
                     "Visa_msaldototal", "mactivos_margen", "mpasivos_margen", "mpayroll", "Visa_fultimo_cierre",
                     "Visa_mpagado", "ctarjeta_visa_transacciones", "mrentabilidad_annual", "Visa_fechaalta", "ctarjeta_master")



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[ , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox = TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )


pdf( paste0( paste0("./work/cluster_jerarquico.pdf" ) ))
plot( hclust.rf )
dev.off()


h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#ahora a mano veo las variables
#dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
#dataset[  , mean(cpayroll_trx),  cluster2 ]
#dataset[  , mean(mcaja_ahorro),  cluster2 ]
#dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
#dataset[  , mean(ctarjeta_visa_transacciones),  cluster2 ]
#dataset[  , mean(mcuentas_saldo),  cluster2 ]
#dataset[  , mean(mrentabilidad_annual),  cluster2 ]
#dataset[  , mean(mprestamos_personales),  cluster2 ]
#dataset[  , mean(mactivos_margen),  cluster2 ]
#dataset[  , mean(mpayroll),  cluster2 ]
#dataset[  , mean(Visa_mpagominimo),  cluster2 ]
#dataset[  , mean(Master_fechaalta),  cluster2 ]
#dataset[  , mean(cliente_edad),  cluster2 ]
#dataset[  , mean(chomebanking_transacciones),  cluster2 ]
#dataset[  , mean(Visa_msaldopesos),  cluster2 ]
#dataset[  , mean(Visa_Fvencimiento),  cluster2 ]
#dataset[  , mean(mrentabilidad),  cluster2 ]
#dataset[  , mean(Visa_msaldototal),  cluster2 ]
#dataset[  , mean(Master_Fvencimiento),  cluster2 ]
#dataset[  , mean(mcuenta_corriente),  cluster2 ]
#dataset[  , mean(Visa_mpagospesos),  cluster2 ]
#dataset[  , mean(Visa_fechaalta),  cluster2 ]
#dataset[  , mean(mcomisiones_mantenimiento),  cluster2 ]
#dataset[  , mean(Visa_mfinanciacion_limite),  cluster2 ]

#cambio la etiqueta del cluster para que lo tome como etiqueta discreta
dataset[ , clasif:= paste("c",cluster2,sep = "") ]

#cargo librerias para analizar clusters
library(magrittr)
library(datos)
library(tidyverse)
library(GGally)
library(RColorBrewer)

res = dataset %>% group_by(etiq) %>% summarise (mean(Visa_status),
                                                mean(ctrx_quarter),
                                                mean(mdescubierto_preacordado),
                                                mean(mcuentas_saldo),
                                                mean(mcuenta_corriente),
                                                mean(Visa_Finiciomora),
                                                mean(mprestamos_personales),
                                                mean(mrentabilidad),
                                                mean(cpayroll_trx),
                                                mean(mcaja_ahorro),
                                                mean(Visa_msaldototal),
                                                mean(mactivos_margen),
                                                mean(mpasivos_margen),
                                                mean(mpayroll),
                                                mean(Visa_fultimo_cierre),
                                                mean(Visa_mpagado),
                                                mean(ctarjeta_visa_transacciones),
                                                mean(mrentabilidad_annual),
                                                mean(Visa_fechaalta),
                                                mean(ctarjeta_master))

View(res)
ggparcoord(data = res,
           columns = 2:21,
           groupColumn = "clasif")