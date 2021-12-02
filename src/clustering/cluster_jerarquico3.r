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
dataset  <-  dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202001  & foto_mes<=202011, ]
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


pdf( paste0( paste0("./work/cluster_jerarquico3.pdf" ) ))
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

res = dataset %>% group_by(clasif) %>% summarise (mean(Visa_status),
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
           groupColumn = "clasif",
           scale = "uniminmax")

library(fmsb)
data_for_radar <- as.data.frame(res)
rownames(data_for_radar) <- data_for_radar$clasif
data_for_radar <- data_for_radar[, -c(1)]
data_for_radar

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


data_for_radar['mean(Visa_status)']=normalize(data_for_radar['mean(Visa_status)'])
data_for_radar['mean(ctrx_quarter)']=normalize(data_for_radar['mean(ctrx_quarter)'])
data_for_radar['mean(mdescubierto_preacordado)']=normalize(data_for_radar['mean(mdescubierto_preacordado)'])
data_for_radar['mean(mcuentas_saldo)']=normalize(data_for_radar['mean(mcuentas_saldo)'])
data_for_radar['mean(mcuenta_corriente)']=normalize(data_for_radar['mean(mcuenta_corriente)'])
data_for_radar['mean(Visa_Finiciomora)']=normalize(data_for_radar['mean(Visa_Finiciomora)'])
data_for_radar['mean(mprestamos_personales)']=normalize(data_for_radar['mean(mprestamos_personales)'])
data_for_radar['mean(mrentabilidad)']=normalize(data_for_radar['mean(mrentabilidad)'])
data_for_radar['mean(cpayroll_trx)']=normalize(data_for_radar['mean(cpayroll_trx)'])
data_for_radar['mean(mcaja_ahorro)']=normalize(data_for_radar['mean(mcaja_ahorro)'])
data_for_radar['mean(Visa_msaldototal)']=normalize(data_for_radar['mean(Visa_msaldototal)'])
data_for_radar['mean(mactivos_margen)']=normalize(data_for_radar['mean(mactivos_margen)'])
data_for_radar['mean(mpasivos_margen)']=normalize(data_for_radar['mean(mpasivos_margen)'])
data_for_radar['mean(mpayroll)']=normalize(data_for_radar['mean(mpayroll)'])
data_for_radar['mean(Visa_fultimo_cierre)']=normalize(data_for_radar['mean(Visa_fultimo_cierre)'])
data_for_radar['mean(Visa_mpagado)']=normalize(data_for_radar['mean(Visa_mpagado)'])
data_for_radar['mean(ctarjeta_visa_transacciones)']=normalize(data_for_radar['mean(ctarjeta_visa_transacciones)'])
data_for_radar['mean(mrentabilidad_annual)']=normalize(data_for_radar['mean(mrentabilidad_annual)'])
data_for_radar['mean(Visa_fechaalta)']=normalize(data_for_radar['mean(Visa_fechaalta)'])
data_for_radar['mean(ctarjeta_master)']=normalize(data_for_radar['mean(ctarjeta_master)'])

#Renombro columnas
names(data_for_radar)[names(data_for_radar) =='mean(Visa_status)'] ='Visa_status'
names(data_for_radar)[names(data_for_radar) =='mean(ctrx_quarter)'] ='ctrx_quarter'
names(data_for_radar)[names(data_for_radar) =='mean(mdescubierto_preacordado)'] ='mdescubierto_preacordado'
names(data_for_radar)[names(data_for_radar) =='mean(mcuentas_saldo)'] ='mcuentas_saldo'
names(data_for_radar)[names(data_for_radar) =='mean(mcuenta_corriente)'] ='mcuenta_corriente'
names(data_for_radar)[names(data_for_radar) =='mean(Visa_Finiciomora)'] ='Visa_Finiciomora'
names(data_for_radar)[names(data_for_radar) =='mean(mprestamos_personales)'] ='mprestamos_personales'
names(data_for_radar)[names(data_for_radar) =='mean(mrentabilidad)'] ='mrentabilidad'
names(data_for_radar)[names(data_for_radar) =='mean(cpayroll_trx)'] ='cpayroll_trx'
names(data_for_radar)[names(data_for_radar) =='mean(mcaja_ahorro)'] ='mcaja_ahorro'
names(data_for_radar)[names(data_for_radar) =='mean(Visa_msaldototal)'] ='Visa_msaldototal'
names(data_for_radar)[names(data_for_radar) =='mean(mes)'] ='mes'
names(data_for_radar)[names(data_for_radar) =='mean(mactivos_margen)'] ='mactivos_margen'
names(data_for_radar)[names(data_for_radar) =='mean(foto_mes)'] ='foto_mes'
names(data_for_radar)[names(data_for_radar) =='mean(mpasivos_margen)'] ='mpasivos_margen'
names(data_for_radar)[names(data_for_radar) =='mean(mpayroll)'] ='mpayroll'
names(data_for_radar)[names(data_for_radar) =='mean(Visa_fultimo_cierre)'] ='Visa_fultimo_cierre'
names(data_for_radar)[names(data_for_radar) =='mean(Visa_mpagado)'] ='Visa_mpagado'
names(data_for_radar)[names(data_for_radar) =='mean(ctarjeta_visa_transacciones)'] ='ctarjeta_visa_transacciones'
names(data_for_radar)[names(data_for_radar) =='mean(mrentabilidad_annual)'] ='mrentabilidad_annual'
names(data_for_radar)[names(data_for_radar) =='mean(Visa_fechaalta)'] ='Visa_fechaalta'
names(data_for_radar)[names(data_for_radar) =='mean(ctarjeta_master)'] ='ctarjeta_master'

data_for_radar


#Funcion para armar el radar
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

#Agrego los valores min y max para cada variable
data_for_radar <- rbind(rep(max(data_for_radar), 20), rep(0, 20), data_for_radar)
data_for_radar

#Grafico los radares

cluster1 <- data_for_radar[c(1, 2, "c1"),]
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(cluster1,caxislabels = c(0, 0.25,0.5,0.75,1),title="cluster1")

cluster2 <- data_for_radar[c(1, 2, "c2"),]
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(cluster2,caxislabels = c(0, 0.25,0.5,0.75,1),title="cluster2")

cluster3 <- data_for_radar[c(1, 2, "c3"),]
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(cluster3,caxislabels = c(0, 0.25,0.5,0.75,1),title="cluster3")

cluster4 <- data_for_radar[c(1, 2, "c4"),]
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(cluster4,caxislabels = c(0, 0.25,0.5,0.75,1),title="cluster4")

cluster5 <- data_for_radar[c(1, 2, "c5"),]
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(cluster5,caxislabels = c(0, 0.25,0.5,0.75,1),title="cluster5")

cluster6 <- data_for_radar[c(1, 2, "c6"),]
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(cluster6,caxislabels = c(0, 0.25,0.5,0.75,1),title="cluster6")

cluster7 <- data_for_radar[c(1, 2, "c7"),]
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(cluster7,caxislabels = c(0, 0.25,0.5,0.75,1),title="cluster7")

#Grafico los radares en dos graficos
titles <- c("cluster1", "cluster2", "cluster3", "cluster4", "cluster5", "cluster6", "cluster7")

# Reduce plot margin using par()
# Split the screen in 4 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,4))

# Create the radar chart
for(i in 4:7){
  nrocluster = paste("c",i,sep="")
  datos = data_for_radar[c(1, 2, nrocluster),]
  create_beautiful_radarchart(
    data = datos, caxislabels = c(0, 0.25,0.5,0.75,1),title = titles[i]
  )
}
par(op)

library("xlsx")
write.csv(res,file="promedios.csv")