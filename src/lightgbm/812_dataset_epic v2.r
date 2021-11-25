#source("~/buckets/b1/crudoB/R/812_dataset_epic.r")
#Necesita para correr en Google Cloud
#256 GB de memoria RAM
#300 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")

require("lightgbm")


#defino la carpeta donde trabajo
#directory.root  <-  "~/buckets/b1/"  #Google Cloud
#setwd( directory.root )
setwd("D:/Cursos/Maestria en Big Data/MBD 2021/DM-Economia-Finanzas")

palancas  <- list()  #variable con las palancas para activar/desactivar

palancas$version  <- "v010"   #Muy importante, ir cambiando la version

palancas$variablesdrift  <- c("internet", "tmobile_app",
                              "cmobile_app_trx","Master_Finiciomora","Master_madelantodolares","Visa_mpagado",
                              "mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos")   #aqui van las columnas que se quieren eliminar

palancas$corregir <-  TRUE    # TRUE o FALSE

palancas$nuevasvars <-  TRUE  #si quiero hacer Feature Engineering manual

palancas$dummiesNA  <-  FALSE #Idea de Santiago Dellachiesa de UAustral

palancas$lag1   <- FALSE    #lag de orden 1
palancas$delta1 <- FALSE    # campo -  lag de orden 1 
palancas$lag2   <- FALSE
palancas$delta2 <- FALSE
palancas$lag3   <- FALSE
palancas$delta3 <- FALSE
palancas$lag4   <- FALSE
palancas$delta4 <- FALSE
palancas$lag5   <- FALSE
palancas$delta5 <- FALSE
palancas$lag6   <- FALSE
palancas$delta6 <- FALSE

palancas$promedio3  <- FALSE  #promedio  de los ultimos 3 meses
palancas$promedio6  <- FALSE

palancas$minimo3  <- FALSE  #minimo de los ultimos 3 meses
palancas$minimo6  <- FALSE

palancas$maximo3  <- FALSE  #maximo de los ultimos 3 meses
palancas$maximo6  <- FALSE

palancas$tendencia6  <- FALSE    #Great power comes with great responsability


palancas$canaritosimportancia  <- FALSE  #si me quedo solo con lo mas importante de canaritosimportancia


#escribo para saber cuales fueron los parametros
write_yaml(  palancas,  paste0( "./work/palanca_",  palancas$version  ,".yaml" ) )

#------------------------------------------------------------------------------

ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}
#------------------------------------------------------------------------------
#Agrega al dataset una variable que va de 1 a 12, el mes, para que el modelo aprenda estacionalidad

AgregarMes  <- function( dataset )
{
  dataset[  , mes := foto_mes %% 100 ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Elimina las variables que uno supone hace Data Drifting

DriftEliminar  <- function( dataset, variables )
{
  dataset[  , c(variables) := NULL ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#A las variables que tienen nulos, les agrega una nueva variable el dummy de is es nulo o no {0, 1}

DummiesNA  <- function( dataset )
{

  nulos  <- colSums( is.na(dataset[foto_mes==202101]) )  #cuento la cantidad de nulos por columna
  colsconNA  <- names( which(  nulos > 0 ) )

  dataset[ , paste0( colsconNA, "_isNA") :=  lapply( .SD,  is.na ),
             .SDcols= colsconNA]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Corrige poniendo a NA las variables que en ese mes estan dañadas

Corregir  <- function( dataset )
{
  #acomodo los errores del dataset

  dataset[ foto_mes==201801,  internet   := NA ]
  dataset[ foto_mes==201801,  thomebanking   := NA ]
  dataset[ foto_mes==201801,  chomebanking_transacciones   := NA ]
  dataset[ foto_mes==201801,  tcallcenter   := NA ]
  dataset[ foto_mes==201801,  ccallcenter_transacciones   := NA ]
  dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
  dataset[ foto_mes==201801,  ccajas_transacciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
  dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
  dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_otras   := NA ]

  dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
  dataset[ foto_mes==201806,  ccallcenter_transacciones   :=  NA ]

  dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
  dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos := NA ]
  dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]

  dataset[ foto_mes==201905,  mrentabilidad     := NA ]
  dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
  dataset[ foto_mes==201905,  mcomisiones      := NA ]
  dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
  dataset[ foto_mes==201905,  mactivos_margen  := NA ]
  dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
  dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

  dataset[ foto_mes==201910,  mpasivos_margen   := NA ]
  dataset[ foto_mes==201910,  mactivos_margen   := NA ]
  dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones       := NA ]
  dataset[ foto_mes==201910,  mrentabilidad     := NA ]
  dataset[ foto_mes==201910,  mrentabilidad_annual        := NA ]
  dataset[ foto_mes==201910,  chomebanking_transacciones  := NA ]
  dataset[ foto_mes==201910,  ctarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  ctarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  mtarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  mtarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  ccajeros_propios_descuentos := NA ]
  dataset[ foto_mes==201910,  mcajeros_propios_descuentos := NA ]

  dataset[ foto_mes==202001,  cliente_vip   := NA ]

  dataset[ foto_mes==202006,  active_quarter   := NA ]
  dataset[ foto_mes==202006,  internet   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
  dataset[ foto_mes==202006,  mcomisiones   := NA ]
  dataset[ foto_mes==202006,  mactivos_margen   := NA ]
  dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
  dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_debito_transacciones   := NA ]
  dataset[ foto_mes==202006,  mautoservicio   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_visa_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_master_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_master_consumo   := NA ]
  dataset[ foto_mes==202006,  ccomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  mcomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  cextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  mextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  tcallcenter   := NA ]
  dataset[ foto_mes==202006,  ccallcenter_transacciones   := NA ]
  dataset[ foto_mes==202006,  thomebanking   := NA ]
  dataset[ foto_mes==202006,  chomebanking_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_consultas   := NA ]
  dataset[ foto_mes==202006,  ccajas_depositos   := NA ]
  dataset[ foto_mes==202006,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_otras   := NA ]
  dataset[ foto_mes==202006,  catm_trx   := NA ]
  dataset[ foto_mes==202006,  matm   := NA ]
  dataset[ foto_mes==202006,  catm_trx_other   := NA ]
  dataset[ foto_mes==202006,  matm_other   := NA ]
  dataset[ foto_mes==202006,  ctrx_quarter   := NA ]
  dataset[ foto_mes==202006,  tmobile_app   := NA ]
  dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]


  dataset[ foto_mes==202010,  internet  := NA ]
  dataset[ foto_mes==202011,  internet  := NA ]
  dataset[ foto_mes==202012,  internet  := NA ]
  dataset[ foto_mes==202101,  internet  := NA ]

  dataset[ foto_mes==202009,  tmobile_app  := NA ]
  dataset[ foto_mes==202010,  tmobile_app  := NA ]
  dataset[ foto_mes==202011,  tmobile_app  := NA ]
  dataset[ foto_mes==202012,  tmobile_app  := NA ]
  dataset[ foto_mes==202101,  tmobile_app  := NA ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
#  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
#  dataset[ , mv_status02       := Master_status +  Visa_status ]
#  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
#  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
#  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

#  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
#                                          ifelse( is.na(Master_status), 10, Master_status), 
#                                          Visa_status)  ]

#  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
#                                          ifelse( is.na(Visa_status), 10, Visa_status), 
#                                          Master_status)  ]


  #combino MasterCard y Visa
#  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

#  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
#  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
#  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
#  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
#  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
#  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
#  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
#  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
#  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
#  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
#  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
#  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
#  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
#  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
#  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
#  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
#  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
#  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
#  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
#  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
#  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
#  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
#  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
#  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
#  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
#  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
#  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
#  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
#  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
#  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
#  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
#  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
#  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
#  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
#  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #Aqui debe usted agregar sus propias nuevas variables
  dataset[ , ctrxquarter_cpayrolltrx         := ctrx_quarter  / cpayroll_trx ]
  dataset[ , ctrxquarter_mcuentassaldo         := ctrx_quarter  / mcuentas_saldo ]
  dataset[ , ctrxquarter_tarjvisatrans         := ctrx_quarter  / ctarjeta_visa_transacciones ]
  dataset[ , ctrxquarter_tarjvisacons        := ctrx_quarter  / mtarjeta_visa_consumo ]
  dataset[ , ctrxquarter_prespersonales         := ctrx_quarter  / mprestamos_personales ]
  dataset[ , ctrxquarter_visastatus         := ctrx_quarter  / Visa_status ]
  dataset[ , ctrxquarter_activosmargen         := ctrx_quarter  / mactivos_margen ]
  dataset[ , ctrxquarter_cajaahorro         := ctrx_quarter  / mcaja_ahorro ]
  dataset[ , ctrxquarter_visapagomin         := ctrx_quarter  / Visa_mpagominimo ]
  
  dataset[ , cpayrolltrx_ctrxquarter         := cpayroll_trx  / ctrx_quarter ]
  dataset[ , cpayrolltrx_mcuentassaldo         := cpayroll_trx  / mcuentas_saldo ]
  dataset[ , cpayrolltrx_tarjvisatrans        := cpayroll_trx  / ctarjeta_visa_transacciones ]
  dataset[ , cpayrolltrx_tarjvisacons         := cpayroll_trx  / mtarjeta_visa_consumo ]
  dataset[ , cpayrolltrx_prespersonales         := cpayroll_trx  / mprestamos_personales ]
  dataset[ , cpayrolltrx_visastatus          := cpayroll_trx  / Visa_status ]
  dataset[ , cpayrolltrx_activosmargen         := cpayroll_trx  / mactivos_margen ]
  dataset[ , cpayrolltrx_cajaahorro         := cpayroll_trx  / mcaja_ahorro ]
  dataset[ , cpayrolltrx_visapagomin         := cpayroll_trx  / Visa_mpagominimo ]
  
  dataset[ , mcuentassaldo_ctrxquarter         := mcuentas_saldo  / ctrx_quarter ]
  dataset[ , mcuentassaldo_cpayrolltrx         := mcuentas_saldo  / cpayroll_trx ]
  dataset[ , mcuentassaldo_tarjvisatrans         := mcuentas_saldo  / ctarjeta_visa_transacciones ]
  dataset[ , mcuentassaldo_tarjvisacons         := mcuentas_saldo  / mtarjeta_visa_consumo ]
  dataset[ , mcuentassaldo_prespersonales          := mcuentas_saldo  / mprestamos_personales ]
  dataset[ , mcuentassaldo_visastatus         := mcuentas_saldo  / Visa_status ]
  dataset[ , mcuentassaldo_activosmargen         := mcuentas_saldo  / mactivos_margen ]
  dataset[ , mcuentassaldo_cajaahorro         := mcuentas_saldo  / mcaja_ahorro ]
  dataset[ , mcuentassaldo_visapagomin         := mcuentas_saldo  / Visa_mpagominimo ]
  
  dataset[ , tarjvisatrans_ctrxquarter         := ctarjeta_visa_transacciones  / ctrx_quarter ]
  dataset[ , tarjvisatrans_cpayrolltrx         := ctarjeta_visa_transacciones  / cpayroll_trx ]
  dataset[ , tarjvisatrans_mcuentassaldo         := ctarjeta_visa_transacciones  / mcuentas_saldo ]
  dataset[ , tarjvisatrans_tarjvisacons         := ctarjeta_visa_transacciones  / mtarjeta_visa_consumo ]
  dataset[ , tarjvisatrans_prespersonales         := ctarjeta_visa_transacciones  / mprestamos_personales ]
  dataset[ , tarjvisatrans_visastatus          := ctarjeta_visa_transacciones  / Visa_status ]
  dataset[ , tarjvisatrans_activosmargen         := ctarjeta_visa_transacciones  / mactivos_margen ]
  dataset[ , tarjvisatrans_cajaahorro         := ctarjeta_visa_transacciones  / mcaja_ahorro ]
  dataset[ , tarjvisatrans_visapagomin         := ctarjeta_visa_transacciones  / Visa_mpagominimo ]
  
  dataset[ , tarjvisacons_ctrxquarter         := mtarjeta_visa_consumo  / ctrx_quarter ]
  dataset[ , tarjvisacons_cpayrolltrx         := mtarjeta_visa_consumo  / cpayroll_trx ]
  dataset[ , tarjvisacons_mcuentassaldo         := mtarjeta_visa_consumo  / mcuentas_saldo ]
  dataset[ , tarjvisacons_tarjvisatrans         := mtarjeta_visa_consumo  / ctarjeta_visa_transacciones ]
  dataset[ , tarjvisacons_prespersonales         := mtarjeta_visa_consumo  / mprestamos_personales ]
  dataset[ , tarjvisacons_visastatus           := mtarjeta_visa_consumo  / Visa_status ]
  dataset[ , tarjvisacons_activosmargen         := mtarjeta_visa_consumo  / mactivos_margen ]
  dataset[ , tarjvisacons_cajaahorro         := mtarjeta_visa_consumo  / mcaja_ahorro ]
  dataset[ , tarjvisacons_visapagomin         := mtarjeta_visa_consumo  / Visa_mpagominimo ]
  
  dataset[ , prespersonales_ctrxquarter         := mprestamos_personales  / ctrx_quarter ]
  dataset[ , prespersonales_cpayrolltrx         := mprestamos_personales  / cpayroll_trx ]
  dataset[ , prespersonales_mcuentassaldo         := mprestamos_personales  / mcuentas_saldo ]
  dataset[ , prespersonales_tarjvisatrans         := mprestamos_personales  / ctarjeta_visa_transacciones ]
  dataset[ , prespersonales_tarjvisacons         := mprestamos_personales  / mtarjeta_visa_consumo ]
  dataset[ , prespersonales_visastatus          := mprestamos_personales  / Visa_status ]
  dataset[ , prespersonales_activosmargen         := mprestamos_personales  / mactivos_margen ]
  dataset[ , prespersonales_cajaahorro         := mprestamos_personales  / mcaja_ahorro ]
  dataset[ , prespersonales_visapagomin         := mprestamos_personales  / Visa_mpagominimo ]
  
  dataset[ , visastatus_ctrxquarter         := Visa_status  / ctrx_quarter ]
  dataset[ , visastatus_cpayrolltrx         := Visa_status  / cpayroll_trx ]
  dataset[ , visastatus_mcuentassaldo         := Visa_status  / mcuentas_saldo ]
  dataset[ , visastatus_tarjvisatrans         := Visa_status  / ctarjeta_visa_transacciones ]
  dataset[ , visastatus_tarjvisacons          := Visa_status  / mtarjeta_visa_consumo ]
  dataset[ , visastatus_prespersonales         := Visa_status  / mprestamos_personales ]
  dataset[ , visastatus_activosmargen          := Visa_status  / mactivos_margen ]
  dataset[ , visastatus_cajaahorro        := Visa_status  / mcaja_ahorro ]
  dataset[ , visastatus_visapagomin         := Visa_status  / Visa_mpagominimo ]
  
  dataset[ , activosmargen_ctrxquarter         := mactivos_margen  / ctrx_quarter ]
  dataset[ , activosmargen_cpayrolltrx         := mactivos_margen  / cpayroll_trx ]
  dataset[ , activosmargen_mcuentassaldo        := mactivos_margen  / mcuentas_saldo ]
  dataset[ , activosmargen_tarjvisatrans         := mactivos_margen  / ctarjeta_visa_transacciones ]
  dataset[ , activosmargen_tarjvisacons         := mactivos_margen  / mtarjeta_visa_consumo ]
  dataset[ , activosmargen_prespersonales         := mactivos_margen  / mprestamos_personales ]
  dataset[ , activosmargen_visastatus         := mactivos_margen  / Visa_status ]
  dataset[ , activosmargen_cajaahorro          := mactivos_margen  / mcaja_ahorro ]
  dataset[ , activosmargen_visapagomin         := mactivos_margen  / Visa_mpagominimo ]
  
  dataset[ , cajaahorro_ctrxquarter         := mcaja_ahorro  / ctrx_quarter ]
  dataset[ , cajaahorro_cpayrolltrx         := mcaja_ahorro  / cpayroll_trx ]
  dataset[ , cajaahorro_mcuentassaldo         := mcaja_ahorro  / mcuentas_saldo ]
  dataset[ , cajaahorro_tarjvisatrans         := mcaja_ahorro  / ctarjeta_visa_transacciones ]
  dataset[ , cajaahorro_tarjvisacons         := mcaja_ahorro  / mtarjeta_visa_consumo ]
  dataset[ , cajaahorro_prespersonales         := mcaja_ahorro  / mprestamos_personales ]
  dataset[ , cajaahorro_visastatus         := mcaja_ahorro  / Visa_status ]
  dataset[ , cajaahorro_activosmargen         := mcaja_ahorro  / mactivos_margen ]
  dataset[ , cajaahorro_visapagomin         := mcaja_ahorro  / Visa_mpagominimo ]
  
  dataset[ , visapagomin_ctrxquarter         := Visa_mpagominimo  / ctrx_quarter ]
  dataset[ , visapagomin_cpayrolltrx          := Visa_mpagominimo  / cpayroll_trx ]
  dataset[ , visapagomin_mcuentassaldo         := Visa_mpagominimo  / mcuentas_saldo ]
  dataset[ , visapagomin_tarjvisatrans       := Visa_mpagominimo  / ctarjeta_visa_transacciones ]
  dataset[ , visapagomin_tarjvisacons         := Visa_mpagominimo  / mtarjeta_visa_consumo ]
  dataset[ , visapagomin_prespersonales         := Visa_mpagominimo  / mprestamos_personales ]
  dataset[ , visapagomin_visastatus         := Visa_mpagominimo  / Visa_status ]
  dataset[ , visapagomin_activosmargen          := Visa_mpagominimo  / mactivos_margen ]
  dataset[ , visapagomin_cajaahorro         := Visa_mpagominimo  / mcaja_ahorro ]
  
  
  
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#esta funcion supone que dataset esta ordenado por   <numero_de_cliente, foto_mes>
#calcula el lag y el delta lag

Lags  <- function( dataset, cols, nlag, deltas )
{

  sufijo  <- paste0( "_lag", nlag )

  dataset[ , paste0( cols, sufijo) := shift(.SD, nlag, NA, "lag"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  #agrego los deltas de los lags, con un "for" nada elegante
  if( deltas )
  {
    sufijodelta  <- paste0( "_delta", nlag )

    for( vcol in cols )
    {
     dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
    }
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el promedio de los ultimos  nhistoria meses

Promedios  <- function( dataset, cols, nhistoria )
{

  sufijo  <- paste0( "_avg", nhistoria )
  
  dataset[ , paste0( cols, sufijo) := frollmean(x=.SD, n=nhistoria, na.rm=TRUE, algo="fast", align="right"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el minimo de los ultimos  nhistoria meses

Minimos  <- function( dataset, cols, nhistoria )
{

  sufijo  <- paste0( "_min", nhistoria )

  dataset[ , paste0( cols, sufijo) := frollapply(x=.SD, FUN="min", n=nhistoria, align="right"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el maximo de los ultimos  nhistoria meses

Maximos  <- function( dataset, cols, nhistoria )
{

  sufijo  <- paste0( "_max", nhistoria )

  dataset[ , paste0( cols, sufijo) := frollapply(x=.SD, FUN="max", n=nhistoria, na.rm=TRUE, align="right"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formual de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

Rcpp::cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  // [[Rcpp::plugins(openmp)]]
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( n );


  //#if defined(_OPENMP)
  //#pragma omp parallel for
  //#endif
  for(int i = 0; i < n; i++)
  {
    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
    }
    else
    {
      out[ i ]  =  NA_REAL ; 
    }
  }

  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos

Tendencia  <- function( dataset, cols )
{
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- 6

  last  <- nrow( dataset )

  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente

  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1

  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }

  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 

    dataset[ , paste0( campo, "_tend") := nueva_col[ (0*last +1):(1*last) ]  ]
  }

}
#------------------------------------------------------------------------------
VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #solo sumo 48750 si vpesos > 1, hackeo 
  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 48750, -1250 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta

  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tamaño 500

  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Elimina del dataset las variables que estan por debajo de la capa geologica de canaritos

CanaritosImportancia  <- function( dataset )
{

  gc()
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  for( i  in 1:(ncol(dataset)/5))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01" ) )

  azar  <- runif( nrow(dataset) )
  entrenamiento  <-  dataset[ , foto_mes>= 202001 &  foto_mes<= 202010 &  foto_mes!=202006 & ( clase01==1 | azar < 0.10 ) ]

  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )

  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202011, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202011, clase01],
                          weight=  dataset[ foto_mes==202011, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )


  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 999983,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.02, 
                 feature_fraction= 0.50,
                 min_data_in_leaf= 4000,
                 num_leaves= 600,
                 early_stopping_rounds= 200 )

  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )

  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]
  
  fwrite( tb_importancia, file="./work/impo.txt", sep="\t" )
  
  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) - sd(pos) ]
  col_inutiles  <- tb_importancia[ pos >= umbral | Feature %like% "canarito",  Feature ]

  for( col in col_inutiles )
  {
    dataset[  ,  paste0(col) := NULL ]
  }

  rm( dtrain, dvalid )
  gc()

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

correr_todo  <- function( palancas )
{
  #cargo el dataset ORIGINAL
  dataset1  <- fread( "./datasetsOri/paquete_premium_202009.csv")
  dataset2  <- fread( "./datasetsOri/paquete_premium_202011.csv")
  
  #dataset1  <- fread( "./datasets/paquete_premium_202009_cortado.csv")
  #dataset2  <- fread( "./datasets/paquete_premium_202011_cortado.csv")

  dataset   <- rbind( dataset1, dataset2 )
  rm( dataset1, dataset2 )
  gc()

  setorder(  dataset, numero_de_cliente, foto_mes )  #ordeno el dataset

  AgregarMes( dataset )  #agrego el mes del año

  if( length(palancas$variablesdrift) > 0 )   DriftEliminar( dataset, palancas$variablesdrift )

  if( palancas$dummiesNA )  DummiesNA( dataset )  #esta linea debe ir ANTES de Corregir  !!

  if( palancas$corregir )  Corregir( dataset )  #esta linea debe ir DESPUES de  DummiesNA

  if( palancas$nuevasvars )  AgregarVariables( dataset )

  cols_analiticas  <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") )

  if( palancas$lag1 )   Lags( dataset, cols_analiticas, 1, palancas$delta1 )
  if( palancas$lag2 )   Lags( dataset, cols_analiticas, 2, palancas$delta2 )
  if( palancas$lag3 )   Lags( dataset, cols_analiticas, 3, palancas$delta3 )
  if( palancas$lag4 )   Lags( dataset, cols_analiticas, 4, palancas$delta4 )
  if( palancas$lag5 )   Lags( dataset, cols_analiticas, 5, palancas$delta5 )
  if( palancas$lag6 )   Lags( dataset, cols_analiticas, 6, palancas$delta6 )

  if( palancas$promedio3 )  Promedios( dataset, cols_analiticas, 3 )
  if( palancas$promedio6 )  Promedios( dataset, cols_analiticas, 6 )

  if( palancas$minimo3 )  Minimos( dataset, cols_analiticas, 3 )
  if( palancas$minimo6 )  Minimos( dataset, cols_analiticas, 6 )

  if( palancas$maximo3 )  Maximos( dataset, cols_analiticas, 3 )
  if( palancas$maximo6 )  Maximos( dataset, cols_analiticas, 6 )

  if( palancas$tendencia6 )  Tendencia( dataset, cols_analiticas)


  if( palancas$canaritosimportancia )  CanaritosImportancia( dataset )



  #dejo la clase como ultimo campo
  nuevo_orden  <- c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
  setcolorder( dataset, nuevo_orden )

  #Grabo el dataset
  fwrite( dataset,
          paste0( "./datasets/dataset_epic_simple_", palancas$version, ".csv.gz" ),
          logical01 = TRUE,
          sep= "," )

}
#------------------------------------------------------------------------------

#Aqui empieza el programa


correr_todo( palancas )


quit( save="no" )


