# Experimentos Colaborativos Exp2
# Workflow  Data Drifting repair

#require vm con
#   8 vCPU
#  64 GB  memoria RAM


#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

require("data.table")
require("yaml")


#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "09_R_DR_ExVarComb"

PARAM$exp_input  <- "09_R_CA_ExVarComb"

PARAM$variables_intrames  <- TRUE   # atencion eesto esta en TRUEEEEEE

#valores posibles  "ninguno" "rank_simple" , "rank_cero_fijo" , "deflacion"
PARAM$metodo  <- "rank_cero_fijo"
# FIN Parametros del script


#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio
#Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes  <- function( dataset )
{
  gc()
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , vm_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , vm_status02       := Master_status +  Visa_status ]
  dataset[ , vm_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , vm_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , vm_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , vm_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , vm_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  dataset[ , vm_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , vm_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , vm_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , vm_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , vm_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , vm_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , vm_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , vm_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , vm_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , vm_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , vm_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , vm_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , vm_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , vm_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , vm_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , vm_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , vm_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , vm_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , vm_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , vmr_Master_mlimitecompra:= Master_mlimitecompra / vm_mlimitecompra ]
  dataset[ , vmr_Visa_mlimitecompra  := Visa_mlimitecompra / vm_mlimitecompra ]
  dataset[ , vmr_msaldototal         := vm_msaldototal / vm_mlimitecompra ]
  dataset[ , vmr_msaldopesos         := vm_msaldopesos / vm_mlimitecompra ]
  dataset[ , vmr_msaldopesos2        := vm_msaldopesos / vm_msaldototal ]
  dataset[ , vmr_msaldodolares       := vm_msaldodolares / vm_mlimitecompra ]
  dataset[ , vmr_msaldodolares2      := vm_msaldodolares / vm_msaldototal ]
  dataset[ , vmr_mconsumospesos      := vm_mconsumospesos / vm_mlimitecompra ]
  dataset[ , vmr_mconsumosdolares    := vm_mconsumosdolares / vm_mlimitecompra ]
  dataset[ , vmr_madelantopesos      := vm_madelantopesos / vm_mlimitecompra ]
  dataset[ , vmr_madelantodolares    := vm_madelantodolares / vm_mlimitecompra ]
  dataset[ , vmr_mpagado             := vm_mpagado / vm_mlimitecompra ]
  dataset[ , vmr_mpagospesos         := vm_mpagospesos / vm_mlimitecompra ]
  dataset[ , vmr_mpagosdolares       := vm_mpagosdolares / vm_mlimitecompra ]
  dataset[ , vmr_mconsumototal       := vm_mconsumototal  / vm_mlimitecompra ]
  dataset[ , vmr_mpagominimo         := vm_mpagominimo  / vm_mlimitecompra ]

 #Aqui debe usted agregar sus propias nuevas variables
   dataset[ , exp_tarjp_rvisa := ((mtarjeta_master_consumo+mtarjeta_visa_consumo)/mpayroll)*(mtarjeta_visa_consumo/ctarjeta_visa_transacciones)]
   dataset[ , exp_tarjp_mcauto := ((mtarjeta_master_consumo+mtarjeta_visa_consumo)/mpayroll)*(mcuenta_debitos_automaticos/ccuenta_debitos_automaticos) ]
   dataset[ , exp_tarjp_ind3 := ((mtarjeta_master_consumo+mtarjeta_visa_consumo)/mpayroll)*((ctrx_quarter*cliente_antiguedad)/(mcomisiones_mantenimiento+mcomisiones_otras)^2)]
   dataset[ , exp_tarjp_consultas := ((mtarjeta_master_consumo+mtarjeta_visa_consumo)/mpayroll)*(log(1+ccajas_otras+ccajas_consultas+ccallcenter_transacciones))]
   dataset[ , exp_tarjp_transtarj := ((mtarjeta_master_consumo+mtarjeta_visa_consumo)/mpayroll)*((ctarjeta_debito_transacciones+ctarjeta_visa_transacciones+ctarjeta_master_transacciones)/(ctarjeta_debito+ctarjeta_visa+ctarjeta_master))]
   dataset[ , exp_tarjp_limsueldo := ((mtarjeta_master_consumo+mtarjeta_visa_consumo)/mpayroll)*((Master_mlimitecompra+Visa_mlimitecompra)/mpayroll)]
   dataset[ , exp_rvisa_mcauto := (mtarjeta_visa_consumo/ctarjeta_visa_transacciones)*(mcuenta_debitos_automaticos/ccuenta_debitos_automaticos)]
   dataset[ , exp_rvisa_ind3 := (mtarjeta_visa_consumo/ctarjeta_visa_transacciones)*((ctrx_quarter*cliente_antiguedad)/(mcomisiones_mantenimiento+mcomisiones_otras)^2)]
   dataset[ , exp_rvisa_consultas := (mtarjeta_visa_consumo/ctarjeta_visa_transacciones)*(log(1+ccajas_otras+ccajas_consultas+ccallcenter_transacciones))]
   dataset[ , exp_rvisa_transtarj := (mtarjeta_visa_consumo/ctarjeta_visa_transacciones)*((ctarjeta_debito_transacciones+ctarjeta_visa_transacciones+ctarjeta_master_transacciones)/(ctarjeta_debito+ctarjeta_visa+ctarjeta_master))]
   dataset[ , exp_rvisa_limsueldo := (mtarjeta_visa_consumo/ctarjeta_visa_transacciones)*((Master_mlimitecompra+Visa_mlimitecompra)/mpayroll)]
   dataset[ , exp_mcauto_ind3  := (mcuenta_debitos_automaticos/ccuenta_debitos_automaticos)*((ctrx_quarter*cliente_antiguedad)/(mcomisiones_mantenimiento+mcomisiones_otras)^2)]
   dataset[ , exp_mcauto_consultas := (mcuenta_debitos_automaticos/ccuenta_debitos_automaticos)*(log(1+ccajas_otras+ccajas_consultas+ccallcenter_transacciones))]
   dataset[ , exp_mcauto_transtarj := (mcuenta_debitos_automaticos/ccuenta_debitos_automaticos)*((ctarjeta_debito_transacciones+ctarjeta_visa_transacciones+ctarjeta_master_transacciones)/(ctarjeta_debito+ctarjeta_visa+ctarjeta_master))]
   dataset[ , exp_mcauto_limsueldo := (mcuenta_debitos_automaticos/ccuenta_debitos_automaticos)*((Master_mlimitecompra+Visa_mlimitecompra)/mpayroll)]
   dataset[ , exp_ind3_consultas := ((ctrx_quarter*cliente_antiguedad)/(mcomisiones_mantenimiento+mcomisiones_otras)^2)*(log(1+ccajas_otras+ccajas_consultas+ccallcenter_transacciones))]
   dataset[ , exp_ind3_transtarj := ((ctrx_quarter*cliente_antiguedad)/(mcomisiones_mantenimiento+mcomisiones_otras)^2)*((ctarjeta_debito_transacciones+ctarjeta_visa_transacciones+ctarjeta_master_transacciones)/(ctarjeta_debito+ctarjeta_visa+ctarjeta_master))]
   dataset[ , exp_ind3_limsueldo  := ((ctrx_quarter*cliente_antiguedad)/(mcomisiones_mantenimiento+mcomisiones_otras)^2)*((Master_mlimitecompra+Visa_mlimitecompra)/mpayroll)]
   dataset[ , exp_consultas_transtarj  := (log(1+ccajas_otras+ccajas_consultas+ccallcenter_transacciones))*((ctarjeta_debito_transacciones+ctarjeta_visa_transacciones+ctarjeta_master_transacciones)/(ctarjeta_debito+ctarjeta_visa+ctarjeta_master))]
   dataset[ , exp_consultas_limsueldo  := (log(1+ccajas_otras+ccajas_consultas+ccallcenter_transacciones))*((Master_mlimitecompra+Visa_mlimitecompra)/mpayroll)]
   dataset[ , exp_transtarj_limsueldo := ((ctarjeta_debito_transacciones+ctarjeta_visa_transacciones+ctarjeta_master_transacciones)/(ctarjeta_debito+ctarjeta_visa+ctarjeta_master))*((Master_mlimitecompra+Visa_mlimitecompra)/mpayroll)]
  

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
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
    dataset[mapply(is.nan, dataset)] <<- 0
  }

}
#------------------------------------------------------------------------------
#deflaciona por IPC
#momento 1.0  31-dic-2020 a las 23:59

drift_deflacion  <- function( campos_monetarios )
{
  vfoto_mes <- c( 201901, 201902, 201903, 201904, 201905, 201906,
                  201907, 201908, 201909, 201910, 201911, 201912,
                  202001, 202002, 202003, 202004, 202005, 202006,
                  202007, 202008, 202009, 202010, 202011, 202012,
                  202101, 202102, 202103, 202104, 202105, 202106,
                  202107  )

  vIPC  <- c( 1.9903030878, 1.9174403544, 1.8296186587,
              1.7728862972, 1.7212488323, 1.6776304408,
              1.6431248196, 1.5814483345, 1.4947526791,
              1.4484037589, 1.3913580777, 1.3404220402,
              1.3154288912, 1.2921698342, 1.2472681797,
              1.2300475145, 1.2118694724, 1.1881073259,
              1.1693969743, 1.1375456949, 1.1065619600,
              1.0681100000, 1.0370000000, 1.0000000000,
              0.9680542110, 0.9344152616, 0.8882274350,
              0.8532444140, 0.8251880213, 0.8003763543,
              0.7763107219  )

  tb_IPC  <- data.table( "foto_mes"= vfoto_mes,
                         "IPC" = vIPC )

  dataset[ tb_IPC,
           on= c("foto_mes"),
           (campos_monetarios) :=  .SD * i.IPC ,
           .SDcols = campos_monetarios ]

}

#------------------------------------------------------------------------------

drift_rank_simple  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ , paste0(campo,"_rank") :=  (frank( get(campo), ties.method="random") - 1) / ( .N -1 ), by= foto_mes]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#El cero se transforma en cero
#los positivos se rankean por su lado
#los negativos se rankean por su lado

drift_rank_cero_fijo  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ get(campo) ==0, paste0(campo,"_rank") := 0 ]
    dataset[ get(campo) > 0, paste0(campo,"_rank") :=   frank(  get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ get(campo) < 0, paste0(campo,"_rank") :=  -frank( -get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa
PARAM$stat$time_start  <- format(Sys.time(), "%Y%m%d %H%M%S")

setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", PARAM$exp_input, "/dataset.csv.gz" )
dataset  <- fread( dataset_input )

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

write_yaml( PARAM, file= "parametros.yml" )   #escribo parametros utilizados

#primero agrego las variables manuales
if( PARAM$variables_intrames )  AgregarVariables_IntraMes( dataset )

#ordeno de esta forma por el ranking
setorder( dataset, foto_mes, numero_de_cliente )

#por como armé los nombres de campos, estos son los campos que expresan variables monetarias
campos_monetarios  <- colnames(dataset)
campos_monetarios  <- campos_monetarios[campos_monetarios %like% "^(m|Visa_m|Master_m|vm_m)"]

#aqui aplico un metodo para atacar el data drifting
#hay que probar experimentalmente cual funciona mejor
switch( 
PARAM$metodo,
  "ninguno"        = cat( "No hay correccion del data drifting" ),
  "rank_simple"    = drift_rank_simple( campos_monetarios ),
  "rank_cero_fijo" = drift_rank_cero_fijo( campos_monetarios ),
  "deflacion"      = drift_deflacion( campos_monetarios ) 
)



fwrite( dataset,
        file="dataset.csv.gz",
        logical01= TRUE,
        sep= "," )

#------------------------------------------------------------------------------
PARAM$stat$time_end  <- format(Sys.time(), "%Y%m%d %H%M%S")
write_yaml( PARAM, file= "parametros.yml" )   #escribo parametros utilizados

#dejo la marca final
cat( format(Sys.time(), "%Y%m%d %H%M%S"),"\n",
     file= "zRend.txt",
     append= TRUE  )
