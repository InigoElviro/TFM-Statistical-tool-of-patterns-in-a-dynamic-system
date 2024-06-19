#FUNCIONES AUXILIARES 

 

function.tabulacion.NA<- function(Estudio,dfNA){ 
  dfNA <- data.frame(col.names = names(Estudio)) 
  dfNA$ValoresNA <- as.vector(colSums(is.na(Estudio))) 
  dfNA$ratioeliminados <- (dfNA$ValoresNA / dim(Estudio)[1]) 
  dfNA <<- dfNA} 

#ASIGNACI√ìN ZONAS ROBOT 

####EXPLICACION#### 
#Programa resumen del proceso de seleccion de variables y modelaje del sistema. 
#1)Carga de datos 
#2)Eliminacion de variables por NA 
#3)Eliminacion de variables por poca variabilidad 
#4)Eliminacion de variables por stepwise 
#5)Eliminacion de variables por p-valores no significativos 
#6)Obtencion de los Zvalues o coeficientes 
#ESTE PROGRAMA ESTA DISE√ëADO PARA QUE INTRODUZCAMOS LAS CABINAS, MODELOS Y COLORES QUE QUEREMOS Y OBTENGAMOS UNA TABLA FINAL CON LOS 
#Z-VALUES OBTENIDOS AL HACER LOS MODELOS GLM CON ERROR POISSON, POR ZONAS Y VARIANTE. PREVIAMENTE SE HA REALIZADO UNA FILTRACI√ìN DE LOS DATOS. 
#ANALISIS DE ZONAS, DE NA's Y DE VARIABILIDAD DE LAS VARIABLES. 
 
 
####LIBRERIAS Y FUNCIONES A UTILIZAR#### 
library(pid) 
library(caret) 
library(dplyr) 
library(gplots) 
library(gam) 
library(lubridate) 
library(tidyverse) 
 



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   *   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 



 
####CARGA DE DATOS Y ELECCION DE VARIANTES#### 
ruta <-"C:/Users/usuario/Desktop/IÒigo/Estudio1_General_12Zonas.csv"
#ruta <- "D:/Stellantis/ÕÒigo/Estudio1_General_12Zonas.csv" 
#ruta <- "C:/Users/inigo/Desktop/Linea de pintura/Estudio1_General_12Zonas.csv" 

df <- read.csv(ruta, header = TRUE, sep=",") 
df <- df[,-c(1)] #Quitamos columna X que viene por ser .csv 
cabinas <- c(1,3) 
modelos <- c(11,12,13,14,17,18) 
colores <- c(14) #c(1,5,11,14,18,19,20,22,23,24,25,46) 

#CABINA 1, MODELO 17, COLOR 14

cabinas <- c(3) 
modelos <- c(14) 
colores <- c(14) 
{
####TABLA REFERENCIA ROBOTS ACTUANTES EN ZONAS#### 
Zonasrobots <- data.frame(matrix(ncol = 16, nrow =0 )) 
nombresrobots <- c("BCIN.Robot.1.1","BCIN.Robot.1.2","BCIN.Robot.2.1","BCIN.Robot.2.2", 
                   "BC1.Robot.1.1","BC1.Robot.1.2","BC1.Robot.2.1","BC1.Robot.2.2", 
                   "BC2.Robot.1.1","BC2.Robot.1.2","BC2.Robot.2.1","BC2.Robot.2.2", 
                   "CC1.Robot.1.3","CC1.Robot.1.4","CC1.Robot.2.3","CC1.Robot.2.4") 
colnames(Zonasrobots) <- nombresrobots 

Zonasrobots[1,1]<-7;Zonasrobots[2,1]<-10 #BCIN1.1 
Zonasrobots[1,2]<-1;Zonasrobots[2,2]<-9 #BCIN1.2 
Zonasrobots[1,3]<-8;Zonasrobots[2,3]<-12 #BCIN2.1 
Zonasrobots[1,4]<-2;Zonasrobots[2,4]<-11 #BCIN2.2 

Zonasrobots[1,5]<-9;Zonasrobots[2,5]<-10 #BC1.1.1 
Zonasrobots[1,6]<-1;Zonasrobots[2,6]<-3;Zonasrobots[3,6]<-5;Zonasrobots[4,6]<-7 #BC1.1.2 
Zonasrobots[1,7]<-11;Zonasrobots[2,7]<-12 #BC1.2.1 
Zonasrobots[1,8]<-2;Zonasrobots[2,8]<-4;Zonasrobots[3,8]<-6;Zonasrobots[4,8]<-8 #BC1.2.2 

Zonasrobots[1,9]<-9;Zonasrobots[2,9]<-10 #BC2.1.1 
Zonasrobots[1,10]<-1;Zonasrobots[2,10]<-3;Zonasrobots[3,10]<-5;Zonasrobots[4,10]<-7 #BC2.1.2 
Zonasrobots[1,11]<-11;Zonasrobots[2,11]<-12 #BC2.2.1 
Zonasrobots[1,12]<-2;Zonasrobots[2,12]<-4;Zonasrobots[3,12]<-6;Zonasrobots[4,12]<-8 #BC2.2.2 

Zonasrobots[1,13]<-5;Zonasrobots[2,13]<-7;Zonasrobots[3,13]<-10 #CC1.1.3 
Zonasrobots[1,14]<-6;Zonasrobots[2,14]<-8;Zonasrobots[3,14]<-12 #CC1.1.4 
Zonasrobots[1,15]<-1;Zonasrobots[2,15]<-3;Zonasrobots[3,15]<-9 #CC1.2.3 
Zonasrobots[1,16]<-2;Zonasrobots[2,16]<-4;Zonasrobots[3,16]<-11 #CC1.2.4 

#PROGRAMA CREACION MODELOS 

#En este programa se ejecuta toda la operativa de los modelos mostrados en el trabajo de fin de grado, desde la eliminaci√≥n de variables debido a su poca variabilidad, hasta la obtenci√≥n de los Z-values de los modelos. 


 
####EMPEZAMOS PROCESO DE EJECUCION POR CADA VARIANTE#### 
 
df$fecha <- parse_date_time(df$fecha,orders = c("Ymd_HMS")) #transformacion a tiempo,fecha,hora... 
df$diaAÒo <- yday(df$fecha) 
df$hora <- hour(df$fecha) 
df$weekday = weekdays(df$fecha) 
filtronometalizado <- c(11,14,15,25,46) 
filtrometalizado <- c(1,5,18,19,20,22,23,24) 
 

zona_modelo=c()
variables=c()

modelos_binomial=c()
estimate_binomial=c()
p_valor_binomial=c()
deviance_binomial=c()

modelos_poisson=c()
estimate_poisson=c()
p_valor_poisson=c()
deviance_poisson=c()

df_B_P=data.frame(zona_modelo,variables,modelos_binomial,estimate_binomial,p_valor_binomial,deviance_binomial,modelos_poisson,estimate_poisson,p_valor_poisson,deviance_poisson)


#FOR PARA GENERAR EL DATAFRAME CON LA ZONA Y VARIANTES  SELECCIONADAS 
for(cabina in cabinas) { 
  for(modelo in modelos){ 
    for(color in colores){ 
      filtracion <- is.element(df$cabina,cabina)&is.element(df$ma1,color)&is.element(df$id_modelo,modelo) #filtro 
      Estudio <- df[filtracion,] #Nos quedamos con la variante concreta 
       
      ###ELIMINACION DE BASES CON NA### 
      #Aqui eliminamos aquellas variables que normalmente no est√°n debido a ser unicolor, estar en la cabina 1 y detectamos otras que se han grabado mal. 
       
      aux.base <- function.tabulacion.NA(Estudio) #Guardamos en la matriz el ratio de NA de las variables de la variante. 
      aux.marcador <- tapply(aux.base$ratioeliminados,aux.base$col.names,quantile,0.1,na.rm=T) #Al ser valor √∫nico da igual el percentil, esta orden valdria si metemos todas las variantes. 
      nombresfueraNA <- names(aux.marcador)[aux.marcador>0.4] 
      Prueba <- Estudio%>%dplyr::select(everything(Estudio),-contains(nombresfueraNA)) 
      #Aclaracion por colores y cabinas para que no haya complicaciones: 
      if((cabina==1)&(color %in% filtronometalizado)){ 
        nombresextra <- names(Prueba%>%dplyr::select(contains(c("BC2","CC1")))) 
      } else if ((cabina==1)&(color %in% filtrometalizado)){ 
        nombresextra <- names(Prueba%>%dplyr::select(contains(c("CC1")))) 
      } else if ((cabina==3)&(color %in% filtronometalizado)){ 
        nombresextra <- names(Prueba%>%dplyr::select(contains(c("BC2"))))} 
       
      nombresfueraNA <- c(nombresfueraNA,nombresextra) #Todos los nombres que quitamos porque no intervienen o porque son NA. 
      Estudio <- Estudio%>%dplyr::select(everything(Estudio),-contains(c(nombresfueraNA,"FechaRobot"))) #Quitamos nombresNA y fecharobot 
      assign(paste("nombresfueraNA",cabina,modelo,color,sep="_"),nombresfueraNA) 
      #Nueva filtraci√≥n para el nuevo modelo con variables importantes. 
      Basemodelo <- Estudio%>%dplyr::select(everything(Estudio),-(c("Suma_pesos":"id_modelo","id_tipo_defecto":"weekday"))) #Eliminamos variables que no interesan. 
       
       
      ####COMENZAMOS MODELOS POR ZONA#### 
      
      listBasemodelozona=list()
      listBasemodelozona_BIN=list()
      listBasemodelozona_POI=list()
       
      for (zona in 1:12){ #Bucle por zona  
        #ELIMINACION VARIABLES QUE NO INTERVIENEN EN ESA DETERMINADA ZONA mediante la obtencion de los robots de la zona. 
        nombresnuevos <- names(Filter(function(x) any(x == zona), Zonasrobots)) #Nombres que debemos quedarnos (ej:"BCIN.Robot.1.1","BCIN.Robot.1.2",....) 
        Basemodelozona <- Basemodelo%>%dplyr::select("id_zona_coche","Num_defectos",contains(nombresnuevos)) 
         
        #VARIABILIDAD de las variables numericas y eliminacion de ellas 
        names <- c(names(Basemodelozona %>% dplyr::select(everything(Basemodelozona),-contains(c("id_zona_coche","Num_defectos"))))) #Cogemos todo menos "id_zona_coche","Num_defectos". 
        quitar <- "sam" 
        for (name in names){ 
          if(length(nearZeroVar(Estudio[,name]))>0){ 
            quitar<-cbind(name,quitar) 
          } 
        } 
        quitar <- quitar[-(length(quitar))]#Quitamos "sam" guardado en la ultima posici√≥n del vector. 
        Basemodelozona <- Basemodelozona %>% dplyr::select(everything(Basemodelozona),-contains(quitar)) #Quitamos las variables sin variabilidad. 
        assign(paste("NombresFueraVar",zona,cabina,modelo,color,sep="_"),quitar) #Guardamos los nombres a quitar en cada zona por variabilidad 
         
        ####MODELO#### 
        #Preparamos la base, pasando a factor las variables categoricas 
        factores <- c(names(Basemodelozona %>% dplyr::select(contains("Ciclo_limpieza"),contains("Ciclo_llenado")))) 
        for(i.variable in factores){ 
          Basemodelozona[,i.variable] <- as.factor(Basemodelozona[,i.variable])} 
        #Por √∫ltimo eliminamos id_zona_coche del analisis porque tiene valor √∫nico. 
        Basemodelozona <- Basemodelozona %>% dplyr::select(everything(Basemodelozona),-contains(c("id_zona_coche")))  
         
        #Ponemos data= na.omit(Basemodelozona) porque sino da error de filas perdidas, se eliminan 7000 registros de 37000. 
        dim(Basemodelozona);dim(na.omit(Basemodelozona)) 

        #LA BASE DE DATOS PARA EL AJUSTE DEL MODELO ESTAD√çSTICO ES BASEMODELOZONA
        
        #SUSTITUIR ESTE AJUSTE GLM POR UNO BINARIO y CREAR NUEVA VARIABLE BINARIA 
        variables=names(Basemodelozona)[-1]
        
        #CENSURO VARIABLES 
        
          prueba=na.omit(Basemodelozona)
          for (i in 2 :(ncol(prueba))) {
            
            aux.limite=as.numeric(quantile(as.numeric(prueba[,i]),0.99))
            aux.mean=mean(as.numeric(prueba[,i]))
            rango=aux.mean+2*(aux.limite-aux.mean)
            if(aux.mean<aux.limite){prueba=subset(prueba,(as.numeric(prueba[,i])<rango))
            }else{prueba=subset(prueba,as.numeric(prueba[,i])>rango)}
            
          }
          Basemodelozona=prueba
        
        Basemodelozona$Num_defectos_BIN=(Basemodelozona$Num_defectos>0)*1
        Basemodelozona_POI=Basemodelozona[Basemodelozona$Num_defectos>0,]
        Basemodelozona_POI$Num_defectos_POI=Basemodelozona_POI$Num_defectos-1
        
        Basemodelozona_BIN=Basemodelozona[,-which(names(Basemodelozona) %in% c("Num_defectos"))]
        Basemodelozona_POI=Basemodelozona_POI[,-which(names(Basemodelozona_POI) %in% c("Num_defectos","Num_defectos_BIN"))]
        
        listBasemodelozona[[zona]]=assign(paste("Basemodelozona_",zona,sep="_",collapse = ""),Basemodelozona) 
        listBasemodelozona_BIN[[zona]]=assign(paste("Basemodelozona_BIN",zona,sep="_",collapse = ""),Basemodelozona_BIN) 
        listBasemodelozona_POI[[zona]]=assign(paste("Basemodelozona_POI",zona,sep="_",collapse = ""),Basemodelozona_POI) 

        GLMmodelo_BINOMIAL <- glm(Num_defectos_BIN~.,data=na.omit(Basemodelozona_BIN),family=binomial) 
        #Stepwisemodelo <- step(GLMmodelo,direction='both',data=na.omit(Basemodelozona)) 
        #Stepwisemodelo$call 
        #GLMmodelo <- glm(formula = Stepwisemodelo$call$formula,family = poisson, data = na.omit(Basemodelozona)) 
        GLMmodelo_POISSON <- glm(Num_defectos_POI~.,data=na.omit(Basemodelozona_POI),family=poisson) 
        
        modelos_binomial=c(row.names(summary(GLMmodelo_BINOMIAL)$coefficients))
        modelos_binomial=gsub("Y","",modelos_binomial)[-1]
        estimate_binomial=c(round(summary(GLMmodelo_BINOMIAL)$coefficients[,1],4))[-1]
        p_valor_binomial=c(round(summary(GLMmodelo_BINOMIAL)$coefficients[,4],4))[-1]
        deviance_binomial=c(summary(GLMmodelo_BINOMIAL)$deviance)
        
        modelos_poisson=c(row.names(summary(GLMmodelo_POISSON)$coefficients))
        modelos_poisson=gsub("Y","",modelos_poisson)[-1]
        estimate_poisson=c(round(summary(GLMmodelo_POISSON)$coefficients[,1],4))[-1]
        p_valor_poisson=c(round(summary(GLMmodelo_POISSON)$coefficients[,4],4))[-1]
        deviance_poisson=c(summary(GLMmodelo_POISSON)$deviance)
        
        zona_modelo=zona
        
        modelos_binomial=modelos_binomial[match(variables,modelos_binomial)]
        estimate_binomial=estimate_binomial[match(variables,modelos_binomial)]
        p_valor_binomial=p_valor_binomial[match(variables,modelos_binomial)]
        
        modelos_poisson=modelos_poisson[match(variables,modelos_poisson)]
        estimate_poisson=estimate_poisson[match(variables,modelos_poisson)]
        p_valor_poisson=p_valor_poisson[match(variables,modelos_poisson)]
        
        
        df_B_P_i=data.frame(zona_modelo,variables,modelos_binomial,estimate_binomial,p_valor_binomial,deviance_binomial,modelos_poisson,estimate_poisson,p_valor_poisson,deviance_poisson)
        df_B_P=rbind(df_B_P,df_B_P_i)
        
        #ASIGNACI√ìN DEL OBJETO GLM 
        
        assign(paste("Binomial",zona,"CABINA",cabina,"MODELO",modelo,"COLOR",color,sep="_",collapse = ""),GLMmodelo_BINOMIAL) 
        assign(paste("Poisson",zona,"CABINA",cabina,"MODELO",modelo,"COLOR",color,sep="_",collapse = ""),GLMmodelo_POISSON) 
        
         
        # #Cogemos los nombres que no han salido por el stepwise. 
        # nombresmodelo <- names(Basemodelozona %>% select(everything(Basemodelozona),-contains("Num_defectos"))) 
        # nombresglm <- rownames(summary(GLMmodelo)$coef)[-1] 
        # assign(paste("nombresmodelo",zona,sep="_"),nombresmodelo) 
        # assign(paste("nombresglm",zona,sep="_"),nombresglm) 
        # BasemodelozonaNA <- na.omit(Basemodelozona) 
        # assign(paste("Basemodelo",zona,"CABINA",cabina,"MODELO",modelo,"COLOR",color,sep=""),Basemodelozona) 
        # assign(paste("BasemodeloNA",zona,"CABINA",cabina,"MODELO",modelo,"COLOR",color,sep=""),BasemodelozonaNA) 
        
        
        
        
        # ####GENERACI√ìN TABLA####
        # ####Escoger todos los nombres de los glm####
        # 
        # nombresglm <- "prueba"  
        # for (zona in 1:12) {
        #   nombresprueba <-  names(summary(get(paste("Binomial",zona,"CABINA",cabina,"MODELO",modelo,"COLOR",color,sep="_",collapse = "")))$aliased)
        #   nombresprueba <- nombresprueba[-1]
        #   nombresglm <- c(nombresglm,nombresprueba)
        # }
        # nombresglm <- nombresglm[-1]
        # nombresglm <- unique(nombresglm)
        # 
        # ###CREACION TABLA####
        # tablamodelo <- data.frame(matrix(ncol = 16,nrow = length(nombresglm)))
        # colnames(tablamodelo) <- c("Variable","Cabina","Modelo","Color","Zona1","Zona2","Zona3","Zona4","Zona5","Zona6",
        #                            "Zona7","Zona8","Zona9","Zona10","Zona11","Zona12")
        # 
        # tablamodelo$Variable <- nombresglm
        # tablamodelo$Cabina <- cabina
        # tablamodelo$Modelo <- modelo
        # tablamodelo$Color <- color
        # 
        # 
        # ####Rellenar tabla####
        # for (zona in 1:12) {
        #   assign("operar", get(paste("Binomial",zona,"CABINA",cabina,"MODELO",modelo,"COLOR",color,sep="_",collapse = "")))
        #   variablesacceso <- rownames(summary(operar)$coef)[-1]
        #   for (variable in variablesacceso){
        #     if(summary(operar)$coef[variable,"Pr(>|z|)" ]>0.05){
        #       tablamodelo[tablamodelo$Variable==variable,paste("Zona",zona,sep="")] <- 3000
        #     } else{
        #       tablamodelo[tablamodelo$Variable==variable,paste("Zona",zona,sep="")] <- summary(operar)$coef[variable,"z value" ] #Si pongo "z value", escribe el z value #PONER QUE ESCRIBA EL COEFICIENTE
        #     }
        #     assign(paste("tablamodelo",cabina,modelo,color,sep="_"),tablamodelo)
        #   }#end variable
        # }#end 12 zonas
        # 
        # 
        # 
        # 
        # 
        # 
        # 
        # 
        # 
        # 
        # 
        # 
        
        
        
        
      }#END ZONA 
    }#END COLOR
  }#END MODELO
}#END CABINA      

colnames(df_B_P)=c("zona",colnames(df_B_P[-1]))
df_B_P$robot=paste0(sapply(strsplit(df_B_P$variables,"\\."), `[`, 2),".Robot",sapply(strsplit(df_B_P$variables,"Robot"), `[`, 2))
table(df_B_P$robot)

deviance_binomial_zona=round(as.numeric(names(table(df_B_P$deviance_binomial))),1)
deviance_poisson_zona=round(as.numeric(names(table(df_B_P$deviance_poisson))),1)

variables_robots=table(paste0(sapply(strsplit(names(Basemodelo)[-c(1:3)],"\\."), `[`, 2),".Robot",sapply(strsplit(names(Basemodelo)[-c(1:3)],"Robot"), `[`, 2)))
df_to_latex(t(as.data.frame(variables_robots)))

##-------------------------------------------------------------------## 
##                       Tabla signos estimados                      ## 
##-------------------------------------------------------------------## 

n_robot=names(table(df_B_P$robot))
zonas=c(1:12)
for (robot in n_robot) {

  prueba=subset(df_B_P, is.element(df_B_P$robot,robot))
  
  df_robot_zona_bin=as.data.frame(tapply(prueba$estimate_binomial,list(prueba$variables,prueba$zona),mean))
  colnames(df_robot_zona_bin)=paste0("zona ",colnames(df_robot_zona_bin)," Bin")
  df_robot_zona_poi=as.data.frame(tapply(prueba$estimate_poisson,list(prueba$variables,prueba$zona),mean))
  colnames(df_robot_zona_poi)=paste0("zona ",colnames(df_robot_zona_poi)," Poi")
  df_robot_zona=cbind(df_robot_zona_bin,df_robot_zona_poi)
  
  p_valor_robot_zona_bin=as.data.frame(tapply(prueba$p_valor_binomial,list(prueba$variables,prueba$zona),mean))
  colnames(p_valor_robot_zona_bin)=paste0("zona ",colnames(p_valor_robot_zona_bin)," Bin")
  p_valor_robot_zona_poi=as.data.frame(tapply(prueba$p_valor_poisson,list(prueba$variables,prueba$zona),mean))
  colnames(p_valor_robot_zona_poi)=paste0("zona ",colnames(p_valor_robot_zona_poi)," Poi")
  p_valor_robot_zona=cbind(p_valor_robot_zona_bin,p_valor_robot_zona_poi)
  
  deviance_robot_zona_bin=as.data.frame(tapply(prueba$deviance_binomial,list(prueba$variables,prueba$zona),mean))
  colnames(deviance_robot_zona_bin)=paste0("zona ",colnames(deviance_robot_zona_bin)," Bin")
  deviance_robot_zona_poi=as.data.frame(tapply(prueba$deviance_poisson,list(prueba$variables,prueba$zona),mean))
  colnames(deviance_robot_zona_poi)=paste0("zona ",colnames(deviance_robot_zona_poi)," Poi")
  deviance_robot_zona=cbind(deviance_robot_zona_bin,deviance_robot_zona_poi)

  
  estimate_sign=as.data.frame(ifelse(df_robot_zona>0, paste0("+"), paste0("-")))
  
  assign(paste(robot,"Estimates",color,sep="_",collapse = ""),estimate_sign)
  assign(paste(robot,"p_valor",color,sep="_",collapse = ""),p_valor_robot_zona)
  
}

##-------------------------------------------------------------------## 
##                                                                   ## 
##-------------------------------------------------------------------## 
}

{
  # aux_df_modelo17_color14_cabina1=list(listBasemodelozona_BIN,listBasemodelozona_BIN,listBasemodelozona_POI,listBasemodelozona_POI,listBasemodelozona)
# saveRDS(aux_df_modelo17_color14_cabina1,file="C:/Users/usuario/Desktop/IÒigo/aux_df_modelo17_color14_cabina1.Rda")
# 
# aux_df_modelo14_color14_cabina1=list(listBasemodelozona_BIN,listBasemodelozona_BIN,listBasemodelozona_POI,listBasemodelozona_POI,listBasemodelozona)
# saveRDS(aux_df_modelo14_color14_cabina1,file="C:/Users/usuario/Desktop/IÒigo/aux_df_modelo14_color14_cabina1.Rda")
# 
# aux_df_modelo17_color14_cabina3=list(listBasemodelozona_BIN,listBasemodelozona_BIN,listBasemodelozona_POI,listBasemodelozona_POI,listBasemodelozona)
# saveRDS(aux_df_modelo17_color14_cabina3,file="C:/Users/usuario/Desktop/IÒigo/aux_df_modelo17_color14_cabina3.Rda")

# aux_df_modelo14_color14_cabina3=list(listBasemodelozona_BIN,listBasemodelozona_BIN,listBasemodelozona_POI,listBasemodelozona_POI,listBasemodelozona)
# saveRDS(aux_df_modelo14_color14_cabina3,file="C:/Users/usuario/Desktop/IÒigo/aux_df_modelo14_color14_cabina3.Rda")
  }


##-------------------------------------------------------------------## 
##                                                                   ## 
##-------------------------------------------------------------------## 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  * *  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 



##~~~~~~~~~~~~~~*~*~*~EXPLORATORIO  M_1/2_B/P ~*~*~*~~~~~~~~~~~~~~~~~## 


##-------------------------------------------------------------------## 
##                           MEZCLA                                  ## 
##-------------------------------------------------------------------## 

##-------------------------------------------------------------------## 
##                     M_1_B_Zona - BINOMIAL                         ## 
##-------------------------------------------------------------------## 

  {
aux.y="Num_defectos_BIN"
aux.family="binomial"

M_1_zona=c()
M_1_robot=c()
M_1_list=c()
M_1_estimate=c()
M_1_p_valor=c()
M_1_AIC=c()
n_var=c()
n_var_signif=c()

df_M_1_list_BIN=data.frame(M_1_robot,M_1_AIC,M_1_zona,M_1_list,M_1_estimate,M_1_p_valor,n_var,n_var_signif)

for (zona in zonas) {
  
  M_1_zona=c()
  M_1_robot=c()
  M_1_list=c()
  M_1_estimate=c()
  M_1_p_valor=c()
  M_1_AIC=c()
  n_var=c()
  n_var_signif=c()
  
  aux.var.robots=0
  
  aux.df=na.omit(listBasemodelozona_BIN[[zona]])
  
  aux.marcador=is.element(df_B_P$zona,zona)
  prueba=subset(df_B_P, aux.marcador==TRUE)
  
  p_valor_robot_zona_bin=as.data.frame(tapply(prueba$p_valor_binomial,list(prueba$variables,prueba$robot),mean))
  colnames(p_valor_robot_zona_bin)=paste0(colnames(p_valor_robot_zona_bin)," Bin")
  p_valor_robot_zona_poi=as.data.frame(tapply(prueba$p_valor_poisson,list(prueba$variables,prueba$robot),mean))
  colnames(p_valor_robot_zona_poi)=paste0(colnames(p_valor_robot_zona_poi)," Poi")
  p_valor_zona=cbind(p_valor_robot_zona_poi,p_valor_robot_zona_poi)
  
  assign(paste(robot,"p_valor",color,sep="_",collapse = ""),p_valor_robot_zona)
  
  aux.list=c()
  
  variables_robots=colnames(aux.df)[!(is.element(colnames(aux.df),"Num_defectos_BIN"))]
  aux.robots=prueba$robot
  
  for (i in 1:ncol(p_valor_robot_zona_bin)) {
    
    aux.var=  variables_robots[is.element(aux.robots,(names(table(prueba$robot)))[i])]
    aux.n_var=length(aux.var)
    aux.var.robots=aux.var.robots+aux.n_var
    aux.var=gsub("Y","",aux.var)
    aux.var=paste("(",aux.var, ")",collapse = "+")
    aux.formula=paste(aux.y, aux.var, sep=" ~ ")
    
    aux.model=glm(aux.formula, data = aux.df, family=aux.family )
    assign(paste(robot,zona,sep="_",collapse = ""),aux.model)
    
    aux.list=c(aux.list,terminos(aux.model)[,1])
    
    aux.sub.list=terminos(aux.model)[,1]
    
    if(length(aux.sub.list[!is.na(aux.sub.list)])>0){
      M_1_zona=c(M_1_zona,rep(zona, each=length(aux.sub.list[!is.na(aux.sub.list)])))
      M_1_AIC=c(M_1_AIC,rep(AIC(aux.model), each=length(aux.sub.list[!is.na(aux.sub.list)])))
      
    }else{
      M_1_zona=c(M_1_zona,zona)
      M_1_AIC=c(M_1_AIC,AIC(aux.model))}
    
    name_robot=(names(table(prueba$robot)))[i]  
    
    M_1_robot=c(M_1_robot,rep(name_robot, each=length(aux.sub.list)))
    M_1_list=c(M_1_list,gsub("Y","",aux.sub.list))
    M_1_estimate=c(M_1_estimate,rep(NA, each=length(aux.sub.list)))
    M_1_p_valor=c(M_1_p_valor,rep(NA, each=length(aux.sub.list)))
    n_var=c(n_var,rep(aux.n_var, each=length(aux.sub.list)))
    n_var_signif=c(n_var_signif,rep(length(aux.sub.list[!is.na(aux.sub.list)]), each=length(aux.sub.list)))
    
  } 
  
  aux.list=aux.list[!is.na(aux.list)]
  aux.list=gsub("Y","",aux.list)
  name_robot=paste((names(table(prueba$robot)))[1:ncol(p_valor_robot_zona_bin)],collapse =", ")
  
  if(length(aux.list)==0 ){
    model_mezcla=NA
    M_1_robot=c(M_1_robot, name_robot)  
    M_1_zona=c(M_1_zona,zona)
    M_1_list=c(M_1_list,NA)
    M_1_estimate=c(M_1_estimate,NA)
    M_1_p_valor=c(M_1_p_valor,NA)
    M_1_AIC=c(M_1_AIC,NA)
    n_var=c(n_var,NA)
    n_var_signif=c(n_var_signif,NA)
    
  }else{
    
    model_mezcla=mezcla_modelo(aux.df,aux.y,as.data.frame(aux.list),aux.family)
    aux.var.mezcla=rownames(as.data.frame(summary(model_mezcla)$coefficients)[-1,])
    M_1_robot=c(M_1_robot, rep(name_robot, each=length(aux.list)))  
    M_1_zona=c(M_1_zona,rep(zona, each=length(aux.list)))
      variables_mezcla=gsub("Y","",paste(aux.list[1:length(aux.list)],collapse="+"))
    M_1_list=c(M_1_list,rep(variables_mezcla, each=length(aux.list)))    
    M_1_estimate=c(M_1_estimate,(as.data.frame(summary(model_mezcla)$coefficients)[-1,])[,1])
    M_1_p_valor=c(M_1_p_valor,(as.data.frame(summary(model_mezcla)$coefficients)[-1,])[,4])
    M_1_AIC=c(M_1_AIC,rep(AIC(model_mezcla), each=length(aux.list)))
    n_var=c(n_var,rep(length(aux.list), each=length(aux.list)))
    n_var_signif=c(n_var_signif,rep(length(terminos(model_mezcla)[,1]), each=length(aux.list)))
    
  }
  
  df_M_1_list_BIN_i=data.frame(M_1_robot,M_1_AIC,M_1_zona,M_1_list,M_1_estimate,M_1_p_valor,n_var,n_var_signif)
  df_M_1_list_BIN=rbind(df_M_1_list_BIN,df_M_1_list_BIN_i)
  
  assign(paste("M_1","zona",zona,sep="_",collapse = ""),model_mezcla)
  
  
}
M_1_list_unicos=M_1_list[!duplicated(M_1_list)]

df_M_1_list_BIN$M_1_AIC=round(df_M_1_list_BIN$M_1_AIC,0)
df_M_1_list_BIN$M_1_robot_zona=paste(df_M_1_list_BIN$M_1_robot,df_M_1_list_BIN$M_1_zona)
df_M_1_list_BIN_unicos=df_M_1_list_BIN[!duplicated(c(df_M_1_list_BIN$M_1_robot_zona)),]
df_M_1_list_BIN_unicos=df_M_1_list_BIN_unicos[ , -which(names(df_M_1_list_BIN_unicos) %in% c("M_1_estimate","M_1_p_valor","M_1_robot_zona"))]
df_M_1_mezclas_BIN=df_M_1_list_BIN_unicos[!duplicated(df_M_1_list_BIN_unicos$M_1_zona, fromLast = TRUE),]

}

#saveRDS(df_M_1_list_BIN_unicos,file="C:/Users/usuario/Desktop/IÒigo/list_M_1_BIN.Rda")
#list_M_1_BIN=readRDS("list_M_1_BIN.Rda")


##-------------------------------------------------------------------## 
##                     M_1_P_Zona - POISSON                          ## 
##-------------------------------------------------------------------## 

  {
aux.y="Num_defectos_POI"
aux.family="poisson"

M_1_zona=c()
M_1_robot=c()
M_1_list=c()
M_1_estimate=c()
M_1_p_valor=c()
M_1_AIC=c()
n_var=c()
n_var_signif=c()

df_M_1_list_POI=data.frame(M_1_robot,M_1_AIC,M_1_zona,M_1_list,M_1_estimate,M_1_p_valor,n_var,n_var_signif)

for (zona in zonas) {
  
  M_1_zona=c()
  M_1_robot=c()
  M_1_list=c()
  M_1_estimate=c()
  M_1_p_valor=c()
  M_1_AIC=c()
  n_var=c()
  n_var_signif=c()
  
  aux.var.robots=0
  
  aux.df=na.omit(listBasemodelozona_POI[[zona]])
  
  aux.marcador=is.element(df_B_P$zona,zona)
  prueba=subset(df_B_P, aux.marcador==TRUE)
  
  p_valor_robot_zona_bin=as.data.frame(tapply(prueba$p_valor_binomial,list(prueba$variables,prueba$robot),mean))
  colnames(p_valor_robot_zona_bin)=paste0(colnames(p_valor_robot_zona_bin)," Bin")
  p_valor_robot_zona_poi=as.data.frame(tapply(prueba$p_valor_poisson,list(prueba$variables,prueba$robot),mean))
  colnames(p_valor_robot_zona_poi)=paste0(colnames(p_valor_robot_zona_poi)," Poi")
  p_valor_zona=cbind(p_valor_robot_zona_bin,p_valor_robot_zona_poi)
  
  assign(paste(robot,"p_valor",color,sep="_",collapse = ""),p_valor_robot_zona)
  
  aux.list=c()
  
  variables_robots=colnames(aux.df)[!(is.element(colnames(aux.df),"Num_defectos_POI"))]
  aux.robots=prueba$robot
  
  for (i in 1:ncol(p_valor_robot_zona_poi)) {
    
    aux.var=  variables_robots[is.element(aux.robots,(names(table(prueba$robot)))[i])]
    aux.n_var=length(aux.var)
    aux.var.robots=aux.var.robots+aux.n_var
    aux.var=gsub("Y","",aux.var)
    aux.var=paste("(",aux.var, ")",collapse = "+")
    aux.formula=paste(aux.y, aux.var, sep=" ~ ")
    
    aux.model=glm(aux.formula, data = aux.df, family=aux.family )
    assign(paste(robot,zona,sep="_",collapse = ""),aux.model)
    
    aux.list=c(aux.list,terminos(aux.model)[,1])
    
    aux.sub.list=terminos(aux.model)[,1]
    
    if(length(aux.sub.list[!is.na(aux.sub.list)])>0){
      M_1_zona=c(M_1_zona,rep(zona, each=length(aux.sub.list[!is.na(aux.sub.list)])))
      M_1_AIC=c(M_1_AIC,rep(AIC(aux.model), each=length(aux.sub.list[!is.na(aux.sub.list)])))
      
    }else{
      M_1_zona=c(M_1_zona,zona)
      M_1_AIC=c(M_1_AIC,AIC(aux.model))}
    
    name_robot=(names(table(prueba$robot)))[i]  
    
    M_1_robot=c(M_1_robot,rep(name_robot, each=length(aux.sub.list)))
    M_1_list=c(M_1_list,gsub("Y","",aux.sub.list))
    M_1_estimate=c(M_1_estimate,rep(NA, each=length(aux.sub.list)))
    M_1_p_valor=c(M_1_p_valor,rep(NA, each=length(aux.sub.list)))
    n_var=c(n_var,rep(aux.n_var, each=length(aux.sub.list)))
    n_var_signif=c(n_var_signif,rep(length(aux.sub.list[!is.na(aux.sub.list)]), each=length(aux.sub.list)))
    
  } 
  
  aux.list=aux.list[!is.na(aux.list)]
  aux.list=gsub("Y","",aux.list)
  name_robot=paste((names(table(prueba$robot)))[1:ncol(p_valor_robot_zona_poi)],collapse =", ")
  
  if(length(aux.list)==0 ){
    model_mezcla=NA
    M_1_robot=c(M_1_robot, name_robot)  
    M_1_zona=c(M_1_zona,zona)
    M_1_list=c(M_1_list,NA)
    M_1_estimate=c(M_1_estimate,NA)
    M_1_p_valor=c(M_1_p_valor,NA)
    M_1_AIC=c(M_1_AIC,NA)
    n_var=c(n_var,NA)
    n_var_signif=c(n_var_signif,NA)
    
  }else{
    
    model_mezcla=mezcla_modelo(aux.df,aux.y,as.data.frame(aux.list),aux.family)
    aux.var.mezcla=rownames(as.data.frame(summary(model_mezcla)$coefficients)[-1,])
    M_1_robot=c(M_1_robot, rep(name_robot, each=length(aux.list)))  
    M_1_zona=c(M_1_zona,rep(zona, each=length(aux.list)))
      variables_mezcla=gsub("Y","",paste(aux.list[1:length(aux.list)],collapse="+"))
    M_1_list=c(M_1_list,rep(variables_mezcla, each=length(aux.list)))
    M_1_estimate=c(M_1_estimate,(as.data.frame(summary(model_mezcla)$coefficients)[-1,])[,1])
    M_1_p_valor=c(M_1_p_valor,(as.data.frame(summary(model_mezcla)$coefficients)[-1,])[,4])
    M_1_AIC=c(M_1_AIC,rep(AIC(model_mezcla), each=length(aux.list)))
    n_var=c(n_var,rep(length(aux.list), each=length(aux.list)))
    n_var_signif=c(n_var_signif,rep(length(terminos(model_mezcla)[,1]), each=length(aux.list)))
    
  }
  
  df_M_1_list_POI_i=data.frame(M_1_robot,M_1_AIC,M_1_zona,M_1_list,M_1_estimate,M_1_p_valor,n_var,n_var_signif)
  df_M_1_list_POI=rbind(df_M_1_list_POI,df_M_1_list_POI_i)
  
  assign(paste("M_1","zona",zona,sep="_",collapse = ""),model_mezcla)
  
  
}
M_1_list_unicos=M_1_list[!duplicated(M_1_list)]

df_M_1_list_POI$M_1_AIC=round(df_M_1_list_POI$M_1_AIC,0)
df_M_1_list_POI$M_1_robot_zona=paste(df_M_1_list_POI$M_1_robot,df_M_1_list_POI$M_1_zona)
df_M_1_list_POI_unicos=df_M_1_list_POI[!duplicated(c(df_M_1_list_POI$M_1_robot_zona)),]
df_M_1_list_POI_unicos=df_M_1_list_POI_unicos[ , -which(names(df_M_1_list_POI_unicos) %in% c("M_1_estimate","M_1_p_valor","M_1_robot_zona"))]
df_M_1_mezclas_POI=df_M_1_list_POI_unicos[!duplicated(df_M_1_list_POI_unicos$M_1_zona, fromLast = TRUE),]

}

#saveRDS(df_M_1_list_POI_unicos,file="C:/Users/usuario/Desktop/IÒigo/list_M_1_POI.Rda")
#list_M_1_POI=readRDS("list_M_1_POI.Rda")


##-------------------------------------------------------------------## 
##                                                                   ## 
##-------------------------------------------------------------------## 



##-------------------------------------------------------------------## 
##                         FRECUENCIA                                ## 
##-------------------------------------------------------------------##


##-------------------------------------------------------------------## 
##                     M_2_B_Zona - BINOMIAL                         ## 
##-------------------------------------------------------------------## 

  {
aux.y="Num_defectos_BIN"
aux.family="binomial"
  
list_M_2_BIN=list()

for (zona in zonas) {
  
  M_2_zona=c()
  AIC_glm=c()
  n_var_glm=c()
  var_glm=c()
  AIC_step_glm=c()
  n_var_step_glm=c()
  var_step_glm=c()
  M_2_df=as.data.frame(cbind(M_2_zona,AIC_glm,n_var_glm,var_glm,AIC_step_glm,n_var_step_glm,var_step_glm))
  

  M_2_zona=zona
  aux.df=na.omit(listBasemodelozona_BIN[[zona]])
  aux.df_variables=aux.df[,!(is.element(colnames(aux.df),"Num_defectos_BIN"))]
  aux.df_variables=sapply(aux.df_variables,function(x){as.numeric(x)})
  
  vector_sd=apply(aux.df_variables,2,sd,na.rm=TRUE)
  vector_medias=apply(aux.df_variables,2,mean,na.rm=TRUE)
  vector_coeficiente_variacion=vector_sd/vector_medias
  aux.position.col=order( as.numeric(vector_coeficiente_variacion), decreasing = T)
  
  # steps=(length(vector_coeficiente_variacion)/3)
  # n_steps=round(c(steps,2*steps,3*steps),0) #3 steps inicial, intermedio y final
  # 
  for (i in 1:length(vector_coeficiente_variacion)) {
    
    aux.var=names(vector_coeficiente_variacion[(aux.position.col[1:i])])
    aux.var.list=paste("(",gsub("Y","",aux.var), ")",collapse = "+")
    aux.formula=paste(aux.y, aux.var.list, sep=" ~ ")
    
    aux.model=glm(aux.formula, data = aux.df, family=aux.family )
    aux.step.model=step(aux.model)
    # if(any(i==n_steps)){aux.step.model=step(aux.model)
    # 
    # 
    # }else{
    #   AIC_step_glm=NA
    #   n_var_step_glm=NA
    #   var_step_glm=NA}
    AIC_step_glm=aux.step.model$aic
    n_var_step_glm=nrow(summary(aux.step.model)$coefficients)
    var_step_glm=formula(aux.step.model)[3]
    
    
    AIC_glm=aux.model$aic
    n_var_glm=nrow(summary(aux.model)$coefficients)
    var_glm=aux.model$formula
    
    
    
    M_2_df_i=as.data.frame(cbind(M_2_zona,AIC_glm,n_var_glm,var_glm,AIC_step_glm,n_var_step_glm,var_step_glm))
    M_2_df=rbind(M_2_df,M_2_df_i)
  }
  
  list_M_2_BIN[[zona]]=M_2_df
}

}

# saveRDS(list_M_2_BIN,file="C:/Users/usuario/Desktop/IÒigo/list_M_2_BIN.Rda")
# list_M_2_BIN=readRDS("C:/Users/usuario/Desktop/IÒigo/list_M_2_BIN.Rda")


##-------------------------------------------------------------------## 
##                                                                   ## 
##-------------------------------------------------------------------## 



##-------------------------------------------------------------------## 
##                     M_2_P_Zona - POISSON                          ## 
##-------------------------------------------------------------------## 

  {

aux.y="Num_defectos_POI"
aux.family="poisson"

list_M_2_POI=list()

for (zona in zonas) {
  
  AIC_glm=c()
  n_var_glm=c()
  var_glm=c()
  AIC_step_glm=c()
  n_var_step_glm=c()
  var_step_glm=c()
  M_2_df=as.data.frame(cbind(AIC_glm,n_var_glm,var_glm,AIC_step_glm,n_var_step_glm,var_step_glm))
  

  aux.df=na.omit(listBasemodelozona_POI[[zona]])
  aux.df_variables=aux.df[,!(is.element(colnames(aux.df),"Num_defectos_POI"))]
  aux.df_variables=sapply(aux.df_variables,function(x){as.numeric(x)})
  
  vector_sd=apply(aux.df_variables,2,sd,na.rm=TRUE)
  vector_medias=apply(aux.df_variables,2,mean,na.rm=TRUE)
  vector_coeficiente_variacion=vector_sd/vector_medias
  aux.position.col=order( as.numeric(vector_coeficiente_variacion), decreasing = T)
  
  # steps=(length(vector_coeficiente_variacion)/3)
  # n_steps=round(c(steps,2*steps,3*steps),0) #3 steps inicial, intermedio y final
  # 
  for (i in 1:length(vector_coeficiente_variacion)) {
    
    aux.var=names(vector_coeficiente_variacion[(aux.position.col[1:i])])
    aux.var.list=paste("(",gsub("Y","",aux.var), ")",collapse = "+")
    aux.formula=paste(aux.y, aux.var.list, sep=" ~ ")
    
    aux.model=glm(aux.formula, data = aux.df, family=aux.family )
    aux.step.model=step(aux.model)
    # if(any(i==n_steps)){aux.step.model=step(aux.model)
    # 
    # 
    # }else{
    #   AIC_step_glm=NA
    #   n_var_step_glm=NA
    #   var_step_glm=NA}
     AIC_step_glm=aux.step.model$aic
    n_var_step_glm=nrow(summary(aux.step.model)$coefficients)
    var_step_glm=formula(aux.step.model)[3]
    
    AIC_glm=aux.model$aic
    n_var_glm=nrow(summary(aux.model)$coefficients)
    var_glm=aux.model$formula
    
    
    
    M_2_df_i=as.data.frame(cbind(AIC_glm,n_var_glm,var_glm,AIC_step_glm,n_var_step_glm,var_step_glm))
    M_2_df=rbind(M_2_df,M_2_df_i)
  }
  
  list_M_2_POI[[zona]]=M_2_df
}

}

# saveRDS(list_M_2_POI,file="C:/Users/usuario/Desktop/IÒigo/list_M_2_POI.Rda")
# list_M_2_POI=readRDS("C:/Users/usuario/Desktop/IÒigo/list_M_2_POI.Rda")

##-------------------------------------------------------------------## 
##                                                                   ## 
##-------------------------------------------------------------------## 



##~~~~~~~~~~~~~~~~~~~~~*~*~*~ZONA 1~*~*~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 



##-------------------------------------------------------------------## 
##                       COMPARATIVA - ZONA 1                        ## 
##-------------------------------------------------------------------## 


##-------------------------------------------------------------------## 
##                       COMPARATIVA - M_1/M_2 BINOMIAL              ## 
##-------------------------------------------------------------------## 

  {
  zona=1

aux.mezcla=(subset(df_M_1_list_BIN_unicos, is.element(df_M_1_list_BIN_unicos$M_1_zona,zona)))
min_M_1=min(aux.mezcla$M_1_AIC,na.rm = T)             
max_M_1=max(aux.mezcla$M_1_AIC,na.rm = T)    

prueba=list_M_2_BIN[[zona]]
prueba$n_var_glm=as.numeric(prueba$n_var_glm)-1
prueba$n_var_step_glm=as.numeric(prueba$n_var_step_glm)-1

min_M_2=min(as.numeric(prueba$AIC_glm))
max_M_2=max(as.numeric(prueba$AIC_glm))
max.x=max(as.numeric(prueba$n_var_glm))+max(as.numeric(prueba$n_var_step_glm))/2

aux.min=min(min_M_1,min_M_2)
aux.max=max(max_M_1,max_M_2)         
                       
plot(prueba$n_var_glm,prueba$AIC_glm,lwd=2, xaxt = "n",xlab="N∫ VARIABLES",ylab="AIC",xlim = c(0,max.x), ylim = c(0.995*aux.min,1.005*aux.max))
lines(prueba$n_var_glm,prueba$AIC_step_glm,lwd=2,col=aux.colours[2])
points(prueba$n_var_glm,prueba$AIC_step_glm,lwd=2,col=aux.colours[2], cex=as.numeric(prueba$n_var_step_glm))

aux.sub.M_1=paste("M_1_",aux.mezcla[-nrow(aux.mezcla),]$M_1_robot)
colfunc = colorRampPalette(c( "#2297E6", "#28E2E5"))
aux.col.mezcla=c(colfunc(length(aux.sub.M_1)),aux.colours[3])
points(aux.mezcla$n_var,aux.mezcla$M_1_AIC,lwd=2,pch=c(1,8)[is.na(aux.mezcla$M_1_list)+1],col=aux.col.mezcla, cex=(aux.mezcla$n_var_signif+c(0,1)[is.na(aux.mezcla$M_1_list)+1]))

title(main = paste("BINOMIAL Zona - ", zona))
legend("topright",legend = c(aux.sub.M_1,"M_1","M_2", "M_2_STEP"),pch=1,pt.lwd = 2 ,pt.cex=2,col=c(aux.col.mezcla,"black",aux.colours[2]), bty = "n")
axis(1, at = seq(0, max.x, by = 1),labels = FALSE)
axis(1, at = seq(0, max.x, by = 5))
segments(x0=aux.mezcla$n_var,x1=aux.mezcla$n_var,y0=0,y1=aux.mezcla$M_1_AIC, col=aux.colours[5], lty=2, lwd=2)
axis(1,at=aux.mezcla$n_var,col.ticks =aux.colours[5],col.axis  =aux.colours[5])

legend("topleft",title = "N∫ Var. Significativas",legend = c("0","1", "3","5"), pch=c(8,1,1,1), pt.lwd = 2.5, pt.cex=c(1,1,3,5), col = "grey45", bty = "n",horiz = T,x.intersp=c(1,1,1.25,1.75))

# basura=prueba[,-which(names(prueba) %in% c("M_2_zona","var_glm","var_step_glm"))]
# basura=lapply(basura,as.numeric)
# basura$AIC_glm=round(basura$AIC_glm,0)
# basura$AIC_step_glm=round(basura$AIC_step_glm,0)
# basura=as.data.frame(basura)
# obten_tabla(basura, hline=TRUE, rownames=TRUE)
}

##-------------------------------------------------------------------## 
##                       COMPARATIVA - M_1/M_2 POISSON               ## 
##-------------------------------------------------------------------## 

  {
  zona=1

aux.mezcla=(subset(df_M_1_list_POI_unicos, is.element(df_M_1_list_POI_unicos$M_1_zona,zona)))
min_M_1=min(aux.mezcla$M_1_AIC,na.rm = T)             
max_M_1=max(aux.mezcla$M_1_AIC,na.rm = T)  

prueba=list_M_2_POI[[zona]]
prueba$n_var_glm=as.numeric(prueba$n_var_glm)-1
prueba$n_var_step_glm=as.numeric(prueba$n_var_step_glm)-1

min_M_2=min(as.numeric(prueba$AIC_glm))
max_M_2=max(as.numeric(prueba$AIC_glm))
max.x=max(as.numeric(prueba$n_var_glm))+max(as.numeric(prueba$n_var_step_glm))/2

aux.min=min(min_M_1,min_M_2)
aux.max=max(max_M_1,max_M_2)         

plot(prueba$n_var_glm,prueba$AIC_glm,lwd=2, xaxt = "n",xlab="N∫ VARIABLES",ylab="AIC",xlim = c(0,max.x), ylim = c(0.995*aux.min,1.005*aux.max))
lines(prueba$n_var_glm,prueba$AIC_step_glm,lwd=2,col=aux.colours[2])
points(prueba$n_var_glm,prueba$AIC_step_glm,lwd=2,col=aux.colours[2], cex=as.numeric(prueba$n_var_step_glm))

aux.sub.M_1=paste("M_1_",aux.mezcla[-nrow(aux.mezcla),]$M_1_robot)
colfunc = colorRampPalette(c( "#2297E6", "#28E2E5"))
aux.col.mezcla=c(colfunc(length(aux.sub.M_1)),aux.colours[3])
points(aux.mezcla$n_var,aux.mezcla$M_1_AIC,lwd=2,pch=c(1,8)[is.na(aux.mezcla$M_1_list)+1],col=aux.col.mezcla, cex=(aux.mezcla$n_var_signif+c(0,1)[is.na(aux.mezcla$M_1_list)+1]))

title(main = paste("POISSON Zona - ", zona))
legend("topright",legend = c(aux.sub.M_1,"M_1","M_2", "M_2_STEP"),pch=1,pt.lwd = 2 ,pt.cex=2,col=c(aux.col.mezcla,"black",aux.colours[2]), bty = "n")
axis(1, at = seq(0, max.x, by = 1),labels = FALSE)
axis(1, at = seq(0, max.x, by = 5))
segments(x0=aux.mezcla$n_var,x1=aux.mezcla$n_var,y0=0,y1=aux.mezcla$M_1_AIC, col=aux.colours[5], lty=2, lwd=2)
axis(1,at=aux.mezcla$n_var,col.ticks =aux.colours[5],col.axis  =aux.colours[5])

legend("topleft",title = "N∫ Var. Significativas",legend = c("0","1", "3","5"), pch=c(8,1,1,1), pt.lwd = 2.5, pt.cex=c(1,1,3,5), col = "grey45", bty = "n",horiz = T,x.intersp=c(1,1,1.25,1.75))
  
# basura=prueba[,-which(names(prueba) %in% c("var_glm","var_step_glm"))]
# basura=lapply(basura,as.numeric)
# basura$AIC_glm=round(basura$AIC_glm,0)
# basura$AIC_step_glm=round(basura$AIC_step_glm,0)
# basura=as.data.frame(basura)
# obten_tabla(basura, hline=TRUE, rownames=TRUE)
}

##-------------------------------------------------------------------## 
##                       MODELOS SELECCIONADOS POR ZONA              ## 
##-------------------------------------------------------------------## 

  {
  zona=1

  #M_1_P  
    
aux.df=na.omit(listBasemodelozona_POI[[zona]])
aux.y="Num_defectos_POI"
aux.family="poisson"
aux.var_x=formula(paste(aux.y,"~",M_1_df_poisson$M_1_list))
aux.M_1_df_poisson=glm(aux.var_x,data=aux.df,family=aux.family,control = glm.control(maxit=100))

  #M_1_B 

aux.df=na.omit(listBasemodelozona_BIN[[zona]])
aux.y="Num_defectos_BIN"
aux.family="binomial"
aux.var_x=formula(paste(aux.y,"~",M_1_df_binomial$M_1_list))
aux.M_1_df_binomial=glm(aux.var_x,data=aux.df,family=aux.family,control = glm.control(maxit=100))

probs = predict(aux.M_1_df_binomial,aux.df, type="response")
real=aux.df$Num_defectos_BIN
prediobj = prediction(probs,real)
perf = performance(prediobj, "tpr","fpr")
M_1_auc = as.numeric(performance(prediobj,"auc")@y.values)


  #M_2_P 

M_2_df_poisson=list_M_2_POI[[zona]]
aux.df=na.omit(listBasemodelozona_POI[[zona]])
aux.y="Num_defectos_POI"
aux.family="poisson"
aux.var_x=formula(paste(aux.y,"~",as.character((M_2_df_poisson$var_step_glm[is.element(M_2_df_poisson$AIC_step_glm,min(as.numeric(M_2_df_poisson$AIC_step_glm)))])[1])))
aux.M_2_df_poisson=glm(aux.var_x,data=aux.df,family=aux.family,control = glm.control(maxit=100))

  #M_2_B

M_2_df_binomial=list_M_2_BIN[[zona]]
aux.df=na.omit(listBasemodelozona_BIN[[zona]])
aux.y="Num_defectos_BIN"
aux.family="binomial"
aux.var_x=formula(paste(aux.y,"~",as.character((M_2_df_binomial$var_step_glm[is.element(M_2_df_binomial$AIC_step_glm,min(as.numeric(M_2_df_binomial$AIC_step_glm)))])[1])))
aux.M_2_df_binomial=glm(aux.var_x,data=aux.df,family=aux.family,control = glm.control(maxit=100))

probs = predict(aux.M_2_df_binomial,aux.df, type="response")
real=aux.df$Num_defectos_BIN
prediobj = prediction(probs,real)
perf = performance(prediobj, "tpr","fpr")
M_2_auc = as.numeric(performance(prediobj,"auc")@y.values)


# destination = 'C:\\Users\\usuario\\Desktop\\Graficos 2023\\COMPARATIVA.pdf'
# pdf(file=destination)

aux.plot.df=list_M_2_POI[[zona]]
plot_M_2(aux.plot.df)
# ggsave("C:\\Users\\usuario\\Desktop\\Graficos 2023\\list_M_2_POI.png", plt,width=9, height=12.6)
# dev.off()

aux.plot.df=list_M_2_BIN[[zona]]
plot_M_2(aux.plot.df)
# ggsave("C:\\Users\\usuario\\Desktop\\Graficos 2023\\list_M_2_BIN.png", plt,width=9, height=12.6)
# dev.off()


}

##-------------------------------------------------------------------## 
##                                                                   ## 
##-------------------------------------------------------------------## 



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ * * * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~HURDLE~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 



##-------------------------------------------------------------------## 
##                     M_1_H_Zona - HURDLE                           ## 
##-------------------------------------------------------------------## 

  {
  zona=1
  M_1_df_poisson=subset(df_M_1_mezclas_POI, is.element(df_M_1_mezclas_POI$M_1_zona,zona))
  M_1_df_binomial=subset(df_M_1_mezclas_BIN, is.element(df_M_1_mezclas_BIN$M_1_zona,zona))
  
  aux.df=na.omit(listBasemodelozona[[zona]])
  aux.df=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
  aux.y="Num_defectos"
  
  aux.var_x=M_1_df_poisson$M_1_list
  aux.var_z=M_1_df_binomial$M_1_list
  
  M_1_H=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
  M_1_H
  summary(M_1_H)
  }

##-------------------------------------------------------------------## 
##                     M_2_H_Zona - HURDLE                           ## 
##-------------------------------------------------------------------## 

  {
  zona=1
  M_2_df_poisson=list_M_2_POI[[zona]]
  M_2_df_binomial=list_M_2_BIN[[zona]]
  
  aux.df=na.omit(listBasemodelozona[[zona]])
  aux.df=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
  aux.y="Num_defectos"
  
  aux.var_x=as.character((M_2_df_poisson$var_step_glm[is.element(M_2_df_poisson$AIC_step_glm,min(as.numeric(M_2_df_poisson$AIC_step_glm)))])[1])
  aux.var_z=as.character((M_2_df_binomial$var_step_glm[is.element(M_2_df_binomial$AIC_step_glm,min(as.numeric(M_2_df_binomial$AIC_step_glm)))])[1])
  
  M_2_H=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
  M_2_H
  summary(M_2_H)
  }

##-------------------------------------------------------------------## 
##                     M_2_H_Zona - !NA, 0.1, 0.01 Y 0.001           ## 
##-------------------------------------------------------------------## 

  {
zona=1
aux.df=na.omit(listBasemodelozona[[zona]])
aux.df=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
aux.y="Num_defectos"

aux.model=M_2_H    
count_NA=any(is.na((((summary(aux.model))$coefficients)$count)[,4] ))
zero_NA=any(is.na((((summary(aux.model))$coefficients)$zero)[,4] ))
any_NA=any(count_NA,zero_NA)
while(any_NA==TRUE){
  
  basura=((summary(aux.model))$coefficients)$count
  basura=basura[!is.na(basura[,4]),]
  basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
  aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
  
  basura=((summary(aux.model))$coefficients)$zero
  basura=basura[!is.na(basura[,4]),]
  basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
  aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
  
  M_2_H_sin_NA=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
  aux.model=M_2_H_sin_NA
  count_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$count)[,4] ))
  zero_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$zero)[,4] ))
  any_NA=any(count_NA,zero_NA)
  
} 
M_2_H_0.1=M_2_H_sin_NA
summary(M_2_H_0.1)

aux.model=M_2_H_0.1    
p_limite=0.01
count_p_valor=any(((((summary(aux.model))$coefficients)$count)[,4] )>p_limite)
zero_p_valor=any(((((summary(aux.model))$coefficients)$zero)[,4] )>p_limite)
any_p_valor=any(count_p_valor,zero_p_valor)
while(any_p_valor==TRUE){
  
  basura=((summary(aux.model))$coefficients)$count
  basura=basura[(basura[,4])<p_limite,]
  basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
  aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
  
  basura=((summary(aux.model))$coefficients)$zero
  basura=basura[(basura[,4])<p_limite,]
  basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
  aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
  
  aux.model=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
  
  count_NA=any(is.na((((summary(aux.model))$coefficients)$count)[,4] ))
  zero_NA=any(is.na((((summary(aux.model))$coefficients)$zero)[,4] ))
  any_NA=any(count_NA,zero_NA)
  while(any_NA==TRUE){
    
    basura=((summary(aux.model))$coefficients)$count
    basura=basura[!is.na(basura[,4]),]
    basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
    aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
    
    basura=((summary(aux.model))$coefficients)$zero
    basura=basura[!is.na(basura[,4]),]
    basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
    aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
    
    M_2_H_sin_NA=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
    aux.model=M_2_H_sin_NA
    count_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$count)[,4] ))
    zero_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$zero)[,4] ))
    any_NA=any(count_NA,zero_NA)
    
  } 
  
  count_p_valor=any(((((summary(aux.model))$coefficients)$count)[,4] )>p_limite)
  zero_p_valor=any(((((summary(aux.model))$coefficients)$zero)[,4] )>p_limite)
  any_p_valor=any(count_p_valor,zero_p_valor)
  
}
M_2_H_0.01=aux.model

aux.model=M_2_H_0.01    
p_limite=0.001
count_p_valor=any(((((summary(aux.model))$coefficients)$count)[,4] )>p_limite)
zero_p_valor=any(((((summary(aux.model))$coefficients)$zero)[,4] )>p_limite)
any_p_valor=any(count_p_valor,zero_p_valor)
while(any_p_valor==TRUE){
  
  basura=((summary(aux.model))$coefficients)$count
  basura=basura[(basura[,4])<p_limite,]
  basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
  aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
  
  basura=((summary(aux.model))$coefficients)$zero
  basura=basura[(basura[,4])<p_limite,]
  basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
  aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
  
  aux.model=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
  
  count_NA=any(is.na((((summary(aux.model))$coefficients)$count)[,4] ))
  zero_NA=any(is.na((((summary(aux.model))$coefficients)$zero)[,4] ))
  any_NA=any(count_NA,zero_NA)
  while(any_NA==TRUE){
    
    basura=((summary(aux.model))$coefficients)$count
    basura=basura[!is.na(basura[,4]),]
    basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
    aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
    
    basura=((summary(aux.model))$coefficients)$zero
    basura=basura[!is.na(basura[,4]),]
    basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
    aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
    
    M_2_H_sin_NA=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
    aux.model=M_2_H_sin_NA
    count_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$count)[,4] ))
    zero_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$zero)[,4] ))
    any_NA=any(count_NA,zero_NA)
    
  } 
  
  count_p_valor=any(((((summary(aux.model))$coefficients)$count)[,4] )>p_limite)
  zero_p_valor=any(((((summary(aux.model))$coefficients)$zero)[,4] )>p_limite)
  any_p_valor=any(count_p_valor,zero_p_valor)
  
}
M_2_H_0.001=aux.model
}

##-------------------------------------------------------------------## 
##                     M_1_H_Zona - TABLA COMPARATIVA                ## 
##-------------------------------------------------------------------## 

  {
  df_comparativa_M_1_H=as.data.frame(round(((summary(M_1_H))$coefficients)$count[,c(1,3,4)],4))
  df_comparativa_M_1_H[nrow(df_comparativa_M_1_H)+1,] = "-"
  aux.df_comparativa_M_1_H=as.data.frame(round(((summary(M_1_H))$coefficients)$zero[,c(1,3,4)],4))
  df_comparativa_M_1_H=rbind(df_comparativa_M_1_H,aux.df_comparativa_M_1_H)
  # obten_tabla(df_comparativa_M_1_H, hline=TRUE, rownames=TRUE)
  
  df_comparativa_M_2_H=as.data.frame(round(((summary(M_2_H))$coefficients)$count[,c(1,3,4)],4))
  df_comparativa_M_2_H[nrow(df_comparativa_M_2_H)+1,] = "-"
  aux.df_comparativa_M_2_H=as.data.frame(round(((summary(M_2_H))$coefficients)$zero[,c(1,3,4)],4))
  df_comparativa_M_2_H=rbind(df_comparativa_M_2_H,aux.df_comparativa_M_2_H)
  # obten_tabla(df_comparativa_M_2_H, hline=TRUE, rownames=TRUE)
  }

##-------------------------------------------------------------------## 
##                     M_2_H_Zona - TABLA COMPARATIVA                ## 
##-------------------------------------------------------------------## 

  {
    
aux.model.0.1=as.data.frame(round(((summary(M_2_H_0.1))$coefficients)$count[,c(1,3,4)],4))
aux.model.0.01=as.data.frame(round(((summary(M_2_H_0.01))$coefficients)$count[,c(1,3,4)],4))
aux.model.0.001=as.data.frame(round(((summary(M_2_H_0.001))$coefficients)$count[,c(1,3,4)],4))
df_comparativa=merge(aux.model.0.1,aux.model.0.01, by= 'row.names', all = TRUE)
rownames(df_comparativa)=df_comparativa$Row.names
df_comparativa=df_comparativa[,-1]
df_comparativa=merge(df_comparativa,aux.model.0.001, by= 'row.names', all = TRUE)

rownames(df_comparativa)=paste("count", 1:nrow((df_comparativa)))
df_comparativa[nrow(df_comparativa)+1,] = "-"
aux.divisor=nrow(df_comparativa)
  
aux.model.0.1=as.data.frame(round(((summary(M_2_H_0.1))$coefficients)$zero[,c(1,3,4)],4))
aux.model.0.01=as.data.frame(round(((summary(M_2_H_0.01))$coefficients)$zero[,c(1,3,4)],4))
aux.model.0.001=as.data.frame(round(((summary(M_2_H_0.001))$coefficients)$zero[,c(1,3,4)],4))
aux.df_comparativa=merge(aux.model.0.1,aux.model.0.01, by= 'row.names', all = TRUE)
rownames(aux.df_comparativa)=aux.df_comparativa$Row.names
aux.df_comparativa=aux.df_comparativa[,-1]
aux.df_comparativa=merge(aux.df_comparativa,aux.model.0.001, by= 'row.names', all = TRUE)

rownames(aux.df_comparativa)=paste("zero", 1:nrow((aux.df_comparativa)))
df_comparativa=rbind(df_comparativa,aux.df_comparativa)
colnames(df_comparativa)=c("Variables",  "Estimate 0.1", "z value 0.1",  "Pr(>|z|) 0.1", "Estimate 0.01", "z value 0.01",  "Pr(>|z|) 0.01", "Estimate 0.001", "z value 0.001",  "Pr(>|z|) 0.001")

df_comparativa[aux.divisor,2]=c("P(Y>0)")

}

##-------------------------------------------------------------------## 
##                     M_2_H_0.001 - AN¡LISIS                        ## 
##-------------------------------------------------------------------## 

  {
zona=1
aux.df=na.omit(listBasemodelozona[[zona]])
aux.df=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
aux.y="Num_defectos"

#Aire_Guia_Media.BCIN.Robot.1.2

{
plot(aux.df$Aire_Guia_Media.BCIN.Robot.1.2,aux.df$Num_defectos, col=aux.colours[2],xlab="Aire_Guia_Media.BCIN.Robot.1.2", ylab="Numero de defectos")
aux.predict=predict(glm((aux.df$Num_defectos)~(aux.df$Aire_Guia_Media.BCIN.Robot.1.2)))
lines((aux.df$Aire_Guia_Media.BCIN.Robot.1.2),aux.predict, col=aux.colours[3])

plot(aux.df$Aire_Guia_Media.BCIN.Robot.1.2,aux.df$Num_defectos>1, col=aux.colours[2],xlab="Aire_Guia_Media.BCIN.Robot.1.2", ylab="Numero de defectos")
aux.predict=predict(glm((aux.df$Num_defectos>1)~(aux.df$Aire_Guia_Media.BCIN.Robot.1.2)))
lines((aux.df$Aire_Guia_Media.BCIN.Robot.1.2),aux.predict, col=aux.colours[3])



aux.predict=predict(M_2_H_0.001, type=c("prob"))
prob_1=1-aux.predict[,c(1)]
prob_3=apply(aux.predict[,-c(0:3)],1,sum)
aux.max=mean(1.2*prob_1)
y_lim=c(0,aux.max)

plot(aux.df$Aire_Guia_Media.BCIN.Robot.1.2,prob_1, ylim=y_lim, col=aux.colours[1],xlab="Aire_Guia_Media.BCIN.Robot.1.2", ylab="Probabilidad de defectos")
lines(smooth.spline(aux.df$Aire_Guia_Media.BCIN.Robot.1.2,prob_1), col = aux.colours[1], lwd = 2)
abline(lm(prob_1~aux.df$Aire_Guia_Media.BCIN.Robot.1.2),col=aux.colours[3], lwd = 2) 

points(aux.df$Aire_Guia_Media.BCIN.Robot.1.2,prob_3, ylim=y_lim, col=aux.colours[2])
lines(smooth.spline(aux.df$Aire_Guia_Media.BCIN.Robot.1.2,prob_3), col = aux.colours[2], lwd = 2)
abline(lm(prob_3~aux.df$Aire_Guia_Media.BCIN.Robot.1.2),col=aux.colours[3], lwd = 2) 

title(main = paste("M_2_H_0.001 - Aire_Guia_Media.BCIN.Robot.1.2"))
legend("topright",legend = c("P(Y>1)","P(Y>3)"),lwd = 2 ,pt.cex=2,col=aux.colours[1:2], bty = "n")

}

aux.min=min(aux.df$Aire_Guia_Media.BCIN.Robot.1.2)
aux.max=max(aux.df$Aire_Guia_Media.BCIN.Robot.1.2)
Aire_Guia_Media.BCIN.Robot.1.2=seq(aux.min,aux.max,by=1)
# aux.min=min(aux.df$VolumenPintura.BC1.Robot.1.2)
# aux.max=max(aux.df$VolumenPintura.BC1.Robot.1.2)
# VolumenPintura.BC1.Robot.1.2=seq(aux.min,aux.max,by=1)
VolumenPintura.BC1.Robot.1.2=mean(aux.df$VolumenPintura.BC1.Robot.1.2)

#CycleTime.BC1.Robot.1.2 en MÕNIMO
{
CycleTime.BC1.Robot.1.2=min(aux.df$CycleTime.BC1.Robot.1.2)
aux.df.modif=data.frame(Aire_Guia_Media.BCIN.Robot.1.2,CycleTime.BC1.Robot.1.2,VolumenPintura.BC1.Robot.1.2)

aux.predict=predict(M_2_H_0.001,newdata=aux.df.modif,type="prob")
prob_1=1-aux.predict[,c(1)]
prob_3=apply(aux.predict[,-c(0:3)],1,sum)

plot(aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2,prob_1, ylim=y_lim, col=aux.colours[1],xlab="Aire_Guia_Media.BCIN.Robot.1.2", ylab="Probabilidad de defectos")
lines(smooth.spline(aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2,prob_1), col = aux.colours[1], lwd = 2)
abline(lm(prob_1~aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2),col=aux.colours[3], lwd = 2) 

points(aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2,prob_3, ylim=y_lim, col=aux.colours[2])
lines(smooth.spline(aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2,prob_3), col = aux.colours[2], lwd = 2)
abline(lm(prob_3~aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2),col=aux.colours[3], lwd = 2) 

# title(main = paste("M_2_H_0.001 - Aire_Guia_Media.BCIN.Robot.1.2 \n CycleTime.BC1.Robot.1.2 en MÕNIMO"))
# legend("topright",legend = c("P(Y>1)","P(Y>3)"),lwd = 2 ,pt.cex=2,col=aux.colours[1:2], bty = "n")
}

#CycleTime.BC1.Robot.1.2 en M¡XIMO
{
CycleTime.BC1.Robot.1.2=max(aux.df$CycleTime.BC1.Robot.1.2)
aux.df.modif=data.frame(Aire_Guia_Media.BCIN.Robot.1.2,CycleTime.BC1.Robot.1.2,VolumenPintura.BC1.Robot.1.2)

aux.predict=predict(M_2_H_0.001,newdata=aux.df.modif,type="prob")
prob_1=1-aux.predict[,c(1)]
prob_3=apply(aux.predict[,-c(0:3)],1,sum)

points(aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2,prob_1, ylim=y_lim, col=aux.colours[1],xlab="Aire_Guia_Media.BCIN.Robot.1.2", ylab="Probabilidad de defectos")
lines(smooth.spline(aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2,prob_1), col = aux.colours[1], lwd = 2)
abline(lm(prob_1~aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2),col=aux.colours[3], lwd = 2) 

points(aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2,prob_3, ylim=y_lim, col=aux.colours[4])
lines(smooth.spline(aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2,prob_3), col = aux.colours[2], lwd = 2)
abline(lm(prob_3~aux.df.modif$Aire_Guia_Media.BCIN.Robot.1.2),col=aux.colours[3], lwd = 2) 

title(main = paste("M_2_H_0.001 - Aire_Guia_Media.BCIN.Robot.1.2 \n CycleTime.BC1.Robot.1.2 en MÕNIMO / M¡XIMO"))
legend("topright",legend = c("P(Y>1)","P_min(Y>3)","P_max(Y>3)"),lwd = 2 ,pt.cex=2,col=aux.colours[c(1:2,4)], bty = "n")
}







basura=predict(M_1_H, type=c("response"))
plot(aux.df$CycleTime.BC1.Robot.1.2,basura)
(aux.df[order(basura,decreasing=TRUE)[1:10],c("Num_defectos","Ciclo_limpieza.BC1.Robot.1.2","VolumenPintura.BC1.Robot.1.2", "CycleTime.BC1.Robot.1.2"    )])

#PROB FALLO

basura=predict(M_1_H, type=c("prob"))
plot(aux.df$VolumenPintura.BC1.Robot.1.2,basura[,1])
points(tapply(aux.df$VolumenPintura.BC1.Robot.1.2, aux.df$VolumenPintura.BC1.Robot.1.2, mean),tapply(aux.df$Num_defectos<1, aux.df$VolumenPintura.BC1.Robot.1.2, mean),col="red")

#PROB M¡S DE 3 FALLOS

plot(aux.df$VolumenPintura.BC1.Robot.1.2,basura[,4])
prob_defectos=apply(basura,1,sum)

prob_defectos=apply(basura[,-c(0:2)],1,sum)
plot(aux.df$VolumenPintura.BC1.Robot.1.2,prob_defectos)



summary(M_2_H_0.001)
basura=predict(M_2_H_0.001, type=c("response"))
plot(aux.df$Aire_Guia_Media.BCIN.Robot.1.2,basura)

#PROB FALLO

basura=predict(M_2_H, type=c("prob"))
plot(aux.df$VolumenPintura.BC1.Robot.1.2,basura[,1])
points(tapply(aux.df$VolumenPintura.BC1.Robot.1.2, aux.df$VolumenPintura.BC1.Robot.1.2, mean),tapply(aux.df$Num_defectos<1, aux.df$VolumenPintura.BC1.Robot.1.2, mean),col="red")



#COMPARACION M_1_H y M_2_H

basura1=predict(M_1_H,type="response")
basura2=predict(M_2_H,type="response")
plot(basura1,basura2) 
 

}

##-------------------------------------------------------------------## 
##                                                                   ## 
##-------------------------------------------------------------------## 




##~~~~~~~~~~~~~~~~~~~~*~*~*~TODAS LAS ZONAS~*~*~*~~~~~~~~~~~~~~~~~~~~## 




##-------------------------------------------------------------------## 
##                          M_1_B / M_1_P                            ## 
##-------------------------------------------------------------------## 

  {
list_M_1_B=list()
list_M_1_P=list()

for (zona in zonas) {
  
  M_1_df_poisson=subset(df_M_1_mezclas_POI, is.element(df_M_1_mezclas_POI$M_1_zona,zona))
  M_1_df_binomial=subset(df_M_1_mezclas_BIN, is.element(df_M_1_mezclas_BIN$M_1_zona,zona))
  
  #M_1_P  
  
  aux.df=na.omit(listBasemodelozona_POI[[zona]])
  aux.y="Num_defectos_POI"
  aux.family="poisson"
  if(is.na(M_1_df_poisson$M_1_list)){ 
    list_M_1_P[[zona]]=NA}else{
      aux.var_x=formula(paste(aux.y,"~",M_1_df_poisson$M_1_list))
      aux.M_1_df_poisson=glm(aux.var_x,data=aux.df,family=aux.family,control = glm.control(maxit=100))
      list_M_1_P[[zona]]=aux.M_1_df_poisson
    }
  #M_1_B 
  
  aux.df=na.omit(listBasemodelozona_BIN[[zona]])
  aux.y="Num_defectos_BIN"
  aux.family="binomial"
  if(is.na(M_1_df_binomial$M_1_list)){ 
    list_M_1_B[[zona]]=NA}else{
      aux.var_x=formula(paste(aux.y,"~",M_1_df_binomial$M_1_list))
      aux.M_1_df_binomial=glm(aux.var_x,data=aux.df,family=aux.family,control = glm.control(maxit=100))
      list_M_1_B[[zona]]=aux.M_1_df_binomial
    }
}

}

##-------------------------------------------------------------------## 
##                          M_2_B / M_2_P                            ## 
##-------------------------------------------------------------------## 

  {
  
  list_M_2_BIN=readRDS("C:/Users/usuario/Desktop/IÒigo/list_M_2_BIN.Rda")
  list_M_2_POI=readRDS("C:/Users/usuario/Desktop/IÒigo/list_M_2_POI.Rda")
  
  list_M_2_B=list()
  list_M_2_P=list()
  
  for (zona in zonas) {
    
    
    M_2_df_poisson=list_M_2_POI[[zona]]
    M_2_df_binomial=list_M_2_BIN[[zona]]
    
    #M_2_B 
    
    aux.df=na.omit(listBasemodelozona_BIN[[zona]])
    aux.y="Num_defectos_BIN"
    aux.family="binomial"
    
    aux.var_x=formula(paste(aux.y,"~",as.character((M_2_df_binomial$var_step_glm[is.element(M_2_df_binomial$AIC_step_glm,min(as.numeric(M_2_df_binomial$AIC_step_glm)))])[1])))
    aux.M_2_df_binomial=glm(aux.var_x,data=aux.df,family=aux.family,control = glm.control(maxit=100))
    list_M_2_B[[zona]]=aux.M_2_df_binomial
    
    #M_2_P
    
    aux.df=na.omit(listBasemodelozona_POI[[zona]])
    aux.y="Num_defectos_POI"
    aux.family="poisson"
    aux.var_x=formula(paste(aux.y,"~",as.character((M_2_df_poisson$var_step_glm[is.element(M_2_df_poisson$AIC_step_glm,min(as.numeric(M_2_df_poisson$AIC_step_glm)))])[1])))
    aux.M_2_df_poisson=glm(aux.var_x,data=aux.df,family=aux.family,control = glm.control(maxit=100))
    list_M_2_P[[zona]]=aux.M_2_df_poisson
  }
  
  }

##-------------------------------------------------------------------## 
##                  M_1_B / M_1_P - M_2_B / M_2_P                    ## 
##-------------------------------------------------------------------##

  {
  M1=c()
  M2=c()
  aux.a=list_M_1_B
  aux.b=list_M_2_B
  for (zona in zonas) {
    a=aux.a[[zona]]
    b=aux.b[[zona]]
    
    aux.modelos=c(a,b)
    
    if(is.na(a)){aic_a=NA}else{aic_a=round(AIC(a),0)}
    if(is.na(b)){aic_b=NA}else{aic_b=round(AIC(b),0)}
    M1=c(M1,aic_a)
    M2=c(M2,aic_b)  
}
  df_comparativa_BIN=data.frame(M1,M2)
  
  M1=c()
  M2=c()
  aux.a=list_M_1_P
  aux.b=list_M_2_P
  for (zona in zonas) {
    a=aux.a[[zona]]
    b=aux.b[[zona]]
    
    aux.modelos=c(a,b)
    
    if(is.na(a)){aic_a=NA}else{aic_a=round(AIC(a),0)}
    if(is.na(b)){aic_b=NA}else{aic_b=round(AIC(b),0)}
    M1=c(M1,aic_a)
    M2=c(M2,aic_b)  
}
  df_comparativa_POI=data.frame(M1,M2)
  
  }

##-------------------------------------------------------------------## 
##                     M_2_H_list - HURDLE                           ## 
##-------------------------------------------------------------------## 

  {
  list_M_2_H=list()
  for(zona in zonas){
    
    M_2_df_poisson=list_M_2_POI[[zona]]
    M_2_df_binomial=list_M_2_BIN[[zona]]
    
    aux.df=na.omit(listBasemodelozona[[zona]])
    aux.df=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
    aux.y="Num_defectos"
    
    aux.var_x=as.character((M_2_df_poisson$var_step_glm[is.element(M_2_df_poisson$AIC_step_glm,min(as.numeric(M_2_df_poisson$AIC_step_glm)))])[1])
    aux.var_z=as.character((M_2_df_binomial$var_step_glm[is.element(M_2_df_binomial$AIC_step_glm,min(as.numeric(M_2_df_binomial$AIC_step_glm)))])[1])
    aux.var_x = gsub('\n    ',"",aux.var_x)
    aux.var_z = gsub('\n    ',"",aux.var_z)
    
    aux.M_2_H=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
    list_M_2_H[[zona]]=aux.M_2_H
  }
}

##-------------------------------------------------------------------## 
##                     M_2_H_list - !NA, 0.1, 0.01 Y 0.001           ## 
##-------------------------------------------------------------------## 

  {
  list_M_2_H_0.1=list()
  list_M_2_H_0.01=list()
  list_M_2_H_0.001=list()
  
  for(zona in zonas){
    
    aux.df=na.omit(listBasemodelozona[[zona]])
    aux.df=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
    aux.y="Num_defectos"
    
    aux.model=list_M_2_H[[zona]]   
    count_NA=any(is.na((((summary(aux.model))$coefficients)$count)[,4] )[-1])
    zero_NA=any(is.na((((summary(aux.model))$coefficients)$zero)[,4] )[-1])
    
    count_all_NA=all(is.na((((summary(aux.model))$coefficients)$count)[,4] )[-1])
    zero_all_NA=all(is.na((((summary(aux.model))$coefficients)$zero)[,4] )[-1])
    all_NA=any(count_all_NA,zero_all_NA)
    
    if(all_NA==T){
      list_M_2_H_0.1[[zona]]=NA
      list_M_2_H_0.01[[zona]]=NA
      list_M_2_H_0.001[[zona]]=NA
    }else{
      
      any_NA=any(count_NA,zero_NA)
      if(any_NA==FALSE){M_2_H_sin_NA=aux.model}
      while(any_NA==TRUE){
        
        basura=((summary(aux.model))$coefficients)$count
        basura=basura[!is.na(basura[,4]),]
        basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
        aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
        
        basura=((summary(aux.model))$coefficients)$zero
        basura=basura[!is.na(basura[,4]),]
        basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
        aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
        
        M_2_H_sin_NA=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
        aux.model=M_2_H_sin_NA
        count_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$count)[,4] )[-1])
        zero_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$zero)[,4] )[-1])
        any_NA=any(count_NA,zero_NA)
        
      } 
      
      list_M_2_H_0.1[[zona]]=M_2_H_sin_NA
      aux.model=M_2_H_sin_NA    
      
      p_limite=0.01
      count_p_valor=any(((((summary(aux.model))$coefficients)$count)[,4] )[-1]>p_limite)
      zero_p_valor=any(((((summary(aux.model))$coefficients)$zero)[,4] )[-1]>p_limite)
      any_p_valor=any(count_p_valor,zero_p_valor)
      
      count_all_p_valor=all(((((summary(aux.model))$coefficients)$count)[,4] )[-1]>p_limite)
      zero_all_p_valor=all(((((summary(aux.model))$coefficients)$zero)[,4] )[-1]>p_limite)
      all_p_valor=any(count_all_p_valor,zero_all_p_valor)
      
      if(all_p_valor==T){
        list_M_2_H_0.01[[zona]]=NA
        list_M_2_H_0.001[[zona]]=NA
      }else{
        
        while(any_p_valor==TRUE){
          
          basura=((summary(aux.model))$coefficients)$count
          basura=basura[(basura[,4])<p_limite,]
          basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
          aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
          
          basura=((summary(aux.model))$coefficients)$zero
          basura=basura[(basura[,4])<p_limite,]
          basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
          aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
          
          aux.model=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
          
          count_NA=any(is.na((((summary(aux.model))$coefficients)$count)[,4] )[-1])
          zero_NA=any(is.na((((summary(aux.model))$coefficients)$zero)[,4] )[-1])
          any_NA=any(count_NA,zero_NA)
          while(any_NA==TRUE){
            
            basura=((summary(aux.model))$coefficients)$count
            basura=basura[!is.na(basura[,4]),]
            basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
            aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
            
            basura=((summary(aux.model))$coefficients)$zero
            basura=basura[!is.na(basura[,4]),]
            basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
            aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
            
            M_2_H_sin_NA=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
            aux.model=M_2_H_sin_NA
            count_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$count)[,4] )[-1])
            zero_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$zero)[,4] )[-1])
            any_NA=any(count_NA,zero_NA)
            
          } 
          
          count_p_valor=any(((((summary(aux.model))$coefficients)$count)[,4] )[-1]>p_limite)
          zero_p_valor=any(((((summary(aux.model))$coefficients)$zero)[,4] )[-1]>p_limite)
          any_p_valor=any(count_p_valor,zero_p_valor)
          
        }
        list_M_2_H_0.01[[zona]]=aux.model
        
        p_limite=0.001
        count_p_valor=any(((((summary(aux.model))$coefficients)$count)[,4] )[-1]>p_limite)
        zero_p_valor=any(((((summary(aux.model))$coefficients)$zero)[,4] )[-1]>p_limite)
        any_p_valor=any(count_p_valor,zero_p_valor)
        
        count_all_p_valor=all(((((summary(aux.model))$coefficients)$count)[,4] )[-1]>p_limite)
        zero_all_p_valor=all(((((summary(aux.model))$coefficients)$zero)[,4] )[-1]>p_limite)
        all_p_valor=any(count_all_p_valor,zero_all_p_valor)
        
        if(all_p_valor==T){
          list_M_2_H_0.001[[zona]]=NA
        }else{
          
          while(any_p_valor==TRUE){
            
            basura=((summary(aux.model))$coefficients)$count
            basura=basura[(basura[,4])<p_limite,]
            basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
            aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
            
            basura=((summary(aux.model))$coefficients)$zero
            basura=basura[(basura[,4])<p_limite,]
            basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
            aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
            
            aux.model=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
            
            count_NA=any(is.na((((summary(aux.model))$coefficients)$count)[,4] )[-1])
            zero_NA=any(is.na((((summary(aux.model))$coefficients)$zero)[,4] )[-1])
            any_NA=any(count_NA,zero_NA)
            while(any_NA==TRUE){
              
              basura=((summary(aux.model))$coefficients)$count
              basura=basura[!is.na(basura[,4]),]
              basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
              aux.var_x=paste("(",gsub("Y","",basura), ")",collapse = "+")
              
              basura=((summary(aux.model))$coefficients)$zero
              basura=basura[!is.na(basura[,4]),]
              basura=rownames(basura)[!is.element(rownames(basura), "(Intercept)")]
              aux.var_z=paste("(",gsub("Y","",basura), ")",collapse = "+")
              
              M_2_H_sin_NA=crear_modelo_hurdle(aux.df,aux.y,aux.var_x,aux.var_z)
              aux.model=M_2_H_sin_NA
              count_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$count)[,4] ))
              zero_NA=any(is.na((((summary(M_2_H_sin_NA))$coefficients)$zero)[,4] ))
              any_NA=any(count_NA,zero_NA)
              
            } 
            
            count_p_valor=any(((((summary(aux.model))$coefficients)$count)[,4] )[-1]>p_limite)
            zero_p_valor=any(((((summary(aux.model))$coefficients)$zero)[,4] )[-1]>p_limite)
            any_p_valor=any(count_p_valor,zero_p_valor)
            
          }
          list_M_2_H_0.001[[zona]]=aux.model
          
        }
      }
    }
  }
}

##-------------------------------------------------------------------## 
##                  M_2_B / M_2_P - MAPA DE CALOR                    ## 
##-------------------------------------------------------------------## 

destination = 'C:\\Users\\usuario\\Desktop\\Graficos 2023\\mapa_calor.pdf'
pdf(file=destination, width=10, height=8)

 #Count
  
  {
  
  zona=1
  aux.df=na.omit(listBasemodelozona_POI[[zona]])
  prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos_POI"))]
  
  variables=(sapply(strsplit(colnames(prueba),"\\."), `[`, 1))
  variables=variables[!duplicated(variables)]
  robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
  robots_mapa=unique(robots_mapa)
  
  aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=robots_mapa
  
  aux.model=list_M_2_P[[zona]]
  aux.z_values=round((summary(aux.model)$coefficients)[-1,3],4)
  aux.names= gsub('Y',"",as.character(names(aux.z_values)))
  aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
  aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
  for (i in 1:length(aux.z_values)) {
    aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
    aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
    aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
  }
  aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
  mapa=aux.mapa
  
  
  
  for (zona in zonas[-1]){
    aux.df=na.omit(listBasemodelozona_POI[[zona]])
    prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos_POI"))]
    robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
    robots_mapa=unique(robots_mapa)
    
    aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
    rownames(aux.mapa)=variables
    colnames(aux.mapa)=robots_mapa
    
    aux.model=list_M_2_P[[zona]]
    aux.z_values=round((summary(aux.model)$coefficients)[-1,3],4)
    aux.names= gsub('Y',"",as.character(names(aux.z_values)))
    aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
    aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
    for (i in 1:length(aux.z_values)) {
      aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
      aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
      aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
    }
    aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
    colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
    mapa=cbind(mapa,aux.mapa)
  }
  
  aux.mapa_count=as.matrix(mapa)
  aux.max=max(abs(aux.mapa_count), na.rm=T)
  aux.min=min(abs(aux.mapa_count), na.rm=T)
 # aux.mapa_count[is.na(aux.mapa_count)] <-0
}
  
  {
  # dif=10^-(nchar(aux.max)-2)
  # colors = c(seq(-aux.max*1.2,-aux.max/2,length.out = 100),seq(-aux.max/2+dif,aux.max/2-dif,length.out = 200),seq(aux.max/2,aux.max*1.2,length.out = 100))
  colors = c(seq(-10,-5,length.out = 100),seq(-4.9,4.9,length.out = 200),seq(5,10,length.out = 100))
  my_palette <- colorRampPalette(c("darkgreen","forestgreen","yellowgreen","greenyellow","white","yellow","orange","darksalmon","firebrick3"))(n = 399)
  heatmap.2(aux.mapa_count , 
            col=my_palette, 
            breaks=colors , symbreaks=T,
            trace=c("both"), #Puede ser trace=("column","row","both","none")
            tracecol="black",
            Colv = NA, Rowv = NA, dendrogram="none",symm=F, #symm solo T cuando es matriz cuadrada
            cexCol = 1.2,cexRow = 1.2, symkey=T,
            scale="none",na.rm = T,
            density.info="none",
            #margins=c(12,12),
            # lhei=c(0.5,1.7,0.1),lwid =c(0.5,1.7),lmat = rbind(c(0,0),c(4,1),c(2,3)),
            lhei=c(0.2,1.6,0.3),lwid =c(0.25,1,0.12),lmat = rbind(c(2,3,0),c(4,1,0),c(0,0,0)),
            key.par=list(mgp=c(1.5, 0.5, 0),mar=c(30,1,5,1)),
            main = c("M_2_P \n Frecuencia de ocurrencia"),
            na.color="grey")
  }#Mapa de calor count

  #Zero
  
  {
    
    zona=1
    aux.df=na.omit(listBasemodelozona_BIN[[zona]])
    prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
    
    variables=(sapply(strsplit(colnames(prueba),"\\."), `[`, 1))
    variables=variables[!duplicated(variables)]
    robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
    robots_mapa=unique(robots_mapa)
    
    aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
    rownames(aux.mapa)=variables
    colnames(aux.mapa)=robots_mapa
    
    aux.model=list_M_2_B[[zona]]
    aux.z_values=round((summary(aux.model)$coefficients)[-1,3],4)
    aux.names= gsub('Y',"",as.character(names(aux.z_values)))
    aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
    aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
    for (i in 1:length(aux.z_values)) {
      aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
      aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
      aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
    }
    aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
    rownames(aux.mapa)=variables
    colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
    mapa=aux.mapa
    
  
  for (zona in zonas[-1]){
    aux.df=na.omit(listBasemodelozona_BIN[[zona]])
    prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
    robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
    robots_mapa=unique(robots_mapa)
    
    aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
    rownames(aux.mapa)=variables
    colnames(aux.mapa)=robots_mapa
    
    aux.model=list_M_2_B[[zona]]
    aux.z_values=round((summary(aux.model)$coefficients)[-1,3],4)
    aux.names= gsub('Y',"",as.character(names(aux.z_values)))
    aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
    aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
    for (i in 1:length(aux.z_values)) {
      aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
      aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
      aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
    }
    aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
    colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
    mapa=cbind(mapa,aux.mapa)
  }
  
  aux.mapa_zero=as.matrix(mapa)
  aux.max=max(abs(aux.mapa_zero), na.rm=T)
  aux.min=min(abs(aux.mapa_zero), na.rm=T)
  # aux.mapa_zero[is.na(aux.mapa_zero)] <-0
}
  
  {
  # dif=10^-(nchar(aux.max)-2)
  # colors = c(seq(-aux.max*1.2,-aux.max/2,length.out = 100),seq(-aux.max/2+dif,aux.max/2-dif,length.out = 200),seq(aux.max/2,aux.max*1.2,length.out = 100))
  colors = c(seq(-10,-5,length.out = 100),seq(-4.9,4.9,length.out = 200),seq(5,10,length.out = 100))
  my_palette <- colorRampPalette(c("darkgreen","forestgreen","yellowgreen","greenyellow","white","yellow","orange","darksalmon","firebrick3"))(n = 399)
  heatmap.2(aux.mapa_zero , 
            col=my_palette, 
            breaks=colors , symbreaks=T,
            trace=c("both"), #Puede ser trace=("column","row","both","none")
            tracecol="black",
            Colv = NA, Rowv = NA, dendrogram="none",symm=F, #symm solo T cuando es matriz cuadrada
            cexCol = 1.2,cexRow = 1.2, symkey=T,
            scale="none",na.rm = T,
            density.info="none",
            #margins=c(12,12),
            # lhei=c(0.5,1.7,0.1),lwid =c(0.5,1.7),lmat = rbind(c(0,0),c(4,1),c(2,3)),
            lhei=c(0.2,1.6,0.3),lwid =c(0.25,1,0.12),lmat = rbind(c(2,3,0),c(4,1,0),c(0,0,0)),
            key.par=list(mgp=c(1.5, 0.5, 0),mar=c(30,1,5,1)),
            main = c("M_2_B \n Frecuencia de ocurrencia"),
            na.color="grey")
  }#Mapa de calor zeros

##-------------------------------------------------------------------## 
##                M_2_B / M_2_P - MAPA DE CALOR segun p-valor        ## 
##-------------------------------------------------------------------## 

p_valor=0.001

#Count

{
  
  zona=1
  aux.df=na.omit(listBasemodelozona_POI[[zona]])
  prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos_POI"))]
  aux.family="poisson"
  
  variables=(sapply(strsplit(colnames(prueba),"\\."), `[`, 1))
  variables=variables[!duplicated(variables)]
  robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
  robots_mapa=unique(robots_mapa)
  
  aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=robots_mapa
  
  aux.model=list_M_2_P[[zona]]
  terminos_signif=row.names(summary(aux.model)$coefficients[-1,])[summary(aux.model)$coefficients[-1,4]<p_valor]
  if(length(terminos_signif)==0){}else{
  
  terminos_signif=gsub("\n","",paste(gsub("Y","",terminos_signif),collapse="+"))
  formula=formula(paste0("Num_defectos_POI~",terminos_signif))
  aux.model=glm(formula, data=aux.df, family = aux.family,control = glm.control(maxit=100))
  
  aux.z_values=round((summary(aux.model)$coefficients)[-1,3],4)
  aux.names= gsub('Y',"",as.character(names(aux.z_values)))
  aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
  aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
  for (i in 1:length(aux.z_values)) {
    aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
    aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
    aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
  }
  }
  aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
  mapa=aux.mapa
  
  
  
  for (zona in zonas[-1]){
    aux.df=na.omit(listBasemodelozona_POI[[zona]])
    prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos_POI"))]
    robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
    robots_mapa=unique(robots_mapa)
    
    aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
    rownames(aux.mapa)=variables
    colnames(aux.mapa)=robots_mapa
    
    aux.model=list_M_2_P[[zona]]
    terminos_signif=row.names(summary(aux.model)$coefficients[-1,])[summary(aux.model)$coefficients[-1,4]<p_valor]
    if(length(terminos_signif)==0){}else{
      
    terminos_signif=gsub("\n","",paste(gsub("Y","",terminos_signif),collapse="+"))
    formula=formula(paste0("Num_defectos_POI~",terminos_signif))
    aux.model=glm(formula, data=aux.df, family = aux.family,control = glm.control(maxit=100))
    
    aux.z_values=round((summary(aux.model)$coefficients)[-1,3],4)
    aux.names= gsub('Y',"",as.character(names(aux.z_values)))
    aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
    aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
    for (i in 1:length(aux.z_values)) {
      aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
      aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
      aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
    }
    }
    aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
    colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
    mapa=cbind(mapa,aux.mapa)
  }
  
  aux.mapa_count=as.matrix(mapa)
  aux.max=max(abs(aux.mapa_count), na.rm=T)
  aux.min=min(abs(aux.mapa_count), na.rm=T)
  # aux.mapa_count[is.na(aux.mapa_count)] <-0
}

{
  # dif=10^-(nchar(aux.max)-2)
  # colors = c(seq(-aux.max*1.2,-aux.max/2,length.out = 100),seq(-aux.max/2+dif,aux.max/2-dif,length.out = 200),seq(aux.max/2,aux.max*1.2,length.out = 100))
  colors = c(seq(-10,-5,length.out = 100),seq(-4.9,4.9,length.out = 200),seq(5,10,length.out = 100))
  my_palette <- colorRampPalette(c("darkgreen","forestgreen","yellowgreen","greenyellow","white","yellow","orange","darksalmon","firebrick3"))(n = 399)
  heatmap.2(aux.mapa_count , 
            col=my_palette, 
            breaks=colors , symbreaks=T,
            trace=c("both"), #Puede ser trace=("column","row","both","none")
            tracecol="black",
            Colv = NA, Rowv = NA, dendrogram="none",symm=F, #symm solo T cuando es matriz cuadrada
            cexCol = 1.2,cexRow = 1.2, symkey=T,
            scale="none",na.rm = T,
            density.info="none",
            #margins=c(12,12),
            # lhei=c(0.5,1.7,0.1),lwid =c(0.5,1.7),lmat = rbind(c(0,0),c(4,1),c(2,3)),
            lhei=c(0.2,1.6,0.3),lwid =c(0.25,1,0.12),lmat = rbind(c(2,3,0),c(4,1,0),c(0,0,0)),
            key.par=list(mgp=c(1.5, 0.5, 0),mar=c(30,1,5,1)),
            main = c("M_2_P \n Frecuencia de ocurrencia"),
            na.color="grey")
}#Mapa de calor count



#Zero

{
  
  zona=1
  aux.df=na.omit(listBasemodelozona_BIN[[zona]])
  prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
  aux.family="binomial"
  
  variables=(sapply(strsplit(colnames(prueba),"\\."), `[`, 1))
  variables=variables[!duplicated(variables)]
  robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
  robots_mapa=unique(robots_mapa)
  
  aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=robots_mapa
  
  aux.model=list_M_2_B[[zona]]
  terminos_signif=row.names(summary(aux.model)$coefficients[-1,])[summary(aux.model)$coefficients[-1,4]<p_valor]
  if(length(terminos_signif)==0){}else{
    
  terminos_signif=gsub("\n","",paste(gsub("Y","",terminos_signif),collapse="+"))
  formula=formula(paste0("Num_defectos_BIN~",terminos_signif))
  aux.model=glm(formula, data=aux.df, family = aux.family,control = glm.control(maxit=100))
  
  aux.z_values=round((summary(aux.model)$coefficients)[-1,3],4)
  aux.names= gsub('Y',"",as.character(names(aux.z_values)))
  aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
  aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
  for (i in 1:length(aux.z_values)) {
    aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
    aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
    aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
  }
  }
  aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
  mapa=aux.mapa
  
  
  for (zona in zonas[-1]){
    aux.df=na.omit(listBasemodelozona_BIN[[zona]])
    prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos_BIN"))]
    robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
    robots_mapa=unique(robots_mapa)
    
    aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
    rownames(aux.mapa)=variables
    colnames(aux.mapa)=robots_mapa
    
    aux.model=list_M_2_B[[zona]]
    terminos_signif=row.names(summary(aux.model)$coefficients[-1,])[summary(aux.model)$coefficients[-1,4]<p_valor]
    if(length(terminos_signif)==0){}else{
    
    terminos_signif=gsub("\n","",paste(gsub("Y","",terminos_signif),collapse="+"))
    formula=formula(paste0("Num_defectos_BIN~",terminos_signif))
    aux.model=glm(formula, data=aux.df, family = aux.family,control = glm.control(maxit=100))
    
    aux.z_values=round((summary(aux.model)$coefficients)[-1,3],4)
    aux.names= gsub('Y',"",as.character(names(aux.z_values)))
    aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
    aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
    for (i in 1:length(aux.z_values)) {
      aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
      aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
      aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
    }
    }
    aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
    colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
    mapa=cbind(mapa,aux.mapa)
  }
  
  aux.mapa_zero=as.matrix(mapa)
  aux.max=max(abs(aux.mapa_zero), na.rm=T)
  aux.min=min(abs(aux.mapa_zero), na.rm=T)
  # aux.mapa_zero[is.na(aux.mapa_zero)] <-0
}

{
  # dif=10^-(nchar(aux.max)-2)
  # colors = c(seq(-aux.max*1.2,-aux.max/2,length.out = 100),seq(-aux.max/2+dif,aux.max/2-dif,length.out = 200),seq(aux.max/2,aux.max*1.2,length.out = 100))
  colors = c(seq(-10,-5,length.out = 100),seq(-4.9,4.9,length.out = 200),seq(5,10,length.out = 100))
  my_palette <- colorRampPalette(c("darkgreen","forestgreen","yellowgreen","greenyellow","white","yellow","orange","darksalmon","firebrick3"))(n = 399)
  heatmap.2(aux.mapa_zero , 
            col=my_palette, 
            breaks=colors , symbreaks=T,
            trace=c("both"), #Puede ser trace=("column","row","both","none")
            tracecol="black",
            Colv = NA, Rowv = NA, dendrogram="none",symm=F, #symm solo T cuando es matriz cuadrada
            cexCol = 1.2,cexRow = 1.2, symkey=T,
            scale="none",na.rm = T,
            density.info="none",
            #margins=c(12,12),
            # lhei=c(0.5,1.7,0.1),lwid =c(0.5,1.7),lmat = rbind(c(0,0),c(4,1),c(2,3)),
            lhei=c(0.2,1.6,0.3),lwid =c(0.25,1,0.12),lmat = rbind(c(2,3,0),c(4,1,0),c(0,0,0)),
            key.par=list(mgp=c(1.5, 0.5, 0),mar=c(30,1,5,1)),
            main = c("M_2_B \n Frecuencia de ocurrencia"),
            na.color="grey")
}#Mapa de calor zeros


##-------------------------------------------------------------------## 
##                     M_2_H_list - MAPA DE CALOR                    ## 
##-------------------------------------------------------------------## 

#Count

{
  
  zona=1
  aux.df=na.omit(listBasemodelozona[[zona]])
  prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos","Num_defectos_BIN"))]
  
  variables=(sapply(strsplit(colnames(prueba),"\\."), `[`, 1))
  variables=variables[!duplicated(variables)]
  robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
  robots_mapa=unique(robots_mapa)
  
  aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=robots_mapa
  
  aux.model=list_M_2_H[[zona]]
  aux.z_values=round((summary(aux.model)$coefficients)$count[-1,3],4)
  aux.names= gsub('Y',"",as.character(names(aux.z_values)))
  aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
  aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
  for (i in 1:length(aux.z_values)) {
    aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
    aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
    aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
  }
  aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
  mapa=aux.mapa
  
  
  
  for (zona in zonas[-1]){
    aux.df=na.omit(listBasemodelozona[[zona]])
    prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos","Num_defectos_BIN"))]
    robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
    robots_mapa=unique(robots_mapa)
    
    aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
    rownames(aux.mapa)=variables
    colnames(aux.mapa)=robots_mapa
    
    aux.model=list_M_2_H[[zona]]
    aux.z_values=round((summary(aux.model)$coefficients)$count[-1,3],4)
    aux.names= gsub('Y',"",as.character(names(aux.z_values)))
    aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
    aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
    for (i in 1:length(aux.z_values)) {
      aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
      aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
      aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
    }
    aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
    colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
    mapa=cbind(mapa,aux.mapa)
  }
  
  aux.mapa_count=as.matrix(mapa)
  aux.max=max(abs(aux.mapa_count), na.rm=T)
  aux.min=min(abs(aux.mapa_count), na.rm=T)
  # aux.mapa_count[is.na(aux.mapa_count)] <-0
}

{
  # dif=10^-(nchar(aux.max)-2)
  # colors = c(seq(-aux.max*1.2,-aux.max/2,length.out = 100),seq(-aux.max/2+dif,aux.max/2-dif,length.out = 200),seq(aux.max/2,aux.max*1.2,length.out = 100))
  colors = c(seq(-10,-5,length.out = 100),seq(-4.9,4.9,length.out = 200),seq(5,10,length.out = 100))
  my_palette <- colorRampPalette(c("darkgreen","forestgreen","yellowgreen","greenyellow","white","yellow","orange","darksalmon","firebrick3"))(n = 399)
  heatmap.2(aux.mapa_count , 
            col=my_palette, 
            breaks=colors , symbreaks=T,
            trace=c("both"), #Puede ser trace=("column","row","both","none")
            tracecol="black",
            Colv = NA, Rowv = NA, dendrogram="none",symm=F, #symm solo T cuando es matriz cuadrada
            cexCol = 1.2,cexRow = 1.2, symkey=T,
            scale="none",na.rm = T,
            density.info="none",
            #margins=c(12,12),
            # lhei=c(0.5,1.7,0.1),lwid =c(0.5,1.7),lmat = rbind(c(0,0),c(4,1),c(2,3)),
            lhei=c(0.2,1.6,0.3),lwid =c(0.25,1,0.12),lmat = rbind(c(2,3,0),c(4,1,0),c(0,0,0)),
            key.par=list(mgp=c(1.5, 0.5, 0),mar=c(30,1,5,1)),
            main = c("M_2_H \n Frecuencia de ocurrencia"),
            na.color="grey")
}#Mapa de calor count



#Zero

{
  
  zona=1
  aux.df=na.omit(listBasemodelozona[[zona]])
  prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos","Num_defectos_BIN"))]
  
  variables=(sapply(strsplit(colnames(prueba),"\\."), `[`, 1))
  variables=variables[!duplicated(variables)]
  robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
  robots_mapa=unique(robots_mapa)
  
  aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=robots_mapa
  
  aux.model=list_M_2_H[[zona]]
  aux.z_values=round((summary(aux.model)$coefficients)$zero[-1,3],4)
  aux.names= gsub('Y',"",as.character(names(aux.z_values)))
  aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
  aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
  for (i in 1:length(aux.z_values)) {
    aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
    aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
    aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
  }
  aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
  rownames(aux.mapa)=variables
  colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
  mapa=aux.mapa
  
  
  for (zona in zonas[-1]){
    aux.df=na.omit(listBasemodelozona[[zona]])
    prueba=aux.df[,-which(names(aux.df) %in% c("Num_defectos","Num_defectos_BIN"))]
    robots_mapa=paste0(sapply(strsplit(colnames(prueba),"\\."), `[`, 2),".Robot",sapply(strsplit(colnames(prueba),"Robot"), `[`, 2))
    robots_mapa=unique(robots_mapa)
    
    aux.mapa=data.frame(matrix(NA, nrow = length(variables), ncol = length(robots_mapa)))
    rownames(aux.mapa)=variables
    colnames(aux.mapa)=robots_mapa
    
    aux.model=list_M_2_H[[zona]]
    aux.z_values=round((summary(aux.model)$coefficients)$zero[-1,3],4)
    aux.names= gsub('Y',"",as.character(names(aux.z_values)))
    aux.rownames=sapply(strsplit((aux.names),"\\."), `[`, 1)
    aux.colnames=paste0(sapply(strsplit((aux.names),"\\."), `[`, 2),".Robot",sapply(strsplit((aux.names),"Robot"), `[`, 2))
    for (i in 1:length(aux.z_values)) {
      aux.position.row=grep(aux.rownames[i], rownames(aux.mapa))
      aux.position.col=grep(aux.colnames[i], colnames(aux.mapa))
      aux.mapa[aux.position.row,aux.position.col]=aux.z_values[i]
    }
    aux.mapa<- apply(aux.mapa, 2, function(x) as.numeric((x)))
    colnames(aux.mapa)=paste0(colnames(aux.mapa),"_Zona_",zona)
    mapa=cbind(mapa,aux.mapa)
  }
  
  aux.mapa_zero=as.matrix(mapa)
  aux.max=max(abs(aux.mapa_zero), na.rm=T)
  aux.min=min(abs(aux.mapa_zero), na.rm=T)
  # aux.mapa_zero[is.na(aux.mapa_zero)] <-0
}

{
  # dif=10^-(nchar(aux.max)-2)
  # colors = c(seq(-aux.max*1.2,-aux.max/2,length.out = 100),seq(-aux.max/2+dif,aux.max/2-dif,length.out = 200),seq(aux.max/2,aux.max*1.2,length.out = 100))
  colors = c(seq(-10,-5,length.out = 100),seq(-4.9,4.9,length.out = 200),seq(5,10,length.out = 100))
  my_palette <- colorRampPalette(c("darkgreen","forestgreen","yellowgreen","greenyellow","white","yellow","orange","darksalmon","firebrick3"))(n = 399)
  heatmap.2(aux.mapa_zero , 
            col=my_palette, 
            breaks=colors , symbreaks=T,
            trace=c("both"), #Puede ser trace=("column","row","both","none")
            tracecol="black",
            Colv = NA, Rowv = NA, dendrogram="none",symm=F, #symm solo T cuando es matriz cuadrada
            cexCol = 1.2,cexRow = 1.2, symkey=T,
            scale="none",na.rm = T,
            density.info="none",
            #margins=c(12,12),
            # lhei=c(0.5,1.7,0.1),lwid =c(0.5,1.7),lmat = rbind(c(0,0),c(4,1),c(2,3)),
            lhei=c(0.2,1.6,0.3),lwid =c(0.25,1,0.12),lmat = rbind(c(2,3,0),c(4,1,0),c(0,0,0)),
            key.par=list(mgp=c(1.5, 0.5, 0),mar=c(30,1,5,1)),
            main = c("M_2_H \n Frecuencia de ocurrencia"),
            na.color="grey")
}#Mapa de calor zeros

dev.off()
#modelo17_color14_cabina1=list(list_M_1_B,list_M_2_B,list_M_1_P,list_M_2_P,list_M_2_H)
# saveRDS(modelo17_color14_cabina1,file="C:/Users/usuario/Desktop/IÒigo/modelo17_color14_cabina1.Rda")
aux_df_modelo17_color14_cabina1=list(listBasemodelozona_BIN,listBasemodelozona_BIN,listBasemodelozona_POI,listBasemodelozona_POI,listBasemodelozona)
saveRDS(aux_df_modelo17_color14_cabina1,file="C:/Users/usuario/Desktop/IÒigo/aux_df_modelo17_color14_cabina1.Rda")



aux_df_modelo14_color14_cabina1=list(listBasemodelozona_BIN,listBasemodelozona_BIN,listBasemodelozona_POI,listBasemodelozona_POI,listBasemodelozona)
saveRDS(aux_df_modelo17_color14_cabina1,file="C:/Users/usuario/Desktop/IÒigo/aux_df_modelo17_color14_cabina1.Rda")

aux_df_modelo17_color14_cabina3=list(listBasemodelozona_BIN,listBasemodelozona_BIN,listBasemodelozona_POI,listBasemodelozona_POI,listBasemodelozona)
saveRDS(aux_df_modelo17_color14_cabina31,file="C:/Users/usuario/Desktop/IÒigo/aux_df_modelo17_color14_cabina1.Rda")

aux_df_modelo14_color14_cabina1=list(listBasemodelozona_BIN,listBasemodelozona_BIN,listBasemodelozona_POI,listBasemodelozona_POI,listBasemodelozona)
saveRDS(aux_df_modelo14_color14_cabina3,file="C:/Users/usuario/Desktop/IÒigo/aux_df_modelo14_color14_cabina1.Rda")


##-------------------------------------------------------------------## 
##  ~~~~~*~~~~~ * ~~~~~*~~~~~*\    FIN   /*~~~~~*~~~~~ * ~~~~~*~~~~~ ## 
##-------------------------------------------------------------------## 



