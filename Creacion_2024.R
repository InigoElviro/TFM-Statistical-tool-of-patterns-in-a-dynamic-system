#BASE DE DATOS BUENA A PARTIR DE LO DADO.
#OPTIMIZAR PARTE DE INSPECCION, SE PUEDE MEJORAR.
#CSV del 2024.
library(readxl)
library(readr)
library(lubridate)
library(dplyr)
library(gtools)

#t <- proc.time()


#------------------------------------#


#setwd("C:/Users/Inigo/Desktop/TFG/Ordenes portátil")
# setwd("D:/Stellantis/Íñigo/2024")


Inspeccion=readRDS("C:/Users/usuario/Desktop/Iñigo/Inspeccion.rds")
dim(Inspeccion)
names(Inspeccion)

Inspeccion$defectos=1
id_inspec_vueltas.df=as.data.frame(tapply(Inspeccion$defectos, Inspeccion$idInspeccion, sum))
id_inspec_vueltas.df$vuelta=Inspeccion$vuelta[match(row.names(id_inspec_vueltas.df), Inspeccion$idInspeccion)]
id_inspec_vueltas.df$bitono=Inspeccion$bitono[match(row.names(id_inspec_vueltas.df), Inspeccion$idInspeccion)]

aux.tabla=table(id_inspec_vueltas.df$vuelta)
aux.tabla.vuelta=rbind(aux.tabla,round(aux.tabla/sum(aux.tabla)*100,3))
obten_tabla(aux.tabla.vuelta,hline = TRUE)

obten_tabla(table(id_inspec_vueltas.df$bitono,id_inspec_vueltas.df$vuelta),hline = TRUE)


#------------------------------------#



#Pruebas correccion datos

{
  # Linea=1
  # vuelta=1
  # 
  # #LINEA1-VUELTA1
  # aux.marcador=is.element(Inspeccion$vuelta,vuelta)
  # prueba=subset(Inspeccion,aux.marcador)
  # aux.marcador=is.element(prueba$Linea,Linea)
  # prueba=subset(prueba,aux.marcador)
  # id_inspec=prueba$idinspeccion[1]
  # aux.marcador=is.element(DefectosL1$idInspeccion,id_inspec)
  # def=subset(DefectosL1,aux.marcador)
  # 
  # n_vuelta=which( colnames(prueba)=="vuelta") 
  # n_chasis=which( colnames(prueba)=="chasis") 
  # n_linea=which( colnames(prueba)=="Linea")   
  # 
  # #LINEA1-VUELTA1, LINEA1-VUELTA2
  # aux.marcador=is.element(Inspeccion$Linea,Linea)
  # prueba=subset(Inspeccion,aux.marcador)
  # aux.marcador=is.element(prueba$vuelta,c(1,2))
  # prueba=subset(prueba,aux.marcador)
  # prueba=prueba[prueba$chasis %in% prueba$chasis[duplicated(prueba$chasis)],]
  # prueba=prueba [!duplicated(prueba[c(n_vuelta,n_chasis,n_linea)]),]
  # prueba=prueba[prueba$chasis %in% prueba$chasis[duplicated(prueba$chasis)],]
  # id_inspec=prueba$idInspeccion[1]
  # aux.marcador=is.element(DefectosL1$idInspeccion,id_inspec)
  # def=subset(DefectosL1,aux.marcador)
  # 
  # 
  # #LINEA1-VUELTA1, LINEA2-VUELTA2
  # prueba=Inspeccion
  # aux.marcador=(is.element(prueba$Linea,c(1))&is.element(prueba$vuelta,c(1)))|(is.element(prueba$Linea,c(2))&is.element(prueba$vuelta,c(2)))
  # prueba=subset(Inspeccion,aux.marcador)
  # prueba=prueba[prueba$chasis %in% prueba$chasis[duplicated(prueba$chasis)],]
  # prueba=prueba [!duplicated(prueba[c(n_vuelta,n_chasis,n_linea)]),]
  # prueba=prueba[prueba$chasis %in% prueba$chasis[duplicated(prueba$chasis)],]
  # id_inspec=prueba$idInspeccion[1]
  # aux.marcador=is.element(DefectosL1$idInspeccion,id_inspec)
  # def=subset(DefectosL1,aux.marcador)
  # id_inspec=prueba$id_inspeccion[1]
  # aux.marcador=is.element(DefectosL1$idInspeccion,id_inspec)
  # def=subset(DefectosL1,aux.marcador)
}

#------------------------------------#

{
    # #Guardar graficos en PDF
    # #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\stellantis\\defectos_pintura.pdf'
    # destination = 'D:\\Users\\Íñigo\\Desktop\\TFG\\Graficos 2023\\stellantis\\defectos_pintura1.pdf'
    # pdf(file=destination)



#Adecuacion de los dataframes y variables temporales necesarias

#Solo se analizan los defectos encontrados en la primera vuelta de inspeccion

{aux.marcador=is.element(Inspeccion$vuelta,1)
  Inspeccion_v1=subset(Inspeccion,aux.marcador)
  
  Inspeccion_v1$defectos=1
  
  # zona=Inspeccion_v1$zona
  # modelo=Inspeccion_v1$nombre_modelo
  Inspeccion_v1$modelo=Inspeccion_v1$nombre_modelo
  variante=Inspeccion_v1$codigo_modelo
  Inspeccion_v1$variante=variante
  # Linea=Inspeccion_v1$Linea
  # linea_origen=Inspeccion_v1$linea_origen
  # vuelta=Inspeccion_v1$vuelta
  color=Inspeccion_v1$color1
  color_web=paste0("#",Inspeccion_v1$color_web1)
  # 
  # defectos=Inspeccion_v1$defectos
  # idInspeccion=Inspeccion_v1$idInspeccion
  # peso=Inspeccion_v1$peso
  size=Inspeccion_v1$size
  size =as.numeric(sub(",", ".", gsub("\\.", "", size)))
  Inspeccion_v1$size=size
  
  fecha=Inspeccion_v1$fecha
  Inspeccion_v1$fecha_prueba=Inspeccion_v1$fecha_prueba=as.POSIXct(Inspeccion_v1$fecha, format="%Y-%m-%d %h:m:s")
  fecha_insp=Inspeccion_v1$fecha_prueba
  Inspeccion_v1$fecha_dia=as.Date(Inspeccion_v1$fecha_prueba)
  fecha_dia=Inspeccion_v1$fecha_dia
  Inspeccion_v1$minute=minute(fecha)
  Inspeccion_v1$hour=hour(fecha)
  Inspeccion_v1$day=day(fecha)
  Inspeccion_v1$day_of_week=lubridate::wday(fecha_insp,week_start = getOption("lubridate.week.start", 1))
  Inspeccion_v1$month=month(fecha)
  Inspeccion_v1$year=year(fecha)
  Inspeccion_v1$day_year=yday(fecha)
  Inspeccion_v1$aux.yday.hour=Inspeccion_v1$day_year*10+Inspeccion_v1$hour
  
  n_zonas=names(table(Inspeccion_v1$zona))
  n_modelos=c("AIRCROSS","CROSSLAND","CORSA","PEUGEOT","COMO" )
  n_variante=names(table(variante))
  
  
}

dim(Inspeccion_v1)
table(Inspeccion_v1$month)
table(Inspeccion_v1$day_year)
table(Inspeccion_v1$year)

obten_tabla(t(table(Inspeccion_v1$month)),hline = TRUE)

table(Inspeccion_v1$color)
table(Inspeccion_v1$modelo)
table(Inspeccion_v1$variante)
table(Inspeccion_v1$modelo,Inspeccion_v1$variante)

obten_tabla(t(table(Inspeccion_v1$modelo)[n_modelos]),hline = TRUE)
obten_tabla(table(Inspeccion_v1$variante),hline = TRUE)

dim(Inspeccion_v1[Inspeccion_v1$zona==7,])#Zona 7: 53 filas de 617170 contenidas en Inspeccion, se elimina esta zona del estudio
aux.zona=names(table(Inspeccion_v1$zona))
aux.tabla=table(Inspeccion_v1$zona)
tabla.zonas=rbind(t(aux.tabla), t(round(aux.tabla/sum(aux.tabla)*100,3)) )
obten_tabla(tabla.zonas,hline = TRUE)
aux.zona=aux.zona[aux.zona!="7"]
Inspeccion_v1=Inspeccion_v1[Inspeccion_v1$zona!="7",]
n_zonas=n_zonas[n_zonas!="7"]

dim(Inspeccion_v1[Inspeccion_v1$codigo_modelo==500,])#Variante 500 : 939 filas de 617170 contenidas en Inspeccion, se elimina esta zona del estudio
n_variante=n_variante[n_variante!="500"] 
Inspeccion_v1=Inspeccion_v1[Inspeccion_v1$variante!="500",]
n_modelos=c("AIRCROSS","CROSSLAND","CORSA","PEUGEOT" )

#------------------------------------#



#Datos agregrados
#-por ID de inspeccion: idInspeccion
#-por color
#-por tipo de vehiculo: modelo
#-por tipo de vehiculo: variante
#-por zona: zona
#-por pais
#-por tipo de vehiculo y zona: modelo_zona
#-por tipo de vehiculo y zona: variante_zona
#-por dia y hora: aux.yday.hour

{
  #-por ID de inspeccion
  
  id_inspec.df=as.data.frame(tapply(Inspeccion_v1$defectos, Inspeccion_v1$idInspeccion, sum))
  colnames(id_inspec.df)="defectos"
  id_inspec.df$size=tapply(Inspeccion_v1$size, Inspeccion_v1$idInspeccion, sum)
  id_inspec.df$size=tapply(Inspeccion_v1$size, Inspeccion_v1$idInspeccion, sum)
  id_inspec.df$Linea=tapply(Inspeccion_v1$Linea, Inspeccion_v1$idInspeccion, mean)
  id_inspec.df$cabina=tapply(Inspeccion_v1$cabina, Inspeccion_v1$idInspeccion, mean)
  id_inspec.df$cabina_global=as.numeric(id_inspec.df$cabina)
  id_inspec.df$linea_origen=tapply(Inspeccion_v1$linea_origen, Inspeccion_v1$idInspeccion, mean)
  id_inspec.df$peso=tapply(Inspeccion_v1$peso, Inspeccion_v1$idInspeccion, sum)
  id_inspec.df$size=tapply(Inspeccion_v1$size, Inspeccion_v1$idInspeccion, sum)
  id_inspec.df$vehiculo=1
  
  id_inspec.df$minute=tapply(Inspeccion_v1$minute, Inspeccion_v1$idInspeccion, mean)
  id_inspec.df$hour=tapply(Inspeccion_v1$hour, Inspeccion_v1$idInspeccion, mean)
  id_inspec.df$day=tapply(Inspeccion_v1$day, Inspeccion_v1$idInspeccion, mean)
  id_inspec.df$day_of_week=tapply(Inspeccion_v1$day_of_week, Inspeccion_v1$idInspeccion, mean)
  
  id_inspec.df$modelo=Inspeccion_v1$nombre_modelo[match(row.names(id_inspec.df), Inspeccion_v1$idInspeccion)]
  id_inspec.df$variante=Inspeccion_v1$codigo_modelo[match(row.names(id_inspec.df), Inspeccion_v1$idInspeccion)]
  id_inspec.df$color_web=Inspeccion_v1$color_web1[match(row.names(id_inspec.df), Inspeccion_v1$idInspeccion)]
  
  id_inspec.df$pais=Inspeccion_v1$pais[match(row.names(id_inspec.df), Inspeccion_v1$idInspeccion)]
  id_inspec.df$fecha_dia=Inspeccion_v1$fecha_dia[match(row.names(id_inspec.df), Inspeccion_v1$idInspeccion)]
  id_inspec.df$aux.yday.hour=Inspeccion_v1$aux.yday.hour[match(row.names(id_inspec.df), Inspeccion_v1$idInspeccion)]
  
  
  zona=id_inspec.df$zona
  modelo=id_inspec.df$modelo
  variante=id_inspec.df$variante
  Linea=id_inspec.df$Linea
  linea_origen=id_inspec.df$linea_origen
  vuelta=id_inspec.df$vuelta
  color=id_inspec.df$color1
  color_web=paste0("#",id_inspec.df$color_web)
  
  defectos=id_inspec.df$defectos
  idInspeccion=id_inspec.df$idInspeccion
  peso=id_inspec.df$peso
  size=id_inspec.df$size
  size =as.numeric(sub(",", ".", gsub("\\.", "", size)))
  
  aux.yday.hour=id_inspec.df$aux.yday.hour
  
  #-por color:
  
  color.df=as.data.frame(tapply(id_inspec.df$defectos, id_inspec.df$color_web, sum))
  colnames(color.df)="defectos"
  color.df$vehiculo=tapply(id_inspec.df$vehiculo, id_inspec.df$color_web, sum)
  color.df$peso=tapply(id_inspec.df$peso, id_inspec.df$color_web, sum)
  color.df$size=tapply(id_inspec.df$size, id_inspec.df$color_web, sum)
  color.df=color.df %>% arrange(match(rownames(color.df), color))
  color.df$defectos_vehiculo=round(color.df$defectos/color.df$vehiculo,2)
  color.df$color=paste0("#",rownames(color.df))
  
  #-por tipo de vehiculo: modelo
  
  modelo.df=as.data.frame(tapply(defectos, modelo, sum))
  colnames(modelo.df)="defectos"
  modelo.df$vehiculo=tapply(id_inspec.df$vehiculo, id_inspec.df$modelo, sum)
  modelo.df$peso=tapply(peso, modelo, sum)
  modelo.df$size=tapply(size, modelo, sum)
  modelo.df=modelo.df %>% arrange(match(rownames(modelo.df), n_modelos))
  modelo.df$defectos_vehiculo=round(modelo.df$defectos/modelo.df$vehiculo,2)
  
  #-por tipo de vehiculo: variante
  
  variante.df=as.data.frame(tapply(id_inspec.df$defectos, id_inspec.df$variante, sum))
  colnames(variante.df)="defectos"
  variante.df$vehiculo=tapply(id_inspec.df$vehiculo, id_inspec.df$variante, sum)
  variante.df$peso=tapply(peso, variante, sum)
  variante.df$size=tapply(size, variante, sum)
  variante.df=variante.df %>% arrange(match(rownames(variante.df), n_variante))  
  variante.df$defectos_vehiculo=round(variante.df$defectos/variante.df$vehiculo,2)
  
  #-por zona - id
  
  zona_id=paste0(Inspeccion_v1$idInspeccion,"_",Inspeccion_v1$zona)
  def_zona=as.data.frame(tapply(Inspeccion_v1$defectos, zona_id, sum))
  colnames(def_zona)="defectos"
  def_zona$idInspeccion=sapply(strsplit(row.names(def_zona),"_"), `[`, 1)
  def_zona$zona=sapply(strsplit(row.names(def_zona),"_"), `[`, 2)
  def_zona$peso=tapply(Inspeccion_v1$peso, zona_id, sum)
  def_zona$size=tapply(Inspeccion_v1$size, zona_id, sum)
  def_zona$vehiculo=1 #Cada id-zona corresponde a 1 vehiculo
  def_zona$defectos_vehiculo=round(def_zona$defectos/def_zona$vehiculo,2)
 
  #-por zona
  
  zona.df=as.data.frame(tapply(Inspeccion_v1$defectos, Inspeccion_v1$zona, sum))
  colnames(zona.df)="defectos"
  zona.df$vehiculo=nrow(id_inspec.df)
  zona.df$peso=tapply(Inspeccion_v1$peso, Inspeccion_v1$zona, sum)
  zona.df$size=tapply(Inspeccion_v1$size, Inspeccion_v1$zona, sum)
  zona.df=zona.df %>% arrange(match(rownames(zona.df), n_zonas))  
  zona.df$defectos_vehiculo=round(zona.df$defectos/zona.df$vehiculo,2)
   
  #-por pais
  
  pais.df=as.data.frame(tapply(defectos, id_inspec.df$pais, sum))
  colnames(pais.df)="defectos"
  pais.df$vehiculo=tapply(id_inspec.df$vehiculo, id_inspec.df$pais, sum)
  pais.df$peso=tapply(peso, id_inspec.df$pais, sum)
  pais.df$size=tapply(size, id_inspec.df$pais, sum)
  pais.df=pais.df %>% arrange(match(rownames(pais.df), id_inspec.df$pais)) 
  pais.df$pais=row.names(pais.df)
  pais.df$defectos_vehiculo=round(pais.df$defectos/pais.df$vehiculo,2)  
  
  #-por color y modelo:
  
  color_modelo=paste0(id_inspec.df$color_web,"_",id_inspec.df$modelo)
  def_col_mod=as.data.frame(tapply(id_inspec.df$defectos, color_modelo, sum))
  colnames(def_col_mod)="defectos"
  def_col_mod$vehiculo=tapply(id_inspec.df$vehiculo, color_modelo, sum)
  def_col_mod$peso=tapply(id_inspec.df$peso, color_modelo, sum)
  def_col_mod$size=tapply(id_inspec.df$size, color_modelo, sum)
  def_col_mod$color=sapply(strsplit(row.names(def_col_mod),"_"), `[`, 1)
  def_col_mod$color=paste0("#",def_col_mod$color)
  def_col_mod$modelo=sapply(strsplit(row.names(def_col_mod),"_"), `[`, 2) 
  def_col_mod$defectos_vehiculo=round(def_col_mod$defectos/def_col_mod$vehiculo,2)  
  
  #-por color y variante:
  
  color_variante=paste0(id_inspec.df$color_web,"_",id_inspec.df$variante)
  def_col_var=as.data.frame(tapply(id_inspec.df$defectos, color_variante, sum))
  colnames(def_col_var)="defectos"
  def_col_var$vehiculo=tapply(id_inspec.df$vehiculo, color_variante, sum)
  def_col_var$peso=tapply(id_inspec.df$peso, color_variante, sum)
  def_col_var$size=tapply(id_inspec.df$size, color_variante, sum)
  def_col_var$color=sapply(strsplit(row.names(def_col_var),"_"), `[`, 1)
  def_col_var$color=paste0("#",def_col_var$color)
  def_col_var$variante=sapply(strsplit(row.names(def_col_var),"_"), `[`, 2) 
  def_col_var$defectos_vehiculo=round(def_col_var$defectos/def_col_var$vehiculo,2)
  
  
  #-por pais y modelo:
  
  pais_modelo=paste0(id_inspec.df$pais,"_",id_inspec.df$modelo)
  def_pais_mod=as.data.frame(tapply(id_inspec.df$defectos, pais_modelo, sum))
  colnames(def_pais_mod)="defectos"
  def_pais_mod$vehiculo=tapply(id_inspec.df$vehiculo, pais_modelo, sum)
  def_pais_mod$peso=tapply(id_inspec.df$peso, pais_modelo, sum)
  def_pais_mod$size=tapply(id_inspec.df$size, pais_modelo, sum)
  def_pais_mod$pais=sapply(strsplit(row.names(def_pais_mod),"_"), `[`, 1)
  def_pais_mod$modelo=sapply(strsplit(row.names(def_pais_mod),"_"), `[`, 2) 
  def_pais_mod$defectos_vehiculo=round(def_pais_mod$defectos/def_pais_mod$vehiculo,2)    
  
  #-por pais y variante:
  
  pais_variante=paste0(id_inspec.df$pais,"_",id_inspec.df$variante)
  def_pais_var=as.data.frame(tapply(id_inspec.df$defectos, pais_variante, sum))
  colnames(def_pais_var)="defectos"
  def_pais_var$vehiculo=tapply(id_inspec.df$vehiculo, pais_variante, sum)
  def_pais_var$peso=tapply(id_inspec.df$peso, pais_variante, sum)
  def_pais_var$size=tapply(id_inspec.df$size, pais_variante, sum)
  def_pais_var$pais=sapply(strsplit(row.names(def_pais_var),"_"), `[`, 1)
  def_pais_var$variante=sapply(strsplit(row.names(def_pais_var),"_"), `[`, 2) 
  def_pais_var$defectos_vehiculo=round(def_pais_var$defectos/def_pais_var$vehiculo,2)   
  
  
  #-por modelo de vehiculo y zona
  
  modelo_zona=paste0(Inspeccion_v1$modelo,"_",zona)
  def_zona_mod=as.data.frame(tapply(Inspeccion_v1$defectos, modelo_zona, sum))
  colnames(def_zona_mod)="defectos"
  def_zona_mod$peso=tapply(Inspeccion_v1$peso, modelo_zona, sum)
  def_zona_mod$size=tapply(Inspeccion_v1$size, modelo_zona, sum)
  def_zona_mod$modelo=sapply(strsplit(row.names(def_zona_mod),"_"), `[`, 1)
  def_zona_mod$zona=sapply(strsplit(row.names(def_zona_mod),"_"), `[`, 2)
  
  #-por variante de vehiculo y zona
  
  variante_zona=paste0(Inspeccion_v1$variante,"_",Inspeccion_v1$zona)
  def_zona_var=as.data.frame(tapply(Inspeccion_v1$defectos, variante_zona, sum))
  colnames(def_zona_var)="defectos"
  def_zona_var$peso=tapply(Inspeccion_v1$peso, variante_zona, sum)
  def_zona_var$size=tapply(Inspeccion_v1$size, variante_zona, sum)
  def_zona_var$variante=sapply(strsplit(row.names(def_zona_var),"_"), `[`, 1)
  def_zona_var$zona=sapply(strsplit(row.names(def_zona_var),"_"), `[`, 2)
  
  #-por dia y hora
  
  df_rsp=as.data.frame(tapply(defectos, aux.yday.hour, sum))
  colnames(df_rsp)="defectos"
  df_rsp$peso=tapply(peso, aux.yday.hour, sum)
  df_rsp$size=tapply(size, aux.yday.hour, sum)
  df_rsp$vehiculo=tapply((defectos)^0, aux.yday.hour, sum)
  df_rsp$defectos_vehiculo=round(df_rsp$defectos/df_rsp$vehiculo,2)
  
  #-por dia y hora
  
  df_yday.hour=as.data.frame(tapply(id_inspec.df$defectos, id_inspec.df$aux.yday.hour, sum))
  colnames(df_yday.hour)="defectos"
  df_yday.hour$peso=tapply(id_inspec.df$peso, id_inspec.df$aux.yday.hour, sum)
  df_yday.hour$size=tapply(id_inspec.df$size, id_inspec.df$aux.yday.hour, sum)
  df_yday.hour$vehiculo=tapply(id_inspec.df$vehiculo, id_inspec.df$aux.yday.hour, sum)
  df_yday.hour$defectos_vehiculo=round(df_yday.hour$defectos/df_yday.hour$vehiculo,2)  
  df_yday.hour$aux.yday.hour=rownames(df_yday.hour)
  
  df_yday.hour$hour=id_inspec.df$hour[match(row.names(df_yday.hour), id_inspec.df$aux.yday.hour)]
  df_yday.hour$day_of_week=id_inspec.df$day_of_week[match(row.names(df_yday.hour), id_inspec.df$aux.yday.hour)]
  
} 

dim(id_inspec.df)
n_vehiculos=table(id_inspec.df$modelo)[n_modelos]
n_variantes=table(id_inspec.df$variante)
obten_tabla(t(n_vehiculos),hline = TRUE)
obten_tabla(n_variantes,hline = TRUE)

tapply(id_inspec.df$defectos,id_inspec.df$modelo,summary)
aux.marcador=(id_inspec.df$defectos>60)
plot(id_inspec.df[aux.marcador,c("defectos","peso","size")])
aux.df=id_inspec.df[aux.marcador,c("defectos","peso","size")]
aux.df[aux.df$defectos>120,]

dim(Inspeccion_v1[Inspeccion_v1$zona==7,])#Zona 7: 53 filas de 617170 contenidas en Inspeccion, se elimina esta zona del estudio
aux.zona=names(table(Inspeccion_v1$zona))
aux.zona=aux.zona[aux.zona!="7"]
Inspeccion_v1=Inspeccion_v1[Inspeccion_v1$zona!="7",]

dim(Inspeccion_v1[Inspeccion_v1$codigo_modelo==500,])#Variante 500 : 939 filas de 617170 contenidas en Inspeccion, se elimina esta zona del estudio
n_variante=n_variante[n_variante!="500"] 



area_variante_zona=data.frame()
for (i in 1:length(n_variante)) {
  
  aux.marcador=is.element(Inspeccion_v1$variante,n_variante[i])
  Inspeccion_variante=subset(Inspeccion_v1,aux.marcador)
  
  
  for (j in 1:length(aux.zona)) {
    
    aux.marcador=is.element(Inspeccion_variante$zona,aux.zona[j])
    Inspeccion_variante_zona=subset(Inspeccion_variante,aux.marcador)
    
    x=Inspeccion_variante_zona$x
    y=Inspeccion_variante_zona$y
    
    #Elimino atipicos
    
    if(j==1){
      aux.inspeccion=Inspeccion_variante_zona
      aux.box.x=boxplot(x, plot=FALSE)
      x_range=c(aux.box.x$stats[1,],aux.box.x$stats[5,])
      aux.inspeccion=subset(aux.inspeccion,(aux.inspeccion$x>x_range[1])&(aux.inspeccion$x<x_range[2]))
      aux.box.y=boxplot(aux.inspeccion$y, plot=FALSE)
      y_range=c(aux.box.y$stats[1,],aux.box.y$stats[5,])
      aux.inspeccion=subset(aux.inspeccion,(aux.inspeccion$y>y_range[1])&(aux.inspeccion$y<y_range[2]))
      
      x=aux.inspeccion$x
      y=aux.inspeccion$y
    }
    
    # k<-chull(x,y)
    # plot(x,y, col=aux.colours[1])
    # polygon(x[k],y[k])
    
    aux.area=data.frame(x,y)
    area = st_as_sf(aux.area, coords = c("x", "y"), crs = 28992)
    hulls <-concaveman(area)
    plot(x,y, col=aux.colours[1])
    plot(hulls, add = TRUE, border = 'grey70',lwd=2, col = NA)
    titulo=paste("Variante:", n_variante[i],"- Zona:",aux.zona[j])
    title(main=titulo)
    area_hull=round(as.numeric(st_area(hulls)),0)
    
    # if(j==c(4,5)){
    #   ventanilla=Inspeccion_variante_zona[Inspeccion_variante_zona$y<summary(Inspeccion_variante_zona$y)[2],]
    #   x2=ventanilla$x
    #   y2=ventanilla$y
    #   aux.ventanilla=data.frame(x2,y2)
    #   area.ventanilla = st_as_sf(aux.ventanilla, coords = c("x2", "y2"), crs = 28992)
    #   hulls.ventanilla <-concaveman(area.ventanilla, concavity = 4)
    #   plot(x2,y2, col=aux.colours[1])
    #   plot(hulls.ventanilla, add = TRUE, border = 'grey70', col = NA)
    #   
    # }
    
    
    area.variante=n_variante[i]
    area.zona=j
    #area=cha(x,y)
    area.datos=c(area.variante,area.zona,area_hull)
    area_variante_zona=rbind(area_variante_zona,area.datos)
  }
  
}
colnames(area_variante_zona)=c("Variante","Zona","Area")
area_variante_zona$Area=as.numeric(area_variante_zona$Area)
Area_total=aggregate(area_variante_zona$Area, by=list(Variante=area_variante_zona$Variante), FUN=sum)
area_variante_zona$Area_total= rep(Area_total$x, each=length(aux.zona))
area_variante_zona$Si_St=round((area_variante_zona$Area/(area_variante_zona$Area_total/5)),2)
obten_tabla(area_variante_zona)

area_zona=aggregate(area_variante_zona$Area, by=list(Zona=area_variante_zona$Zona), FUN=sum)
colnames(area_zona)=c("Zona","Area_zona")
area_zona$Area_zona=area_zona$Area_zona/length(n_zonas)
area_zona$Si_St=round((area_zona$Area_zona)/(sum(area_zona$Area_zona)/5),2)

area_variante=aggregate(area_variante_zona$Area, by=list(Variante=area_variante_zona$Variante), FUN=sum)
colnames(area_variante)=c("Variante","Area_variante")
area_variante$Area_variante=area_variante$Area_variante/length(n_modelos)
area_variante$Si_St=round((area_variante$Area_variante)/(sum(area_variante$Area_variante)/5),2)

    # #Cerrar PDF 
    # dev.off()

#------------------------------------#

    # #Guardar graficos en PDF
    # #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\stellantis\\defectos_pintura.pdf'
    # destination = 'D:\\Users\\Íñigo\\Desktop\\TFG\\Graficos 2023\\stellantis\\defectos_pintura2.pdf'
    # pdf(file=destination)

#DESCRIPTIVA - ESTUDIO DE LOS DEFECTOS SEGUN:
#-modelos
#-variantes
#-colores

aux.title=NULL
aux.legend=NULL
aux.x_lab=NULL
relat=NULL
lwd=1

#-colores
{
  
  #RELATIVOS: 
  
  #Numero de defectos por vehiculo para cada color
  aux.data=color.df$defectos_vehiculo   
  aux.col=color.df$color
  aux.title="FRACCION DE DEFECTOS / VEHICULO \n POR COLORES"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab=NULL,text.bar=T)    
  #my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,relat = T)
  
  #ABSOLUTOS
  
  #Numero de defectos
  aux.data=color.df$defectos
  aux.col=as.character(as.data.frame(table(color_web))[,1])
  aux.title="Nº DE DEFECTOS POR COLORES"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab=NULL,text.bar=T)
  #obten_tabla(table(color_web))
  
  #Numero de vehiculos
  aux.data=color.df$vehiculo
  aux.col=as.character(as.data.frame(table(color_web))[,1])
  aux.title="Nº DE VEHICULOS POR COLORES"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab=NULL,text.bar=T)
  
  obten_tabla(color.df[,-c(which(colnames(color.df)==c("peso","size")))])
  
  
  #DISTRIBUCIÓN DE COLORES, Y DEFECTOS/VEHICULO POR TIPO DE VEHICULO
  
  #Numero de defectos por vehiculo para cada color por modelo
  prueba=def_col_mod[,c("color","modelo","vehiculo","defectos_vehiculo")]
  pie_list=c()
  for (i in 1:length(n_modelos)) {
    
    aux.marcador=is.element(prueba$modelo,n_modelos[i])
    prueba1=subset(prueba,aux.marcador)
    participacion=round(100*(prueba1$vehiculo)/sum(prueba1$vehiculo),2)
    etiquetas = paste0(prueba1$color, "\n", as.numeric(prueba1$defectos_vehiculo) , "\n",participacion, "%")
    pie=pie2(participacion,main=n_modelos[i],labels = etiquetas,col=prueba1$color)
    pie_list=c(pie_list,pie)
  }
  
  #Numero de defectos por vehiculo para cada color por variante
  prueba=def_col_var[,c("color","variante","vehiculo","defectos_vehiculo")]
  pie_list=c()
  for (i in 1:length(n_variante)) {
    
    aux.marcador=is.element(prueba$variante,as.numeric(n_variante[i]))
    prueba1=subset(prueba,aux.marcador)
    participacion=round(100*(prueba1$vehiculo)/sum(prueba1$vehiculo),2)
    etiquetas = paste0(prueba1$color, "\n", as.numeric(prueba1$defectos_vehiculo) , "\n",participacion, "%")
    aux.i=round((as.numeric(n_variante[i]))/100,0)
    pie=pie2(participacion,main=paste((n_modelos[aux.i]),"-", n_variante[i]),labels = etiquetas,col=prueba1$color)
    pie_list=c(pie_list,pie)
  }
  
  {#Defectos por colores para cada modelo
    # prueba=table(modelo,color_web)[n_modelos,]
    # pie_list=c()
    # for (i in 1:length(n_modelos)) {
    #   prueba1=prueba[i,]
    #   prueba1=prueba1[prueba1!=0]
    #   etiquetas = paste0(names(prueba1), "\n", as.numeric(prueba1), "\n",round(100 * prueba1/sum(prueba1), 2), "%")
    #   pie=pie2(prueba1,main=n_modelos[i],labels = etiquetas,col=row.names(as.data.frame(prueba1)))
    #   pie_list=c(pie_list,pie)
    # }
    # 
    # #Defectos por colores para cada variante
    # prueba=table(variante,color_web)[n_variante,]
    # pie_list=c()
    # for (i in 1:length(n_variante)) {
    #   prueba1=prueba[i,]
    #   prueba1=prueba1[prueba1!=0]
    #   etiquetas = paste0(names(prueba1), "\n", as.numeric(prueba1), "\n",round(100 * prueba1/sum(prueba1), 2), "%")
    #   pie=pie2(prueba1,main=n_variante[i],labels = etiquetas,col=row.names(as.data.frame(prueba1)))
    #   pie_list=c(pie_list,pie)
    # }
  }
  
}

#-modelos
{
  
  #Relativos: Numero de defectos por vehiculo 
  
  aux.data=modelo.df$defectos_vehiculo   
  aux.col=aux.colours[1:length(n_modelos)]
  aux.title="FRACCION DE DEFECTOS / VEHICULO \n POR MODELO"
  aux.x_lab=rownames(modelo.df)
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T) 
  #my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,relat = T)
  
  #Absolutos: Numero de defectos 
  
  #Numero de defectos 
  aux.data=modelo.df$defectos
  aux.col=aux.colours[1:length(aux.data)]
  aux.x_lab=rownames(modelo.df)
  aux.title="Nº DE DEFECTOS \n POR MODELO"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  #Numero de vehiculos
  aux.data=modelo.df$vehiculo
  aux.col=aux.colours[1:length(table(modelo))]
  aux.title="Nº DE VEHICULOS \n POR MODELO"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)  
  
  obten_tabla(modelo.df[,-c(which(colnames(color.df)==c("peso","size")))])
  
}

#-variantes
{
  
  #Relativos: Numero de defectos por vehiculo 
  
  aux.data=variante.df$defectos_vehiculo  
  aux.x_lab=as.numeric(rownames(variante.df))
  aux.col=aux.colours[round(aux.x_lab/100,0)]
  aux.title="FRACCION DE DEFECTOS / VEHICULO \n POR VARIANTE"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T) 
  #my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,relat = T)
  #obten_tabla(t(rbind(n_variante,aux.data)))
  
  #Absolutos: Numero de defectos 
  
  #Numero de defectos 
  aux.data=variante.df$defectos
  aux.x_lab=row.names(variante.df)
  aux.col=aux.colours[round(as.numeric(aux.x_lab)/100,0)]
  aux.title="Nº DE DEFECTOS \n  POR VARIANTE"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T) 
  
  #Numero de vehiculos
  aux.data=variante.df$vehiculo
  aux.col=aux.colours[round(as.numeric(aux.x_lab)/100,0)]
  aux.title="Nº DE VEHICULOS \n  POR VARIANTE"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)  
  
  obten_tabla(variante.df[,-c(which(colnames(color.df)==c("peso","size")))])  
}

#-linea
{
  
  #Relativos: Numero de defectos por linea 
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$Linea), sum))
  colnames(aux.prueba)=paste0("L",names(table(Inspeccion_v1$Linea))) 
  aux_vehic=c(sum(!is.na(aux.prueba$L1)),sum(!is.na(aux.prueba$L2)))
  aux.prueba[is.na(aux.prueba)]=0   
  aux.col=aux.colours[1:length(table(Inspeccion_v1$Linea))]
  aux.x_lab=colnames(aux.prueba)
  aux.texture=T
  aux.data=round(colSums(aux.prueba)/aux_vehic,2)
  aux.title="FRACCION DE DEFECTOS / VEHICULO \n POR LINEA"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  tabla_linea=rbind(colSums(aux.prueba),aux_vehic,aux.data)
  rownames(tabla_linea)=c("Nº defectos","Nº vehiculos","Nº defectos / vehiculo")
  obten_tabla(tabla_linea)  
  
}

#-cabina
{
  
  #Relativos: Numero de defectos por cabina 
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,as.numeric(Inspeccion_v1$cabina)), sum))
  colnames(aux.prueba)=paste0("CABINA_",names(table(as.numeric(Inspeccion_v1$cabina)))) 
  aux_vehic=c(sum(!is.na(aux.prueba$CABINA_1)),sum(!is.na(aux.prueba$CABINA_2)),sum(!is.na(aux.prueba$CABINA_3)))
  aux.prueba[is.na(aux.prueba)]=0   
  aux.col=aux.colours[1:length(table(as.numeric(Inspeccion_v1$cabina)))]
  aux.x_lab=colnames(aux.prueba)
  aux.texture=T
  aux.data=round(colSums(aux.prueba)/aux_vehic,2)
  aux.title="FRACCION DE DEFECTOS / VEHICULO \n POR CABINA"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  tabla_cabina=rbind(colSums(aux.prueba),aux_vehic,aux.data)
  rownames(tabla_cabina)=c("Nº defectos","Nº vehiculos","Nº defectos / vehiculo")
  obten_tabla(tabla_cabina)  
  
}

#Numero de defectos segun id de inspeccion (vehiculo)
#-Global: Histograma
#-Por modelo: Histograma, boxplot y ECDF
#-Por variante: Histograma, boxplot y ECDF

{
  #Distribucion del numero de defectos en cada vehiculo
  table(id_inspec.df$defectos) 
  obten_tabla(t(summary(id_inspec.df$defectos)))
  id_inspec.df[id_inspec.df$defectos==summary(id_inspec.df$defectos)[6],]
  
  par(mfrow=c(1,1))
  
  #ECDF del numero de defectos en cada vehiculo, censura en quantil 0,99
  aux.df=id_inspec.df
  y="defectos"
  aux.marca="modelo"
  ecdf_modelo=ecdf(aux.df,y,aux.marca)
  ecdf_censura=as.data.frame(ecdf_modelo[2])
  obten_tabla(ecdf_censura[,n_modelos])
  
  aux.marca="variante"
  ecdf_variante=ecdf(aux.df,y,aux.marca)
  ecdf_censura=as.data.frame(ecdf_variante[2])
  obten_tabla(ecdf_censura)
  
  #Censura de los defectos en identificadores con mas de 60 defectos
  id_inspec.df$defectos[id_inspec.df$defectos>60]=60
  summary(id_inspec.df$defectos) 
  
  
  #Histograma del numero de defectos en cada vehiculo (nº de ocurrencias de cierta cantidad de defectos en 1 mismo ID)
  hist(id_inspec.df$defectos, main="HISTOGRAMA DE DEFECTOS", xlab = "Numero de defectos",ylab = "Numero de vehiculos", labels = TRUE , col=aux.colours[1]) 
  
  #Histograma del numero de defectos en cada modelo (nº de ocurrencias de cierta cantidad de defectos en 1 mismo ID)
  par(mfrow=c(2,2))
  for (i in 1:length(n_modelos)) {
    aux.marcador=is.element(id_inspec.df$modelo,n_modelos[i])
    aux.subset=subset(id_inspec.df,aux.marcador)
    y_max=sum(aux.subset$defectos<summary(aux.subset$defectos)[4])
    hist(aux.subset$defectos, main=paste("HISTOGRAMA DE DEFECTOS:", "\n", n_modelos[i]), xlab = "Numero de defectos",ylab = "Numero de vehiculos",ylim = c(0,y_max) , labels = TRUE , col=aux.colours[i]) 
  }
  par(mfrow=c(1,1))
  
  #Histograma del numero de defectos en cada variante (nº de ocurrencias de cierta cantidad de defectos en 1 mismo ID)
  n_variante=names(table(id_inspec.df$variante))
  col_variantes=round(as.numeric(n_variante)/100)
  par(mfrow=c(2,2))
  for (i in 1:length(n_variante)) {
    aux.marcador=is.element(id_inspec.df$variante,n_variante[i])
    aux.subset=subset(id_inspec.df,aux.marcador)
    aux.i=col_variantes[i]
    y_max=sum(aux.subset$defectos<summary(aux.subset$defectos)[4])
    hist(aux.subset$defectos, main=paste("HISTOGRAMA DE DEFECTOS:", "\n", n_modelos[aux.i],"-",n_variante[i]), xlab = "Numero de defectos",ylab = "Numero de vehiculos", ylim = c(0,y_max) , labels = TRUE , col=aux.colours[aux.i]) 
  }
  par(mfrow=c(1,1))
  
  #Boxplot del numero de vehiculos segun el numero de defectos por modelo (nº de ocurrencias de cierta cantidad de defectos en 1 mismo ID)
  par(mfrow=c(1,1))
  prueba=id_inspec.df[,c("defectos","modelo")]
  max.y=60
  aux.prueba=as.data.frame(tapply(prueba$defectos, list(rownames(prueba),prueba$modelo), sum))
  #aux.prueba[is.na(aux.prueba)]
  boxplot(aux.prueba, ylim = c(0, max.y) ,col=aux.colours[1:length(n_modelos)], main= "DEFECTOS POR MODELOS" )
  
  #Boxplot del numero de vehiculos segun el numero de defectos por variante (nº de ocurrencias de cierta cantidad de defectos en 1 mismo ID)
  prueba=id_inspec.df[,c("defectos","variante")]
  max.y=60
  aux.prueba=as.data.frame(tapply(prueba$defectos, list(rownames(prueba),prueba$variante), sum))
  boxplot(aux.prueba, ylim = c(0, max.y) ,col=aux.colours[col_variantes], main= "DEFECTOS POR VARIANTES" )
  
}

#Defectos segun paises
#-ecdf   
#-% de defectos
#-defectos, modelos y variantes segun paises
#-boxplot paises con mas de 1000 defectos

{
  
  #ECDF Nº defectos por vehiculo para cada pais
  orden_paises=as.data.frame(table(id_inspec.df$pais))
  colnames(orden_paises)=c("pais","Freq")
  orden_paises=arrange(orden_paises, desc(Freq))[1:40,1]
  aux.df=id_inspec.df
  i_prev=1
  for (i in 1:4) {
    aux.marcador=is.element(aux.df$pais, orden_paises[i_prev:(i*10)])
    aux.subset=subset(aux.df,aux.marcador)
    y="defectos"
    aux.marca="pais"
    ecdf_pais=ecdf(aux.subset,y,aux.marca)
    ecdf_censura=as.data.frame(ecdf_pais[2])
    obten_tabla(t(ecdf_censura))
    i_prev=i*10
  }    
  
  #RELATIVOS
  
  #Numero de defectos por vehiculo para cada pais    
  i_prev=1
  for (i in 1:4) {
    aux.marcador=is.element(pais.df$pais, orden_paises[i_prev:(i*10)])
    aux.subset=subset(pais.df,aux.marcador)
    aux.title="Nº DEFECTOS / VEHICULO POR PAIS"
    aux.data=aux.subset$defectos_vehiculo
    aux.col=aux.colours[1:10]
    aux.x_lab=aux.subset$pais
    my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
    i_prev=i*10
    obten_tabla(t(rbind(aux.x_lab,aux.data)))
    
  }  
  
  #Numero de defectos por vehiculo para cada pais por modelo
  pais.df=pais.df[order(pais.df$vehiculo,decreasing = T),]
  paises_1000=pais.df[(pais.df$vehiculo>1000),"pais"]
  
  prueba=def_pais_mod[,c("pais","modelo","defectos","vehiculo","defectos_vehiculo")]
  prueba=subset(prueba,is.element(prueba$pais,paises_1000))
  pie_list=c()
  for (i in 1:length(table(prueba$pais))) {
    
    aux.marcador=is.element(prueba$pais,paises_1000[i])
    prueba1=subset(prueba,aux.marcador)
    prueba1=prueba1[order(match(prueba1$modelo, n_modelos)),]
    participacion=round(100*(prueba1$vehiculo)/sum(prueba1$vehiculo),2)
    etiquetas = paste0(prueba1$modelo, "\n", as.numeric(prueba1$defectos_vehiculo) , "\n",participacion, "%")
    pie=pie2(participacion,main=paises_1000[i],labels = etiquetas,col=aux.colours[1:length(n_modelos)])
    pie_list=c(pie_list,pie)
  }
  
  #Numero de defectos por vehiculo para cada pais por variante
  prueba=def_pais_var[,c("pais","variante","defectos","vehiculo","defectos_vehiculo")]
  prueba=subset(prueba,is.element(prueba$pais,paises_1000))
  pie_list=c()
  for (i in 1:length(table(prueba$pais))) {
    
    aux.marcador=is.element(prueba$pais,paises_1000[i])
    prueba1=subset(prueba,aux.marcador)
    prueba1=prueba1[order(match(prueba1$variante, n_variante)),]
    participacion=round(100*(prueba1$vehiculo)/sum(prueba1$vehiculo),2)
    etiquetas = paste0(prueba1$variante, "\n", as.numeric(prueba1$defectos_vehiculo) , "\n",participacion, "%")
    pie=pie2(participacion,main=paises_1000[i],labels = etiquetas,col=aux.colours[round(as.numeric(prueba1$variante)/100,0)])
    pie_list=c(pie_list,pie)
  }
  
  #Boxplot del numero de vehiculos segun el numero de defectos por variante (nº de ocurrencias de cierta cantidad de defectos en 1 mismo ID)
  prueba=id_inspec.df[,c("defectos","pais")]
  max.y=60
  prueba1=subset(prueba,is.element(prueba$pais,paises_1000))
  aux.prueba=as.data.frame(tapply(prueba1$defectos, list(rownames(prueba1),prueba1$pais), sum))
  boxplot(aux.prueba, ylim = c(0, max.y) ,col=aux.colours[1:length(paises_1000)], main= "DEFECTOS POR VARIANTES" )
  
  
  #ABSOLUTOS
  {
    # tabla_paises=table(id_inspec.df$pais) %>%  as.data.frame() %>% arrange(desc(Freq))
    # colnames(tabla_paises)=c("pais","defectos")
    # tabla_paises$pais=as.character(tabla_paises$pais)
    # tabla_paises$defectos=as.numeric(tabla_paises$defectos)
    # obten_tabla(t(summary(tabla_paises$defectos)))
    # OTROS=sum(tabla_paises$defectos[tabla_paises$defectos<1000])
    # prueba=c("OTROS",OTROS)
    # tabla_paises=rbind(tabla_paises,prueba)
    # tabla_paises$defectos=as.numeric(tabla_paises$defectos)
    # aux.marcador=tabla_paises$defectos>1000
    # 
    # table(aux.marcador)
    # tabla_paises=subset(tabla_paises,aux.marcador) 
    # aux.data=tabla_paises[,2]
    # aux.col=aux.colours[1:nrow(tabla_paises)]
    # aux.title="PAISES"
    # aux.x_lab=tabla_paises$pais
    # my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,relat = T)
    # 
    # 
    # aux.marcador=(tabla_paises$defectos/sum(tabla_paises$defectos)*100)>10
    # top_paises=(tabla_paises[aux.marcador,])[,1]
    # top_paises=top_paises[-length(top_paises)]
    # 
    # pais_modelo=paste0(Inspeccion_v1$pais,"_",modelo)
    # defectos_paises_modelos=as.data.frame(tapply(Inspeccion_v1$defectos, pais_modelo, sum))
    # colnames(defectos_paises_modelos)="defectos"
    # defectos_paises_modelos$pais=sapply(strsplit(row.names(defectos_paises_modelos),"_"), `[`, 1)
    # defectos_paises_modelos$modelo=sapply(strsplit(row.names(defectos_paises_modelos),"_"), `[`, 2)
    # aux.marcador=is.element(defectos_paises_modelos$pais,top_paises)
    # tabla_top_paises=subset(defectos_paises_modelos,aux.marcador) 
    # 
    # pie_list=c()
    # for (i in 1:length(top_paises)) {
    #   aux.marcador=is.element(defectos_paises_modelos$pais,top_paises[i])
    #   prueba=subset(defectos_paises_modelos,aux.marcador)
    #   prueba=prueba[order(match(prueba$modelo, n_modelos)),]
    #   etiquetas = paste0(prueba$modelo, "\n", as.numeric(prueba$defectos), "\n",round(100 * prueba$defectos/sum(prueba$defectos), 2), "%")
    #   pie=pie2(prueba$defectos,main=top_paises[i],labels = etiquetas,col= aux.colours[1:length(n_modelos)])
    #   pie_list=c(pie_list,pie)
    # }
    # 
    # pais_variante=paste0(Inspeccion_v1$pais,"_",variante)
    # defectos_paises_variantes=as.data.frame(tapply(Inspeccion_v1$defectos, pais_variante, sum))
    # colnames(defectos_paises_variantes)="defectos"
    # defectos_paises_variantes$pais=sapply(strsplit(row.names(defectos_paises_variantes),"_"), `[`, 1)
    # defectos_paises_variantes$variante=sapply(strsplit(row.names(defectos_paises_variantes),"_"), `[`, 2)
    # aux.marcador=is.element(defectos_paises_variantes$pais,top_paises)
    # tabla_top_paises=subset(defectos_paises_variantes,aux.marcador) 
    # 
    # pie_list=c()
    # for (i in 1:length(top_paises)) {
    #   aux.marcador=is.element(defectos_paises_variantes$pais,top_paises[i])
    #   prueba=subset(defectos_paises_variantes,aux.marcador)
    #   prueba=prueba[order(match(prueba$variante, n_modelos)),]
    #   etiquetas = paste0(prueba$variante, "\n",round(100 * prueba$defectos/sum(prueba$defectos), 2), "%") 
    #     {aux.len=rep_len(c(0.8),length(prueba$variante))
    #     aux.p=prueba$defectos/sum(prueba$defectos)*100
    #     aux.base.positions=which(aux.p>10)
    #     aux.base.positions=c(1,aux.base.positions)
    #     aux.base.positions=aux.base.positions[-duplicated( aux.base.positions)]
    #     aux.base.2=(which(aux.p>10)+1)
    #     aux.base.2=aux.base.2[!is.element(aux.base.2,aux.base.positions)]
    #     
    #     aux.len[aux.p>10]=0.2
    #     # aux.positions=order(round(aux.p*1000,0))*0.25-0.20
    #     # aux.len[aux.p<10]=aux.positions[!is.element(1:length(prueba$variante),aux.base.positions)]
    #     # 
    #     aux.p=(1:length(prueba$variante))[-aux.base.positions]
    #     aux.p=aux.p[!is.element(aux.p,aux.base.2)]
    #     aux.len[aux.base.2]=0.05
    #     for (j in 1:length(aux.p)) {
    #       aux.j=aux.p[j]
    #       aux.len[aux.j]=aux.len[aux.j-1]+0.4
    #     }
    #   }
    #   n_variante=names(table((prueba$variante)))
    #   col_variantes=round(as.numeric(n_variante)/100)
    #   
    #   pie=pie3(prueba$defectos,main=top_paises[i],labels = etiquetas,col= aux.colours[col_variantes],len=aux.len)
    #   pie_list=c(pie_list,pie)
    # }
  }
  
}

#Analisis temporal
#-Efecto horario
#-Defectos por cada dia de la semana
#-Defectos en la semana
#-Defectos entre semana y fin de semana

{
  
  #RELATIVOS    
  # df=id_inspec.df
  # aux.classif="defectos"          #Numero de defectos en cada identificador
  # efecto_hora(df,aux.classif)
  # 
  # df=df_yday.hour
  # aux.classif="defectos_vehiculo"
  
  df_yday.hour$wday_hour=paste0(df_yday.hour$day_of_week,df_yday.hour$hour)
  df_wday_hour=as.data.frame(tapply(df_yday.hour$defectos_vehiculo, df_yday.hour$wday_hour, mean))
  colnames(df_wday_hour)="defectos_vehiculo"
  df_wday_hour$hour=(tapply(df_yday.hour$hour, df_yday.hour$wday_hour, mean))
  df_wday_hour$day_of_week=(tapply(df_yday.hour$day_of_week, df_yday.hour$wday_hour, mean))
  df=df_wday_hour
  aux.classif="defectos_vehiculo"
  
  efecto_hora(df,aux.classif)
  accidentes_dia_semana(df,aux.classif,orden.horiz=TRUE, cambio.fecha=F,lang=NULL, aux.ylab=aux.classif)
  accidentes_todos_los_dias_semana_color(df,aux.classif, cambio.fecha=F, lang=NULL, aux.ylab=aux.classif)
  accidentes_todos_los_dias_semana(df,aux.classif, cambio.fecha=F, aux.ylab=aux.classif)
  
  par(mfrow=c(1,1))
  # aux.df=df_yday.hour
  # y="defectos_vehiculo"
  # aux.marca="day_of_week"
  # ecdf_semana=ecdf(aux.df, y, aux.marca)
  # ecdf_censura=as.data.frame(ecdf_semana[2])
  # obten_tabla(ecdf_censura)
  
  #ABSOLUTOS
  par(mfrow=c(1,1))
  df=id_inspec.df
  aux.classif="defectos"
  accidentes_dia_semana(df,aux.classif,orden.horiz=TRUE, cambio.fecha=F, lang=NULL, aux.ylab=aux.classif)
  accidentes_todos_los_dias_semana_color(df,aux.classif, cambio.fecha=F, lang=NULL, aux.ylab=aux.classif)
  accidentes_todos_los_dias_semana(df,aux.classif, cambio.fecha=F, aux.ylab=aux.classif)
  
  aux.classif="vehiculo"
  accidentes_dia_semana(df,aux.classif,orden.horiz=TRUE, cambio.fecha=F, lang=NULL, aux.ylab=aux.classif)
  accidentes_todos_los_dias_semana_color(df,aux.classif, cambio.fecha=F, lang=NULL, aux.ylab=aux.classif)
  accidentes_todos_los_dias_semana(df,aux.classif, cambio.fecha=F, aux.ylab=aux.classif)
  
  #Producción de vehiculos defectuosos
  # df=id_inspec.df
  # aux.classif="vehiculo"
  # aux.temp="fecha_dia"
  # accidentes_dia(df,aux.classif,aux.temp, lang=NULL)
  
  aux.df=id_inspec.df
  y="defectos"
  aux.marca="day_of_week"
  ecdf_semana=ecdf(aux.df, y, aux.marca)
  ecdf_censura=as.data.frame(ecdf_semana[2])
  obten_tabla(ecdf_censura)
}

    # #Cerrar PDF 
    # dev.off()




#Defectos por modelo, variante, zona, zona-modelo y variante-modelo

    # #Guardar graficos en PDF
    # #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\stellantis\\defectos_pintura.pdf'
    # destination = 'D:\\Users\\Íñigo\\Desktop\\TFG\\Graficos 2023\\stellantis\\defectos_pintura3_rel.pdf'
    # pdf(file=destination)

#Relativos

{
  
  #Defectos por zona
  #-Numero de defectos
  #-Peso
  #-Size
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0   
  aux.col=aux.colours[1]
  aux.x_lab=colnames(aux.prueba)
  aux.texture=T
  aux.data=round(colSums(aux.prueba)/nrow(aux.prueba),2)
  aux.title="FRACCION DE DEFECTOS POR ZONA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0 
  aux.data=round(colSums(aux.prueba)/nrow(aux.prueba),0)
  aux.title="FRACCION DE PESO DEFECTOS POR ZONA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0 
  aux.data=round(colSums(aux.prueba)/nrow(aux.prueba),2)
  aux.title="FRACCION DE SIZE DEFECTOS POR ZONA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  
  #Defectos por zona adimensionalizada
  #-Numero de defectos
  #-Peso
  #-Size
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0   
  aux.col=aux.colours[1]
  aux.x_lab=colnames(aux.prueba)
  aux.texture=T
  aux.data=round((colSums(aux.prueba)/nrow(aux.prueba))/(area_zona$Si_St),2)
  aux.title="FRACCION DE DEFECTOS POR ZONA ADIMENSIONALIZADA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0 
  aux.data=round((colSums(aux.prueba)/nrow(aux.prueba))/(area_zona$Si_St),0)
  aux.title="FRACCION PESO DEFECTOS POR ZONA ADIMENSIONALIZADA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0 
  aux.data=round((colSums(aux.prueba)/nrow(aux.prueba))/(area_zona$Si_St),2)
  aux.title="FRACCION DE SIZE DEFECTOS POR ZONA ADIMENSIONALIZADA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  
  #Defectos por modelo
  #-Numero de defectos
  #-Peso
  #-Size
  
  aux.col=aux.colours[1:nrow(modelo.df)]
  aux.x_lab=rownames(modelo.df)
  
  aux.data=round(modelo.df$defectos_vehiculo,2)
  aux.title="FRACCION DE DEFECTOS POR MODELO"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.data=round(modelo.df$peso/modelo.df$vehiculo,0)
  aux.title="FRACCION DE PESO DEFECTOS POR MODELO"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.data=round(modelo.df$size/modelo.df$vehiculo,2)
  aux.title="FRACCION DE SIZE DEFECTOS POR MODELO"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  #Defectos por variante
  #-Numero de defectos
  #-Peso
  #-Size
  
  aux.col=aux.colours[round(as.numeric(n_variante)/100)]
  aux.x_lab=rownames(variante.df)
  
  aux.data=round(variante.df$defectos_vehiculo,2)
  aux.title="FRACCION DE DEFECTOS \n POR VARIANTE"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.data=round(variante.df$peso/variante.df$vehiculo,0)
  aux.title="FRACCION DE PESO DEFECTOS \n POR VARIANTE"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.data=round(variante.df$size/variante.df$vehiculo,2)
  aux.title="FRACCION DE SIZE DEFECTOS \n POR VARIANTE"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  #Defectos por variante adimensionalizada
  #-Numero de defectos
  #-Peso
  #-Size
  
  # aux.col=aux.colours[round(as.numeric(n_variante)/100)]
  # aux.x_lab=rownames(variante.df)
  # 
  # aux.data=round((variante.df$defectos_vehiculo)/(area_variante$Si_St),2)
  # aux.title="FRACCION DE DEFECTOS POR \n VARIANTE ADIMENSIONALIZADA"
  # my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  # 
  # aux.data=round((variante.df$peso/variante.df$vehiculo)/(area_variante$Si_St),0)
  # aux.title="FRACCION DE PESO DEFECTOS POR \n VARIANTE ADIMENSIONALIZADA"
  # my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  # 
  # aux.data=round((variante.df$size/variante.df$vehiculo)/(area_variante$Si_St),2)
  # aux.title="FRACCION DE SIZE DEFECTOS POR \n VARIANTE ADIMENSIONALIZADA"
  # my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  
  
  #Defectos por zona y modelo
  #-Numero de defectos
  #-Peso
  #-Size
  
  # par(mfrow=c(1,1))
  # #par(mfrow=c(2,2))
  # for (i in 1:length(n_modelos)) {
  #   
  #   
  #   modelo_zona=paste0(n_modelos[i],"_",Inspeccion_v1$zona)
  #   aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,modelo_zona), sum))
  #   aux.prueba[is.na(aux.prueba)]=0  
  #   aux.data=round(colSums(aux.prueba)/nrow(aux.prueba),2)
  #   aux.col=aux.colours[i]
  #   aux.texture=T
  #   aux.x_lab=names(table(Inspeccion_v1$zona))
  #   aux.title=paste("FRACCION DE DEFECTOS POR ZONA","\n",n_modelos[i])
  #   my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T ) 
  #   
  #   aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,modelo_zona), sum))
  #   aux.prueba[is.na(aux.prueba)]=0  
  #   aux.data=round(colSums(aux.prueba)/nrow(aux.prueba),0)
  #   aux.title=paste("FRACCION DE PESO DEFECTOS POR ZONA","\n",n_modelos[i])
  #   my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
  #   
  #   aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,modelo_zona), sum))
  #   aux.prueba[is.na(aux.prueba)]=0  
  #   aux.data=round(colSums(aux.prueba)/nrow(aux.prueba),2)
  #   aux.title=paste("FRACCION DE SIZE DEFECTOS POR ZONA","\n",n_modelos[i])
  #   my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
  # }
  # par(mfrow=c(1,1))
  # 
  
  #Defectos por zona y variante
  #-Peso
  #-Size
  
  color_variante=aux.colours[round(as.numeric(n_variante)/100)]
  #par(mfrow=c(2,2))
  for (i in 1:length(n_variante)) {
    
    
    variante_zona=paste0(n_variante[i],"_",Inspeccion_v1$zona)
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round(colSums(aux.prueba)/nrow(aux.prueba),2)
    aux.col=color_variante[i]
    aux.texture=T
    aux.x_lab=names(table(Inspeccion_v1$zona))
    aux.title=paste("FRACCION DE DEFECTOS POR ZONA","\n",n_variante[i])
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T ) 
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round(colSums(aux.prueba)/nrow(aux.prueba),0)
    aux.title=paste("FRACCION DE PESO DEFECTOS POR ZONA","\n",n_variante[i])
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round(colSums(aux.prueba)/nrow(aux.prueba),2)
    aux.title=paste("FRACCION DE SIZE DEFECTOS POR ZONA","\n",n_variante[i])
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
  }
  par(mfrow=c(1,1))
  
  
  #Defectos por zona y variante adimensionalizada
  #-Peso
  #-Size
  
  color_variante=aux.colours[round(as.numeric(n_variante)/100)]
  #par(mfrow=c(2,2))
  i_prev=1
  for (i in 1:length(n_variante)) {
    
    
    variante_zona=paste0(n_variante[i],"_",Inspeccion_v1$zona)
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round((colSums(aux.prueba)/nrow(aux.prueba))/(area_variante_zona$Si_St[i_prev:(i_prev+length(n_zonas)-1)]),2)
    aux.col=color_variante[i]
    aux.texture=T
    aux.x_lab=names(table(Inspeccion_v1$zona))
    aux.title=paste("FRACCION DE DEFECTOS POR ZONA","\n",n_variante[i], "ADIMENSIONALIZADA")
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T ) 
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round((colSums(aux.prueba)/nrow(aux.prueba))/(area_variante_zona$Si_St[i_prev:(i_prev+length(n_zonas)-1)]),0)
    aux.title=paste("FRACCION DE PESO DEFECTOS POR ZONA","\n",n_variante[i], "ADIMENSIONALIZADA")
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round((colSums(aux.prueba)/nrow(aux.prueba))/(area_variante_zona$Si_St[i_prev:(i_prev+length(n_zonas)-1)]),2)
    aux.title=paste("FRACCION DE SIZE DEFECTOS POR ZONA","\n",n_variante[i], "ADIMENSIONALIZADA")
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
    
    i_prev=i*5+1
  }
  par(mfrow=c(1,1))
  
  
  
}

    # #Cerrar PDF 
    # dev.off()

#Absolutos

    # #Guardar graficos en PDF
    # #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\stellantis\\defectos_pintura.pdf'
    # destination = 'D:\\Users\\Íñigo\\Desktop\\TFG\\Graficos 2023\\stellantis\\defectos_pintura3_abs.pdf'
    # pdf(file=destination)

{
  
  #Defectos por zona
  #-Numero de defectos
  #-Peso
  #-Size
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0   
  aux.col=aux.colours[1]
  aux.x_lab=colnames(aux.prueba)
  aux.texture=T
  aux.data=round(colSums(aux.prueba),0)
  aux.title="NUMERO DE DEFECTOS POR ZONA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0 
  aux.data=round(colSums(aux.prueba),0)
  aux.title="PESO DEFECTOS POR ZONA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0 
  aux.data=round(colSums(aux.prueba),0)
  aux.title="SIZE DEFECTOS POR ZONA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  
  
  
  #Defectos por zona adimensionalizada
  #-Numero de defectos
  #-Peso
  #-Size
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0   
  aux.col=aux.colours[1]
  aux.x_lab=colnames(aux.prueba)
  aux.texture=T
  aux.data=round((colSums(aux.prueba))/(area_zona$Si_St),0)
  aux.title="NUMERO DE DEFECTOS POR ZONA ADIMENSIONALIZADA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0 
  aux.data=round((colSums(aux.prueba))/(area_zona$Si_St),0)
  aux.title="PESO DEFECTOS POR ZONA ADIMENSIONALIZADA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,Inspeccion_v1$zona), sum))
  aux.prueba[is.na(aux.prueba)]=0 
  aux.data=round((colSums(aux.prueba))/(area_zona$Si_St),0)
  aux.title="SIZE DEFECTOS POR ZONA ADIMENSIONALIZADA"
  my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  
  #Defectos por modelo
  #-Numero de defectos
  #-Peso
  #-Size
  
  aux.col=aux.colours[1:nrow(modelo.df)]
  aux.legend=rownames(modelo.df)
  
  aux.data=round(modelo.df$defectos,0)
  aux.title="NUMERO DEFECTOS POR MODELO"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend,aux.x_lab=NULL,text.bar=T)
  
  aux.data=round(modelo.df$peso,0)
  aux.title="PESO DEFECTOS POR MODELO"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend,aux.x_lab=NULL,text.bar=T)
  
  aux.data=round(modelo.df$size,0)
  aux.title="SIZE DEFECTOS POR MODELO"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend,aux.x_lab=NULL,text.bar=T)
  
  #Defectos por variante
  #-Numero de defectos
  #-Peso
  #-Size
  
  aux.col=aux.colours[round(as.numeric(n_variante)/100)]
  aux.x_lab=rownames(variante.df)
  
  aux.data=round(variante.df$defectos,0)
  aux.title="NUMERO DEFECTOS POR VARIANTE"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.data=round(variante.df$peso,0)
  aux.title="PESO DEFECTOS POR VARIANTE"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.data=round(variante.df$size,0)
  aux.title="SIZE DEFECTOS POR VARIANTE"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  
  
  #Defectos por variante adimensionalizada
  #-Numero de defectos
  #-Peso
  #-Size
  
  aux.col=aux.colours[round(as.numeric(n_variante)/100)]
  aux.x_lab=rownames(variante.df)
  
  aux.data=round((variante.df$defectos)/(area_variante$Si_St),0)
  aux.title="NUMERO DEFECTOS POR \n VARIANTE ADIMENSIONALIZADA"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.data=round((variante.df$peso)/(area_variante$Si_St),0)
  aux.title="PESO DEFECTOS POR \n VARIANTE ADIMENSIONALIZADA"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  aux.data=round((variante.df$size)/(area_variante$Si_St),0)
  aux.title="SIZE DEFECTOS POR \n VARIANTE ADIMENSIONALIZADA"
  my_bp(aux.data,aux.col,aux.texture=NULL,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  
  
  
  #Defectos por zona y modelo
  #-Numero de defectos
  #-Peso
  #-Size
  
  par(mfrow=c(1,1))
  #par(mfrow=c(2,2))
  for (i in 1:length(n_modelos)) {
    
    
    modelo_zona=paste0(n_modelos[i],"_",Inspeccion_v1$zona)
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,modelo_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round(colSums(aux.prueba),0)
    aux.col=aux.colours[i]
    aux.texture=T
    aux.x_lab=names(table(Inspeccion_v1$zona))
    aux.title=paste("DEFECTOS POR ZONA","\n",n_modelos[i])
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T) 
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,modelo_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round(colSums(aux.prueba),0)
    aux.title=paste("PESO DEFECTOS POR ZONA","\n",n_modelos[i])
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,modelo_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round(colSums(aux.prueba),0)
    aux.title=paste("SIZE DEFECTOS POR ZONA","\n",n_modelos[i])
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T)
  }
  par(mfrow=c(1,1))
  
  
  #Defectos por zona y variante
  #-Peso
  #-Size
  
  color_variante=aux.colours[round(as.numeric(n_variante)/100)]
  #par(mfrow=c(2,2))
  for (i in 1:length(n_variante)) {
    
    
    variante_zona=paste0(n_variante[i],"_",Inspeccion_v1$zona)
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round(colSums(aux.prueba),0)
    aux.col=color_variante[i]
    aux.texture=T
    aux.x_lab=names(table(Inspeccion_v1$zona))
    aux.title=paste("DEFECTOS POR ZONA","\n",n_variante[i])
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T ) 
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round(colSums(aux.prueba),0)
    aux.title=paste("PESO DEFECTOS POR ZONA","\n",n_variante[i])
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round(colSums(aux.prueba),0)
    aux.title=paste("SIZE DEFECTOS POR ZONA","\n",n_variante[i])
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
  }
  par(mfrow=c(1,1))
  
  
  #Defectos por zona y variante adimensionalizada
  #-Peso
  #-Size
  
  color_variante=aux.colours[round(as.numeric(n_variante)/100)]
  #par(mfrow=c(2,2))
  i_prev=1
  for (i in 1:length(n_variante)) {
    
    
    variante_zona=paste0(n_variante[i],"_",Inspeccion_v1$zona)
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$defectos, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round((colSums(aux.prueba))/(area_variante_zona$Si_St[i_prev:(i_prev+length(n_zonas)-1)]),0)
    aux.col=color_variante[i]
    aux.texture=T
    aux.x_lab=names(table(Inspeccion_v1$zona))
    aux.title=paste("DEFECTOS POR ZONA","\n",n_variante[i], "ADIMENSIONALIZADA")
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T ) 
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$peso, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round((colSums(aux.prueba))/(area_variante_zona$Si_St[i_prev:(i_prev+length(n_zonas)-1)]),0)
    aux.title=paste("PESO DEFECTOS POR ZONA","\n",n_variante[i], "ADIMENSIONALIZADA")
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
    
    aux.prueba=as.data.frame(tapply(Inspeccion_v1$size, list(Inspeccion_v1$idInspeccion,variante_zona), sum))
    aux.prueba[is.na(aux.prueba)]=0  
    aux.data=round((colSums(aux.prueba))/(area_variante_zona$Si_St[i_prev:(i_prev+length(n_zonas)-1)]),0)
    aux.title=paste("SIZE DEFECTOS POR ZONA","\n",n_variante[i], "ADIMENSIONALIZADA")
    my_bp(aux.data,aux.col,aux.texture,aux.title,aux.legend=NULL,aux.x_lab,text.bar=T )
    
    i_prev=i*5+1
  }
  par(mfrow=c(1,1))
  
  
  
}


    # #Cerrar PDF 
    # dev.off() 


}




dev.off() 



library (openxlsx)
write.csv(Inspeccion , file= "Inspeccion_2024.csv")

proc.time()-t #visualiza el tiempo de ejecución
{
  # filtros = is.element(Inspeccion$area,c(5))#is.element(Inspeccion$vuelta,c(1)) &is.element(Inspeccion$pasada,c(1)) &is.element(Inspeccion$area,c(5))
  # Inspeccion <- Inspeccion[filtros,]
  # Inspeccion <- Inspeccion[,-which(names(Inspeccion)%in%c("area"))]
  
  # #PASADA REAL
  # Inspeccion <- Inspeccion[order(Inspeccion$fecha),]
  # Inspeccion$cumulativo <- 1
  # for (i in 2:dim(Inspeccion)[1]){
  #   if(Inspeccion$idInspeccion[i]==Inspeccion$idInspeccion[i-1]){
  #     Inspeccion$cumulativo[i]=0
  #   }else{Inspeccion$cumulativo[i]=1}
  # }
  # Inspeccion$pasadareal <- ave(Inspeccion$cumulativo, Inspeccion$n_pedido, FUN=cumsum) #Hace la suma por grupos, y como está en orden temporal, al segundo registro le asignará un 2(1+1).
  # Inspeccion <- Inspeccion[,-which(names(Inspeccion)%in%c("cumulativo"))]
  # #primerapasada <- is.element(Inspeccion$pasadareal,2)
  # #Inspeccion <- Inspeccion[primerapasada,]
  
  
  # 
  # ####FIN DE CICLO POR FILAS####
  # ####CABINA 1#####
  # #1º Realizamos C1, luego C3 y los juntamos en fichero general Finciclo.
  # rutaFincicloC1 <- "D:/Stellantis/Íñigo/CABINAS ESTUDIO 1/cabina TC1 robots.xlsx"
  # FincicloC1 <- as.data.frame(read_excel(rutaFincicloC1, range = cell_limits(c(9, 1), c(NA, 39)))) #Seleccionamos lo que queremos
  # # range = 'A9:AM191425'   Como hay un bug en la función ponemos cell_limits( c(9,1) que es 9A y c(NA,39) que es todo hasta AM=39)
  # #Lo restringimos como dataframe para no tener problemas con reshape
  # FincicloC1 <- FincicloC1[!is.na(FincicloC1$VEHICLE),] #Quitamos primera linea de NA
  # 
  # #Para poder realizar esto, hemos cambiado el excel, primero Date y luego transformación fecha.
  # FincicloC1$Time <- format(FincicloC1$Time, format = "%H:%M:%S") #Me quedo solo con la hora
  # #FincicloC1$Time <- as.POSIXct(FincicloC1$Time,format="%H:%M:%S")
  # FincicloC1$Fecha <- parse_date_time(x = FincicloC1$Fecha,
  #                                     orders = c("%m/%d/%Y","%d-%m-%Y"))
  # FincicloC1$Date <- FincicloC1$Fecha
  # FincicloC1$FechaRobot <- paste(FincicloC1$Date,FincicloC1$Time)
  # FincicloC1 <- FincicloC1[FincicloC1$NoEnAutomatico=='Y',]
  # filtroscab1 = is.element(FincicloC1$VolumenPintura, c(0))&is.element(FincicloC1$Rev_Campana_Min, c(0))&is.element(FincicloC1$Rev_Campana_Max,c(0)) &is.element(FincicloC1$Rev_Campana_Media,c(0)) &is.element(FincicloC1$Aire_Guia_Min,c(0)) &is.element(FincicloC1$Aire_Guia_Max,c(0)) &is.element(FincicloC1$Aire_Guia_Media,c(0)) &is.element(FincicloC1$Alta_Tension_Min,c(0)) &is.element(FincicloC1$Alta_Tension_Max,c(0)) &is.element(FincicloC1$Alta_Tension_Media,c(0)) &is.element(FincicloC1$CycleTime,c(0))
  # FincicloC1 <- FincicloC1[filtroscab1==FALSE,]
  # 
  # names <- c(names(FincicloC1%>% select("VolumenPintura",contains("Rev_Campana"),contains("Aire_Guia"),contains("Alta_Tension"),"CycleTime")))
  # for (name in names){
  #   filtro <- is.element(FincicloC1[,name],0)
  #   FincicloC1[,name][filtro]=NA 
  # }
  # 
  # #variables a eliminar c(10,14,21:26)
  # FincicloC1 <- select(FincicloC1, -contains("pass"))
  # FincicloC1 <- FincicloC1[,-which(names(FincicloC1) %in% c("PO","Empuje","OK","NoEnAutomatico","Num_Aplicaciones","Fecha"))]
  # FincicloC1 <- FincicloC1 %>% select("Date","FechaRobot","VEHICLE":"CycleTime") 
  # FincicloC1 <- FincicloC1[order(FincicloC1$VEHICLE,FincicloC1$FechaRobot),] #IMPORTANTE PARA EL BUCLE POSTERIOR DE CONTEO
  # FincicloC1$conteo1 <- 1
  # #abs(difftime(FincicloC1$FechaRobot[13],FincicloC1$FechaRobot[12],units = c('min')))
  # #CONTADOR PARA COMPROBAR QUE ESTAMOS EN EL MISMO COCHE Y PASADA, SE LE ASIGNA 1 AL PRIMER REGISTRO DE LA PASADA DE ESE COCHE.
  # #LO REALIZAMOS PARA PODER AGRUPAR LOS COCHES Y HACER LA TRANSFORMACION ANCHA SIN EL PROBLEMA DE LA FECHA.
  # 
  # for (i in 2:dim(FincicloC1)[1]){
  #   if (FincicloC1$VEHICLE[i]==FincicloC1$VEHICLE[i-1]){
  #     if((abs(difftime(FincicloC1$FechaRobot[i],FincicloC1$FechaRobot[i-1],units = c('min'))))>=60){ #Ponemos 60mins, aunque podriamos poner cualquier otro valor lógico.
  #       FincicloC1$conteo1[i]=1
  #     }else{FincicloC1$conteo1[i]=0}}
  #   else {FincicloC1$conteo1[i]=1}
  # }
  # #CONTAMOS DE FORMA REAL EN QUE PASADA ESTAMOS.
  # FincicloC1$contadorrealC1 <- ave(FincicloC1$conteo1, FincicloC1$VEHICLE, FUN=cumsum)
  # FincicloC1 <- FincicloC1[,-which(names(FincicloC1)%in%c("conteo1","Date"))]
  # library(stringr)
  # FincicloC1$Equipment <- str_replace_all(FincicloC1$Equipment, fixed(" "), ".")
  # Fin.ciclo.filaC1 <-   reshape(FincicloC1,  
  #                               timevar = "Equipment",  
  #                               idvar = c("VEHICLE","contadorrealC1","ProcessArea","Work Cell", "MANG_COLOR_MET", "MANG_SOL_MET" , "MODELO"),     
  #                               direction = "wide", 
  #                               sep='.')
  # #El error que nos da es por culpa de datos duplicados que nos llegan
  # 
  # aux.dfC1 <- Fin.ciclo.filaC1 %>% select(contains("FechaRobot"))
  # aux.dfC1$minimotiempo  <- apply(aux.dfC1,1,min, na.rm=TRUE) #Para obtener la fecha minima.
  # Fin.ciclo.filaC1$Fechaminima <- aux.dfC1$minimotiempo
  # Fin.ciclo.filaC1 <- Fin.ciclo.filaC1[,-which(names(Fin.ciclo.filaC1)%in%c("contadorrealC1"))]
  # 
  # ####Cabina 3####
  # rutaFincicloC3 <- "D:/Stellantis/Íñigo/CABINAS ESTUDIO 1/cabina TC3 robots.xlsx"
  # FincicloC3 <- as.data.frame(read_excel(rutaFincicloC3, range = cell_limits(c(9, 1), c(NA, 39)))) #Seleccionamos lo que queremos
  # 
  # # range = 'A9:AL253846'   Como hay un bug en la función ponemos cell_limits( c(9,1) que es 9A y c(NA,39) que es todo hasta AM=39)                         
  # #Lo restringimos como dataframe para no tener problemas con reshape
  # FincicloC3 <- FincicloC3[!is.na(FincicloC3$VEHICLE),] #Quitamos primera linea de NA
  # FincicloC3$Time <- format(FincicloC3$Time, format = "%H:%M:%S") #Me quedo solo con la hora
  # #FincicloC3$Time <- as.POSIXct(FincicloC3$Time,format="%H:%M:%S")
  # FincicloC3$Fecha <- parse_date_time(x = FincicloC3$Fecha,
  #                                     orders = c("%m/%d/%Y","%d-%m-%Y"))
  # FincicloC3$Date <- FincicloC3$Fecha 
  # FincicloC3$FechaRobot <- paste(FincicloC3$Date,FincicloC3$Time)
  # FincicloC3 <- FincicloC3[FincicloC3$NoEnAutomatico=='Y',]
  # filtroscab3 = is.element(FincicloC3$VolumenPintura,c(0))&is.element(FincicloC3$Rev_Campana_Min, c(0))&is.element(FincicloC3$Rev_Campana_Max,c(0)) &is.element(FincicloC3$Rev_Campana_Media,c(0)) &is.element(FincicloC3$Aire_Guia_Min,c(0)) &is.element(FincicloC3$Aire_Guia_Max,c(0)) &is.element(FincicloC3$Aire_Guia_Media,c(0)) &is.element(FincicloC3$Alta_Tension_Min,c(0)) &is.element(FincicloC3$Alta_Tension_Max,c(0)) &is.element(FincicloC3$Alta_Tension_Media,c(0)) &is.element(FincicloC3$CycleTime,c(0))
  # FincicloC3 <- FincicloC3[filtroscab3==FALSE,]
  # 
  # names <- c(names(FincicloC3%>% select("VolumenPintura",contains("Rev_Campana"),contains("Aire_Guia"),contains("Alta_Tension"),"CycleTime")))
  # for (name in names){
  #   filtro <- is.element(FincicloC3[,name],0)
  #   FincicloC3[,name][filtro]=NA 
  # }
  # 
  # #variables a eliminar c(10,14,21:26)
  # FincicloC3 <- select(FincicloC3, -contains("pass"))
  # FincicloC3 <- FincicloC3[,-which(names(FincicloC3) %in% c("PO","Empuje","OK","NoEnAutomatico","Num_Aplicaciones","Fecha"))]
  # FincicloC3 <- FincicloC3 %>% select("Date","FechaRobot","VEHICLE":"CycleTime") 
  # FincicloC3 <- FincicloC3[order(FincicloC3$VEHICLE,FincicloC3$FechaRobot),] #IMPORTANTE PARA EL BUCLE POSTERIOR DE CONTEO
  # FincicloC3$conteo3 <- 1
  # #abs(difftime(FincicloC3$FechaRobot[13],FincicloC3$FechaRobot[12],units = c('min')))
  # #CONTADOR PARA COMPROBAR QUE ESTAMOS EN EL MISMO COCHE Y PASADA, SE LE ASIGNA 1 AL PRIMER REGISTRO DE LA PASADA DE ESE COCHE.
  # #LO REALIZAMOS PARA PODER AGRUPAR LOS COCHES Y HACER LA TRANSFORMACION ANCHA SIN EL PROBLEMA DE LA FECHA.
  # for (i in 2:dim(FincicloC3)[1]){
  #   if (FincicloC3$VEHICLE[i]==FincicloC3$VEHICLE[i-1]){
  #     if((abs(difftime(FincicloC3$FechaRobot[i],FincicloC3$FechaRobot[i-1],units = c('min'))))>=60){ #Ponemos 120mins, aunque podriamos poner cualquier otro valor lógico.
  #       FincicloC3$conteo3[i]=1
  #     }else{FincicloC3$conteo3[i]=0}}
  #   else {FincicloC3$conteo3[i]=1}
  # }
  # #CONTAMOS DE FORMA REAL EN QUE PASADA ESTAMOS.
  # FincicloC3$contadorrealC3 <- ave(FincicloC3$conteo3, FincicloC3$VEHICLE, FUN=cumsum)
  # FincicloC3 <- FincicloC3[,-which(names(FincicloC3)%in%c("conteo3","Date"))]
  # library(stringr)
  # FincicloC3$Equipment <- str_replace_all(FincicloC3$Equipment, fixed(" "), ".")
  # Fin.ciclo.filaC3 <-   reshape(FincicloC3,  
  #                               timevar = "Equipment",  
  #                               idvar = c("VEHICLE","contadorrealC3","ProcessArea","Work Cell", "MANG_COLOR_MET", "MANG_SOL_MET" , "MODELO"),     
  #                               direction = "wide", 
  #                               sep='.')
  # #El error que nos da es por culpa de datos duplicados que nos llegan.
  # 
  # aux.dfC3 <- Fin.ciclo.filaC3 %>% select(contains("FechaRobot"))
  # aux.dfC3$minimotiempo  <- apply(aux.dfC3,1,min, na.rm=TRUE) #Para obtener la fecha minima.
  # Fin.ciclo.filaC3$Fechaminima <- aux.dfC3$minimotiempo
  # Fin.ciclo.filaC3 <- Fin.ciclo.filaC3[,-which(names(Fin.ciclo.filaC3)%in%c("contadorrealC3"))]
  # 
  # 
  # ####MEZCLA FINCICLO.FILA####
  # library(gtools)
  # Fin.ciclo.fila <- smartbind(Fin.ciclo.filaC1,Fin.ciclo.filaC3)
  # ####CREACION DE LA COLUMNA PASADAS EN FIN.CICLO.FILA####
  # Fin.ciclo.fila$Fechaminima <- parse_date_time(x = Fin.ciclo.fila$Fechaminima,
  #                                               orders = c("ymd_HMS")) #pasa a formato tiempo (POSIXct) debido a que smartbind lo deshace.
  # 
  # Fin.ciclo.fila  <- Fin.ciclo.fila [order(Fin.ciclo.fila$Fechaminima),] #lo ordena temporalmente. IMPORTANTE para luego(CREACION COCHES DESDE CAMPANA).
  # Fin.ciclo.fila$cumulativo <- 1 #para que pueda sumar.
  # Fin.ciclo.fila$pasadapintura <- ave(Fin.ciclo.fila$cumulativo, Fin.ciclo.fila$VEHICLE, FUN=cumsum) #Hace la suma por grupos, y como está en orden temporal, al segundo registro le asignará un 2(1+1).
  # ####CREACION COCHES DESDE LIMPIEZA CAMPANA####
  # marcador1 <- is.element(Fin.ciclo.fila$ProcessArea,c("41 Acabado 1"))
  # marcador3 <- is.element(Fin.ciclo.fila$ProcessArea,c("43 Acabado 3"))
  # dfeb1 <- Fin.ciclo.fila[marcador1,]
  # dfeb3 <- Fin.ciclo.fila[marcador3,]
  # ######CABINA1 cochesdesdecamp#####
  # aut1<- dfeb1%>%select(contains("FechaRobot."),contains("Limpieza_campana."))
  # names <- c("BCIN.Robot.1.1","BCIN.Robot.1.2","BCIN.Robot.2.1","BCIN.Robot.2.2","BC1.Robot.1.1","BC1.Robot.1.2","BC1.Robot.2.1","BC1.Robot.2.2","BC2.Robot.1.1","BC2.Robot.1.2","BC2.Robot.2.1","BC2.Robot.2.2","CC1.Robot.1.3","CC1.Robot.1.4","CC1.Robot.2.3","CC1.Robot.2.4")
  # for (variable in names) {
  #   nombrevariable <- paste("cumulativo",variable,sep = "_")
  #   aut1[,nombrevariable]<- 1 }
  # for (variable in names) {
  #   nombrevariable <- paste("Limpieza_campana_nuevo",variable,sep = "_")
  #   elegirvariable <- paste("Limpieza_campana.",variable,sep = "")
  #   aut1[,nombrevariable] <- aut1%>%select(elegirvariable)
  #   
  # }
  # #For que rellena los NA según la frecuencia.
  # for (variable in names) {
  #   elemento <- paste("Limpieza_campana_nuevo_",variable,sep = "")
  #   frecuencia <- as.vector(table(aut1[,elemento])) #se obtienen los No frente a los Si
  #   bueno <- sum(is.na(aut1[,elemento]))/(dim(aut1)[1]) # Se calcula el ratio de NA's que hay en la base
  #   relacion <- round(frecuencia[1]/frecuencia[2],) #Relacion de NO frente a SI
  #   
  #   if(length(frecuencia)>1){ #Si existen ambos valores SI y NO para una variable (Nº1)
  #     if(bueno<=0.85){ #Si el ratio de NA's es menor de 0.85, es decir, si hay menos del 85% de datos omitidos. (Nº2)
  #       for (i in 1:dim(aut1)[1]) { #bucle que recorre toda la base (Nº3)
  #         if(i<=relacion) { #para completar los primeros registros de la frecuencia, por si son NA que se rellenen y luego no colapse el programa. (Nº4)
  #           if(!is.na(aut1[,elemento][i])){ #si no es NA, coge el valor original
  #             aut1[,elemento][i] <- aut1[,elemento][i]
  #           }else if (is.na(aut1[,elemento][i])){ #si es NA realiza operación de relleno con frecuencia.(Nº5)
  #             contador <- 0 #pongo contador a 0
  #             
  #             for (j in 1:relacion) { #desde el registro 1 hasta el relacion(valor numerico entero) (Nº6)
  #               if(is.na(aut1[,elemento][i+j])){ #Si el valor es NA, el contador se mantiene con el valor actual
  #                 contador <- contador 
  #               }else if (aut1[,elemento][i+j]=='N'){ #Si el valor es NO, el contador suma 1. (Limpieza desde...)
  #                 contador <- contador + 1}
  #               else if (aut1[,elemento][i+j]=='Y'){ #Si el valor es YES, se reinicia el contador, se ha realizado limpieza
  #                 contador <- 0}
  #             }#end for j (Nº6)
  #             
  #             if(contador==relacion){aut1[,elemento][i]='Y'} #Si el contador alcanza la relación, se debe poner un SI, ya que se ha alcanzado la relación de No frente a Si.
  #             else{ aut1[,elemento][i]='N'} #Si el contador sigue sin alzanzar la relacion, se mantiene el NO hasta que llegue.
  #           }#Acabo proceso de relleno de los primeros registros que su valor original es NA. (Nº5)
  #         }#end if(i<=relacion) (Nº4)
  #         
  #         else { #Si el valor del registro i es superior al valor de la relacion(Nº7)
  #           if(!is.na(aut1[,elemento][i])){ #Si no es NA, se guarda valor original
  #             aut1[,elemento][i] <- aut1[,elemento][i]
  #           } else if (is.na(aut1[,elemento][i])){ #Si es NA, proceso de relleno por visualización de relación.
  #             contador <- 0
  #             for (j in 1:relacion) {
  #               if(aut1[,elemento][i-j]=='N'){
  #                 contador <- contador + 1}
  #               else if (aut1[,elemento][i-j]=='Y'){
  #                 contador <- 0
  #               }}
  #             if(contador==relacion){aut1[,elemento][i]='Y'}
  #             else{ aut1[,elemento][i]='N'}
  #             
  #           }
  #         } #(Nº7)   
  #       }#end for(i in 1:dim(aut1)[1]) #(Nº3)
  #     }#end if(bueno<=0.85) 
  #     else {aut1[,elemento]<-NA} #(Nº2)
  #   }else if(length(frecuencia)==1){ #Si no existen SI y NO en la variable
  #     if (names(table(aut1[,elemento]))=="Y"){ # Si solo hay SI escribo todo SI
  #       aut1[,elemento]='Y'} 
  #     else{aut1[,elemento]='N'} # Si solo hay NO escribo todo NO
  #   } else if(length(frecuencia)==0) { #SI TODA LA VARIABLE ES NA
  #     aut1[,elemento]<-NA
  #   } #end Nº1
  # }#end for variable in names  
  # 
  # #For que transforma los 'Y' en 0.
  # for (variable in names) {
  #   elemento <- paste("Limpieza_campana_nuevo_",variable,sep = "")
  #   marcadorY <- is.element(aut1[,elemento],c('Y'))
  #   elementocumulativo <- paste("cumulativo_",variable,sep = "")
  #   aut1[marcadorY,elementocumulativo]<-0
  #   
  # }
  # #For que realiza el contador
  # for (variable in names){
  #   CochesDesdeLimpcampana <- paste("CochesDesdeLimpcampana",variable,sep="" )
  #   cogercumulativo <- paste("cumulativo_",variable,sep="")
  #   cogerlimpiezanuevo <- paste("Limpieza_campana_nuevo_", variable, sep="")
  #   aut1[,CochesDesdeLimpcampana] <- ave(aut1[,cogercumulativo], aut1[,cogerlimpiezanuevo] ,cumsum(is.element(aut1[,cogerlimpiezanuevo] ,'Y')), FUN = cumsum)
  # }
  # #For que pasa al archivo bueno  
  # for (variable in names) {
  #   bueno <- paste("Limpieza_campana.",variable,sep = "")
  #   creado <- paste("CochesDesdeLimpcampana",variable, sep = "")
  #   dfeb1[,bueno] <- aut1[,creado] 
  # }
  # 
  # 
  # ######CABINA3 cochesdesdecamp#####
  # aut3<- dfeb3%>%select(contains("FechaRobot."),contains("Limpieza_campana."))
  # names <- c("BCIN.Robot.1.1","BCIN.Robot.1.2","BCIN.Robot.2.1","BCIN.Robot.2.2","BC1.Robot.1.1","BC1.Robot.1.2","BC1.Robot.2.1","BC1.Robot.2.2","BC2.Robot.1.1","BC2.Robot.1.2","BC2.Robot.2.1","BC2.Robot.2.2","CC1.Robot.1.3","CC1.Robot.1.4","CC1.Robot.2.3","CC1.Robot.2.4")
  # for (variable in names) {
  #   nombrevariable <- paste("cumulativo",variable,sep = "_")
  #   aut3[,nombrevariable]<- 1 }
  # for (variable in names) {
  #   nombrevariable <- paste("Limpieza_campana_nuevo",variable,sep = "_")
  #   elegirvariable <- paste("Limpieza_campana.",variable,sep = "")
  #   aut3[,nombrevariable] <- aut3%>%select(elegirvariable)
  #   
  # }
  # #For que rellena los NA según la frecuencia.
  # for (variable in names) {
  #   elemento <- paste("Limpieza_campana_nuevo_",variable,sep = "")
  #   frecuencia <- as.vector(table(aut3[,elemento])) #se obtienen los No frente a los Si
  #   bueno <- sum(is.na(aut3[,elemento]))/(dim(aut3)[1]) # Se calcula el ratio de NA's que hay en la base
  #   relacion <- round(frecuencia[1]/frecuencia[2],) #Relacion de NO frente a SI
  #   
  #   if(length(frecuencia)>1){ #Si existen ambos valores SI y NO para una variable (Nº1)
  #     if(bueno<=0.85){ #Si el ratio de NA's es menor de 0.85, es decir, si hay menos del 85% de datos omitidos. (Nº2)
  #       for (i in 1:dim(aut3)[1]) { #bucle que recorre toda la base (Nº3)
  #         if(i<=relacion) { #para completar los primeros registros de la frecuencia, por si son NA que se rellenen y luego no colapse el programa. (Nº4)
  #           if(!is.na(aut3[,elemento][i])){ #si no es NA, coge el valor original
  #             aut3[,elemento][i] <- aut3[,elemento][i]
  #           }else if (is.na(aut3[,elemento][i])){ #si es NA realiza operación de relleno con frecuencia.(Nº5)
  #             contador <- 0 #pongo contador a 0
  #             
  #             for (j in 1:relacion) { #desde el registro 1 hasta el relacion(valor numerico entero) (Nº6)
  #               if(is.na(aut3[,elemento][i+j])){ #Si el valor es NA, el contador se mantiene con el valor actual
  #                 contador <- contador 
  #               }else if (aut3[,elemento][i+j]=='N'){ #Si el valor es NO, el contador suma 1. (Limpieza desde...)
  #                 contador <- contador + 1}
  #               else if (aut3[,elemento][i+j]=='Y'){ #Si el valor es YES, se reinicia el contador, se ha realizado limpieza
  #                 contador <- 0}
  #             }#end for j (Nº6)
  #             
  #             if(contador==relacion){aut3[,elemento][i]='Y'} #Si el contador alcanza la relación, se debe poner un SI, ya que se ha alcanzado la relación de No frente a Si.
  #             else{ aut3[,elemento][i]='N'} #Si el contador sigue sin alzanzar la relacion, se mantiene el NO hasta que llegue.
  #           }#Acabo proceso de relleno de los primeros registros que su valor original es NA. (Nº5)
  #         }#end if(i<=relacion) (Nº4)
  #         
  #         else { #Si el valor del registro i es superior al valor de la relacion(Nº7)
  #           if(!is.na(aut3[,elemento][i])){ #Si no es NA, se guarda valor original
  #             aut3[,elemento][i] <- aut3[,elemento][i]
  #           } else if (is.na(aut3[,elemento][i])){ #Si es NA, proceso de relleno por visualización de relación.
  #             contador <- 0
  #             for (j in 1:relacion) {
  #               if(aut3[,elemento][i-j]=='N'){
  #                 contador <- contador + 1}
  #               else if (aut3[,elemento][i-j]=='Y'){
  #                 contador <- 0
  #               }}
  #             if(contador==relacion){aut3[,elemento][i]='Y'}
  #             else{ aut3[,elemento][i]='N'}
  #             
  #           }
  #         } #(Nº7)   
  #       }#end for(i in 1:dim(aut3)[1]) #(Nº3)
  #     }#end if(bueno<=0.85) 
  #     else {aut3[,elemento]<-NA} #(Nº2)
  #   }else if(length(frecuencia)==1){ #Si no existen SI y NO en la variable
  #     if (names(table(aut3[,elemento]))=="Y"){ # Si solo hay SI escribo todo SI
  #       aut3[,elemento]='Y'} 
  #     else{aut3[,elemento]='N'} # Si solo hay NO escribo todo NO
  #   } else if(length(frecuencia)==0) { #SI TODA LA VARIABLE ES NA
  #     aut3[,elemento]<-NA
  #   } 
  # }#end for variable in names  
  # 
  # #For que transforma los 'Y' en 0.
  # for (variable in names) {
  #   elemento <- paste("Limpieza_campana_nuevo_",variable,sep = "")
  #   marcadorY <- is.element(aut3[,elemento],c('Y'))
  #   elementocumulativo <- paste("cumulativo_",variable,sep = "")
  #   aut3[marcadorY,elementocumulativo]<-0
  #   
  # }
  # #For que realiza el contador
  # for (variable in names){
  #   CochesDesdeLimpcampana <- paste("CochesDesdeLimpcampana",variable,sep="" )
  #   cogercumulativo <- paste("cumulativo_",variable,sep="")
  #   cogerlimpiezanuevo <- paste("Limpieza_campana_nuevo_", variable, sep="")
  #   aut3[,CochesDesdeLimpcampana] <- ave(aut3[,cogercumulativo], aut3[,cogerlimpiezanuevo] ,cumsum(is.element(aut3[,cogerlimpiezanuevo] ,'Y')), FUN = cumsum)
  # }
  # #For que pasa a el archivo bueno  
  # for (variable in names) {
  #   bueno <- paste("Limpieza_campana.",variable,sep = "")
  #   creado <- paste("CochesDesdeLimpcampana",variable, sep = "")
  #   dfeb3[,bueno] <- aut3[,creado] 
  # }
  # #####
  # Fin.ciclo.fila <- smartbind(dfeb1,dfeb3)
  # ####VOLVEMOS A TOMAR LA HORA Y FECHA####
  # Fin.ciclo.fila$Fechaminima <- parse_date_time(x = Fin.ciclo.fila$Fechaminima,
  #                                               orders = c("ymd_HMS")) #pasa a formato tiempo (POSIXct) debido a que smartbind lo deshace.
  # Fin.ciclo.fila  <- Fin.ciclo.fila [order(Fin.ciclo.fila$Fechaminima),] #lo ordena temporalmente.
  # 
  # 
  # #Eliminamos variables creadas como Fechaminima(224) y cumulativo(297).
  # Fin.ciclo.fila <- Fin.ciclo.fila[,-which(names(Fin.ciclo.fila) %in% c("cumulativo"))] #SI QUEREMOS FILTRAR POR TIEMPO EN PINTADO, DEBEMOS MANTENER FECHAMINIMA.
  # ####APLICACION DEL FILTRO PASADA 1 EN FIN.CICLO.FILA####
  # #Fin.ciclo.fila <- Fin.ciclo.fila[Fin.ciclo.fila$pasadapintura==1,]
  # 
  # 
  # #####MEZCLADO GENERAL####
  # #ACOMODAMOS TODOS LOS NOMBRES, PARA QUE JUNTE POR PASADA.
  # colnames(Fin.ciclo.fila)[colnames(Fin.ciclo.fila) == "pasadapintura"] <- "pasadarealpintura"
  # 
  # 
  # #FILTRADO POR PASADAS, SINO DA PROBLEMAS, DUPLICACION DE DATOS####
  # #marcadorpasada1Insp <- is.element(Inspeccion$pasadareal,1); marcadorpasada1Fincicl <- is.element(Fin.ciclo.fila$pasadareal,1) ; marcadorpasada1vent <- is.element(Ventilaciones$pasadareal,1)
  # #marcadorpasada2Insp <- is.element(Inspeccion$pasadareal,2); marcadorpasada2Fincicl <- is.element(Fin.ciclo.fila$pasadareal,2) ; marcadorpasada2vent <- is.element(Ventilaciones$pasadareal,2)
  # #marcadorpasada3Insp <- is.element(Inspeccion$pasadareal,3); marcadorpasada3Fincicl <- is.element(Fin.ciclo.fila$pasadareal,3) ; marcadorpasada3vent <- is.element(Ventilaciones$pasadareal,3)
  # #marcadorpasada4Insp <- is.element(Inspeccion$pasadareal,4); marcadorpasada4Fincicl <- is.element(Fin.ciclo.fila$pasadareal,4) ; marcadorpasada4vent <- is.element(Ventilaciones$pasadareal,4)
  # marcadorcabinaISNP <- is.element(Inspeccion$cabina,c(1,3))#PARA ESTE FICHERO
  # 
  # #Ventilaciones <- Ventilaciones[marcadorpasada1vent,]
  # #Fin.ciclo.fila <- Fin.ciclo.fila[marcadorpasada1Fincicl,]
  # Inspeccion <- Inspeccion[marcadorcabinaISNP,] #marcadorpasada1Insp & 
  # 
  # ####TODO MEZZLADO Y ELIMINACION DE COCHES RAROS####
  # General <- merge(Fin.ciclo.fila,Inspeccion,by.x = c("MANG_COLOR_MET","MANG_SOL_MET"), by.y=c("manguera1","manguera2"), all = FALSE)
  # #General <- merge(Fin.ciclo.fila,Inspeccion,by.x = "VEHICLE", by.y="n_pedido", all = FALSE)
  # filtrotiemposmalos <- difftime(General$fecha,General$Fechaminima,units = c("min")) > 0 #Es decir, si inspeccion es posterior a pintura
  # General <- General[filtrotiemposmalos,]
  # 
  # ####FILTRO COGER PRIMER HORA####
  # coches <- as.vector(names(table(General$VEHICLE)))
  # for ( coche in coches){
  #   tiempos <-as.vector(names(table(General$Fechaminima[General$VEHICLE==coche]))) #TRANSFORMAR
  #   tiempos <- parse_date_time(x = tiempos,
  #                              orders = c("ymd_HMS")) #pasa a formato tiempo (POSIXct) debido a que smartbind lo deshace.
  #   tiempos <- tiempos[order(tiempos,decreasing = F)]
  #   for (tiempo in tiempos) {
  #     menores <- as.vector(names(table(General$fecha[General$VEHICLE==coche & General$Fechaminima==tiempo])))
  #     menores <- parse_date_time(x = menores,
  #                                orders = c("ymd_HMS")) #pasa a formato tiempo (POSIXct) debido a que smartbind lo deshace.
  #     menores <- menores[order(menores,decreasing = F)]
  #     if (length(menores)>1) {
  #       for (i in 2:length(menores)) {
  #         filtroquitar <- is.element(General$VEHICLE,coche) & is.element(General$Fechaminima,tiempo) & is.element(General$fecha,menores[i])
  #         General <- General[!filtroquitar,]
  #       }
  #       
  #     } else{}
  #   }
  # }
  # 
  # #filtrotiemposextremos <- difftime(General$fecha,General$Fechaminima,units = c("min"))<420 #Si la inspeccion se hace 5h más tarde es porque seguramente nos encontremos en otra pasada.
  # #General <- General[filtrotiemposextremos,]
  # table(list(General$cabina,General$ProcessArea))
  # #controlador <- (is.element(General$cabina,3) & is.element(General$ProcessArea,"41 Acabado 1")) | (is.element(General$cabina,1) & is.element(General$ProcessArea,"43 Acabado 3"))
  # #ESTAMAL <- General[controlador,]
  # #ESTAMAL <-ESTAMAL%>% select("VEHICLE","Fechaminima","fecha","cabina",contains("Process"))
  # #ESTAMAL$DIFERENCIA <- difftime(ESTAMAL$fecha,ESTAMAL$Fechaminima,units = c("min"))
  # #VEMOS QUE HAY COCHES QUE ENTRAN EN NUESTRO FILTRO, PERO QUE SON MÍNIMO, LOS ELIMINAMOS.
  # controlador <- (is.element(General$cabina,1) & is.element(General$ProcessArea,"41 Acabado 1")) | (is.element(General$cabina,3) & is.element(General$ProcessArea,"43 Acabado 3"))
  # General <- General[controlador,]
  # 
  # 
  # #table(duplicated(General)) #Comprobar si hay valores duplicados.
  # 
  # ####ADECUACION BASE ####
  # ####MODIFICACION DE VARIABLES###
  # 
  # #Eliminamos variables repetidas
  # General <- General[,-which(names(General)%in%c("Work Cell","MANG_COLOR_MET","MANG_SOL_MET","ProcessArea" ,"tnc","moldura","tb","flags","id_color","id_color2","id_pais","ventanilla_metalica","x_min","y_min","xmax","ymax","id_region","id_defecto_pintado","id_camara","id_inspeccion","fecha_horno"))]
  # General <- select(General, -contains("sen_"))
  # #General <- select(General,-c("c01":"c11"))
  # #Quitamos variables repetidas y otras que no nos sirven por simplificar.
  # 
  # General$Num_defectos <- NA
  # General$Suma_pesos <- NA
  # 
  # #Ordenamos las variables
  # General <- General %>% select("VEHICLE","pasadareal","cabina","id_modelo",contains(".BCIN.Robot.1.1"),contains(".BCIN.Robot.1.2"),contains(".BCIN.Robot.2.1"),contains(".BCIN.Robot.2.2"),
  #                               contains(".BC1.Robot.1.1"),contains(".BC1.Robot.1.2"),contains(".BC1.Robot.2.1"),contains(".BC1.Robot.2.2"),
  #                               contains(".BC2.Robot.1.1"),contains(".BC2.Robot.1.2"),contains(".BC2.Robot.2.1"),contains(".BC2.Robot.2.2"),
  #                               contains(".CC1.Robot.1.3"),contains(".CC1.Robot.1.4"),contains(".CC1.Robot.2.3"),contains(".CC1.Robot.2.4"),
  #                               "id_tipo_defecto":"techo_solar","pasada":"Linea","Num_defectos":"Suma_pesos") #AQUI ELIMINAMOS pasadareal.x que es pasada de pintura irreal.
  # 
  # 
  # colnames(General)[colnames(General) == "pasadareal"] <- "PasadaReal"
  # General <- General[order(General$fecha),]
  # 
  # #LA PASADA LA DEBE MARCAR EL TÚNEL QUE ES EL QUE TENEMOS DATOS COMPLETOS
  # Primerapasada <- General[General$PasadaReal==1&General$pasada==1,]
  # Segundapasada <- General[General$PasadaReal==2,]
  # Tercerapasada <- General[General$PasadaReal==3,]
  # Cuartapasada  <- General[General$PasadaReal==4,]
  # Quintapasada  <- General[General$PasadaReal==5,]
  # Sextapasada   <- General[General$PasadaReal==6,]
  # 
  # 
  # ####GENERAR EXCELL Y CSV####
  # library (openxlsx)
  # write.csv(Primerapasada , file= "Estudio1_Primerapasada.csv")
  # 
  # #write.xlsx(Quintapasada, file = "Estudio2_Quintapasada.xlsx")
  # 
  # #write.csv(Primerapasada, file= "FICHERONUEVAORDENACIONYCAMPANA_0_0_0_T_0_0_0_0_000000_000000_00_00.csv")
  # #library (openxlsx)
  # #write.xlsx(General, file = "Estudio1_0_0_0_T_0_0_0_0_000000_000000_0_0.xlsx")
  # 
  # #PARA VER SI SE HAN COLADO DATOS
  # #comprobacion <- as.data.frame( as.vector( tapply( Primerapasada$VEHICLE,  Primerapasada$VEHICLE  , min)) ) 
  # #comprobacion$fechamax <- as.vector(tapply(Primerapasada$fecha, Primerapasada$VEHICLE  , max))
  # #comprobacion$fechamin <- as.vector(tapply(Primerapasada$fecha, Primerapasada$VEHICLE  , min))
  # #comprobacion$diferencia <- comprobacion$fechamax-comprobacion$fechamin
  # #table(comprobacion$diferencia)
}
