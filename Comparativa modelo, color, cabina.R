#Comparativa entre modelos, y cabinas


#Modelo 17, color 14, cabina 1

  {modelo17_color14_cabina1=readRDS("C:/Users/usuario/Desktop/Iñigo/modelo17_color14_cabina1.Rda")
  list_M_1_B=modelo17_color14_cabina1[[1]]
  list_M_2_B=modelo17_color14_cabina1[[2]]
  list_M_1_P=modelo17_color14_cabina1[[3]]
  list_M_2_P=modelo17_color14_cabina1[[4]]
  list_M_2_H=modelo17_color14_cabina1[[5]]
  aux_df_modelo17_color14_cabina1=readRDS("C:/Users/usuario/Desktop/Iñigo/aux_df_modelo17_color14_cabina1.Rda")
  }

#Modelo 14, color 14, cabina 1

  {modelo14_color14_cabina1=readRDS("C:/Users/usuario/Desktop/Iñigo/modelo14_color14_cabina1.Rda")
  list_M_1_B_14=modelo14_color14_cabina1[[1]]
  list_M_2_B_14=modelo14_color14_cabina1[[2]]
  list_M_1_P_14=modelo14_color14_cabina1[[3]]
  list_M_2_P_14=modelo14_color14_cabina1[[4]]
  list_M_2_H_14=modelo14_color14_cabina1[[5]]
  aux_df_modelo14_color14_cabina1=readRDS("C:/Users/usuario/Desktop/Iñigo/aux_df_modelo14_color14_cabina1.Rda")
  }

#Modelo 17, color 14, cabina 3

  {modelo17_color14_cabina3=readRDS("C:/Users/usuario/Desktop/Iñigo/modelo17_color14_cabina2.Rda")
  list_M_1_B_17_14_3=modelo17_color14_cabina3[[1]]
  list_M_2_B_17_14_3=modelo17_color14_cabina3[[2]]
  list_M_1_P_17_14_3=modelo17_color14_cabina3[[3]]
  list_M_2_P_17_14_3=modelo17_color14_cabina3[[4]]
  list_M_2_H_17_14_3=modelo17_color14_cabina3[[5]]
  aux_df_modelo17_color14_cabina3=readRDS("C:/Users/usuario/Desktop/Iñigo/aux_df_modelo17_color14_cabina3.Rda")
  }

#Modelo 14, color 14, cabina 3

  {modelo14_color14_cabina3=readRDS("C:/Users/usuario/Desktop/Iñigo/modelo14_color14_cabina2.Rda")
  list_M_1_B_14_14_3=modelo14_color14_cabina3[[1]]
  list_M_2_B_14_14_3=modelo14_color14_cabina3[[2]]
  list_M_1_P_14_14_3=modelo14_color14_cabina3[[3]]
  list_M_2_P_14_14_3=modelo14_color14_cabina3[[4]]
  list_M_2_H_14_14_3=modelo14_color14_cabina3[[5]]
  aux_df_modelo14_color14_cabina3=readRDS("C:/Users/usuario/Desktop/Iñigo/aux_df_modelo14_color14_cabina3.Rda")
  }



#Comparativa

aux.a=list_M_2_B
aux.b=list_M_2_B_14

aux.tabla=tabla_comparativa(aux.a,aux.b)
# apply(aux.tabla, 2, function(x) duplicated(x))




#Comparativa

  #VARIABLES, ESTIMATES, P-VALUE

list.aux.tabla=c()
aux.a=modelo17_color14_cabina1
aux.b=modelo14_color14_cabina1

p_valor=0.001
for (i in 1:(length(aux.b)-1)) {
  list.aux.tabla[[i]]=tabla_comparativa(aux.a[[i]],aux.b[[i]], p_valor)
}

for (i in 3:4) {
  
  obten_tabla(list.aux.tabla[[i]],hline = TRUE, rownames = FALSE)
  
}



list.aux.tabla=c()
aux.c=modelo17_color14_cabina3
aux.d=modelo14_color14_cabina3

p_valor=0.001
for (i in 1:(length(aux.b)-1)) {
  list.aux.tabla[[i]]=tabla_comparativa(aux.c[[i]],aux.d[[i]], p_valor)
}

for (i in 3:4) {
  
  obten_tabla(list.aux.tabla[[i]],hline = TRUE, rownames = FALSE)
  
}






  #AIC

list_df_comparativa=c()
for (i in 1:(length(modelo14_color14_cabina1))) {
  
  aux.a=modelo17_color14_cabina1[[i]]
  aux.b=modelo14_color14_cabina1[[i]]
  
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
  list_df_comparativa[[i]]=data.frame(M1,M2)


}






#AIC M_2

list_df_comparativa=c()
aux.a=modelo17_color14_cabina1
aux.b=modelo14_color14_cabina1
  
for (i in 1:length(aux.b)) {

  M1=c()
  M2=c()
  
  aux.a_i=aux.a[[i]]
  aux.b_i=aux.b[[i]]
  
  for (zona in zonas) {
    a=aux.a_i[[zona]]
    b=aux.b_i[[zona]]
    

    if(all(is.na(a))){aic_a=NA}else{aic_a=round(AIC(a),0)}
    if(all(is.na(b))){aic_b=NA}else{aic_b=round(AIC(b),0)}
    M1=c(M1,aic_a)
    M2=c(M2,aic_b)  
  }
  
  list_df_comparativa[[i]]=data.frame(M1,M2)
  
  
}

prueba=cbind(list_df_comparativa[[2]],list_df_comparativa[[4]],list_df_comparativa[[5]])
prueba=prueba[,c(1,3,5,2,4,6)]
colnames(prueba)=c("Binomial V1", "Poisson V1", "Hurdle V1","Binomial V2", "Poisson V2", "Hurdle V2")
obten_tabla(prueba,hline = TRUE)

prueba=cbind(list_df_comparativa[[2]],list_df_comparativa[[4]],list_df_comparativa[[5]])
prueba=prueba[,c(1,3,5,2,4,6)]
colnames(prueba)=c("Binomial V1", "Poisson V1", "Hurdle V1","Binomial V2", "Poisson V2", "Hurdle V2")
obten_tabla(prueba,hline = TRUE)

list_df_comparativa=c()
aux.c=modelo17_color14_cabina3
aux.d=modelo14_color14_cabina3

for (i in length(aux.b)) {
  
  M1=c()
  M2=c()
  aux.c_i=aux.c[[i]]
  aux.d_i=aux.d[[i]]
  
  for (zona in zonas) {
    c=aux.c_i[[zona]]
    d=aux.d_i[[zona]]
    
    aux.modelos=c(c,d)
    
    if(is.na(c)){aic_c=NA}else{aic_c=round(AIC(c),0)}
    if(is.na(d)){aic_d=NA}else{aic_d=round(AIC(d),0)}
    M1=c(M1,aic_c)
    M2=c(M2,aic_d)  
  }
  list_df_comparativa[[i]]=data.frame(M1,M2)
  
  
}

prueba=cbind(list_df_comparativa[[2]],list_df_comparativa[[4]],list_df_comparativa[[5]])
prueba=prueba[,c(1,3,5,2,4,6)]
colnames(prueba)=c("Binomial V1", "Poisson V1", "Hurdle V1","Binomial V2", "Poisson V2", "Hurdle V2")
obten_tabla(prueba,hline = TRUE)


#R2 M_2

{

list_df_comparativa=c()
aux.a=modelo17_color14_cabina1
aux.b=modelo14_color14_cabina1
AUX.y.R2=c("Num_defectos_BIN","Num_defectos_BIN","Num_defectos_POI","Num_defectos_POI","Num_defectos")
aux.df.a=aux_df_modelo17_color14_cabina1
aux.df.b=aux_df_modelo14_color14_cabina1
  
for (i in 1:length(aux.b)) {
  
  M1=c()
  M2=c()
  
  aux.a_i=aux.a[[i]]
  aux.b_i=aux.b[[i]]
  aux.y_i=AUX.y.R2[[i]]

  aux.df.a_i=aux.df.a[[i]]
  aux.df.b_i=aux.df.b[[i]]
    
  for (zona in zonas) {
    a=aux.a_i[[zona]]
    b=aux.b_i[[zona]]
    
    if(all(is.na(a))){R2_a=NA}else{    
      probs = predict(a,aux.df.a_i[[zona]], type="response")
      r2=postResample(pred = probs, obs = (aux.df.a_i[[zona]])[[aux.y_i]])[2]
      R2_a=round(r2,3)}
    if(all(is.na(b))){R2_b=NA}else{
      probs = predict(b,aux.df.b_i[[zona]], type="response")
      r2=postResample(pred = probs, obs = (aux.df.b_i[[zona]])[[aux.y_i]])[2]
      R2_b=round(r2,3)}
    M1=c(M1,R2_a)
    M2=c(M2,R2_b)  
  }
  
  list_df_comparativa[[i]]=data.frame(M1,M2)
  
  
}

prueba=cbind(list_df_comparativa[[2]],list_df_comparativa[[4]],list_df_comparativa[[5]])
prueba=prueba[,c(1,3,5,2,4,6)]
colnames(prueba)=c("Binomial V1", "Poisson V1", "Hurdle V1","Binomial V2", "Poisson V2", "Hurdle V2")
obten_tabla(prueba,hline = TRUE)


list_df_comparativa=c()
aux.c=modelo17_color14_cabina3
aux.d=modelo14_color14_cabina3
AUX.y.R2=c("Num_defectos_BIN","Num_defectos_BIN","Num_defectos_POI","Num_defectos_POI","Num_defectos")
aux.df.c=aux_df_modelo17_color14_cabina3
aux.df.d=aux_df_modelo14_color14_cabina3

for (i in 1:length(aux.c)) {
  
  M1=c()
  M2=c()
  aux.c_i=aux.c[[i]]
  aux.d_i=aux.d[[i]]
  aux.y_i=AUX.y.R2[[i]]
  
  aux.df.c_i=aux.df.c[[i]]
  aux.df.d_i=aux.df.d[[i]]
  
  for (zona in zonas) {
    c=aux.c_i[[zona]]
    d=aux.d_i[[zona]]
    
    if(all(is.na(c))){R2_c=NA}else{    
      probs = predict(c,aux.df.c_i[[zona]], type="response")
      r2=postResample(pred = probs, obs = (aux.df.c_i[[zona]])[[aux.y_i]])[2]
      R2_c=round(r2,3)}
    if(all(is.na(d))){R2_d=NA}else{
      probs = predict(d,aux.df.d_i[[zona]], type="response")
      r2=postResample(pred = probs, obs = (aux.df.d_i[[zona]])[[aux.y_i]])[2]
      R2_d=round(r2,3)}
    M1=c(M1,R2_c)
    M2=c(M2,R2_d)  
  }
  list_df_comparativa[[i]]=data.frame(M1,M2)
  
  
}

prueba=cbind(list_df_comparativa[[2]],list_df_comparativa[[4]],list_df_comparativa[[5]])
prueba=prueba[,c(1,3,5,2,4,6)]
colnames(prueba)=c("Binomial V1", "Poisson V1", "Hurdle V1","Binomial V2", "Poisson V2", "Hurdle V2")
obten_tabla(prueba,hline = TRUE)


}








#Hurdle 12 zonas Modelo 17, color 14, cabina 1



comparativa_hurdle_count=c()
for (zona in zonas) {
  aux.comparativa_hurdle_count=data.frame(zona,row.names((summary(list_M_2_H[[zona]])$coefficients)$count),round((summary(list_M_2_H[[zona]])$coefficients)$count,2))
  comparativa_hurdle_count=rbind(comparativa_hurdle_count,aux.comparativa_hurdle_count)
}
obten_tabla(comparativa_hurdle_count,hline = TRUE, rownames = FALSE)

comparativa_hurdle_zero=c()
for (zona in zonas) {
  aux.comparativa_hurdle_zero=data.frame(zona,row.names((summary(list_M_2_H[[zona]])$coefficients)$zero),round((summary(list_M_2_H[[zona]])$coefficients)$zero,2))
  comparativa_hurdle_zero=rbind(comparativa_hurdle_zero,aux.comparativa_hurdle_zero)
}
obten_tabla(comparativa_hurdle_zero,hline = TRUE, rownames = FALSE)

