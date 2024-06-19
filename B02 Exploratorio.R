

##-------------------------------------------------------------------## 
##  ~~~~~*~~~~~ * ~~~~~*~~~~~ EXPLORATORIO ~~~~~*~~~~~ * ~~~~~*~~~~~ ## 
##-------------------------------------------------------------------## 



aux.df = response.hourly.itinere[  is.element(response.hourly.itinere$itinere.key, c(1,2) ),  ]
aux.year=names(table(aux.df$year))[-which(names(table(aux.df$year))=="2020")]
df_general=subset(aux.df, is.element(aux.df$year, aux.year))
aux.df=df_general
#aux.df$itinere.key=(aux.df$itinere.key>1)*1 #Paso de itinere.key-IDA
  #Paso de itinere.key-IDA == 1--->itinere.key-IDA == 0
  #Paso de itinere.key-VUELTA == 2--->itinere.key-VUELTA == 1

y=aux.df$y 
aux.df$y_b=(y>0)+0
y_b=aux.df$y_b
aux.y=y_b
modelos.df=data.frame()

aux.df$y_count=aux.df$y
aux.df1=aux.df[aux.df$y_count>0,]
aux.df1$y=aux.df1$y_count-1

aux.df_binomial=aux.df
aux.df_poisson=aux.df1
aux.df_hurdle=aux.df

#Base_0 

{
  # aux.family="binomial"
  # variables_1=c("(c1h+s1h+c2h+s2h+c3h+s3h)*(entre_semana+sabado+festivo)*(itinere.key)")
  # variables_2=c("entre_semana")
  # variables_3=c("sabado")
  # variables_4=c("festivo") #domingos y fiestas
  # variables_5=c("salida_0_15+puesta_0_15+noche +banda_15_50")
  # variables_6=c("lluvia.decalada")
  # 
  # variables=rbind(variables_1,variables_2,variables_3,variables_4,variables_5,variables_6)
  # tiempos=data.frame()
  # model_list=c()      
  # 
  # for (i in 1:length(variables)) {
  #   t0<- proc.time()
  #   formula=as.formula(paste("y_b", variables[i], sep=" ~ "))
  #   aux.model= glm(formula, data=aux.df, family = aux.family,control = glm.control(maxit=100))
  #   aux.t_i=proc.time()-t0
  #   tiempos=rbind(tiempos,aux.t_i)
  #   model_list=c(model_list,list(aux.model))           #Guarda el modelo el la lista de los modelos
  #   
  # }
  # tiempos=tiempos[,1:3]
  # colnames(tiempos)=c("user",  "system", "elapsed" )
  # tiempos
  # obten_modelos(model_list)
  
  
  
  
  # aux.formula.base.y <- formula(y~( s1d+c1d +s2d +c2d+ s3d +c3d ) + as.factor(itinere.key) + ( s1h +c1h +s2h+ c2h ) * (as.factor(itinere.key) *( fiesta+ finde))  )
  # aux.formula.deslumbramiento.y <- formula(y~  (s1d+c1d +s2d +c2d+ s3d +c3d ) +  as.factor(itinere.key) +  deslumbramiento* as.factor(itinere.key)+ ( s1h +c1h +s2h+ c2h ) * (as.factor(itinere.key) *( fiesta+ finde))  )
  # aux.formula.lluvia.y <- formula(y~( s1d+c1d +s2d +c2d+ s3d +c3d ) + as.factor(itinere.key) + lluvia* as.factor(itinere.key)+ ( s1h +c1h +s2h+ c2h ) * (as.factor(itinere.key)  *( fiesta+ finde))   )
  # aux.formula.lluvia.deslumbramiento.y <- formula(y~  (s1d+c1d +s2d +c2d+ s3d +c3d ) +  as.factor(itinere.key) +  lluvia* as.factor(itinere.key)+  deslumbramiento* as.factor(itinere.key)+ ( s1h +c1h +s2h+ c2h ) * (as.factor(itinere.key) *( fiesta+ finde))  )
  # aux.formula.alturasol.y <- formula(y~ altitud  + as.factor(itinere.key) + lluvia* as.factor(itinere.key)+ ( s1h +c1h +s2h+ c2h ) * (as.factor(itinere.key) *( fiesta+ finde))  )
  # 
  # aux.df$y_count <- aux.df$y
  # aux.df$y <-  0+ (aux.df$y>0)
  # 
  # aux.formula.y <- aux.formula.base.y
  # aux.model  <- glm( aux.formula.y, data= aux.df, family = "binomial")
  # summary(aux.model)
  # AIC(aux.model)
  # 
  # start.time <- Sys.time()
  # aux.model.step <- step(aux.model  )
  # end.time <- Sys.time()
  # time.taken <- round(end.time - start.time,2)
  # time.taken
  # summary(aux.model.step)
  # AIC(aux.model.step)
  
  
  
  
  
  
  
  
  
  
  
  
  # aux.base0_0=glm(y_b~(c1h+s1h+c2h+s2h+c3h+s3h)*(entre_semana+sabado+festivo)*(itinere.key),data=aux.df, family = "binomial",control = glm.control(maxit=100))
  # aux.base0_1=glm(y_b~(c1h+s1h+c2h+s2h+c3h+s3h),data=aux.df, family = "binomial",control = glm.control(maxit=100))
  # aux.base0_2=glm(y_b~entre_semana,data=aux.df, family = "binomial",control = glm.control(maxit=100))
  # aux.base0_3=glm(y_b~sabado,data=aux.df, family = "binomial",control = glm.control(maxit=100))
  # aux.base0_4=glm(y_b~domingo,data=aux.df, family = "binomial",control = glm.control(maxit=100))
  # list.base0_0=list(aux.base0_0,aux.base0_1,aux.base0_2,aux.base0_3,aux.base0_4)
  # obten_modelos(list.base0_0) 
  
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~BINOMIAL~*~~~~~~~~~~~~~~~~~~~~~~~~~~~## 

aux.df=aux.df_binomial
aux.y="y_b"
aux.family="binomial"

##-------------------------------------------------------------------## 
##                          M_B_1 - MEZCLA                           ## 
##-------------------------------------------------------------------## 

  {
  
      #Base 
      
      {
        aux.base0=glm(y_b~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(itinere.key==2),data=aux.df, family = "binomial",control = glm.control(maxit=100))
        aux.model=aux.base0
        summary(aux.base0) #El modelo aux.base0 es el modelo del cual se construir?n el resto mediante :
        # 1) Pimer modelo creado: Modelo de adici?n de las variables de inter?s - Funci?n crear_modelo (Incluida en Funci?n auto.modelo )
        # 2) Siguientes modelos: Modelos con las variables de inter?s y significativas - Funci?n auto.modelo   
        # 3) Mejor modelos: Selecci?n del mejor modelo de entre los anteriores   
        
        aux.model.name="model_base"
        aux.modelo_base=aux.base0
        #auto_model(aux.modelo_base, aux.model.name)
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, , aux.family=aux.family)
        
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_base=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_base=mejor_modelo_base[[2]]
  }
      
      #Lluvia
      
      {
        var.adicional="lluvia.decalada"
        aux.model.name="model_lluvia"
        aux.modelo_base=modelo_base
        
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_lluvia=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_lluvia=mejor_modelo_lluvia[[2]]
        
  }
      
      #Fin de semana
      
      {
        var.adicional="finde"
        aux.model.name="model_wday"
        aux.modelo_base=modelo_base
        
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_wday=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_wday=mejor_modelo_wday[[2]]   
        
        
  }
      
      #Mezcla modelos 
      
      {
        aux.lluvia=terminos(modelo_lluvia)
        aux.wday=terminos(modelo_wday)
        aux.list=c(aux.lluvia,aux.wday)   
        model_mezcla=mezcla_modelo(aux.df,aux.y,aux.list)
        
        aux.model.name="model_mezcla"
        aux.modelo_base=model_mezcla
        #auto_model(aux.modelo_base, aux.model.name)
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, aux.family=aux.family)
        
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_mezcla=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_mezcla=mejor_modelo_mezcla[[2]]
        
        
        
  }
      
      #Adición variables solares en el modelo mezcla
      
      {
        #Bloque solar (salida_0_15+noche+puesta_0_15+banda_15_50)   
        
        aux.modelo_base=modelo_mezcla
        var.adicional="(salida_0_15+noche+puesta_0_15+banda_15_50)" 
        aux.model.name="model_global"
        
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_global=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_solar=mejor_modelo_global[[2]]   
        
  }     
    
    M_B_1=modelo_solar
    M_B_1.df=modelos.df
    
}#MODELIZACION

  #view(M_B_1.df)


##-------------------------------------------------------------------## 
##                          M_B_1 - FIN                              ## 
##-------------------------------------------------------------------## 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 
  
##-------------------------------------------------------------------## 
##                          M_B_2 - FRECUENCIA                       ## 
##-------------------------------------------------------------------## 
  
  {  
    #Base 
    
    {
      aux.base0=glm(y_b~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(itinere.key==2),data=aux.df, family = "binomial",control = glm.control(maxit=100))
      aux.model=aux.base0
      summary(aux.base0) #El modelo aux.base0 es el modelo del cual se construir?n el resto mediante :
      # 1) Pimer modelo creado: Modelo de adici?n de las variables de inter?s - Funci?n crear_modelo (Incluida en Funci?n auto.modelo )
      # 2) Siguientes modelos: Modelos con las variables de inter?s y significativas - Funci?n auto.modelo   
      # 3) Mejor modelos: Selecci?n del mejor modelo de entre los anteriores   
      
      aux.model.name="model_base"
      aux.modelo_base=aux.base0
      #auto_model(aux.modelo_base, aux.model.name)
      auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, aux.family=aux.family)
      
      aux.model.df=auto.modelo[[1]]
      aux.model.list=auto.modelo[[2]]
      modelo.names_list=aux.model.df$model.name
      modelos.df=rbind(modelos.df,aux.model.df)
      
      mejor_modelo_base=mejor_modelo(aux.model.list,modelo.names_list)
      modelo_base=mejor_modelo_base[[2]]
  }
    
    #Solar
    
    {
      #Bloque solar (salida_0_15+noche+puesta_0_15+banda_15_50)   
      
      aux.modelo_base=modelo_base
      var.adicional="(salida_0_15+noche+puesta_0_15+banda_15_50)" 
      aux.model.name="model_solar"
      
      auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
      aux.model.df=auto.modelo[[1]]
      aux.model.list=auto.modelo[[2]]
      modelo.names_list=aux.model.df$model.name
      modelos.df=rbind(modelos.df,aux.model.df)
      
      mejor_modelo_solar=mejor_modelo(aux.model.list,modelo.names_list)
      modelo_solar=mejor_modelo_solar[[2]]   
      
  }  
    
    #Fin de semana
    
    {
      var.adicional="finde"
      aux.model.name="model_wday"
      aux.modelo_base=modelo_solar
      
      auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
      aux.model.df=auto.modelo[[1]]
      aux.model.list=auto.modelo[[2]]
      modelo.names_list=aux.model.df$model.name
      modelos.df=rbind(modelos.df,aux.model.df)
      
      mejor_modelo_wday=mejor_modelo(aux.model.list,modelo.names_list)
      modelo_wday=mejor_modelo_wday[[2]]   
      
      
  }
    
    #Lluvia
    
    {
      var.adicional="lluvia.decalada"
      aux.model.name="model_lluvia"
      aux.modelo_base=modelo_wday
      
      auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
      aux.model.df=auto.modelo[[1]]
      aux.model.list=auto.modelo[[2]]
      modelo.names_list=aux.model.df$model.name
      modelos.df=rbind(modelos.df,aux.model.df)
      
      mejor_modelo_lluvia=mejor_modelo(aux.model.list,modelo.names_list)
      modelo_lluvia=mejor_modelo_lluvia[[2]]
      
  }

    M_B_2=modelo_lluvia
    M_B_2.df=modelos.df
    
  } #MODELIZACION

  #view(M_B_2.df)


##-------------------------------------------------------------------## 
##                          M_B_2 - FIN                              ## 
##-------------------------------------------------------------------## 
  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 
  

  
  
  
  
  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~POISSON~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 
  
aux.df=aux.df_poisson
aux.y="y"
aux.family="poisson"  
    
##-------------------------------------------------------------------## 
##                          M_P_1 - MEZCLA                           ## 
##-------------------------------------------------------------------## 

  {
  
      #Base 
      
      {
        aux.base0=glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(itinere.key==2),data=aux.df, family =aux.family ,control = glm.control(maxit=100))
        aux.model=aux.base0
        summary(aux.base0) #El modelo aux.base0 es el modelo del cual se construir?n el resto mediante :
        # 1) Pimer modelo creado: Modelo de adici?n de las variables de inter?s - Funci?n crear_modelo (Incluida en Funci?n auto.modelo )
        # 2) Siguientes modelos: Modelos con las variables de inter?s y significativas - Funci?n auto.modelo   
        # 3) Mejor modelos: Selecci?n del mejor modelo de entre los anteriores   
        
        aux.model.name="model_base"
        aux.modelo_base=aux.base0
        #auto_model(aux.modelo_base, aux.model.name)
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, aux.family=aux.family)
        
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_base=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_base=mejor_modelo_base[[2]]
  }
      
      #Lluvia
      
      {
        var.adicional="lluvia.decalada"
        aux.model.name="model_lluvia"
        aux.modelo_base=modelo_base
        
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_lluvia=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_lluvia=mejor_modelo_lluvia[[2]]
        
  }
      
      #Fin de semana
      
      {
        var.adicional="finde"
        aux.model.name="model_wday"
        aux.modelo_base=modelo_base
        
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_wday=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_wday=mejor_modelo_wday[[2]]   
        
        
  }
      
      #Mezcla modelos 
      
      {
        aux.lluvia=terminos(modelo_lluvia)
        aux.wday=terminos(modelo_wday)
        aux.list=c(aux.lluvia,aux.wday)   
        model_mezcla=mezcla_modelo(aux.df,aux.y,aux.list)
        
        aux.model.name="model_mezcla"
        aux.modelo_base=model_mezcla
        #auto_model(aux.modelo_base, aux.model.name)
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, aux.family=aux.family)
        
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_mezcla=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_mezcla=mejor_modelo_mezcla[[2]]
        
        
        
  }
      
      #Adición variables solares en el modelo mezcla
      
      {
        #Bloque solar (salida_0_15+noche+puesta_0_15+banda_15_50)   
        
        aux.modelo_base=modelo_mezcla
        var.adicional="(salida_0_15+noche+puesta_0_15+banda_15_50)" 
        aux.model.name="model_global"
        
        auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
        aux.model.df=auto.modelo[[1]]
        aux.model.list=auto.modelo[[2]]
        modelo.names_list=aux.model.df$model.name
        modelos.df=rbind(modelos.df,aux.model.df)
        
        mejor_modelo_global=mejor_modelo(aux.model.list,modelo.names_list)
        modelo_solar=mejor_modelo_global[[2]]   
        
  }     
    
    M_P_1=modelo_solar
    M_P_1.df=modelos.df
    
}#MODELIZACION

  #view(M_P_1.df)


##-------------------------------------------------------------------## 
##                          M_P_1 - FIN                              ## 
##-------------------------------------------------------------------## 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 
  
##-------------------------------------------------------------------## 
##                          M_P_2 - FRECUENCIA                       ## 
##-------------------------------------------------------------------## 
  
  {   
    #Base 
    
    {

      aux.base0=glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(itinere.key==2),data=aux.df, family = aux.family,control = glm.control(maxit=100))
      aux.model=aux.base0
      summary(aux.base0) #El modelo aux.base0 es el modelo del cual se construir?n el resto mediante :
      # 1) Pimer modelo creado: Modelo de adici?n de las variables de inter?s - Funci?n crear_modelo (Incluida en Funci?n auto.modelo )
      # 2) Siguientes modelos: Modelos con las variables de inter?s y significativas - Funci?n auto.modelo   
      # 3) Mejor modelos: Selecci?n del mejor modelo de entre los anteriores   
      
      aux.model.name="model_base"
      aux.modelo_base=aux.base0
      #auto_model(aux.modelo_base, aux.model.name)
      auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, aux.family=aux.family)
      
      aux.model.df=auto.modelo[[1]]
      aux.model.list=auto.modelo[[2]]
      modelo.names_list=aux.model.df$model.name
      modelos.df=rbind(modelos.df,aux.model.df)
      
      mejor_modelo_base=mejor_modelo(aux.model.list,modelo.names_list)
      modelo_base=mejor_modelo_base[[2]]
  }
    
    #Solar
    
    {
      #Bloque solar (salida_0_15+noche+puesta_0_15+banda_15_50)   
      
      aux.modelo_base=modelo_base
      var.adicional="(salida_0_15+noche+puesta_0_15+banda_15_50)" 
      aux.model.name="model_solar"
      
      auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
      aux.model.df=auto.modelo[[1]]
      aux.model.list=auto.modelo[[2]]
      modelo.names_list=aux.model.df$model.name
      modelos.df=rbind(modelos.df,aux.model.df)
      
      mejor_modelo_solar=mejor_modelo(aux.model.list,modelo.names_list)
      modelo_solar=mejor_modelo_solar[[2]]   
      
  }  
    
    #Fin de semana
    
    {
      var.adicional="finde"
      aux.model.name="model_wday"
      aux.modelo_base=modelo_solar
      
      auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
      aux.model.df=auto.modelo[[1]]
      aux.model.list=auto.modelo[[2]]
      modelo.names_list=aux.model.df$model.name
      modelos.df=rbind(modelos.df,aux.model.df)
      
      mejor_modelo_wday=mejor_modelo(aux.model.list,modelo.names_list)
      modelo_wday=mejor_modelo_wday[[2]]   
      
      
  }
    
    #Lluvia
    
    {
      var.adicional="lluvia.decalada"
      aux.model.name="model_lluvia"
      aux.modelo_base=modelo_wday
      
      auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional, aux.family=aux.family)
      aux.model.df=auto.modelo[[1]]
      aux.model.list=auto.modelo[[2]]
      modelo.names_list=aux.model.df$model.name
      modelos.df=rbind(modelos.df,aux.model.df)
      
      mejor_modelo_lluvia=mejor_modelo(aux.model.list,modelo.names_list)
      modelo_lluvia=mejor_modelo_lluvia[[2]]
      
  }

    M_P_2=modelo_lluvia
    M_P_2.df=modelos.df
  } #MODELIZACION

  #view(M_P_2.df)


##-------------------------------------------------------------------## 
##                          M_P_2 - FIN                              ## 
##-------------------------------------------------------------------## 
  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 
  
  
  {
    
    #obten_confint(M_B_1)
    #obten_coeff_confint(M_B_1,hline=TRUE)    
    #riesgo_confint(M_B_1,hline=TRUE)
      
    #obten_confint(M_B_2)
    #obten_coeff_confint(M_B_2,hline=TRUE)
    #riesgo_confint(M_B_2,hline=TRUE)
      
    #obten_confint(M_P_1)
    #obten_coeff_confint(M_P_1,hline=TRUE)
    #riesgo_confint(M_P_1,hline=TRUE)
      
    #obten_confint(M_P_2)
    #obten_coeff_confint(M_P_2,hline=TRUE)
    #riesgo_confint(M_P_2,hline=TRUE)
  } #Coeficientes e intervalos de confianza
  
  {

    #Guardar gráficos en PDF
    #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\binario_poisson.pdf'
    #destination = 'D:\\Users\\Íñigo\\Desktop\\TFG\\Graficos 2023\\binario_poisson.pdf'
    #pdf(file=destination)

    # plot_importancia(M_B_1)
    # plot_model(M_B_1,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
    # # plot_model.df(modelos.df)
    # 
    # plot_importancia(M_B_2)
    # plot_model(M_B_2,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
    # # plot_model.df(modelos.df)
    # 
    # plot_importancia(M_P_1)
    # plot_model(M_P_1,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
    # # plot_model.df(modelos.df)
    # 
    # plot_importancia(M_P_2)
    # plot_model(M_P_2,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
    # # plot_model.df(modelos.df)
    
    #Cerrar PDF 
    #dev.off()
  } #Guardar graficos
  
 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ * * * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 

##-------------------------------------------------------------------## 
##                           TABLA - MODELOS B_P                     ## 
##-------------------------------------------------------------------## 


  {
  aux.var_M_B_1=row.names(summary(M_B_1)$coefficients) 
  aux.var_M_P_1=row.names(summary(M_P_1)$coefficients)
  n_var=1:length(aux.var_M_B_1)
  modelo="M1"
  prueba=match(aux.var_M_B_1,aux.var_M_P_1)
  var_M_B_1=c(aux.var_M_B_1[!is.na(prueba)],aux.var_M_B_1[is.na(prueba)])
  var_BnoP=c(NA*(1:length(aux.var_M_B_1[!is.na(prueba)])),aux.var_M_B_1[is.na(prueba)])
  var_PyB=c(aux.var_M_B_1[!is.na(prueba)],NA*(1:length(aux.var_M_B_1[is.na(prueba)])))
  prueba=match(aux.var_M_P_1,aux.var_M_B_1)
  var_M_P_1=c(aux.var_M_P_1[!is.na(prueba)],aux.var_M_P_1[is.na(prueba)],NA*(1:(length(aux.var_M_B_1)-length(c(aux.var_M_P_1[!is.na(prueba)],aux.var_M_P_1[is.na(prueba)])))))
  var_PnoB=c(NA*(1:length(aux.var_M_P_1[!is.na(prueba)])),aux.var_M_P_1[is.na(prueba)],NA*(1:(length(aux.var_M_B_1)-length(c(aux.var_M_P_1[!is.na(prueba)],aux.var_M_P_1[is.na(prueba)])))))
  df_B_P1=data.frame(n_var,modelo,var_M_B_1,var_M_P_1,var_PyB,var_PnoB,var_BnoP)
  colnames(df_B_P1)=c("n_var","modelo","Binary", "Poisson", "PnB", "P-B", "B-P")
  
  # M_1_VD=ggVennDiagram(list(M_B_1=aux.var_M_B_1,M_P_1=aux.var_M_P_1), category.names = c("M_B_1","M_P_1"))+scale_fill_gradient(low = "#132b43", high = "#2596be")+theme(legend.position = "none")+ coord_flip()
  
  aux.var_M_B_2=row.names(summary(M_B_2)$coefficients) 
  aux.var_M_P_2=row.names(summary(M_P_2)$coefficients)
  n_var=1:length(aux.var_M_B_2)
  modelo="M2"
  prueba=match(aux.var_M_B_2,aux.var_M_P_2)
  var_M_B_2=c(aux.var_M_B_2[!is.na(prueba)],aux.var_M_B_2[is.na(prueba)])
  var_BnoP=c(NA*(1:length(aux.var_M_B_2[!is.na(prueba)])),aux.var_M_B_2[is.na(prueba)])
  var_PyB=c(aux.var_M_B_2[!is.na(prueba)],NA*(1:length(aux.var_M_B_2[is.na(prueba)])))
  prueba=match(aux.var_M_P_2,aux.var_M_B_2)
  var_M_P_2=c(aux.var_M_P_2[!is.na(prueba)],aux.var_M_P_2[is.na(prueba)],NA*(1:(length(aux.var_M_B_2)-length(c(aux.var_M_P_2[!is.na(prueba)],aux.var_M_P_2[is.na(prueba)])))))
  var_PnoB=c(NA*(1:length(aux.var_M_P_2[!is.na(prueba)])),aux.var_M_P_2[is.na(prueba)],NA*(1:(length(aux.var_M_B_2)-length(c(aux.var_M_P_2[!is.na(prueba)],aux.var_M_P_2[is.na(prueba)])))))
  df_B_P2=data.frame(n_var,modelo,var_M_B_2,var_M_P_2,var_PyB,var_PnoB,var_BnoP)
  colnames(df_B_P2)=c("n_var","modelo","Binary", "Poisson", "PnB", "P-B", "B-P")
  
  # M_2_VD=ggVennDiagram(list(M_B_2=aux.var_M_B_2,M_P_2=aux.var_M_P_2), category.names = c("M_B_2","M_P_2"))+scale_fill_gradient(low = "#132b43", high = "#2596be")+theme(legend.position = "none")+ coord_flip()
  
  df_B_P=rbind(df_B_P1,df_B_P2)
  row.names(df_B_P)=paste0(df_B_P$modelo,"_var_",df_B_P$n_var)  
  
  # require(gridExtra)
  # grid.arrange(M_1_VD, M_2_VD, nrow=2)
  
  # M_VD=ggVennDiagram(list(aux.var_M_B_1,aux.var_M_P_1,aux.var_M_B_2,aux.var_M_P_2), category.names = c("M_B_1","M_P_1","M_B_2","M_P_2"))+scale_fill_gradient(low = "#132b43", high = "#2596be")+theme(legend.position = "none")+ coord_flip()
  

}#Tabla MODELOS B_P y Diagrama de Venn

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~HURDLE~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 

aux.df=df_general
aux.y="y"
aux.df$itinere.key_0_1=(aux.df$itinere.key>1)*1

##-------------------------------------------------------------------## 
##                           M_H_1 - INICIO                          ## 
##-------------------------------------------------------------------## 

  {
  
  aux.var_x=c(df_B_P1$PnB,df_B_P1$`P-B`)
  aux.var_x=aux.var_x[!is.na(aux.var_x)]
  aux.var_x=gsub("itinere.key == 2","itinere.key_0_1",aux.var_x)
  
  aux.var_z=c(df_B_P1$PnB,df_B_P1$`B-P`)
  aux.var_z=aux.var_z[!is.na(aux.var_z)]
  aux.var_z=gsub("itinere.key == 2","itinere.key_0_1",aux.var_z)
    
  M_H_1=crear_modelo_hurdle(aux.df,aux.y,aux.var_x, aux.var_z)
  
  }#MODELIZACION


##-------------------------------------------------------------------## 
##                           M_H_1 - FIN                             ## 
##-------------------------------------------------------------------## 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 

##-------------------------------------------------------------------## 
##                           M_H_2 - INICIO                          ## 
##-------------------------------------------------------------------## 

  {
    aux.var_x=c(df_B_P2$PnB,df_B_P2$`P-B`)
    aux.var_x=aux.var_x[!is.na(aux.var_x)]
    aux.var_x=gsub("itinere.key == 2","itinere.key_0_1",aux.var_x)
    
    aux.var_z=c(df_B_P2$PnB,df_B_P2$`B-P`)
    aux.var_z=aux.var_z[!is.na(aux.var_z)]
    aux.var_z=gsub("itinere.key == 2","itinere.key_0_1",aux.var_z)
    
    M_H_2=crear_modelo_hurdle(aux.df,aux.y,aux.var_x, aux.var_z)
    }#MODELIZACION
  
##-------------------------------------------------------------------## 
##                           M_H_2 - FIN                             ## 
##-------------------------------------------------------------------## 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 

##-------------------------------------------------------------------## 
##                              RESUMEN                              ## 
##-------------------------------------------------------------------## 


{
AIC_M_B_1=round(AIC(M_B_1),0)
AIC_M_B_2=round(AIC(M_B_2),0)
AIC_M_P_1=round(AIC(M_P_1),0)
AIC_M_P_2=round(AIC(M_P_2),0)
AIC_M_H_1=round(AIC(M_H_1),0)
AIC_M_H_2=round(AIC(M_H_2),0)

aic.df=rbind(c(AIC_M_B_1,AIC_M_B_2),c(AIC_M_P_1,AIC_M_P_2),c(AIC_M_H_1,AIC_M_H_2))
colnames(aic.df)=c("M_1","M_2")
rownames(aic.df)=c("Binomial","Poisson","Hurdle")


nrow_M_B_1=nrow(summary(M_B_1)$coefficients)-1
nrow_M_B_2=nrow(summary(M_B_2)$coefficients)-1
nrow_M_P_1=nrow(summary(M_P_1)$coefficients)-1
nrow_M_P_2=nrow(summary(M_P_2)$coefficients)-1
nrow_M_H_1=paste(nrow((summary(M_H_1)$coefficients)$count)-1,"(count), ",nrow((summary(M_H_1)$coefficients)$zero)-1,"(zero)")
nrow_M_H_2=paste(nrow((summary(M_H_2)$coefficients)$count)-1,"(count), ", nrow((summary(M_H_2)$coefficients)$zero)-1,"(zero)")
nrow.df=rbind(c(nrow_M_B_1,nrow_M_B_2),c(nrow_M_P_1,nrow_M_P_2),c(nrow_M_H_1,nrow_M_H_2))

aic.nrow.df=cbind(aic.df,nrow.df)
aic.nrow.df=aic.nrow.df[,c(1,3,2,4)]
colnames(aic.nrow.df)=c("AIC","Nº Variables","AIC","Nº Variables")
obten_tabla(aic.nrow.df,hline = TRUE)

}

{
df_comparativa_M_1_H=as.data.frame(round(((summary(M_H_1))$coefficients)$count[,c(1,3,4)],4))
df_comparativa_M_1_H[nrow(df_comparativa_M_1_H)+1,] = "-"
aux.df_comparativa_M_1_H=as.data.frame(round(((summary(M_H_1))$coefficients)$zero[,c(1,3,4)],4))
df_comparativa_M_1_H=rbind(df_comparativa_M_1_H,aux.df_comparativa_M_1_H)
# obten_tabla(df_comparativa_M_1_H, hline=TRUE, rownames=TRUE)

df_comparativa_M_2_H=as.data.frame(round(((summary(M_H_2))$coefficients)$count[,c(1,3,4)],4))
df_comparativa_M_2_H[nrow(df_comparativa_M_2_H)+1,] = "-"
aux.df_comparativa_M_2_H=as.data.frame(round(((summary(M_H_2))$coefficients)$zero[,c(1,3,4)],4))
df_comparativa_M_2_H=rbind(df_comparativa_M_2_H,aux.df_comparativa_M_2_H)
# obten_tabla(df_comparativa_M_2_H, hline=TRUE, rownames=TRUE)
}

#Guardar gráficos en PDF
#destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\binario_poisson.pdf'
#destination = 'D:\\Users\\Íñigo\\Desktop\\TFG\\Graficos 2023\\binario_poisson.pdf'
destination = 'C:\\Users\\usuario\\Desktop\\Graficos 2023\\resultados_alt.pdf'
pdf(file=destination)  
  

{
  aux.df=df_general
  aux.y="y"
  aux.df$itinere.key_0_1=(aux.df$itinere.key>1)*1
  aux.df=aux.df[,-which(names(aux.df) %in% c("itinere"))]
  aux.df=na.omit(aux.df)
  {

  
    aux.predict1=predict(M_H_1, type=c("prob"))
    aux.predict2=predict(M_H_2, type=c("prob"))
    
    prob1_1=1-aux.predict1[,c(1)]
    prob1_3=apply(aux.predict1[,-c(0:3)],1,sum)
    prob2_1=1-aux.predict2[,c(1)]
    prob2_3=apply(aux.predict2[,-c(0:3)],1,sum)
    
    aux.max=max(c(max(1.2*prob1_1),max(1.2*prob2_1)))
    y_lim=c(0,aux.max)
    
    plot(aux.df$hour,prob1_1, ylim=y_lim, col=aux.colours[1],xlab="Hora", ylab="Probabilidad de ALT")
    lines(smooth.spline(aux.df$hour,prob1_1), col = aux.colours[1], lwd = 2)
    abline(lm(prob1_1~aux.df$hour),col=aux.colours[3], lwd = 2) 
    
    points(aux.df$hour,prob1_3, ylim=y_lim, col=aux.colours[2])
    lines(smooth.spline(aux.df$hour,prob1_3), col = aux.colours[2], lwd = 2)
    abline(lm(prob1_3~aux.df$hour),col=aux.colours[3], lwd = 2) 
    
    title(main = paste("M_1_H"))
    legend("topright",legend = c("P(Y>1)","P(Y>3)"),lwd = 2 ,pt.cex=2,col=aux.colours[1:2], bty = "n")
    
    
    
    plot(aux.df$hour,prob1_1, ylim=y_lim, col=aux.colours[1],xlab="Hora", ylab="Probabilidad de ALT")
    lines(smooth.spline(aux.df$hour,prob1_1), col = aux.colours[1], lwd = 2)
    abline(lm(prob1_1~aux.df$hour),col=aux.colours[3], lwd = 2) 
    
    points(aux.df$hour,prob2_3, ylim=y_lim, col=aux.colours[2])
    lines(smooth.spline(aux.df$hour,prob2_3), col = aux.colours[2], lwd = 2)
    abline(lm(prob2_3~aux.df$hour),col=aux.colours[3], lwd = 2) 
    
    title(main = paste("M_2_H"))
    legend("topright",legend = c("P(Y>1)","P(Y>3)"),lwd = 2 ,pt.cex=2,col=aux.colours[1:2], bty = "n")
    
  }


  for (i in 1:4) {
    
    fecha.elegida=i
    if(fecha.elegida==1){
      aux.año="2019"
      aux.dia=11 #Viernes
      aux.mes=1
      aux.fecha=paste(aux.año,aux.mes,aux.dia, sep="-")
      aux.yday=yday(aux.fecha)
    }else if (fecha.elegida==2){
      aux.año="2019"
      aux.dia=13 #Domingo
      aux.mes=1
      aux.fecha=paste(aux.año,aux.mes,aux.dia, sep="-")
      aux.yday=yday(aux.fecha)
    }else if (fecha.elegida==3){
      aux.año="2019"
    aux.dia=3#Jueves
    aux.mes=10
    aux.fecha=paste(aux.año,aux.mes,aux.dia, sep="-")
    aux.yday=yday(aux.fecha)
    }else if (fecha.elegida==4){ 
      aux.año="2019"
    aux.dia=6#Domingo
    aux.mes=10
    aux.fecha=paste(aux.año,aux.mes,aux.dia, sep="-")
    aux.yday=yday(aux.fecha)}
    
    aux.model=M_H_2
    aux.marcador = is.element(aux.df$year,aux.año)&is.element(aux.df$month,aux.mes)&is.element(aux.df$day_year,aux.yday)
    aux.y = predict(aux.model, type= "prob", newdata= aux.df[ aux.marcador, ] )
    aux.x =aux.df$hour[aux.marcador ]
    
    prob_1=1-aux.y[,c(1)]
    aux.ylim.prob=0.35
    
    plot(aux.x, prob_1, type="n", xlab="Hour", ylab="Estimated P(0 WTA)", ylim = c(0,aux.ylim.prob))
    lines(aux.x[1:24], prob_1[1:24], lwd=3, col=aux.colours[[1]])
    lines(aux.x[1:24], prob_1[24+1:24], lwd=3, col=aux.colours[[2]])
    title(main = paste("M_2_H - ",lubridate::wday(aux.fecha,abbr = FALSE, label = T),aux.dia,"-",aux.mes,"-",aux.año))
    legend("topright",legend = c("IDA","VUELTA"),lwd = 2 ,pt.cex=2,col=aux.colours[1:2], bty = "n")
  }
  
} 

dev.off()
#Cerrar PDF 


##-------------------------------------------------------------------## 
##  ~~~~~*~~~~~ * ~~~~~*~~~~~*\    FIN   /*~~~~~*~~~~~ * ~~~~~*~~~~~ ## 
##-------------------------------------------------------------------## 





años=c(2023)
delta_2023=subset(delta_09_23, year %in% años)

obten_tabla(t(round(table(delta_09_23$GRADO)/length(table(delta_09_23$year)),2)), hline = TRUE)



