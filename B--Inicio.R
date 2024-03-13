#Este archivo solo sirve de inicio para el resto, reduciendo el tiempo de espera

#BLOQUE LIBRERÍAS  

  {
  library(tidyverse)
  library(lubridate)
  #library(plyr)
  library(dplyr)
  library(readxl)
  library(stringr)
  library(purrr)
  library("car")
  library(sjPlot)
  library(gam)
  library(scales)
  library(data.table)
  library(sjmisc)
  library(caret)
  library(ggplot2)
  library("latex2exp")
  library(suncalc)
  library(effects)
  library(ciTools)
  library(trending)
  library(patchwork)
  library(climaemet)#Libreria aemet
  library(remotes)
  library(plotROC) 
  library(ROCR)
  library(rJava)
  #library(qdap)
  library(tm)
  #library(SnowballC)  
  library(pscl)
  library(lmtest)
  library(mpath)
  library('TeachingDemos')  
  library(mapSpain)
  library(bbmle)
    
    
  library(gmodels)
  library(Hmisc)
  library(MASS)
  library(SignifReg)
  library(Hmisc)
  library(zoo)
  library(spatstat)
  library(tikzDevice)
}
  
#BLOQUE FUNCIONES

  {
    
  {aux.colours1=c("steelblue","brown2", "olivedrab3", "purple2", "goldenrod1", "seagreen2", "violetred1")
  aux.colours2=c(palette())[-1]
  rainbowcols = rainbow(10)
  aux.colours=c(aux.colours1,  aux.colours2, rainbowcols)}#Paleta de colores paara gráficos
    
  #Funciones descriptivas
    
  func.discrete.descrip=function(aux.df, aux.time, aux.name_classif, FUN="mean"){
      
      aux.class=aux.df[[aux.name_classif]]
      aux.table=table(aux.time,aux.class)
      aux.summary=round(apply(aux.table,2,FUN,na.rm=T),2)
      aux.table2=table(aux.class,aux.time)
      
      # y_max = 1.2*max(aux.table)
      # aux.nvar=length(table(aux.class))
      # aux.bp=barplot(aux.table2, beside=TRUE,  ylim =c(0,y_max), main=aux.name_classif,cex.main=2, legend.text = TRUE,col=aux.colours[1:(aux.nvar)])
      # aux.exp=length(digits( y_max/10 ))-1
      # axis(2,at=seq(0,y_max,by=10^aux.exp ))
      
      ##Significa que se ha comentado para evitar cargar varias veces el mismo gr?fico
      
      ##ylim.sup=max(aux.table2)*1.2
      ##aux.ylim=c(0,ylim.sup)
      #barplot(aux.table2, cex.names = 0.75)
      #barplot(aux.table2,legend.text=c("Rural","Unallocated","Urban"), cex.names = 0.75)
      ##barplot(aux.table2,beside=TRUE,ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=0.75), cex.names = 0.75)
      #barplot(aux.table2, cex.names = 0.75)
      #legend("topleft", legend=c("Rural","Unallocated","Urban"),cex = 0.5,pch = 15,col=factor(aux.name_classif))
      #legend("topleft", legend=names(table(aux.class)),cex = 0.5,pch = 15)
      #print(aux.summary)
      return(aux.summary)
    }#End_function_
    
  func.discrete.descrip.quantile=function(aux.df, aux.time, aux.name_classif, FUN="quantile"){
      
      
      aux.class=aux.df[[aux.name_classif]]
      aux.table=table(aux.time,aux.class)
      aux.summary=round(apply(aux.table,2,FUN,probs=0.9,na.rm=T),2)
      #barplot(aux.table)
      aux.boxplot=as.data.frame.matrix(aux.table)
      ylim.sup=max(aux.table)*1.2
      ylim.inf=min(aux.table)-max(aux.table)*0.2
      
      aux.ylim=c(ylim.inf,ylim.sup)
      aux.nvar=length(table(aux.class))
      aux.exp=length(digits( ylim.sup/10 ))-1
      
      aux.table2=table(aux.class,aux.time)
      ylim.sup=max(aux.table2)*1.2
      aux.ylim=c(0,ylim.sup)
      
      if (length(levels(aux.class))>5){aux.ncol=ceiling(length(levels(aux.class))/10)
      }
      else {aux.ncol=1}
        
      barplot(aux.table2,beside=TRUE, ylim=aux.ylim, main=aux.name_classif,cex.main=2, args.legend = list(bty = "n", x = "topleft"),col=aux.colours[1:(aux.nvar)])
      legend.text=names(table(aux.class))
      legend("topright", ncol=(10/aux.ncol), legend.text, bty="n", fill = aux.colours[1:(aux.nvar)])
      aux.exp=length(digits( ylim.sup/10 ))-1
      axis(2,at=seq(0,round(ylim.sup/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
      
      boxplot(aux.boxplot, ylim=aux.ylim,legend = TRUE, main=aux.name_classif,cex.main=2,col=aux.colours[1:(aux.nvar)])
      legend("topright", ncol=aux.ncol, legend.text, border="black", fill = aux.colours[1:(aux.nvar)])
      axis(2,at=seq(0,round(ylim.sup/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
      
      
      
      print(aux.summary)
      return(aux.summary)
    }#End_function_
    
  func.summary=function(Modelo){
      
      aux.y=summary(Modelo)$coefficients
      aux.y=as.data.frame(aux.y)
      print(aux.y %>% arrange(desc(abs(aux.y[,3]))) %>% filter(aux.y[,4]<0.001))
      
      cat("\nAIC: ", summary(Modelo)$aic)
      cat("\nNull deviance:     ", summary(Modelo)$null.deviance, " on ", summary(Modelo)$df.null," degreees of freedom")
      cat("\nResidual deviance: ", summary(M5)$deviance, " on ", summary(Modelo)$df.residual," degreees of freedom")
      
    }#End_function_
  
  
  #Funciones análisis temporales
  
  efecto_hora=function(df){
    
    df$c1h=cos(2*pi*df$hour/24)
    df$s1h=sin(2*pi*df$hour/24)
    df$c2h=cos(4*pi*df$hour/24)
    df$s2h=sin(4*pi*df$hour/24)
    df$c3h=cos(6*pi*df$hour/24)
    df$s3h=sin(6*pi*df$hour/24)
    df$c4h=cos(8*pi*df$hour/24)
    df$s4h=sin(8*pi*df$hour/24)
    df$y=df$accidentes
    aux_glm= glm(y~c1h+s1h+c2h+s2h+c3h+s3h+c4h+s4h+day_of_week, data=df, family = "poisson")
    summary(aux_glm)
    aux.x=df$hour
    aux.y=summary(aux_glm)$coef[2,1]*df$c1h+
      summary(aux_glm)$coef[3,1]*df$s1h+
      summary(aux_glm)$coef[4,1]*df$c2h+
      summary(aux_glm)$coef[5,1]*df$s2h+
      summary(aux_glm)$coef[6,1]*df$c3h+
      summary(aux_glm)$coef[7,1]*df$s3h+
      summary(aux_glm)$coef[8,1]*df$c4h+
      summary(aux_glm)$coef[9,1]*df$s4h
    ciclico_hora=plot(aux.x, exp(aux.y), main="efecto hora",xlab = "",ylab = "",cex.axis=2.5)
    abline(v=c(3,6,9,12,15,18,21,24), lty=3, lwd=0.1)
  }#End_function_
  
  accidentes_dia_semana=function(df,orden.horiz=NULL, lang=NULL){
        
    if (is.null(lang)){lang="ESP"}
    if(lang=="ESP"){aux.text=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")
    aux.text1="Leyenda"
    aux.text2=c('MÁXIMO', 'MÍNIMO')
    aux.text3="Nº accidentes"
    }
    if(lang=="ENG"){aux.text=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    aux.text1="Legend"
    aux.text2=c('MAXIMUM', 'MINIMUM')
    aux.text3="Nº crashes"
    }
    
    accidentados=df$accidentes
    hora=df$hour
    dia_semana=df$day_of_week
    accidentados_por_hora_dia=tapply(accidentados, list(hora, dia_semana),sum , na.rm=T)
    accidentados_por_hora_dia= as.data.frame(accidentados_por_hora_dia)
    accidentados_por_hora_dia=accidentados_por_hora_dia[,c(3,4,5,2,7,6,1)]
    accidentados_por_hora_dia$Total=rowSums(accidentados_por_hora_dia, na.rm=T)
    colnames(accidentados_por_hora_dia)=aux.text
    par(mfrow=c(2,2))
    aux.x=rownames(accidentados_por_hora_dia)
    par(mfrow=c(2,2))

    if (is.null(orden.horiz)){
      orden.horiz=FALSE
    }    
    if (orden.horiz==TRUE){
      orden_dias=c(1,2,5,6,3,4,7)
    }    
    
    if (orden.horiz==FALSE){
      orden_dias=c(1:7)
    }
    
    for(aux.i.day in 1:7){

      i.day=orden_dias[aux.i.day]
      aux.y=accidentados_por_hora_dia[,i.day]
      
      if (aux.i.day==1){aux.ylab=aux.text3
      }
      else{aux.ylab=""
      }
      
      plot(aux.x, aux.y, main=colnames(accidentados_por_hora_dia[i.day]),cex.main=2,xlab = "",xaxt="n",ylab = aux.ylab,yaxt="n",cex=1.5)
      abline(v=c(3,6,9,12,15,18,21,24), lty=3, lwd=0.1)
      abline(v=which.min(aux.y)-1, col=4, lwd=2)
      abline(v=which.max(aux.y)-1, col=2, lwd=2)
      
      axis(1,at=seq(0,24,by=1),cex.axis=1.25,labels = FALSE,tck=-0.02)
      axis(2,at=seq(0,aux.y[which.max(aux.y)],by=1),labels = FALSE,tck=-0.02)
      axis(1,at=seq(0,24,by=3),cex.axis=1.25)
      axis(2,at=seq(0,aux.y[which.max(aux.y)],by=5),cex.axis=1.25)  
      
    }
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=aux.text1, cex.main=2)
    legend("left", legend =aux.text2, lwd=c(3,3),lty=c(1,1), cex=1.5, col = c(2,4),seg.len=3, bty='n')
    
    par(mfrow=c(1,1))

  }#End_function_
  
  accidentes_todos_los_dias_semana_color=function(df, lang=NULL){
        
    if (is.null(lang)){lang="ESP"}
    if(lang=="ESP"){aux.text=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")
    aux.text1="Leyenda"
    }
    if(lang=="ENG"){aux.text=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    aux.text1="Legend"
    }
    
    accidentados=df$accidentes
    hora=df$hour
    dia_semana=df$day_of_week
    accidentados_por_hora_dia=tapply(accidentados, list(hora, dia_semana),sum , na.rm=T)
    accidentados_por_hora_dia= as.data.frame(accidentados_por_hora_dia)
    accidentados_por_hora_dia=accidentados_por_hora_dia[,c(3,4,5,2,7,6,1)]
    accidentados_por_hora_dia$Total=rowSums(accidentados_por_hora_dia, na.rm=T)
    ylim.sup=max(accidentados_por_hora_dia[1:7], na.rm=T)*1.05
    par(mfrow=c(1,1))
    aux.x=rownames(accidentados_por_hora_dia)
    for(i.day in 1:7){
      aux.y=accidentados_por_hora_dia[,i.day]
      if(i.day==1) plot(aux.x, aux.y, ylim = c(0, ylim.sup),lwd=1.5,cex.axis=1.5,ylab = "Nº accidentes",xlab = "",cex.lab=1.5)
      lines(aux.x, aux.y, lty=i.day,col=i.day,lwd=1.5)
      #if(i.day==6|i.day==7)lines(aux.x, aux.y, lty=i.day, col=c("green"))
      abline(v=which.min(aux.y)-1, col=4, lty=i.day)
      abline(v=which.max(aux.y)-1, col=2, lty=i.day)
      cat(i.day, which.min(aux.y)-1,which.max(aux.y)-1, sep="&", fill=T )
      legend("topright", legend=aux.text, lty=1:7,col=1:7,lwd=3, cex=1, bg="white")
      
    }
    
  }#End_function_
  
  accidentes_todos_los_dias_semana=function(df){
    
    if (missing(lang)){lang="ESP"}    
    if(lang=="ESP"){aux.text=c("Entre semana","Fin de semana")
    }
    if(lang=="ENG"){aux.text=c("Weekday","Weekend")
    }

    
    accidentados=df$accidentes
    hora=df$hour
    dia_semana=df$day_of_week
    accidentados_por_hora_dia=tapply(accidentados, list(hora, dia_semana),sum , na.rm=T)
    accidentados_por_hora_dia= as.data.frame(accidentados_por_hora_dia)
    accidentados_por_hora_dia=accidentados_por_hora_dia[,c(3,4,5,2,7,6,1)]
    accidentados_por_hora_dia$Total=rowSums(accidentados_por_hora_dia, na.rm=T)
    ylim.sup=max(accidentados_por_hora_dia[1:7], na.rm=T)*1.05
    par(mfrow=c(1,1))
    aux.x=rownames(accidentados_por_hora_dia)
    for(i.day in 1:7){
      aux.y=accidentados_por_hora_dia[,i.day]
      if(i.day==1) plot(aux.x, aux.y, ylim = c(0, ylim.sup),cex.axis=1.5,ylab = "Nº accidentes",xlab = "",cex.lab=1.5)
      lines(aux.x, aux.y, lty=i.day)
      if(i.day==6|i.day==7)lines(aux.x, aux.y, lty=i.day, col=c("green"))
      abline(v=which.min(aux.y)-1, col=4, lty=i.day)
      abline(v=which.max(aux.y)-1, col=2, lty=i.day)
      cat(i.day, which.min(aux.y)-1,which.max(aux.y)-1, sep="&", fill=T )
      
    }
    legend("topright", legend=aux.text, col=c("black","green"), lwd=3,  cex=1.1, bg="white")

      
  }#End_function__extra
 
  ida_vuelta_todos_los_dias_semana_color=function(df,acc.indiv){
    
    if (missing(acc.indiv)){acc.indiv=FALSE}
    if(acc.indiv==TRUE){
      

    aux.df=df[df$itinere.key==1,]
    accidentes_todos_los_dias_semana_color(aux.df)
    mtext('IDA',  cex=2, bty='n')
    
    
    aux.df=df[df$itinere.key==2,]
    accidentes_todos_los_dias_semana_color(aux.df)    
    mtext('VUELTA',  cex=2, bty='n')
    
    }
    else{accidentes_todos_los_dias_semana_color(df)}
  }#End_function__extra
   
  efecto_dia_año=function(df){
    
    df$c1d=cos(2*pi*df$day_year/365)
    df$s1d=sin(2*pi*df$day_year/365)
    df$c2d=cos(4*pi*df$day_year/365)
    df$s2d=sin(4*pi*df$day_year/365)
    df$c3d=cos(6*pi*df$day_year/365)
    df$s3d=sin(6*pi*df$day_year/365)
    df$c4d=cos(8*pi*df$day_year/365)
    df$s4d=sin(8*pi*df$day_year/365)
    df$y=df$accidentes
    aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_of_week, data=df,family = "poisson")
    summary(aux_glm)
    aux.x=df$day_year
    aux.y=summary(aux_glm)$coef[2,1]*df$c1d+
      summary(aux_glm)$coef[3,1]*df$s1d+
      summary(aux_glm)$coef[4,1]*df$c2d+
      summary(aux_glm)$coef[5,1]*df$s2d+
      summary(aux_glm)$coef[6,1]*df$c3d+
      summary(aux_glm)$coef[7,1]*df$s3d+
      summary(aux_glm)$coef[8,1]*df$c4d+
      summary(aux_glm)$coef[9,1]*df$s4d
    plot(aux.x, exp(aux.y), main="",cex.axis=2.5)
    abline(v=c(0,32,60,91,121,152,182,213,244,274,305,335,365), lty=3, lwd=0.1)
    a=c(0,32,60,91,121,152,182,213,244,274,305,335)
    b=c(32,60,91,121,152,182,213,244,274,305,335,366)
    meses_plot=(a+b)/2
    axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
    
  } #End_function_
  
  efecto_dia_año=function(df,aux.superposition, lang=NULL){
    
      
    aux.df=df

      periodo=unique(df$year)
      año=(periodo[1]) 
      aux.marcador=is.element(df$year,año)
      df=subset(df,aux.marcador==TRUE)
      
      df$c1d=cos(2*pi*df$day_year/365)
      df$s1d=sin(2*pi*df$day_year/365)
      df$c2d=cos(4*pi*df$day_year/365)
      df$s2d=sin(4*pi*df$day_year/365)
      df$c3d=cos(6*pi*df$day_year/365)
      df$s3d=sin(6*pi*df$day_year/365)
      df$c4d=cos(8*pi*df$day_year/365)
      df$s4d=sin(8*pi*df$day_year/365)
      df$y=df$accidentes
      aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_of_week, data=df,family = "poisson")
      summary(aux_glm)
      aux.x=df$day_year
      aux.y=summary(aux_glm)$coef[2,1]*df$c1d+
        summary(aux_glm)$coef[3,1]*df$s1d+
        summary(aux_glm)$coef[4,1]*df$c2d+
        summary(aux_glm)$coef[5,1]*df$s2d+
        summary(aux_glm)$coef[6,1]*df$c3d+
        summary(aux_glm)$coef[7,1]*df$s3d+
        summary(aux_glm)$coef[8,1]*df$c4d+
        summary(aux_glm)$coef[9,1]*df$s4d
      plot(aux.x, exp(aux.y),ylim=c(0.9,1.1), main="",cex.axis=2.5, col=1, xlab="", ylab="")
      abline(v=c(0,32,60,91,121,152,182,213,244,274,305,335,365), lty=3, lwd=0.1)
      a=c(0,32,60,91,121,152,182,213,244,274,305,335)
      b=c(32,60,91,121,152,182,213,244,274,305,335,366)
      meses_plot=(a+b)/2
      
      if (is.null(lang)){lang="ESP"}
      if(lang=="ESP"){axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" )    )
        }
      if(lang=="ENG"){axis(3,at=meses_plot, cex.axis=1.5,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec" )    )
        }
      
    if (missing(aux.superposition)){aux.superposition=FALSE}
    if(aux.superposition==TRUE){
      
      for (i in 2:length(periodo)) {
        df=aux.df
        año=(periodo[i]) 
        aux.marcador=is.element(df$year,año)
        df=subset(df,aux.marcador==TRUE)
        
        df$c1d=cos(2*pi*df$day_year/365)
        df$s1d=sin(2*pi*df$day_year/365)
        df$c2d=cos(4*pi*df$day_year/365)
        df$s2d=sin(4*pi*df$day_year/365)
        df$c3d=cos(6*pi*df$day_year/365)
        df$s3d=sin(6*pi*df$day_year/365)
        df$c4d=cos(8*pi*df$day_year/365)
        df$s4d=sin(8*pi*df$day_year/365)
        df$y=df$accidentes
        aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_of_week, data=df,family = "poisson")
        summary(aux_glm)
        aux.x=df$day_year
        aux.y=summary(aux_glm)$coef[2,1]*df$c1d+
          summary(aux_glm)$coef[3,1]*df$s1d+
          summary(aux_glm)$coef[4,1]*df$c2d+
          summary(aux_glm)$coef[5,1]*df$s2d+
          summary(aux_glm)$coef[6,1]*df$c3d+
          summary(aux_glm)$coef[7,1]*df$s3d+
          summary(aux_glm)$coef[8,1]*df$c4d+
          summary(aux_glm)$coef[9,1]*df$s4d
        points(aux.x, exp(aux.y), main="",cex.axis=2.5, col=i)

        }
      legend("topleft",legend=periodo,pch=1, pt.lwd =3 ,pt.cex=2,cex=2, col = c(1:length(periodo)), bg="white")
    }
    
  }#End_function_
  
  accidentes_año=function(df, lang=NULL){
    
    par(mfrow=c(1,1))
    accidentados=df$accidentes
    dia=df$day_year
    accidentados_por_dia=tapply(accidentados, list(dia),sum)
    accidentados_por_dia= as.data.frame(accidentados_por_dia)
    aux.x=rownames(accidentados_por_dia)
    aux.y=accidentados_por_dia[,1]
    ylim.sup=max(accidentados_por_dia, na.rm=T)*1.05
    plot(aux.x, aux.y, ylim = c(0, ylim.sup),xlab = "",ylab = "",cex.axis=2.5)
    abline(v=which.min(aux.y)-1, col=4)
    abline(v=which.max(aux.y)-1, col=2)
    abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
    cat(which.min(aux.y)-1,which.max(aux.y)-1, sep="&", fill=T )
    abline(h=c(quantile(aux.y,probs=0.99),quantile(aux.y,probs=0.01)), lty=3, lwd=0.1, col=3)
    cat(quantile(aux.y,probs=0.01),quantile(aux.y,probs=0.99),mean(aux.y),sd(aux.y), sep="&", fill=T )
    abline(h=c(mean(aux.y)), lty=3, lwd=0.1, col=5)
    a=c(0,32,60,91,121,152,182,213,244,274,305,335)
    b=c(32,60,91,121,152,182,213,244,274,305,335,366)
    meses_plot=(a+b)/2

    if (is.null(lang)){lang="ESP"}
    if(lang=="ESP"){axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" )    )
    }
    if(lang=="ENG"){axis(3,at=meses_plot, cex.axis=1.5,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec" )    )
    }    
  }#End_function_
  
  efecto_dia_semana=function(df,dias,año){
    
    aux.marcador=is.element(df$wday,dias)
    aux_day_week=yday(df$FECHAACCIDENTE[aux.marcador])
    aux_day_week=as.data.frame(aux_day_week)
    names(aux_day_week)="y_day"
    aux_day_week$y=df$accidentes[aux.marcador]
    aux_day_week$year=year(df$FECHAACCIDENTE[aux.marcador])
    aux_collapse=tapply(aux_day_week$y_day, aux_day_week$year+aux_day_week$y_day/366, mean)
    aux_collapse=as.data.frame(aux_collapse)
    names(aux_collapse)="y_day"
    aux_collapse$aux_day_week=tapply(wday(df$FECHAACCIDENTE[aux.marcador]), aux_day_week$year+aux_day_week$y_day/366, mean)
    aux_collapse$year=tapply(aux_day_week$year, aux_day_week$year+aux_day_week$y_day/366, mean)
    aux_collapse$y=tapply(aux_day_week$y, aux_day_week$year+aux_day_week$y_day/366, sum)
    aux_collapse$y.accidente=tapply(aux_day_week$y>0, aux_day_week$year+aux_day_week$y_day/366, sum)
    aux_collapse=aux_collapse[aux_collapse$year==año,]
    aux_collapse$c1d=cos(2*pi*aux_collapse$y_day/365)
    aux_collapse$s1d=sin(2*pi*aux_collapse$y_day/365)
    aux_collapse$c2d=cos(4*pi*aux_collapse$y_day/365)
    aux_collapse$s2d=sin(4*pi*aux_collapse$y_day/365)
    aux_collapse$c3d=cos(6*pi*aux_collapse$y_day/365)
    aux_collapse$s3d=sin(6*pi*aux_collapse$y_day/365)
    aux_collapse$c4d=cos(8*pi*aux_collapse$y_day/365)
    aux_collapse$s4d=sin(8*pi*aux_collapse$y_day/365)
    aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=aux_collapse,family = "poisson")
    aux.x=aux_collapse$y_day
    aux.y=predict(aux_glm,type = "response")
    plot(aux.x, aux.y, main="",cex.axis=2.5)
    abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
    abline(h=mean(aux_collapse$y), col="red")
    lines(lowess(aux.x, aux.y, f=0.01),lty=3, lwd=1.2, col = "blue")
    a=c(0,32,60,91,121,152,182,213,244,274,305,335)
    b=c(32,60,91,121,152,182,213,244,274,305,335,366)
    meses_plot=(a+b)/2
    axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
    
  }#End_function_
  
  efecto_dia_hora=function(df,hora,año){
    
    aux.marcador=is.element(df$hour,c(hora))
    aux_day_week=yday(df$FECHAACCIDENTE[aux.marcador])
    aux_day_week=as.data.frame(aux_day_week)
    names(aux_day_week)="y_day"
    aux_day_week$y=df$accidentes[aux.marcador]
    aux_day_week$year=year(df$FECHAACCIDENTE[aux.marcador])
    aux_collapse=tapply(aux_day_week$y_day, aux_day_week$year+aux_day_week$y_day/366, mean)
    aux_collapse=as.data.frame(aux_collapse)
    names(aux_collapse)="y_day"
    aux_collapse$aux_day_week=tapply(wday(df$FECHAACCIDENTE[aux.marcador]), aux_day_week$year+aux_day_week$y_day/366, mean)
    aux_collapse$year=tapply(aux_day_week$year, aux_day_week$year+aux_day_week$y_day/366, mean)
    aux_collapse$y=tapply(aux_day_week$y, aux_day_week$year+aux_day_week$y_day/366, sum)
    aux_collapse$y.accidente=tapply(aux_day_week$y>0, aux_day_week$year+aux_day_week$y_day/366, sum)
    aux_collapse=aux_collapse[aux_collapse$year==año,]
    aux_collapse$c1d=cos(2*pi*aux_collapse$y_day/365)
    aux_collapse$s1d=sin(2*pi*aux_collapse$y_day/365)
    aux_collapse$c2d=cos(4*pi*aux_collapse$y_day/365)
    aux_collapse$s2d=sin(4*pi*aux_collapse$y_day/365)
    aux_collapse$c3d=cos(6*pi*aux_collapse$y_day/365)
    aux_collapse$s3d=sin(6*pi*aux_collapse$y_day/365)
    aux_collapse$c4d=cos(8*pi*aux_collapse$y_day/365)
    aux_collapse$s4d=sin(8*pi*aux_collapse$y_day/365)
    
    aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=aux_collapse, 
                 family = "poisson")
    aux.x=aux_collapse$y_day
    aux.y=predict(aux_glm,type = "response")
    plot(aux.x, aux.y, main="",cex.axis=2.5)
    abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
    abline(h=mean(aux_collapse$y), col="red")
    lines(lowess(aux.x, aux.y, f=0.01),lty=3, lwd=1.2, col = "blue")  
    a=c(0,32,60,91,121,152,182,213,244,274,305,335)
    b=c(32,60,91,121,152,182,213,244,274,305,335,366)
    meses_plot=(a+b)/2
    axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
    
  }#End_function_
  
  efecto_dia_semana_hora=function(df,hora.min,hora.max,año){
    ancho_graf=length(hora.min:hora.max)
    if(ancho_graf<=4){par(mfrow=c(1,ancho_graf))} else{par(mfrow=c(2,4))} 
    
    for(i.hour in hora.min:hora.max){
      aux.marcador=is.element(df$hour,c(i.hour))&(!is.na(df$hour))
      aux_day_week=wday(df$FECHAACCIDENTE[aux.marcador])
      aux_day_week=as.data.frame(aux_day_week)
      names(aux_day_week)="w_day"
      aux_day_week$y=df$accidentes[aux.marcador]
      aux_day_week$year=year(df$FECHAACCIDENTE[aux.marcador])
      aux_collapse=tapply(aux_day_week$w_day, aux_day_week$year+aux_day_week$w_day/7, mean)
      aux_collapse=as.data.frame(aux_collapse)
      names(aux_collapse)="w_day"
      aux_collapse$aux_day_week=tapply(wday(df$FECHAACCIDENTE[aux.marcador]), aux_day_week$year+aux_day_week$w_day/7, mean)
      aux_collapse$year=tapply(aux_day_week$year, aux_day_week$year+aux_day_week$w_day/7, mean)
      aux_collapse$y=tapply(aux_day_week$y, aux_day_week$year+aux_day_week$w_day/7, sum)
      aux_collapse$y.accidente=tapply(aux_day_week$y>0, aux_day_week$year+aux_day_week$w_day/7, sum)
      aux_collapse=aux_collapse[aux_collapse$year==año,]
      aux_collapse$c1d=cos(2*pi*aux_collapse$w_day/7)
      aux_collapse$s1d=sin(2*pi*aux_collapse$w_day/7)
      aux_collapse$c2d=cos(4*pi*aux_collapse$w_day/7)
      aux_collapse$s2d=sin(4*pi*aux_collapse$w_day/7)
      aux_collapse$c3d=cos(6*pi*aux_collapse$w_day/7)
      aux_collapse$s3d=sin(6*pi*aux_collapse$w_day/7)
      aux_collapse$c4d=cos(8*pi*aux_collapse$w_day/7)
      aux_collapse$s4d=sin(8*pi*aux_collapse$w_day/7)
      aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=aux_collapse, family = "poisson")
      aux.x=aux_collapse$w_day
      aux.y=predict(aux_glm,type = "response")
      plot(aux.x, aux.y,cex.axis=2.5)
    }
    mtext("Efecto dia de la semana - hora", side = 3, line = - 2, outer = TRUE)
    par(mfrow=c(1,1))  
  }#End_function_
  
  itinere.key_dia_semana=function(df,max.indiv,orden.horiz=NULL, lang=NULL){
    
    if (is.null(lang)){lang="ESP"}
    if(lang=="ESP"){aux.text=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")
    aux.text1="Leyenda"
    aux.text2=c('IDA', 'VUELTA','MÁXIMO GLOBAL', 'MÍNIMO GLOBAL')
    aux.text3=c('IDA', 'VUELTA','MÁXIMO EN IDA', 'MÁXIMO EN VUELTA','MÍNIMO EN IDA', 'MÍNIMO EN VUELTA')
    aux.text4= "Nº accidentes"
    }
    if(lang=="ENG"){aux.text=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    aux.text1="Legend"
    aux.text2=c('GO TO', 'RETURN','GLOBAL MAXIMUM', 'GLOBAL MINIMUM')
    aux.text3=c('GO TO', 'RETURN','MAXIMUM IN GO TO,', 'MAXIMUM IN RETURN','MINIMUM IN GO TO', 'MINIMUM IN RETURN')
    aux.text4= "Nº crashes"
    }
    
    accidentados=df$accidentes
    ida=1*is.element( df$itinere.key, c(1))
    vuelta=1*is.element(df$itinere.key, c(2))
    
    hora=df$hour
    dia_semana=df$day_of_week
    accidentados_por_hora_dia=tapply(accidentados, list(hora, dia_semana),sum , na.rm=T)
    accidentados_por_hora_dia= as.data.frame(accidentados_por_hora_dia)
    ida=tapply(ida, list(hora, dia_semana),sum , na.rm=T)
    ida= as.data.frame(ida)
    vuelta=tapply(vuelta, list(hora, dia_semana),sum , na.rm=T)
    vuelta= as.data.frame(vuelta)
    
    ida=ida[,c(3,4,5,2,7,6,1)]
    vuelta=vuelta[,c(3,4,5,2,7,6,1)]
    accidentados_por_hora_dia=accidentados_por_hora_dia[,c(3,4,5,2,7,6,1)]
    
    ida$Total=rowSums(ida, na.rm=T)
    vuelta$Total=rowSums(vuelta, na.rm=T)
    accidentados_por_hora_dia$Total=rowSums(accidentados_por_hora_dia, na.rm=T)
    
    colnames(ida)=aux.text
    colnames(vuelta)=aux.text
    
    
    par(mfrow=c(2,2))
    aux.x=rownames(ida)
    if (is.null(orden.horiz)){
      orden.horiz=FALSE
    }    
    if (orden.horiz==TRUE){
      orden_dias=c(1,2,5,6,3,4,7)
    }    
    
    if (orden.horiz==FALSE){
      orden_dias=c(1:7)
    }
    
    
    for(aux.i.day in 1:7){
      
      i.day=orden_dias[aux.i.day]
      aux.y_ida=ida[,i.day]
      aux.y_vuelta=vuelta[,i.day]
      aux.y=accidentados_por_hora_dia[,i.day]
      
      max.y=(max(c(max(aux.y_ida,na.rm = TRUE), max(aux.y_vuelta, na.rm = TRUE)) )) 
      aux.max.y=round(1.2*max.y,0)
      aux.y_lim=c(0,aux.max.y)
      
      if (aux.i.day==1){aux.ylab=aux.text4
      }
      else{aux.ylab=""
      } 
      
      plot(aux.x, aux.y_ida, ylim=aux.y_lim, main=colnames(ida[i.day]),cex.main=2,cex.axis=1.5,xlab = "",xaxt="n",ylab =aux.ylab,yaxt="n", pch=16, col="green",cex=1.5)
      points(aux.x, aux.y_vuelta,pch=17, col="purple",cex=1.5)
      abline(v=c(3,6,9,12,15,18,21,24), lty=3, lwd=0.1)
      
      if (missing(max.indiv)){max.indiv=FALSE}
      if(max.indiv==TRUE){
        abline(v=which.max(aux.y_ida)-1, col=2, lwd=2,lty=2)   
        abline(v=which.max(aux.y_vuelta)-1, col=2, lwd=3,lty=3)   
        abline(v=which.min(aux.y_ida)-1, col=4, lwd=2,lty=2)
        abline(v=which.min(aux.y_vuelta)-1, col=4, lwd=3,lty=3)        
      }
      else{
        abline(v=which.max(aux.y)-1, col=2, lwd=2)      
        abline(v=which.min(aux.y)-1, col=4, lwd=2)
        
      }
      
      axis(1,at=seq(0,24,by=1),cex.axis=1.25,labels = FALSE,tck=-0.02)
      axis(2,at=seq(0,aux.y[which.max(aux.y)],by=1),labels = FALSE,tck=-0.02)
      axis(1,at=seq(0,24,by=3),cex.axis=1.25)
      axis(2,at=seq(0,aux.y[which.max(aux.y)],by=5),cex.axis=1.25)        
    }
    
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=aux.text1, cex.main=2)
    
    if (max.indiv==FALSE){
      legend("left", legend =aux.text2, pch=c(16,17,NA,NA), lwd=c(NA,NA,2,3),lty=c(NA,NA,1,1), pt.cex=3, cex=1.5, col = c('green', 'purple',2,4),seg.len=3, bty='n')
    }
    if(max.indiv==TRUE){
      legend("left", legend =aux.text3, pch=c(16,17,NA,NA,NA,NA), lwd=c(NA,NA,2,3,2,3),lty=c(NA,NA,2,3,2,3), pt.cex=3, cex=1.5, col = c('green', 'purple',2,2,4,4),seg.len=3, bty='n')
    }
    
    par(mfrow=c(1,1))
  }#End_function_
  
  
  #Funciones obtención de datos
  
  obten_tabla=function(aux.tabla){
    aux.tabla=as.matrix(aux.tabla)
    fin=dim(aux.tabla)[1]
    aux.tabla=cbind(aux.tabla)
    cat("",colnames(aux.tabla),sep="&",fill=TRUE)
    for (i in 1:fin) {
      cat(rownames(aux.tabla)[i],aux.tabla[i,],sep="&",fill=TRUE)
    }
    cat(sep="\n")
  }#End_function_
  
  obten_modelos=function(my_list){
    #cat(paste("Modelo"),paste(""),paste(""),paste("AIC"),sep="&",fill=TRUE)
    cat(paste("Modelo"),paste("Deviance"),paste("AIC"),sep="&",fill=TRUE)
    for(i in 1:length(my_list)) { 
      Modelo=my_list[[i]]
      #cat(paste("M", i, sep = ""),summary(Modelo)$df.null,summary(Modelo)$df.residual,round(summary(Modelo)$aic),sep="&",fill=TRUE)
      cat(paste("M", i, sep = ""),round(summary(Modelo)$deviance),round(summary(Modelo)$aic),sep="&",fill=TRUE)
    }
  }#End_function_
  
  obten_coeff=function(modelo){
    cat(paste("Coeficiente"),paste("Estimate"),paste("Std. Error"),paste("z value"),paste("Pr(>|z|)"),paste(""),sep="&",fill=TRUE)
    aux.coeff=summary(modelo)$coeff
    fin=dim(aux.coeff)[1]
    aux.coeff=cbind(aux.coeff,exp(aux.coeff[,1]))
    for (i in 1:fin) {
      cat(rownames(aux.coeff)[i], round(aux.coeff[i,],digits=3),sep="&",fill=TRUE)
    }
  }#End_function_
  
  obten_confint=function(modelo){
    cat(paste("Coeficiente"),paste("2.5 %"),paste("97.5 %"),sep="&",fill=TRUE)
    aux.conf=confint(modelo)
    fin=dim(aux.conf)[1]
    aux.conf=cbind(aux.conf,exp(aux.conf[,1]),exp(aux.conf[,2]))
    for (i in 1:fin) {
      cat(rownames(aux.conf)[i], format(round(aux.conf[i,],digits=3), scientific=FALSE),sep="&",fill=TRUE)
    }
  }#End_function_
  
  riesgo_confint=function(modelo){
    cat(paste("Coeficiente"),paste("Estimado"),paste("2.5 %"),paste("97.5 %"),paste("Riesgo añadido"),paste("Riesgo Mínimo"),paste("Riesgo Máximo"),sep="&",fill=TRUE)
    aux.conf=confint(modelo)
    fin=dim(aux.conf)[1]
    
    aux.estimate=as.numeric(summary(modelo)$coefficients[,1])
    int_min=aux.conf[,1]
    int_max=aux.conf[,2]
    exp.estimate=exp(aux.estimate)
    exp_min=exp(aux.conf[,1])
    exp_max=exp(aux.conf[,2])
    
    riesgo.conf=cbind(aux.estimate,int_min,int_max,exp.estimate,exp_min,exp_max)

    
    for (i in 1:fin) {
      cat(rownames(aux.conf)[i], format(round(riesgo.conf[i,],digits=3), scientific=FALSE),sep="&",fill=TRUE)
    }
  }#End_function_
  
  
  frame_poligono=function(data_frame, poligono, años){
    
    #PASOS PREVIOS A LOS MODELOS 2009-2021#
    poligono=as.character(poligono)
    poligono=toupper(poligono)
    delta_09_22=subset(data_frame, ubicacion==poligono)
    delta_09_22=subset(delta_09_22, year %in% años)
    
    
    y=delta_09_22$y
    hour=delta_09_22$hour
    # wday=delta_09_22$wday
    # month=delta_09_22$month
    day_year=delta_09_22$day_year
    
    # delta_09_22$dias_años=as.numeric(difftime(as.Date(delta_09_22$FECHAACCIDENTE), as.Date("2008-12-31"), unit="days"))
    # dias_años=delta_09_22$dias_años
    
    
    aux.yday.hour=(delta_09_22$year*100000+delta_09_22$day_year*100+delta_09_22$hour)
    #aux.yday.hour = round((delta_09_22$dias_años + (delta_09_22$hour-1)/24),6)
    respuesta=tapply(y, list(aux.yday.hour ), sum, na.rm=TRUE)
    respuesta=as.data.frame(respuesta)
    respuesta[is.na(respuesta)] = 0
    names(respuesta)=c('y')
    
    # respuesta$dias_años=tapply(dias_años, list(aux.yday.hour), mean, na.rm=TRUE)
    # respuesta$hour=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
    # respuesta$wday=tapply(wday, list(aux.yday.hour), mean, na.rm=TRUE)
    # respuesta$month=tapply(month, list(aux.yday.hour), mean, na.rm=TRUE)
    # respuesta$arm_dia=tapply(dias_años, list(aux.yday.hour), mean, na.rm=TRUE)
    # respuesta$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
    
    respuesta$lluvia=(tapply(delta_09_22$lluvia, list(aux.yday.hour), sum, na.rm=TRUE)>0)
    respuesta$lluvia_pura=(tapply(delta_09_22$lluvia_pura, list(aux.yday.hour), sum, na.rm=TRUE)>0)
    respuesta$niebla=(tapply(delta_09_22$niebla, list(aux.yday.hour), sum, na.rm=TRUE)>0)
    respuesta$itinere=tapply(delta_09_22$itinere, list(aux.yday.hour), sum, na.rm=TRUE)
    respuesta$alcance=(tapply(delta_09_22$alcance, list(aux.yday.hour), sum, na.rm=TRUE)>0)
    respuesta$ubicacion=c(poligono)
    respuesta$leve=(tapply(delta_09_22$GRADO==1, list(aux.yday.hour), sum, na.rm=TRUE)>0)
    grave1=(tapply(delta_09_22$GRADO==2, list(aux.yday.hour), sum, na.rm=TRUE)>0)
    grave2=(tapply(delta_09_22$GRADO==3, list(aux.yday.hour), sum, na.rm=TRUE)>0)
    respuesta$grave=grave1+grave2
    respuesta$muertos=(tapply(delta_09_22$GRADO==4, list(aux.yday.hour), sum, na.rm=TRUE)>0)
    
    #respuesta$fin_de_semana=is.element(respuesta$wday,c(6,7))
    
    dim(respuesta)
    respuesta=rownames_to_column(respuesta, "aux.yday.hour")
    respuesta$aux.yday.hour=as.numeric(respuesta$aux.yday.hour)
    
    aux.date=seq(ymd('2009-01-01'),ymd('2021-12-31'), by = 'days')
    aux.date=seq(as.POSIXct("2009-01-01 0","%Y-%m-%d %H", tz="UTC"),to=as.POSIXct("2021-12-31 23", "%Y-%m-%d %H", tz="UTC"), by="hour")
    head(aux.date)
    aux.date=cbind(aux.date,year(aux.date),yday(aux.date),hour(aux.date))
    aux.date=cbind(aux.date,aux.date[,2]*100000+aux.date[,3]*100+aux.date[,4])
    aux.v=aux.date[,2:5]
    colnames(aux.v)=c("year","day_year","hour","aux.yday.hour")
    #aux.yday.hour=round((delta$year*100000+delta$day_year*100+delta$hour), 6)
    
    #aux.vday_year=1:(365*length(años)+sum(leap_year(años)))
    # aux.vday_year=aux.date[,4]
    # aux.vday_hour=1:24
    # aux.v = expand.grid(aux.vday_year , (aux.vday_hour-1)/24)
    # aux.v$aux.yday.hour=round((aux.v$Var1+aux.v$Var2), 6)
    
    prueba=merge(respuesta, aux.v, all = TRUE)
    prueba[is.na(prueba)] = 0
    # prueba$Var1=NULL
    # prueba$Var2=NULL
    respuesta=prueba
    
    aux.yday.hour=respuesta$aux.yday.hour
    day_year=respuesta$day_year
    hour=respuesta$hour
    respuesta$arm_dia=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
    respuesta$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
    
    # respuesta$day_years=respuesta$day_year+365*(respuesta$year-2009)+sum(leap_year(respuesta$year-2009))
    # day_years=respuesta$day_years
    # respuesta$arm_dia=tapply(day_years, list(aux.yday.hour), mean, na.rm=TRUE)
    
    #dias=365*length(años)+sum(leap_year(años))
    
    respuesta$c1d=cos(2*pi*respuesta$arm_dia/366)
    respuesta$s1d=sin(2*pi*respuesta$arm_dia/366)
    respuesta$c2d=cos(4*pi*respuesta$arm_dia/366)
    respuesta$s2d=sin(4*pi*respuesta$arm_dia/366)
    respuesta$c3d=cos(6*pi*respuesta$arm_dia/366)
    respuesta$s3d=sin(6*pi*respuesta$arm_dia/366)
    respuesta$c4d=cos(8*pi*respuesta$arm_dia/366)
    respuesta$s4d=sin(8*pi*respuesta$arm_dia/366)
    respuesta$c5d=cos(10*pi*respuesta$arm_dia/366)
    respuesta$s5d=sin(10*pi*respuesta$arm_dia/366)
    
    respuesta$c1h=cos(2*pi*respuesta$arm_hora/24)
    respuesta$s1h=sin(2*pi*respuesta$arm_hora/24)
    respuesta$c2h=cos(4*pi*respuesta$arm_hora/24)
    respuesta$s2h=sin(4*pi*respuesta$arm_hora/24)
    respuesta$c3h=cos(6*pi*respuesta$arm_hora/24)
    respuesta$s3h=sin(6*pi*respuesta$arm_hora/24)
    
    aux.poligono=respuesta
    return(aux.poligono)
  }#End_function_
  
  terminos=function(modelo,aux.p_valor=NULL){
    if (is.null(aux.p_valor)){aux.p_valor=0.001}         #Establece umbral de aceptación según p-valor
    
    aux.terms=as.data.frame(summary(modelo)$coefficients)
    lista=(aux.terms %>% filter(aux.terms[,4]<aux.p_valor) %>% rownames())
    terminos=gsub(":","*",lista[2:length(lista)])
    terminos=as.data.frame(terminos)
    return(terminos)
  }#End_function_
  
  terminos_inter=function(modelo,aux.p_valor=NULL){
    if (is.null(aux.p_valor)){aux.p_valor=0.001}         #Establece umbral de aceptación según p-valor
    
    aux.terms=as.data.frame(summary(modelo)$coefficients)
    lista=(aux.terms %>% filter(aux.terms[,4]<aux.p_valor) %>% rownames())
    #term_interaction=unlist(str_split(lista[grepl(":", lista)], ":"))
    #aux.exception=term_interaction[!duplicated(term_interaction)]
    #aux.exception=term_interaction[!term_interaction %in% lista]
    
    inter=grepl(":", lista)
    if(any(inter==TRUE)){
      
      prueba=lista[inter]
      multi_int=prueba
      #multi_int=prueba[lengths(regmatches(prueba, gregexpr(":", prueba)))]
      aux.comb=c()
      for (j in 1:length(multi_int)) {
        aux.multi_int=unlist(str_split(multi_int[j], ":"))
        n_int=(lengths(regmatches(multi_int[j], gregexpr(":", multi_int[j]))))
        for (aux_n_int in 1:n_int) {
          prueba=t(combn(c(aux.multi_int), aux_n_int))
          
          for (k in 1:nrow(prueba)) {
            aux.prueba=prueba [k,]
            aux.var=paste(aux.prueba,collapse = ":")
            aux.comb=c(aux.comb,aux.var)
          }
        }  
      }
      aux.exception =as.data.frame(aux.comb)}    
    else {aux.exception=NULL}
    
    aux.exception=unique(aux.exception)
    return(aux.exception)
  }#End_function_
  
  terminos_frame=function(modelo,aux.p_valor=NULL){
    if (is.null(aux.p_valor)){aux.p_valor=0.001}         #Establece umbral de aceptación según p-valor
    
    aux.y=as.data.frame(summary(modelo)$coefficients)
    lista=(aux.y %>% filter(aux.y[,4]<aux.p_valor) %>% rownames())
    terminos=gsub(":","*",lista[2:length(lista)])
    terminos=(terminos)
    return(terminos)
  }#End_function_
  
  # pie2=function (x, labels = names(x), edges = 200, radius = 1.6, clockwise = FALSE, 
  #                init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
  #                col = NULL, border = NULL, lty = NULL, main = NULL, ...) 
  
  pie2=function (x, labels = names(x), edges = 200, radius = 0.5, clockwise = FALSE, 
                 init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
                 col = NULL, border = NULL, lty = NULL, main = NULL, ...) {
    if (!is.numeric(x) || any(is.na(x) | x < 0)) 
      stop("'x' values must be positive.")
    if (is.null(labels)) 
      labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L]) 
      xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    dev.hold()
    on.exit(dev.flush())
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col)) 
      col <- if (is.null(density)) 
        c("white", "lightblue", "mistyrose", 
                 "lightcyan", "lavender", "cornsilk")
                 else par("fg")
    if (!is.null(col)) 
      col <- rep_len(col, nx)
    if (!is.null(border)) 
      border <- rep_len(border, nx)
    if (!is.null(lty)) 
      lty <- rep_len(lty, nx)
    angle <- rep(angle, nx)
    if (!is.null(density)) 
      density <- rep_len(density, nx)
    twopi <- if (clockwise) 
      -2 * pi
    else 2 * pi
    t2xy <- function(t) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
      polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
              border = border[i], col = col[i], lty = lty[i])
      P <- t2xy(mean(x[i + 0:1]))
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        lines(c(1, 1.35) * P$x, c(1, 1.35) * P$y)
        text(1.5 * P$x, 1.5 * P$y, labels[i], xpd = TRUE, 
             adj = ifelse(P$x < 0, 1, 0), ...)
      }
    }
    title(main = main, ...)
    invisible(NULL)
  }#End_function_
  
  plot_importancia=function(aux.model){
    a=varImp(aux.model, scale = FALSE)
    ggplot(a, aes(x=reorder(rownames(a),Overall), y=Overall)) +
      geom_point( color="blue", size=4, alpha=0.6)+
      geom_segment( aes(x=rownames(a), xend=rownames(a), y=0, yend=Overall), 
                    color='skyblue') +
      xlab('Variable')+
      ylab('Overall Importance')+
      theme_light() +
      coord_flip()+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
    
  }#End_function_

  getSeason <- function(input.date){
    numeric.date <- 100*month(input.date)+day(input.date)
    ## input Seasons upper limits in the form MMDD in the "break =" option:
    cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
    # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
    levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
    return(cuts)
  }#End_function_ Devuelve las 4 estaciones 

  func.tasa.discrete.descrip.quantile=function(aux.df, aux.class.time, aux.name_classif,aux.lluvia.real,lang=NULL, FUN="quantile"){
    
    
    if (is.null(lang)){lang="ESP"}
    if(lang=="ESP"){aux.text="Nº de accidentes bajo"
    aux.text1="por cada 1000 horas de lluvia"
        }
    if(lang=="ENG"){aux.text="Nº of accidents under"
    aux.text1="every 1000 hours of rain"
    }
    
    aux.class=aux.df[[aux.name_classif]]
    aux.time=as.numeric(unlist(aux.df[,aux.class.time]))
    aux.table=table(aux.time,aux.class)
    #aux.summary=apply(aux.table,2,FUN,probs=0.9,na.rm=T)
    #barplot(aux.table)
    aux.boxplot=as.data.frame.matrix(aux.table)
    ylim.sup=max(aux.table)*1.2
    ylim.inf=min(aux.table)-max(aux.table)*0.2
    
    aux.ylim=c(ylim.inf,ylim.sup)
    aux.nvar=length(table(aux.class))
    aux.exp=length(digits( ylim.sup/10 ))-1
    
    if(missing(aux.lluvia.real)){aux.dataframe=aux.df
       aux.dataframe$dia_lluvia=0
       aux.dataframe$dia_lluvia[aux.class==1]=1
      }
      
    else{aux.dataframe=aux.lluvia.real
      aux.dataframe$dia_lluvia=1*(aux.lluvia.real$total!= 0)
    }
    
    aux.time=as.numeric(unlist(aux.df[,aux.class.time]))
    aux.table2=table(aux.class,aux.time)
   
      aux.time=as.numeric(unlist(aux.dataframe[,aux.class.time]))
      dia_lluvia=as.table(tapply(aux.dataframe$dia_lluvia,list(aux.time),sum, na.rm=TRUE))
      dia_sin_lluvia=days_in_month(1:12)-dia_lluvia
      mes_lluvias=rbind(dia_sin_lluvia,dia_lluvia)
      aux.table3=round(aux.table2/mes_lluvias,4)
      aux.table3[is.na(aux.table3)] = 0
    
      horas_lluvia=as.table(tapply(aux.dataframe$dia_lluvia,list(aux.time),sum, na.rm=TRUE))
      horas_sin_lluvia=days_in_month(1:12)*24-horas_lluvia
      horas_lluvias=rbind(horas_sin_lluvia,horas_lluvia)
      aux.table4=round(aux.table2/horas_lluvias,4)*1000 #Tasa por 1000 horas
      aux.table4[is.na(aux.table4)] = 0
      aux.summary=apply(t(aux.table4),2,FUN,probs=0.9,na.rm=T)
  
    
    ylim.sup=max(aux.table4)*1.2
    aux.ylim=c(0,ylim.sup)
    aux.main=paste(aux.text,aux.name_classif,aux.text1, sep=" ")
    aux.main = paste( strwrap(aux.main, width = 35),collapse = "\n" )
    barplot(aux.table4,beside=TRUE, ylim=aux.ylim,legend = TRUE, main=aux.main,cex.main=2, args.legend = list(bty = "n", x = "topleft"),col=aux.colours[1:(aux.nvar)])
    aux.exp=length(digits( ylim.sup/10 ))-1
    axis(2,at=seq(0,round(ylim.sup/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    grid(nx = NA,  ny = NULL)
    
    # legend.text=names(table(aux.class))
    # boxplot(t(aux.table4), ylim=aux.ylim,legend = TRUE, main=aux.name_classif,cex.main=2,col=aux.colours[1:(aux.nvar)])
    # legend("topright", legend.text, border="black", fill = aux.colours[2:(aux.nvar+1)])
    # axis(2,at=seq(0,ylim.sup,by=10^aux.exp ))
    #
    
    
    print(aux.summary)
    return(aux.summary)
  }#End_function_
  
  
  #Funciones angulo solar
  
  datos_luz_solar=function(lat_zgz,lon_zgz,aux.df,suavizado=NULL) {
    
    #Se obtienen las fechas completas (año, mes, dia y hora) para el periodo de años desde el 2009 a 2022.
    
    aux.df$year=as.integer(aux.df$aux.yday.hour/100000)
    aux.df$day_year=as.integer(aux.df$aux.yday.hour/100)-as.integer(aux.df$aux.yday.hour/100000)*1000
    d = with(aux.df, paste(year, day_year))
    d = as.Date(d, "%Y %j")
    aux.df$dia=d
    h=aux.df$hour
    d = paste(d, paste(h))
    d = as.POSIXct(d, format ="%Y-%m-%d %H")
    aux.df$fecha=d
    
    luz_solar=getSunlightTimes(aux.df$dia, lat = lat_zgz, lon = lon_zgz)
    
    aux.df$salida=luz_solar$sunrise
    aux.df$puesta=luz_solar$sunset
    
    aux.df$luz=(aux.df$salida < aux.df$fecha) & (aux.df$fecha < aux.df$puesta)
    aux.df$un_accidente=aux.df$y
    aux.df$un_accidente[aux.df$un_accidente>=1]=1
    tabla_luz=table(aux.df$un_accidente,aux.df$luz)
    
    
    #Se ha tomado como criterio de luz natural suficiente las horas situadas entre la salida y puesta del sol.
    
    #Se observa como se obtienen un nÃºmero similar de casos ocurridos bajo condiciones lumÃ­nicas insuficientes, 7831, 
    #que frente aquellos sucesos donde exist?a luz natural, 7482.
    #Cabe mecionar que este estudio solo busca determinar las condiciones luminicas naturales, 
    #es decir, no tiene en consideraciÃ³n otras fuentes de luz artificiales, como bien pueden ser la iluminaciÃ³n pblica, o la del propio vehÃ­culo.
    
    
    #ObtenciÃ³n del deslumbramiento solar
    
    #SegÃºn An Empirical Investigation of the Impacts of Sun-Related Glare on Traffic Flow,
    #el deslumbramiento solar es un suceso relevante en el trÃ¡fico, produciendose principalmente con angulos de 0 a 15ª.
    #https://d1wqtxts1xzle7.cloudfront.net/41285459/An_Empirical_Investigation_of_the_Impact20160117-9818-1fsc0k6-libre.pdf?1453033601=&response-content-disposition=inline%3B+filename%3DAn_Empirical_Investigation_of_the_Impact.pdf&Expires=1676734006&Signature=AcpBMWQcQipSh2akKQ0~nkP0-LqrdVb3CuSqL63m7ZfJ6~VJqrcSb8x~Wxn2cb0zVlefFm5jpkQVtVR-j85bncuwz6RRJir4JfHvHRPTJR-FRwaZfDOZb4-JwYUM6rCy~5MhBcMwpxkbaRZNdQkvCJZc-RPB-uLp6rO1huYLYqRtf~DVn1LR1usvaYPrTVzC5VCN2xUeFIavcnASODT~~WwYxpTwVEmsyyj1~gUCtSl5s6JFAVLOv~79NhGIiAMBozWa3~yyzUwGu-HGcZy~C1FPaqnZqp-IpWBghyEOOWig-tGwT2S15B0atNwQUVXAVXWPzm8ztFXA5qdN3wPr3w__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA
    
    angulo_solar=getSunlightPosition(aux.df$fecha, lat = lat_zgz, lon = lon_zgz)
    angulo_solar$altitud=angulo_solar$altitude*180/pi
    
    aux.df$altitud=angulo_solar$altitud
    aux.df2=aux.df
    
    aux.df$deslumbramiento=(0 <= aux.df$altitud) & (aux.df$altitud <= 15) & (aux.df$luz=TRUE)
    tabla_deslumb=table(aux.df$un_accidente,aux.df$deslumbramiento)
    
    acc_deslum=aggregate(aux.df$un_accidente , by=list(round(aux.df$altitud,0)) , FUN=sum, na.rm=TRUE)
    
    
    acc_deslum=as.data.frame(acc_deslum)
    colnames(acc_deslum)=c("Altitud","Nº_accidentes")
    aux.plot.luz=plot(acc_deslum)
    
    x=acc_deslum[,1]
    y=acc_deslum[,2]
    
    if (is.null(suavizado)){suavizado=0.75}
    # En caso de no dar un indice de suavizado se toma por defecto 0.75, equivalente al default de loess.
    
    aux.loess.plot=loess(y ~ x,data=acc_deslum, span=suavizado)
    lines(acc_deslum$Altitud,aux.loess.plot$fitted,col=aux.colours[1],lwd=3)                         
    abline(v=c(-15,15), col=aux.colours[2], lwd=2)
    
    #Si se representa el angulo solar desde 0 a pi/2
    
    acc_deslum_posit=aggregate(aux.df$un_accidente , by=list(abs(round(aux.df$altitud,0))), FUN=sum, na.rm=TRUE)
    
    acc_deslum_posit=as.data.frame(acc_deslum_posit)
    colnames(acc_deslum_posit)=c("Altitud","Nº_accidentes")
    aux.plot.luz.posit=plot(acc_deslum_posit)
    
    x=acc_deslum_posit[,1]
    y=acc_deslum_posit[,2]
    
    aux.loess.plot=loess(y ~ x,data=acc_deslum_posit, span=suavizado)
    lines(acc_deslum_posit$Altitud,aux.loess.plot$fitted,col=aux.colours[1],lwd=3)
    abline(v=15, col=aux.colours[2], lwd=2)
    
    return(list(aux.df2,"Accidentes con luz natural",tabla_luz,"Accidentes con deslumbramiento",tabla_deslumb,aux.plot.luz,aux.plot.luz.posit))
    
  }#End_function_
  
  datos_luz_solar_ida_salida_poligono=function(lat_zgz,lon_zgz,aux.df,suaviz,tamaño=NULL) {
    
    #Se obtienen las fechas completas (año, mes, dia y hora) para el periodo de años desde el 2009 a 2022.
    aux.df$y[aux.df$y>=30]=1
    
    aux.df$year=as.integer(aux.df$aux.yday.hour/100000)
    aux.df$day_year=as.integer(aux.df$aux.yday.hour/100)-as.integer(aux.df$aux.yday.hour/100000)*1000
    d = with(aux.df, paste(year, day_year))
    d = as.Date(d, "%Y %j")
    aux.df$dia=d
    h=aux.df$hour
    d = paste(d, paste(h))
    d = as.POSIXct(d, format ="%Y-%m-%d %H")
    aux.df$fecha=d
    
    luz_solar=getSunlightTimes(aux.df$dia, lat = lat_zgz, lon = lon_zgz)
    
    aux.df$salida=luz_solar$sunrise
    aux.df$puesta=luz_solar$sunset
    
    aux.df$luz=(aux.df$salida < aux.df$fecha) & (aux.df$fecha < aux.df$puesta)
    # aux.df$un_accidente=aux.df$y
    # aux.df$un_accidente[aux.df$un_accidente>=1]=1
    tabla_luz=table(aux.df$y,aux.df$luz)
    
    
    #Se ha tomado como criterio de luz natural suficiente las horas situadas entre la salida y puesta del sol.
    
    #Se observa como se obtienen un nÃºmero similar de casos ocurridos bajo condiciones lumÃ­nicas insuficientes, 6535, 
    #que frente aquellos sucesos donde exist?a luz natural, 7531.
    #Cabe mecionar que este estudio solo busca determinar las condiciones luminicas naturales, 
    #es decir, no tiene en consideraciÃ³n otras fuentes de luz artificiales, como bien pueden ser la iluminaciÃ³n pblica, o la del propio vehÃ­culo.
    
    
    #ObtenciÃ³n del deslumbramiento solar
    
    #SegÃºn An Empirical Investigation of the Impacts of Sun-Related Glare on Traffic Flow,
    #el deslumbramiento solar es un suceso relevante en el trÃ¡fico, produciendose principalmente con angulos de 0 a 15ª.
    #https://d1wqtxts1xzle7.cloudfront.net/41285459/An_Empirical_Investigation_of_the_Impact20160117-9818-1fsc0k6-libre.pdf?1453033601=&response-content-disposition=inline%3B+filename%3DAn_Empirical_Investigation_of_the_Impact.pdf&Expires=1676734006&Signature=AcpBMWQcQipSh2akKQ0~nkP0-LqrdVb3CuSqL63m7ZfJ6~VJqrcSb8x~Wxn2cb0zVlefFm5jpkQVtVR-j85bncuwz6RRJir4JfHvHRPTJR-FRwaZfDOZb4-JwYUM6rCy~5MhBcMwpxkbaRZNdQkvCJZc-RPB-uLp6rO1huYLYqRtf~DVn1LR1usvaYPrTVzC5VCN2xUeFIavcnASODT~~WwYxpTwVEmsyyj1~gUCtSl5s6JFAVLOv~79NhGIiAMBozWa3~yyzUwGu-HGcZy~C1FPaqnZqp-IpWBghyEOOWig-tGwT2S15B0atNwQUVXAVXWPzm8ztFXA5qdN3wPr3w__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA
    
    angulo_solar=getSunlightPosition(aux.df$fecha, lat = lat_zgz, lon = lon_zgz)
    angulo_solar$altitud=angulo_solar$altitude*180/pi
    
    aux.df$altitud=round(angulo_solar$altitud,0)
    tabla_altitud_salida=table(aux.df$altitud,aux.df$ida_vuelta)
    
    aux.df$deslumbramiento=(0 <= aux.df$altitud) & (aux.df$altitud <= 15) & (aux.df$luz=TRUE)
    tabla_deslumb=table(aux.df$y,aux.df$deslumbramiento)
    
    altitud_salida=as.data.frame(tapply(aux.df$y, list(aux.df$altitud ,aux.df$ida_vuelta),  FUN =sum, na.rm=TRUE))
    altitud_salida$total=rowSums(altitud_salida[,c(1:3)],na.rm=TRUE)
    altitud_salida$altitud=row.names(altitud_salida)
    altitud_salida[is.na(altitud_salida)] = 0
    x=altitud_salida$altitud
    y=altitud_salida$total
    aux.plot.luz=plot(x,y)
    
    
    #Tendencia global
    aux.loess.plot=loess(y ~ x,data=altitud_salida, span=suaviz)
    lines(altitud_salida$altitud,aux.loess.plot$fitted,col=aux.colours[1],lwd=3)                         
    abline(v=c(-15,15), col=aux.colours[2], lwd=2)   
    #Tendencia salida
    aux.loess.plot=loess(altitud_salida$SAL ~ x,data=altitud_salida, span=suaviz)
    lines(altitud_salida$altitud,aux.loess.plot$fitted,col=aux.colours[3],lwd=3)  
    #Tendencia ida
    aux.loess.plot=loess(altitud_salida$IDA ~ x,data=altitud_salida, span=suaviz)
    lines(altitud_salida$altitud,aux.loess.plot$fitted,col=aux.colours[4],lwd=3)   
    
    if (is.null(tamaño)){tamaño=1}
    texto=c("GLOBAL","SALIDA","IDA")
    legend(x = "topright", bty="n", legend=texto, pch=16,cex=tamaño, col=c(aux.colours[1],aux.colours[3],aux.colours[4]))
    
    
    #Si se representa el angulo solar desde 0 a pi/2 
    
    
    aux.df$abs_altitud=round(abs(angulo_solar$altitud),0)
    tabla_abs_altitud_salida=table(aux.df$abs_altitud,aux.df$ida_vuelta)
    
    abs_altitud_salida=as.data.frame(tapply(aux.df$y, list(aux.df$abs_altitud ,aux.df$ida_vuelta),  FUN =sum, na.rm=TRUE))
    abs_altitud_salida$total=rowSums(abs_altitud_salida[,c(1:3)],na.rm=TRUE)
    abs_altitud_salida$altitud=row.names(abs_altitud_salida)
    abs_altitud_salida[is.na(abs_altitud_salida)] = 0
    x=abs_altitud_salida$altitud
    y=abs_altitud_salida$total
    aux.plot.luz.posit=plot(x,y)
    
    
    #Tendencia global
    aux.loess.plot=loess(y ~ x,data=abs_altitud_salida, span=suaviz)
    lines(abs_altitud_salida$altitud,aux.loess.plot$fitted,col=aux.colours[1],lwd=3)                         
    abline(v=c(-15,15), col=aux.colours[2], lwd=2)   
    #Tendencia salida
    aux.loess.plot=loess(abs_altitud_salida$SAL ~ x,data=abs_altitud_salida, span=suaviz)
    lines(abs_altitud_salida$altitud,aux.loess.plot$fitted,col=aux.colours[3],lwd=3)  
    #Tendencia ida
    aux.loess.plot=loess(abs_altitud_salida$IDA ~ x,data=abs_altitud_salida, span=suaviz)
    lines(abs_altitud_salida$altitud,aux.loess.plot$fitted,col=aux.colours[4],lwd=3)   
    
    if (missing(tamaño)){tamaño=1}
    texto=c("GLOBAL","SALIDA","IDA")
    legend(x = "topright", bty="n", legend=texto, pch=16,cex=tamaño, col=c(aux.colours[1],aux.colours[3],aux.colours[4]))

    
    return(list("Accidentes con luz natural",tabla_luz,"Accidentes con deslumbramiento",tabla_deslumb,aux.plot.luz,aux.plot.luz.posit))
    
  }#End_function_
  
  
  #Funciones lluvia real
  
  acierto_lluvia_aero=function(datos_reales,datos_extraidos){
    
    
    #Se modifica la base datos que recoge la lluvia medida por la estaciÃ³n metereolÃ³gica del aeropuerto de Zaragoza en cada dia-año. 
    lluvia_real=datos_reales[,3:4]
    dim(lluvia_real)
    colnames(lluvia_real)=c("fecha","lluvia")
    lluvia_real$dia_hora=ymd(lluvia_real$fecha)
    lluvia_real$year=year(lluvia_real$dia_hora)
    lluvia_real$yday=yday(lluvia_real$dia_hora)
    lluvia_real$aux.yday=(lluvia_real$year*1000+lluvia_real$yday)
    years=c(2009:2022)
    lluvia_real=subset(lluvia_real, year %in% years)  
    lluvia_real$lluvia_real01=as.integer(as.logical(lluvia_real$lluvia))
    dim(lluvia_real)
    
    #Se modifica la base datos que para cada hora-dia-año recoge si ha habido o no accidente y extrae textualmente si en el momento del accidente estaba lloviendo.
    datos_extraidos$year=round(datos_extraidos$aux.yday.hour/100000,)
    datos_extraidos$day_year=round((datos_extraidos$aux.yday.hour-round(datos_extraidos$aux.yday.hour/100000,)*100000)/100,0)
    
    datos_extraidos$aux.yday=(datos_extraidos$year*1000+datos_extraidos$day_year)
    datos_extraidos$arm_dia=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
    prueba=tapply(datos_extraidos$lluvia, list(datos_extraidos$aux.yday), sum, na.rm=TRUE)
    #prueba=prueba[-1]
    prueba=as.integer(as.logical(prueba))
    length(prueba)
    lluvia_real$lluvia_extrac_textual=prueba
    y=tapply(datos_extraidos$y, list(datos_extraidos$aux.yday), sum, na.rm=TRUE)
    lluvia_real$y=y
    
    lluvia_aeropuerto=lluvia_real$lluvia_real01
    lluvia_accidente=lluvia_real$lluvia_extrac_textual
    df=data.frame(lluvia_aeropuerto,lluvia_accidente,y)
    df=df[df$y>0,]
    df=df[,-3]
    aux.tabla.lluvia=addmargins(table(df))
    
    #aux.tabla.lluvia=addmargins(table(lluvia_real$lluvia_real01,lluvia_real$lluvia_extrac_textual))
    
    
    aciertos=aux.tabla.lluvia[1,1]+aux.tabla.lluvia[2,2]
    fallos=aux.tabla.lluvia[1,2]+aux.tabla.lluvia[2,1]
    porcentaje_acierto=aciertos/(aciertos+fallos)*100
    porcentaje_acierto_lluvia=aux.tabla.lluvia[2,2]/(aux.tabla.lluvia[2,2]+aux.tabla.lluvia[1,2])*100
    llovia=aux.tabla.lluvia[2,2]
    llovia_acc=aux.tabla.lluvia[1,2]
    
    Variable=c("Acierto_global","Acierto_llueve", "Lluvia en ambos ", "Lluvia accidente no en la estaciÃ³n")
    b=c(porcentaje_acierto,porcentaje_acierto_lluvia, llovia, llovia_acc)
    tabla_aciertos=as.data.frame(Variable)
    tabla_aciertos$Resultados=b
    
    
    aux.tabla.lluvia
    porcentaje_acierto
    return(list(aux.tabla.lluvia,tabla_aciertos))
    
  }#End_function_
  
  acierto_lluvia_polig=function(datos_reales,datos_extraidos){
    
    
    #Se modifica la base datos que recoge la lluvia medida por la estaciÃ³n metereolÃ³gica del aeropuerto de Zaragoza en cada dia-año. 
    lluvia_real=datos_reales[,3:4]
    dim(lluvia_real)
    colnames(lluvia_real)=c("fecha","lluvia")
    lluvia_real$dia_hora=ymd(lluvia_real$fecha)
    lluvia_real$year=year(lluvia_real$dia_hora)
    lluvia_real$yday=yday(lluvia_real$dia_hora)
    lluvia_real$aux.yday=(lluvia_real$year*1000+lluvia_real$yday)
    years=c(2009:2022)
    lluvia_real=subset(lluvia_real, year %in% years)  
    lluvia_real$lluvia_real01=as.integer(as.logical(lluvia_real$lluvia))
    dim(lluvia_real)
    
    #Se modifica la base datos que para cada hora-dia-año recoge si ha habido o no accidente y extrae textualmente si en el momento del accidente estaba lloviendo.
    datos_extraidos$year=round(datos_extraidos$aux.yday.hour/100000,)
    datos_extraidos$day_year=round((datos_extraidos$aux.yday.hour-round(datos_extraidos$aux.yday.hour/100000,)*100000)/100,0)
    
    datos_extraidos$aux.yday=(datos_extraidos$year*1000+datos_extraidos$day_year)
    #datos_extraidos$arm_dia=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
    prueba=tapply(datos_extraidos$lluvia, list(datos_extraidos$aux.yday), sum, na.rm=TRUE)
    #  prueba=as.integer(as.logical(prueba))
    length(prueba)
    lluvia_real$lluvia_extrac_textual=prueba
    y=tapply(datos_extraidos$y, list(datos_extraidos$aux.yday), sum, na.rm=TRUE)
    lluvia_real$y=y
    
    lluvia_aeropuerto=lluvia_real$lluvia_real01
    lluvia_accidente=lluvia_real$lluvia_extrac_textual
    df=data.frame(lluvia_aeropuerto,lluvia_accidente,y)
    df=df[df$y>0,]
    df=df[,-3]
    aux.tabla.lluvia=addmargins(table(df))
    
    aciertos=aux.tabla.lluvia[1,1]+aux.tabla.lluvia[2,2]
    fallos=aux.tabla.lluvia[1,2]+aux.tabla.lluvia[2,1]
    porcentaje_acierto=aciertos/(aciertos+fallos)*100
    porcentaje_acierto_lluvia=aux.tabla.lluvia[2,2]/(aux.tabla.lluvia[2,2]+aux.tabla.lluvia[1,2])*100
    llovia=aux.tabla.lluvia[2,2]
    llovia_acc=aux.tabla.lluvia[1,2]
    
    Variable=c("Acierto_global","Acierto_llueve", "Lluvia en ambos ", "Lluvia accidente no en la estaciÃ³n")
    b=c(porcentaje_acierto,porcentaje_acierto_lluvia, llovia, llovia_acc)
    tabla_aciertos=as.data.frame(Variable)
    tabla_aciertos$Resultados=b
    
    
    aux.tabla.lluvia
    porcentaje_acierto
    return(list(aux.tabla.lluvia,tabla_aciertos))
    
  }#End_function_
  
  acierto_lluvia=function(datos_reales,datos_extraidos){
    
    datos_extraidos=respuesta
    
    #Se modifica la base datos que para cada hora-dia-año recoge si ha habido o no accidente y extrae textualmente si en el momento del accidente estaba lloviendo.
    datos_extraidos$year=round(datos_extraidos$aux.yday.hour/100000,)
    datos_extraidos$day_year=round((datos_extraidos$aux.yday.hour-round(datos_extraidos$aux.yday.hour/100000,)*100000)/100,0)
    datos_extraidos$aux.yday=(datos_extraidos$year*1000+datos_extraidos$day_year)
    tiempo=base_lluvia$aux.yday
    datos_extraidos=left_join(datos_extraidos,base_lluvia,by="aux.yday")
    datos_extraidos=datos_extraidos %>% rename(fecha = fecha.x,year = year.x)
    
    #datos_extraidos = datos_extraidos %>% filter(fecha >= base_lluvia$fecha[1])
    datos_extraidos = subset(datos_extraidos, datos_extraidos$fecha >= base_lluvia$fecha[1])
    
    
    #datos_extraidos$arm_dia=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
    prueba=tapply(datos_extraidos$lluvia, list(datos_extraidos$aux.yday), mean, na.rm=TRUE)
    lluvia=as.data.frame(prueba) 
    colnames(lluvia)[1]=c("lluvia_acc")
    lluvia$y=tapply(datos_extraidos$y, list(datos_extraidos$aux.yday), sum, na.rm=TRUE)
    lluvia$lluvia_aero=tapply(datos_extraidos$lluvia_aero, list(datos_extraidos$aux.yday), mean, na.rm=TRUE)
    lluvia$lluvia_mue=tapply(datos_extraidos$lluvia_mue, list(datos_extraidos$aux.yday), mean, na.rm=TRUE)
    lluvia$lluvia_lote=tapply(datos_extraidos$lluvia_lote, list(datos_extraidos$aux.yday), mean, na.rm=TRUE)
    lluvia$lluvia_total=tapply(datos_extraidos$total, list(datos_extraidos$aux.yday), mean, na.rm=TRUE)
    
    lluvia_1_0 =1*(as.data.frame(lluvia[] != 0)) #El multiplicar por 1 pasa de TRUE/FALSE A 1/0
    dim(lluvia_1_0)
    lluvia_bases=lluvia_1_0$lluvia_total
    lluvia_accidente=lluvia_1_0$lluvia_acc
    y=lluvia_1_0$y
    df=data.frame(lluvia_bases,lluvia_accidente,y)
    df=df[df$y>0,]
    df=df[,-3]  
    aux.tabla.lluvia=addmargins(table(df))
    
    aciertos=aux.tabla.lluvia[1,1]+aux.tabla.lluvia[2,2]
    fallos=aux.tabla.lluvia[1,2]+aux.tabla.lluvia[2,1]
    porcentaje_acierto=aciertos/(aciertos+fallos)*100
    porcentaje_acierto_lluvia=aux.tabla.lluvia[2,2]/(aux.tabla.lluvia[2,2]+aux.tabla.lluvia[1,2])*100
    llovia=aux.tabla.lluvia[2,2]
    llovia_acc=aux.tabla.lluvia[1,2]
    
    # #Figura ROC
    # prediobj =prediction(lluvia_accidente,lluvia_bases)
    # perf =  performance(prediobj, "tpr","fpr")
    # plot(perf,main = "Curva ROC",xlab="Tasa de falsos positivos", ylab="Tasa de verdaderos positivos")
    # abline(a=0,b=1,col="blue",lty=2)
    # grid()
    # auc = as.numeric(performance(prediobj,"auc")@y.values)
    # legend("bottomright",legend=paste(" AUC =",round(auc,4)))
    
    
    Variable=c("Acierto_global","Acierto_llueve", "Lluvia en ambos ", "Lluvia accidente no en la estaciÃ³n")
    b=c(porcentaje_acierto,porcentaje_acierto_lluvia, llovia, llovia_acc)
    tabla_aciertos=as.data.frame(Variable)
    tabla_aciertos$Resultados=b
    
    
    aux.tabla.lluvia
    porcentaje_acierto
    return(list(aux.tabla.lluvia,tabla_aciertos))
    
  }#End_function_
  
  acierto_lluvia_horaria=function(datos_reales,datos_extraidos){
    
    
    #Se modifica la base datos que para cada hora-dia-año recoge si ha habido o no accidente y extrae textualmente si en el momento del accidente estaba lloviendo.
    datos_join=full_join(datos_extraidos,datos_reales,by="aux.yday.hour")
    datos_join=datos_join %>% rename(fecha = fecha.x,year = year.x, hour= hour.x)
    
    #datos_join = datos_join %>% filter(fecha>= datos_reales$fecha[1])
    datos_extraidos = subset(datos_extraidos, datos_extraidos$fecha >= base_lluvia$fecha[1])
    
    
    
    #datos_extraidos = datos_extraidos %>% filter(fecha >= base_lluvia$fecha[1])
    
    lluvia=datos_join[,c("y","lluvia","lluvia_mue","lluvia_lote","lluvia_zgz","lluvia_galle","total")]
    
    lluvia_1_0 =1*(as.data.frame(lluvia[] != 0)) #El multiplicar por 1 pasa de TRUE/FALSE A 1/0
    dim(lluvia_1_0)
    lluvia_bases=lluvia_1_0$total
    lluvia_accidente=lluvia_1_0$lluvia
    y=lluvia_1_0$y
    df=data.frame(lluvia_bases,lluvia_accidente,y)
    df=df[df$y>0,]
    df=df[,-3]  
    aux.tabla.lluvia=addmargins(table(df))
    
    # lluvia_1_0 =1*(as.data.frame(lluvia[] != 0)) #El multiplicar por 1 pasa de TRUE/FALSE A 1/0
    # dim(lluvia_1_0)
    # lluvia_bases=lluvia_1_0$lluvia_total
    # lluvia_accidente=lluvia_1_0$lluvia_acc
    # y=lluvia_1_0$y
    # df=data.frame(lluvia_bases,lluvia_accidente,y)
    # df=df[df$y>0,]
    # df=df[,-3]  
    # aux.tabla.lluvia=addmargins(table(df))
    
    aciertos=aux.tabla.lluvia[1,1]+aux.tabla.lluvia[2,2]
    fallos=aux.tabla.lluvia[1,2]+aux.tabla.lluvia[2,1]
    porcentaje_acierto=aciertos/(aciertos+fallos)*100
    porcentaje_acierto_lluvia=aux.tabla.lluvia[2,2]/(aux.tabla.lluvia[2,2]+aux.tabla.lluvia[1,2])*100
    llovia=aux.tabla.lluvia[2,2]
    llovia_acc=aux.tabla.lluvia[1,2]
    
    
    
    Variable=c("Acierto_global","Acierto_llueve", "Lluvia en ambos ", "Lluvia accidente no en la estaciÃ³n")
    b=c(porcentaje_acierto,porcentaje_acierto_lluvia, llovia, llovia_acc)
    tabla_aciertos=as.data.frame(Variable)
    tabla_aciertos$Resultados=b
    
    
    aux.tabla.lluvia
    porcentaje_acierto
    return(list(aux.tabla.lluvia,tabla_aciertos))
    
  }#End_function_
  
  acierto_lluvia_decalada=function(datos_reales,datos_extraidos,decalaje=NULL){
    if (is.null(decalaje)){decalaje=0}
    
    
    #Se modifica la base datos que para cada hora-dia-año recoge si ha habido o no accidente y extrae textualmente si en el momento del accidente estaba lloviendo.
    datos_join=full_join(datos_extraidos,datos_reales,by="aux.yday.hour")
    datos_join=datos_join %>% rename(fecha = fecha.x,year = year.x, hour= hour.x)
    
    #datos_join = datos_join %>% filter(fecha>= datos_reales$fecha[1])
    datos_extraidos = subset(datos_extraidos, datos_extraidos$fecha >= base_lluvia$fecha[1])
    
    #lluvia=datos_join[,c("y","lluvia","lluvia_mue","lluvia_lote","lluvia_zgz","lluvia_galle","total")]
    lluvia=datos_join[,c("y","lluvia",colnames(datos_reales[c(1:which(colnames(datos_reales)=="total"))]))]
    
    lluvia_1_0 =1*(as.data.frame(lluvia[] != 0)) #El multiplicar por 1 pasa de TRUE/FALSE A 1/0
    lluvia_1_0$hour=datos_join$hour.x
    lluvia_1_0$aux.yday.hour=datos_join$aux.yday.hour
    lluvia_1_0$fecha=datos_join$fecha
    lluvia_1_0=subset(lluvia_1_0, !is.na(fecha))
    lluvia_1_0[is.na(lluvia_1_0)] = 0
    dim(lluvia_1_0)
    
    lluvia.decalada=which(lluvia_1_0$total != 0)+decalaje #Posiciones de la lluvia de las bases
    aux.p=which(colnames(lluvia_1_0) == "total") 
    hora.dcalada=lluvia_1_0[lluvia.decalada,aux.p]
    (lluvia_1_0[lluvia.decalada,aux.p])*1
    lluvia_1_0$total[lluvia.decalada]=lluvia_1_0$total[lluvia.decalada]+(lluvia_1_0$total[lluvia.decalada]==0)*1
    
    lluvia_1_0$bases=rowSums(lluvia_1_0[,c(3:6)])#Recoge en cuantas bases se recoge lluvia
    lluvia_1_0$estaciones=getSeason(as.POSIXct(lluvia_1_0$fecha)+(0:365))
    
    lluvia_bases=lluvia_1_0$total #Recoge el total de lluvia medido por las bases
    lluvia_accidente=lluvia_1_0$lluvia #Recoge la lluvia en el momento del accidente
    y=lluvia_1_0$y
    df=data.frame(lluvia_bases,lluvia_accidente,y)
    df=df[df$y>0,]
    df=df[,-3]  
    aux.tabla.lluvia=addmargins(table(df))
    
    aciertos=aux.tabla.lluvia[1,1]+aux.tabla.lluvia[2,2]
    fallos=aux.tabla.lluvia[1,2]+aux.tabla.lluvia[2,1]
    porcentaje_acierto=aciertos/(aciertos+fallos)*100
    porcentaje_acierto_lluvia=aux.tabla.lluvia[2,2]/(aux.tabla.lluvia[2,2]+aux.tabla.lluvia[1,2])*100
    llovia=aux.tabla.lluvia[2,2]
    llovia_acc=aux.tabla.lluvia[1,2]
    
    #Figura ROC
    prediobj =prediction(lluvia_accidente,lluvia_bases)
    perf =  performance(prediobj, "tpr","fpr")
    plot(perf,main = "Curva ROC",xlab="Tasa de falsos positivos", ylab="Tasa de verdaderos positivos")
    abline(a=0,b=1,col="blue",lty=2)
    grid()
    auc = as.numeric(performance(prediobj,"auc")@y.values)
    legend("bottomright",legend=paste(" AUC =",round(auc,4)))
    
    
    
    Variable=c("Acierto_global","Acierto_llueve", "Lluvia en ambos ", "Lluvia accidente no en la estaciÃ³n")
    b=c(porcentaje_acierto,porcentaje_acierto_lluvia, llovia, llovia_acc)
    tabla_aciertos=as.data.frame(Variable)
    tabla_aciertos$Resultados=b
    
    
    aux.tabla.lluvia
    porcentaje_acierto
    return(list(aux.tabla.lluvia,tabla_aciertos))
  }#End_function_
  
  acierto_lluvia_decalada_verano=function(datos_reales,datos_extraidos,decalaje=NULL){
    if (is.null(decalaje)){decalaje=0}
    
    
    #Se modifica la base datos que para cada hora-dia-año recoge si ha habido o no accidente y extrae textualmente si en el momento del accidente estaba lloviendo.
    datos_join=full_join(datos_extraidos,datos_reales,by="aux.yday.hour")
    datos_join=datos_join %>% rename(fecha = fecha.x,year = year.x, hour= hour.x)
    
    #datos_join = datos_join %>% filter(fecha>= datos_reales$fecha[1])
    datos_extraidos = subset(datos_extraidos, datos_extraidos$fecha >= base_lluvia$fecha[1])
    
    #lluvia=datos_join[,c("y","lluvia","lluvia_mue","lluvia_lote","lluvia_zgz","lluvia_galle","total")]
    lluvia=datos_join[,c("y","lluvia",colnames(datos_reales[c(1:which(colnames(datos_reales)=="total"))]))]
    
    lluvia_1_0 =1*(as.data.frame(lluvia[] != 0)) #El multiplicar por 1 pasa de TRUE/FALSE A 1/0
    lluvia_1_0$hour=datos_join$hour.x
    lluvia_1_0$aux.yday.hour=datos_join$aux.yday.hour
    lluvia_1_0$fecha=datos_join$fecha
    lluvia_1_0=subset(lluvia_1_0, !is.na(fecha))
    lluvia_1_0[is.na(lluvia_1_0)] = 0
    dim(lluvia_1_0)
    
    lluvia.decalada=which(lluvia_1_0$total != 0)+decalaje #Posiciones de la lluvia de las bases
    aux.p=which(colnames(lluvia_1_0) == "total") 
    hora.dcalada=lluvia_1_0[lluvia.decalada,aux.p]
    (lluvia_1_0[lluvia.decalada,aux.p])*1
    lluvia_1_0$total[lluvia.decalada]=lluvia_1_0$total[lluvia.decalada]+(lluvia_1_0$total[lluvia.decalada]==0)*1
    
    lluvia_1_0$bases=rowSums(lluvia_1_0[,c(3:6)])#Recoge en cuantas bases se recoge lluvia
    lluvia_1_0$estaciones=getSeason(as.POSIXct(lluvia_1_0$fecha)+(0:365))
    
    lluvia_bases=lluvia_1_0$total #Recoge el total de lluvia medido por las bases
    lluvia_accidente=lluvia_1_0$lluvia #Recoge la lluvia en el momento del accidente
    verano=1*(lluvia_1_0$estaciones=="Summer")
    y=lluvia_1_0$y
    df=data.frame(lluvia_bases,lluvia_accidente,y,verano)
    df=df[df$y>0,]
    df=df[,-3]  
    #aux.tabla.lluvia=addmargins(table(df))
    
    aux.tabla.lluvia=addmargins(table(df))
    
    Variable=c("Acierto_global","Acierto_llueve", "Lluvia en ambos ", "Lluvia accidente no en la estaciÃ³n")
    tabla_aciertos=as.data.frame(Variable)  
    
    for (i in 1:2) {
      
      aciertos=aux.tabla.lluvia[1,1,i]+aux.tabla.lluvia[2,2,i]
      fallos=aux.tabla.lluvia[1,2,i]+aux.tabla.lluvia[2,1,i]
      porcentaje_acierto=aciertos/(aciertos+fallos)*100
      porcentaje_acierto_lluvia=aux.tabla.lluvia[2,2,i]/(aux.tabla.lluvia[2,2,i]+aux.tabla.lluvia[1,2,i])*100
      llovia=aux.tabla.lluvia[2,2,i]
      llovia_acc=aux.tabla.lluvia[1,2,i]
      
      b=c(porcentaje_acierto,porcentaje_acierto_lluvia, llovia, llovia_acc)
      tabla_aciertos[1+i]=b
      
    }
    
    # 
    # #Figura ROC
    # prediobj =prediction(lluvia_accidente,lluvia_bases)
    # perf =  performance(prediobj, "tpr","fpr")
    # plot(perf,main = "Curva ROC",xlab="Tasa de falsos positivos", ylab="Tasa de verdaderos positivos")
    # abline(a=0,b=1,col="blue",lty=2)
    # grid()
    # auc = as.numeric(performance(prediobj,"auc")@y.values)
    # legend("bottomright",legend=paste(" AUC =",round(auc,4)))
    # 
    # 
    # 
    
    
    aux.tabla.lluvia
    porcentaje_acierto
    
    return(list(aux.tabla.lluvia,tabla_aciertos))
  }#End_function_
  
  
  #Funciones anÃ¡lisis textual avanzado
  
  clean_corpus = function(corpus){
    corpus = tm_map(corpus, stripWhitespace)
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, content_transformer(tolower))
    corpus = tm_map(corpus, removeWords, stopwords("sp"))
    return(corpus)
  }#End_function_
  
  
  #Funciones Ida y Vuelta
  
  I_V=function(df){ 
    
    hora_real=df$HORA
    hora_turno=df$HORATRABAJO
    tabla_I_V=table(hora_real,hora_turno)
    
    mapa_I_V=heatmap(tabla_I_V[,], Rowv = NA, Colv = NA, xlab = "Hora_Turno", ylab = "Hora_Real")
    
    return(list(tabla_I_V,mapa_I_V))
  }#End_function_
  
  plot_turno=function(df){   
    turno_0=df[,1]
    turno_99=df[,ncol(df)]
    turno_0_99=turno_0+turno_99
    lim_y=1.2*max(turno_0_99)
    
    plot(turno_0, col="blue", type="l", lty = 1, lwd=2, ylim=c(0,lim_y))
    lines(turno_99, col="red", lty = 1, lwd=2)
    lines(turno_0_99, col="green", lty = 1, lwd=2)
    legend("topleft", legend=c("IDA - Turno_0", "VUELTA - turno_99", "IDA Y VUELTA"), col=c( "blue","red","green"),lty = 1, lwd=2, cex=0.8)
  }#End_function_
  
  plot_turno_proporciones=function(df){ 
    total=sum(df)
    turno_0=df[,1]/total
    turno_99=df[,ncol(df)]/total
    turno_0_99=turno_0+turno_99
    lim_y=1.2*max(turno_0_99)
    
    plot(turno_0, col="blue", type="l", lty = 1, lwd=2, ylim=c(0,lim_y))
    lines(turno_99, col="red", lty = 1, lwd=2)
    lines(turno_0_99, col="green", lty = 1, lwd=2)
    legend("topleft", legend=c("IDA - Turno_0", "VUELTA - turno_99", "IDA Y VUELTA"), col=c( "blue","red","green"),lty = 1, lwd=2, cex=0.8)
  }#End_function_
  
  
  #Funciones automatizaciÃ³n modelizaciÃ³n
  
  crear_modelo=function(aux.df,aux.y,aux.model,aux.p_valor=NULL){
    if (is.null(aux.p_valor)){aux.p_valor=0.001}         #Establece umbral de aceptación según p-valor
    
    y=aux.df$aux.y
    aux.var=terminos(aux.model,aux.p_valor)[,1]
    new_str = gsub('TRUE',"",aux.var)
    aux.var=paste("(",new_str, ")",collapse = "+")
    formula=as.formula(paste("y", aux.var, sep=" ~ "))
    aux.modelo= glm(formula, data=aux.df, family = "poisson",control = glm.control(maxit=100))
    return(aux.modelo)
  }#End_function_
  
  adicion_modelo=function(aux.df,aux.y,aux.model.base,var.adicional,aux.p_valor=NULL){
    if (is.null(aux.p_valor)){aux.p_valor=0.001}         #Establece umbral de aceptación según p-valor
    
    y=aux.df$aux.y
    aux.var=terminos(aux.model.base,aux.p_valor)[,1]
    new_str = gsub('TRUE',"",aux.var)
    aux.var=paste("(",new_str, ")",collapse = "+")
    aux.var_int=paste("(",aux.var,")*",var.adicional,collapse = "")             #Añade interacción de la variable adicional
    aux.var=paste(aux.var,var.adicional, aux.var_int,sep = "+")                               #Añade adición de la variable adicional
    
    
    formula=as.formula(paste("y", aux.var, sep=" ~ "))
    aux.modelo= glm(formula, data=aux.df, family = "poisson",control = glm.control(maxit=100))
    return(aux.modelo)
  }#End_function_
  
  mezcla_modelo=function(aux.df,aux.y,aux.list){
    y=aux.df$aux.y
    aux.var=(aux.list)[[1]]
    new_str = gsub('TRUE',"",aux.var)
    aux.var=paste("(",new_str, ")",collapse = "+")
    formula=as.formula(paste("y", aux.var, sep=" ~ "))
    aux.modelo= glm(formula, data=aux.df, family = "poisson",control = glm.control(maxit=100))
    return(aux.modelo)
  }#End_function_
  
  datos_luz_solar_modelo=function(lat_zgz,lon_zgz,aux.df,suavizado=NULL) {
    
    luz_solar=getSunlightTimes(aux.df$dia, lat = lat_zgz, lon = lon_zgz)
    
    aux.df$salida=luz_solar$sunrise
    aux.df$puesta=luz_solar$sunset
    
    aux.df$luz=(aux.df$salida < aux.df$fecha) & (aux.df$fecha < aux.df$puesta)
    aux.df$un_accidente=aux.df$y
    aux.df$un_accidente[aux.df$un_accidente>=1]=1
    tabla_luz=table(aux.df$un_accidente,aux.df$luz)
    
    
    #Se ha tomado como criterio de luz natural suficiente las horas situadas entre la salida y puesta del sol.
    
    #Se observa como se obtienen un nÃºmero similar de casos ocurridos bajo condiciones lumÃ­nicas insuficientes, 6535, 
    #que frente aquellos sucesos donde exist?a luz natural, 7531.
    #Cabe mecionar que este estudio solo busca determinar las condiciones luminicas naturales, 
    #es decir, no tiene en consideraciÃ³n otras fuentes de luz artificiales, como bien pueden ser la iluminaciÃ³n pblica, o la del propio vehÃ­culo.
    
    
    #ObtenciÃ³n del deslumbramiento solar
    
    #SegÃºn An Empirical Investigation of the Impacts of Sun-Related Glare on Traffic Flow,
    #el deslumbramiento solar es un suceso relevante en el trÃ¡fico, produciendose principalmente con angulos de 0 a 15ª.
    #https://d1wqtxts1xzle7.cloudfront.net/41285459/An_Empirical_Investigation_of_the_Impact20160117-9818-1fsc0k6-libre.pdf?1453033601=&response-content-disposition=inline%3B+filename%3DAn_Empirical_Investigation_of_the_Impact.pdf&Expires=1676734006&Signature=AcpBMWQcQipSh2akKQ0~nkP0-LqrdVb3CuSqL63m7ZfJ6~VJqrcSb8x~Wxn2cb0zVlefFm5jpkQVtVR-j85bncuwz6RRJir4JfHvHRPTJR-FRwaZfDOZb4-JwYUM6rCy~5MhBcMwpxkbaRZNdQkvCJZc-RPB-uLp6rO1huYLYqRtf~DVn1LR1usvaYPrTVzC5VCN2xUeFIavcnASODT~~WwYxpTwVEmsyyj1~gUCtSl5s6JFAVLOv~79NhGIiAMBozWa3~yyzUwGu-HGcZy~C1FPaqnZqp-IpWBghyEOOWig-tGwT2S15B0atNwQUVXAVXWPzm8ztFXA5qdN3wPr3w__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA
    
    angulo_solar=getSunlightPosition(aux.df$fecha, lat = lat_zgz, lon = lon_zgz)
    angulo_solar$altitud=angulo_solar$altitude*180/pi
    
    aux.df$altitud=angulo_solar$altitud
    aux.df2=aux.df
    
    aux.df$deslumbramiento=(0 <= aux.df$altitud) & (aux.df$altitud <= 15) & (aux.df$luz=TRUE)
    tabla_deslumb=table(aux.df$un_accidente,aux.df$deslumbramiento)
    
    acc_deslum=aggregate(aux.df$un_accidente , by=list(round(aux.df$altitud,0)) , FUN=sum, na.rm=TRUE)
    
    
    acc_deslum=as.data.frame(acc_deslum)
    colnames(acc_deslum)=c("Altitud","Nª_accidentes")
    aux.plot.luz=plot(acc_deslum)
    
    x=acc_deslum[,1]
    y=acc_deslum[,2]
    
    if (is.null(suavizado)){suavizado=0.75}
    # En caso de no dar un indice de suavizado se toma por defecto 0.75, equivalente al default de loess.
    
    aux.loess.plot=loess(y ~ x,data=acc_deslum, span=suavizado)
    lines(acc_deslum$Altitud,aux.loess.plot$fitted,col="blue",lwd=3)                         
    abline(v=c(-15,15), col="red", lwd=2)
    
    #Si se representa el angulo solar desde 0 a pi/2
    
    acc_deslum_posit=aggregate(aux.df$un_accidente , by=list(abs(round(aux.df$altitud,0))), FUN=sum, na.rm=TRUE)
    
    acc_deslum_posit=as.data.frame(acc_deslum_posit)
    colnames(acc_deslum_posit)=c("Altitud","Nª_accidentes")
    aux.plot.luz.posit=plot(acc_deslum_posit)
    
    x=acc_deslum_posit[,1]
    y=acc_deslum_posit[,2]
    
    aux.loess.plot=loess(y ~ x,data=acc_deslum_posit, span=suavizado)
    lines(acc_deslum_posit$Altitud,aux.loess.plot$fitted,col="blue",lwd=3)
    abline(v=15, col="red", lwd=2)
    
    return(list(aux.df2,"Accidentes con luz natural",tabla_luz,"Accidentes con deslumbramiento",tabla_deslumb,aux.plot.luz,aux.plot.luz.posit))
    
  } #End_function_
  
  crear_modelo_zero=function(aux.df,aux.y,aux.list){
    
    y=aux.df$aux.y
    aux.var=(aux.list)[[1]]
    new_str = gsub('TRUE',"",aux.var)
    aux.var=paste("(",new_str, ")",collapse = "+")
    formula=as.formula(paste("y", aux.var, sep=" ~ "))
    aux.modelo= zeroinfl(formula, data=aux.df, family = "poisson",control = glm.control(maxit=100))
    return(aux.modelo)
  }#End_function_
  
  crear_modelo_zero=function(aux.df,aux.y,aux.model,aux.p_valor=NULL){
    if (is.null(aux.p_valor)){aux.p_valor=0.001}         #Establece umbral de aceptación según p-valor
    
    y=aux.df$aux.y
    aux.var=terminos(aux.model,aux.p_valor)[,1]
    new_str = gsub('TRUE',"",aux.var)
    aux.var=paste("(",new_str, ")",collapse = "+")
    formula=as.formula(paste("y", aux.var, sep=" ~ "))
    aux.modelo= zeroinfl(formula, data=aux.df, dist = "poisson",control = zeroinfl.control(maxit=100)))
    return(aux.modelo)
  }#End_function_
  
  
  #Funciones intervalos de confianza

  int_conf=function(aux.df,aux.marcador,aux.model){
    
    aux_predict=predict(aux.model,newdata = aux.df[aux.marcador,],type="link",se.fit=TRUE)
    valor_ajustado=exp(aux_predict$fit)
    fit=aux_predict$fit
    se.fit=aux_predict$se.fit
    
    predict.df=aux_predict$fit
    extremo_inf=0*aux_predict$fit #Creo df para intervalo
    extremo_sup=0*aux_predict$fit
    
    int.conf.df=as.data.frame(round(valor_ajustado,5))
    colnames(int.conf.df)="estimado"
    int.conf.df$extremo_inf=NA 
    int.conf.df$extremo_sup=NA 
    
    for (i in 1:length(aux_predict$fit)) {
      
      #Int_conf 95%---->1-alpha/2=0.975
      extremo_inf[i]=round(exp(fit[i]-qnorm(0.975)*se.fit[i]),5) 
      extremo_sup[i]=round(exp(fit[i]+qnorm(0.975)*se.fit[i]),5)
      
      predict.df[i]=paste(round(valor_ajustado[i],5)," ( ",extremo_inf[i],", " ,extremo_sup[i]," )",collapse = "")
      
      int.conf.df$extremo_inf=extremo_inf
      int.conf.df$extremo_sup=extremo_sup
      
    }
    
    newdata = aux.df[aux.marcador,]
    rownames(int.conf.df)=newdata$aux.yday.hour.itinere
    int.conf.df$year=newdata$year
    int.conf.df$day_year=newdata$day_year
    int.conf.df$weekday=newdata$weekday
    int.conf.df$mday=newdata$mday
    int.conf.df$hour=newdata$hour
    int.conf.df$month=newdata$month
    int.conf.df$itinere.key=newdata$itinere.key
    
    predict.df=t(as.data.frame(predict.df))
    
    # solapamiento = data.frame(matrix(nrow = (length(aux_predict$fit)-1), ncol = length(aux_predict$fit))) 
    # 
    # 
    # for (i in 1:(length(aux_predict$fit)-1)) {
    #   for(j in (i+1):length(aux_predict$fit)){
    #     solapamiento[i,j]=(extremo_inf[j]< (extremo_inf[i]|extremo_sup[i]) ) & ( (extremo_inf[i]|extremo_sup[i]) <extremo_sup[j])
    #     }
    #   }
    
    return(list(predict.df,int.conf.df))
  }#End_function_
  
  plot_con_int_diario=function(aux.df,aux.marcador,aux.model){
    
    int.conf.df=(int_conf(aux.df,aux.marcador,aux.model))[2]
    aux.y=(as.data.frame(int.conf.df))
    aux.y$day_year_itinere=aux.y$day_year*10+aux.y$itinere.key
    
    aux.prueba=tapply(aux.y$estimado,list(aux.y$day_year_itinere),mean, na.rm=TRUE)
    aux.prueba=(as.data.frame(aux.prueba))
    colnames(aux.prueba)="estimado"
    aux.prueba$extremo_inf=tapply(aux.y$extremo_inf,list(aux.y$day_year_itinere),mean, na.rm=TRUE)
    aux.prueba$extremo_sup=tapply(aux.y$extremo_sup,list(aux.y$day_year_itinere),mean, na.rm=TRUE)
    aux.prueba$day_year=tapply(aux.y$day_year,list(aux.y$day_year_itinere),mean, na.rm=TRUE)
    
    # aux.row=as.integer(row.names(aux.prueba))
    # aux.prueba$itinere.key=aux.row-round(aux.row/10,0)*10
    # aux.y=aux.prueba
    
    marcador=is.element(aux.y$itinere.key,0)
    aux.x=aux.y$day_year[marcador]
    est=aux.y$estimado[marcador]
    inf=aux.y$extremo_inf[marcador]
    sup=aux.y$extremo_sup[marcador]
    
    min=min(aux.y$extremo_inf)*0.8
    max=max(aux.y$extremo_sup)*1.2
    titulo=paste(aux.y$hour[1],": 00 -",aux.y$year[1])
    
    plot(aux.x,est, ylim=(c(min,max)), type = "l", lwd=2, col=1)
    lines(aux.x,inf)#extremo_inf
    lines(aux.x,sup)#extremo_sup
    polygon(c(aux.x, rev(aux.x)), c(inf, rev(sup)), col = 1,  density = 10, angle = 45)
    
    title(titulo, line=2.5)
    abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
    a=c(0,32,60,91,121,152,182,213,244,274,305,335)
    b=c(32,60,91,121,152,182,213,244,274,305,335,366)
    meses_plot=(a+b)/2
    axis(3,at=meses_plot, cex.axis=1,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
    
    
    for (i in 1: max(unique(aux.y$itinere.key)) ) {
      
      marcador=is.element(aux.y$itinere.key,i)
      aux.x=aux.y$day_year[marcador]
      est=aux.y$estimado[marcador]
      inf=aux.y$extremo_inf[marcador]
      sup=aux.y$extremo_sup[marcador]
      
      lines(aux.x,est, lwd=2, col = (i+1))
      lines(aux.x,inf)#extremo_inf
      lines(aux.x,sup)#extremo_sup
      
      polygon(c(aux.x, rev(aux.x)), c(inf, rev(sup)), col = (i+1),  density = 10, angle = 45)
      
      
    }
    legend(x = "topright",legend = unique(aux.y$itinere.key),  lty =1, col = (unique(aux.y$itinere.key)+1),lwd = 2)
  }  #End_function_
  
  plot_con_int_horario_antigua=function(aux.df,aux.marcador,aux.model){
    
    #aux.itinere.key: 0-misión, 1-itinere ida, 2-itinere vuelta
    
    int.conf.df=(int_conf(aux.df,aux.marcador,aux.model))[2]
    aux.y=(as.data.frame(int.conf.df))
    aux.y$hour_itinere=aux.y$hour*10+aux.y$itinere.key
    
    aux.prueba=tapply(aux.y$estimado,list(aux.y$hour_itinere),mean, na.rm=TRUE)
    aux.prueba=(as.data.frame(aux.prueba))
    colnames(aux.prueba)="estimado"
    aux.prueba$extremo_inf=tapply(aux.y$extremo_inf,list(aux.y$hour_itinere),mean, na.rm=TRUE)
    aux.prueba$extremo_sup=tapply(aux.y$extremo_sup,list(aux.y$hour_itinere),mean, na.rm=TRUE)
    aux.prueba$day_year=tapply(aux.y$day_year,list(aux.y$hour_itinere),mean, na.rm=TRUE)
    
    marcador=is.element(aux.y$itinere.key,0)
    aux.x=aux.y$hour[marcador]
    est=aux.y$estimado[marcador]
    inf=aux.y$extremo_inf[marcador]
    sup=aux.y$extremo_sup[marcador]
    
    min=min(aux.y$extremo_inf)*0.8
    max=max(aux.y$extremo_sup)*1.2
    titulo=paste(aux.y$weekday[1],",",aux.y$mday[1],"-",aux.y$month[1],"-",aux.y$year[1])
    
    plot(aux.x,est, ylim=(c(min,max)), type = "l", lwd=2, col=1)
    lines(aux.x,inf)#extremo_inf
    lines(aux.x,sup)#extremo_sup
    
    polygon(c(aux.x, rev(aux.x)), c(inf, rev(sup)), col = 1,  density = 10, angle = 45)
    
    title(titulo, line=2.5)
    abline(v=c(3,6,9,12,15,18,21), lty=3, lwd=0.1)
    
    for (i in 1: max(unique(aux.y$itinere.key)) ) {
      
      marcador=is.element(aux.y$itinere.key,i)
      aux.x=aux.y$hour[marcador]
      est=aux.y$estimado[marcador]
      inf=aux.y$extremo_inf[marcador]
      sup=aux.y$extremo_sup[marcador]
      
      lines(aux.x,est, lwd=2, col = (i+1))
      lines(aux.x,inf)#extremo_inf
      lines(aux.x,sup)#extremo_sup
      
      polygon(c(aux.x, rev(aux.x)), c(inf, rev(sup)), col = (i+1),  density = 10, angle = 45)
      
    }
    legend(x = "topright",legend = unique(aux.y$itinere.key),  lty =1, col = (unique(aux.y$itinere.key)+1),lwd = 2)
  }  #End_function_
  
  plot_con_int_horario=function(aux.df,aux.año,aux.dias,aux.model){
    
    int.conf.df=data.frame()
    for (i in aux.dias) {
      aux.marcador=is.element(response.hourly.itinere$year,aux.año)&is.element(response.hourly.itinere$day_year,i)
      aux.int.conf.df=(int_conf(aux.df,aux.marcador,aux.model))[2]
      aux.int.conf.df=as.data.frame(aux.int.conf.df)
      int.conf.df=rbind(int.conf.df,aux.int.conf.df)
    }
    
    aux.y=(as.data.frame(int.conf.df))
    aux.y$hour_itinere=aux.y$hour*10+aux.y$itinere.key
    
    aux.prueba=tapply(aux.y$estimado,list(aux.y$hour_itinere),mean, na.rm=TRUE)
    aux.prueba=(as.data.frame(aux.prueba))
    colnames(aux.prueba)="estimado"
    aux.prueba$extremo_inf=tapply(aux.y$extremo_inf,list(aux.y$hour_itinere),mean, na.rm=TRUE)
    aux.prueba$extremo_sup=tapply(aux.y$extremo_sup,list(aux.y$hour_itinere),mean, na.rm=TRUE)
    aux.prueba$day_year=tapply(aux.y$day_year,list(aux.y$hour_itinere),mean, na.rm=TRUE)
    
    min=min(aux.y$extremo_inf)*0.8
    max=max(aux.y$extremo_sup)*1.2
    
    for (i in aux.dias) {
      
      aux.marcador_dia=is.element(aux.y$day_year,i)
      aux.dia=subset(aux.y,aux.marcador_dia==TRUE)  
      
      marcador=is.element(aux.dia$itinere.key,0)
      aux.x=aux.dia$hour[marcador]
      est=aux.dia$estimado[marcador]
      inf=aux.dia$extremo_inf[marcador]
      sup=aux.dia$extremo_sup[marcador]
      
      titulo=paste(aux.dia$weekday[1],",",aux.dia$mday[1],"-",aux.dia$month[1],"-",aux.dia$year[1])
      
      plot(aux.x,est, ylim=(c(min,max)), type = "l", lwd=2, col=1)
      lines(aux.x,inf)#extremo_inf
      lines(aux.x,sup)#extremo_sup
      
      polygon(c(aux.x, rev(aux.x)), c(inf, rev(sup)), col = 1,  density = 10, angle = 45)
      
      title(titulo, line=2.5)
      abline(v=c(3,6,9,12,15,18,21), lty=3, lwd=0.1)
      
      for (i in 1: max(unique(aux.dia$itinere.key)) ) {
        
        marcador=is.element(aux.dia$itinere.key,i)
        aux.x=aux.dia$hour[marcador]
        est=aux.dia$estimado[marcador]
        inf=aux.dia$extremo_inf[marcador]
        sup=aux.dia$extremo_sup[marcador]
        
        lines(aux.x,est, lwd=2, col = (i+1))
        lines(aux.x,inf)#extremo_inf
        lines(aux.x,sup)#extremo_sup
        
        polygon(c(aux.x, rev(aux.x)), c(inf, rev(sup)), col = (i+1),  density = 10, angle = 45)
        
      }
      legend(x = "topright",legend = unique(aux.y$itinere.key),  lty =1, col = (unique(aux.y$itinere.key)+1),lwd = 2)
      
    }
  }  #End_function_ 
  
  
  crear_modelo_sin_NA=function(aux.df,aux.y,aux.model){
    aux.marcador=!is.na(aux.model$coefficients)
    aux.model$coefficients[aux.marcador]
    
    y=aux.df$aux.y
    aux.var=terminos(aux.model)[[1]]
    new_str = gsub('TRUE',"",aux.var)
    aux.var=paste("(",new_str, ")",collapse = "+")
    formula=as.formula(paste("y", aux.var, sep=" ~ "))
    aux.modelo= glm(formula, data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
    
    
    return(aux.modelo)
  }#End_function_
  
  
  #Funciones informe

  analisis_general=function(df, años){
    
    tamaño=df$PLANTILLAE
    tamaño=cut(tamaño, breaks=c(0,50,249,Inf))
    levels(tamaño)
    levels(tamaño)=c("Pequeña Eª","Mediana Eª","Gran Eª")
    df$tamaño=tamaño
    
    #Conclusiones años 
    
    #1? Selecciono solo los poligonos y años 
    accidentes_en_poligonos=subset(df, !is.na(df$ubicacion))
    aux.marcador=is.element(accidentes_en_poligonos$year,años)
    accidentes_en_poligonos_17_19=subset(accidentes_en_poligonos,aux.marcador==TRUE)
    
    #2? Obtengo la media de trabajadores por empresa o el dato mÃ¡s reciente
    
    aux.acc=accidentes_en_poligonos_17_19 %>% group_by(CIF_EMPRESA)  %>% mutate(nª_trabajadores=mean(PLANTILLAE))
    aux.acc$nª_trabajadores=as.integer(aux.acc$nª_trabajadores)
    aux.acc=aux.acc %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_reciente=PLANTILLAE[which.max(FECHAACCIDENTE)])
    aux.acc$plantilla_reciente=as.integer(aux.acc$plantilla_reciente)
    aux.acc=aux.acc %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT=PLANTILLACT[which.max(FECHAACCIDENTE)])
    aux.acc$plantilla_ACT=as.integer(aux.acc$plantilla_ACT)
    
    # aux.acc=aux.acc  %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT2=PLANTILLACT[which.max(1:n())] )
    # aux.acc$plantilla_ACT2=as.integer(aux.acc$plantilla_ACT2)
    
    aux.acc=aux.acc%>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT_reciente=mean(PLANTILLACT))
    aux.acc$plantilla_ACT_reciente=as.integer(aux.acc$plantilla_ACT_reciente)
    
    #3? Elimino accidentes repetidos y autonomos, y creo tamaño promedio de empresa
    
    accidentes_en_poligonos_17_19=(aux.acc[!duplicated(aux.acc$IPF_MD5,aux.acc$FECHAACCIDENTE),])
    accidentes_en_poligonos_17_19=subset(accidentes_en_poligonos_17_19, SITUACION==1|SITUACION==2)
    
    tamaño_promedio=accidentes_en_poligonos_17_19$nª_trabajadores
    tamaño_promedio=cut(tamaño_promedio, breaks=c(0,50,249,Inf))
    levels(tamaño_promedio)
    levels(tamaño_promedio)=c("Pequeña Eª","Mediana Eª","Gran Eª")
    accidentes_en_poligonos_17_19$tamaño_promedio=tamaño_promedio
    
    #4? Selecciono solo "in itinere"
    accidentes_en_poligonos_17_19_itinere=subset(accidentes_en_poligonos_17_19, itinere==TRUE)
    
    
    #------------------------------------#
    
    
    
    #------------------------------------#
    #par(mfrow=c(1,2))
    par(mfrow=c(1,1))
    0.75
    tabla_accidentes=table(accidentes_en_poligonos_17_19_itinere$year)
    ylim.sup=max(tabla_accidentes)*1.2
    aux.ylim=c(0,ylim.sup)
    titulo="TOTAL ALT POR AÃ‘OS"
    bp=barplot(tabla_accidentes, beside=TRUE,  yaxt = "n", ylim=aux.ylim, main=titulo, cex.main=1.5,cex.names = 2, col=rgb(0.18,0.36,0.58,1.0) )
    abline(h=seq(1, ylim.sup, by=20), col="gray")
    bp=barplot(tabla_accidentes, beside=TRUE,add=TRUE,  yaxt = "n", ylim=aux.ylim, main=titulo, cex.main=1.5,cex.names = 2, col=rgb(0.18,0.36,0.58,1.0) )
    axis(2, at = seq(0, ylim.sup, by=20), las = 1, cex.axis=1.5, col = NA)
    text(bp, as.matrix(tabla_accidentes), round(as.matrix(tabla_accidentes), 1),cex=2,pos=3)
    #dev.print(pdf, 'Informe_3.01_TOTAL_ALT_POR_AÃ‘OS.pdf' ,  height=10, width=10 )
    
    tabla_accidentes=table(accidentes_en_poligonos_17_19_itinere$ubicacion)
    etiquetas = paste0(names(tabla_accidentes), "\n", as.numeric(tabla_accidentes), "\n",round(100 * tabla_accidentes/sum(tabla_accidentes), 2), "%")
    pie2(tabla_accidentes,labels = etiquetas, main="TOTAL POR UBICACIÓN", cex.main=1.5, cex=1.5, col = c("steelblue","brown2", "olivedrab3", "purple2"))
    #dev.print(pdf, 'Informe_3.02_TOTAL_POR_UBICACION.pdf' ,  height=10, width=10 )
    
    itinere_poligonos=table(accidentes_en_poligonos_17_19_itinere$year,accidentes_en_poligonos_17_19_itinere$ubicacion)
    ylim.sup=max(itinere_poligonos)*1.2
    aux.ylim=c(0,ylim.sup)
    titulo="TOTAL ALT POR ZONA Y AÃ‘O"
    if(nrow(itinere_poligonos)==1){itinere_poligonos=itinere_poligonos[c(1,3,2,4)]}else{ itinere_poligonos=itinere_poligonos[,c(1,3,2,4)]}
    aux.colores=c("steelblue","brown2", "olivedrab3")
    aux.años=table(accidentes_en_poligonos_17_19_itinere$year)
    bp=barplot(itinere_poligonos, beside=TRUE,yaxt = "n", ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1.5, cex.names = 1.1, col = aux.colores[1:length(aux.años)])
    axis(2, at = seq(0, ylim.sup, by=10), las = 1, cex.axis=0.75, col = NA)
    abline(h=seq(1, ylim.sup, by=10), col="gray")
    bp=barplot(itinere_poligonos, beside=TRUE,add=TRUE,yaxt = "n", ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1.5, cex.names = 1.1, col = aux.colores[1:length(aux.años)])
    text(bp, as.matrix(itinere_poligonos), round(as.matrix(itinere_poligonos), 1),font=2,cex=1.25,pos=3)
    itinere_poligonos=table(accidentes_en_poligonos_17_19_itinere$year,accidentes_en_poligonos_17_19_itinere$ubicacion)
    if(nrow(itinere_poligonos)==1){itinere_poligonos=itinere_poligonos[c(1,3,2,4)]} else{ itinere_poligonos=itinere_poligonos[,c(1,3,2,4)]}
    #dev.print(pdf, 'Informe_3.03_TOTAL_POR_UBICACION.pdf' ,  height=10, width=10 )
    
    #------------------------------------#
    
    #Elimino las empresas repetidas
    
    subset_2017_2019=subset(accidentes_en_poligonos_17_19)[!duplicated(accidentes_en_poligonos_17_19$CIF_EMPRESA),]
    #subset_2017_2019=subset(accidentes_en_poligonos_17_19[!duplicated(accidentes_en_poligonos_17_19[c("CIF_EMPRESA","ubicacion")]),] )
    ubicacion=subset_2017_2019$ubicacion     
    #tamañosubset_2017_2019=subset_2017_2019$tamaño
    tamañosubset_2017_2019=subset_2017_2019$tamaño_promedio
    tabla_2017_2019=tapply(subset_2017_2019$plantilla_ACT, list(tamañosubset_2017_2019, ubicacion), sum, na.rm=TRUE)
    tabla_2017_2019[is.na(tabla_2017_2019)] = 0
    sum(subset_2017_2019$plantilla_ACT[subset_2017_2019$LUGAR==3|subset_2017_2019$LUGAR==2],na.rm=TRUE)
    #tabla_2017_2019=tapply(subset_2017_2019$plantilla_reciente, list(tamañosubset_2017_2019, ubicacion), sum)
    tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
    tabla_2017_2019$total=rowSums(tabla_2017_2019,na.rm=TRUE)
    aux.function=function(x){if(is.numeric(x)) y=sum(x,na.rm=TRUE) else y="Total";return(y)}
    tabla_2017_2019=tabla_2017_2019 %>% bind_rows(summarise_all(., ~aux.function(.x)))
    tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
    row.names(tabla_2017_2019)[4]="Total"
    tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
    tabla_2017_2019
    obten_tabla(tabla_2017_2019)
    
    trabajadores2017_2019=as.numeric(tabla_2017_2019[4,1:4])
    etiquetas = paste0(names(tabla_2017_2019[4,1:4]), "\n", trabajadores2017_2019, "\n",round(100 * trabajadores2017_2019[1:4]/sum(trabajadores2017_2019), 2), "%")
    pie2(trabajadores2017_2019,labels = etiquetas ,main="Nª DE TRABAJADORES TOTALES POR ZONA", cex.main=1.5, cex=1.5, col = c("steelblue", "olivedrab3","brown2", "purple2"))
    #dev.print(pdf, 'Informe_3.04_Nª_DE_TRABAJADORES_TOTALES_POR_ZONA.pdf' ,  height=10, width=10 )
    
    empresas_trabajadores=t(tabla_2017_2019)[5,1:3]
    etiquetas = paste0(names(empresas_trabajadores), "\n", empresas_trabajadores, "\n",round(100 * empresas_trabajadores/sum(empresas_trabajadores), 2), "%")
    pie2(empresas_trabajadores,labels = etiquetas,main="Nª DE TRABAJADORES TOTALES \n POR TAMAÑO DE EMPRESA", cex.main=1.5, cex=1.5, col = c("steelblue","brown2", "olivedrab3"))
    #dev.print(pdf, 'Informe_3.05_Nª_DE_TRABAJADORES_TOTALES_POR_TAMAÑO_DE_EMPRESA.pdf' ,  height=10, width=10 )
    
    tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
    ylim.sup=max(tabla_2017_2019[1:3,1:4],na.rm=TRUE)*1.2
    aux.ylim=c(0,ylim.sup)
    titulo="Nª DE TRABAJADORES POR UBICACIÓN \n Y TAMAÑO DE EMPRESA"
    bp=barplot((as.matrix(tabla_2017_2019[1:3,1:4])),yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1.5, cex.names = 1.25, col = c("steelblue","brown2", "olivedrab3"))
    text(bp, as.matrix(tabla_2017_2019[1:3,1:4]), round(as.matrix(tabla_2017_2019[1:3,1:4]), 1),font=2,cex=1,pos=3)
    #dev.print(pdf, 'Informe_3.06_Nª_DE_TRABAJADORES_POR_UBICACION_Y_TAMAÑO_DE_EMPRESA.pdf' ,  height=10, width=10 )
    
    
    #------------------------------------#
    subset=accidentes_en_poligonos_17_19_itinere
    table(subset$year)
    
    tabla_17_19_itinere=table(subset$tamaño,subset$ubicacion)
    tabla_17_19_itinere=table(subset$tamaño_promedio,subset$ubicacion)
    tabla_17_19_itinere=as.data.frame.matrix(tabla_17_19_itinere)
    tabla_17_19_itinere$total=rowSums(tabla_17_19_itinere,na.rm=TRUE)
    #tabla_17_19_itinere=tabla_17_19_itinere %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
    aux.function=function(x){if(is.numeric(x)) y=sum(x) else y="Total";return(y)}
    tabla_17_19_itinere=tabla_17_19_itinere %>% bind_rows(summarise_all(., ~aux.function(.x)))
    tabla_17_19_itinere=as.data.frame.matrix(tabla_17_19_itinere)
    row.names(tabla_17_19_itinere)[4]="Total"
    tabla_17_19_itinere=tabla_17_19_itinere[c(3,2,1,4),]
    tabla_17_19_itinere
    obten_tabla(tabla_17_19_itinere)
    
    n=length(levels(df$ubicacion))+1
    aux.df=(tabla_17_19_itinere[1:3,1:n])
    aux.df= as.data.frame(sapply(aux.df, as.numeric))
    porcentaje_tabla_17_19_itinere=as.data.frame((prop.table(as.matrix(aux.df),2)))
    porcentaje_tabla_17_19_itinere[4,]=colSums(porcentaje_tabla_17_19_itinere)
    porcentaje_tabla_17_19_itinere=sapply(porcentaje_tabla_17_19_itinere, function(x) percent(x, accuracy=1))
    obten_tabla(porcentaje_tabla_17_19_itinere)
    
    accidentes2017_2019=as.numeric(tabla_17_19_itinere[4,1:4])
    etiquetas = paste0(names(tabla_17_19_itinere[4,1:4]), "\n", accidentes2017_2019, "\n",round(100 * accidentes2017_2019[1:4]/sum(accidentes2017_2019), 2), "%")
    pie2(accidentes2017_2019,labels = etiquetas ,main="Nª TOTAL DE ALT POR UBICACIÓN", cex.main=1.5, cex=1.5, col = c("steelblue","brown2", "olivedrab3", "purple2"))
    #dev.print(pdf, 'Informe_3.07_Nª_TOTAL_DE_ALT_POR_UBICACIÓN.pdf' ,  height=10, width=10 )
    
    empresas_17_19=t(tabla_17_19_itinere)[5,1:3]
    etiquetas = paste0(names(empresas_17_19), "\n", empresas_17_19, "\n",round(100 * empresas_17_19/sum(empresas_17_19), 2), "%")
    pie2(empresas_17_19,labels = etiquetas,main="Nª TOTAL DE ALT POR TAMAÑO DE EMPRESA", cex.main=1.5, cex=1.5, col = c("steelblue","brown2", "olivedrab3"))
    #dev.print(pdf, 'Informe_3.08_Nª_TOTAL_DE_ALT_POR_TAMAÑO_DE_EMPRESA.pdf' ,  height=10, width=10 )
    #0.75
    ylim.sup=max(tabla_17_19_itinere[1:3,1:4])*1.2
    aux.ylim=c(0,ylim.sup)
    #titulo="Nª TOTAL DE ACCIDENTES LABORALES DE TRÃFICO 'IN ITINERE' \n EN PEQUEÃ‘AS, MEDIANAS Y GRANDES EMPRESAS Y ZONA"
    titulo=""
    bp=barplot(t(as.matrix(tabla_17_19_itinere[1:3,1:4])), yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1, cex.names = 1.25, col = c("steelblue","brown2", "olivedrab3", "purple2"))
    grid(nx =NA , ny = NULL, lty = 1, col = "gray")
    bp=barplot(t(as.matrix(tabla_17_19_itinere[1:3,1:4])),add=TRUE, yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1, cex.names = 1.25, col = c("steelblue","brown2", "olivedrab3", "purple2"))
    text(bp, t(as.matrix(tabla_17_19_itinere[1:3,1:4])), round(t(as.matrix(tabla_17_19_itinere[1:3,1:4])), 1),font=2,cex=1.25,pos=3)
    #dev.print(pdf, 'Informe_3.09_Nª_TOTAL_DE_ACCIDENTES_LABORALES_DE_TRÃFICO_IN_ITINERE_EN_EMPRESAS_Y_ZONA.pdf' ,  height=10, width=10 )
    
    ylim.sup=max(tabla_17_19_itinere[1:3,1:4])*1.2
    aux.ylim=c(0,ylim.sup)
    titulo="Nª DE ALT POR TAMAÑO \n DE EMPRESA Y UBICACIÓN"
    bp=barplot((as.matrix(tabla_17_19_itinere[1:3,1:4])), beside=TRUE, yaxt = "n", ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1.5, cex.names = 1.25, col = c("steelblue","brown2", "olivedrab3", "purple2"))
    text(bp, as.matrix(tabla_17_19_itinere[1:3,1:4]), round(as.matrix(tabla_17_19_itinere[1:3,1:4]), 1),font=2,cex=1.25,pos=3)
    #dev.print(pdf, 'Informe_3.10_Nª_DE_ALT_POR_TAMAÑO_DE_EMPRESA_Y_UBICACION.pdf' ,  height=10, width=10 )
    
    
    #------------------------------------#
    promedio_trabajadores=tabla_2017_2019[c(3,2,1,4),]
    promedio=as.numeric(round((promedio_trabajadores[4,1:5]/promedio_trabajadores[4,5]),2))
    promedio_trabajadores[5,]=label_percent()(promedio)
    promedio_trabajadores
    obten_tabla(promedio_trabajadores)
    
    aux.df=(promedio_trabajadores[1:3,1:5])
    aux.df= as.data.frame(sapply(aux.df, as.numeric))
    porcentaje_trabajadores=as.data.frame(round(prop.table(as.matrix(aux.df),2),2))
    porcentaje_trabajadores[4,]=colSums(porcentaje_trabajadores)
    porcentaje_trabajadores=sapply(porcentaje_trabajadores, function(x) percent(x, accuracy=1))
    porcentaje_trabajadores
    obten_tabla(porcentaje_trabajadores)
    
    tabla_17_19_itinere
    obten_tabla(tabla_17_19_itinere)
    
    aux.df=(tabla_17_19_itinere[1:3,1:4])
    aux.df= as.data.frame(sapply(aux.df, as.numeric))
    porcentaje_acc_itinere=as.data.frame(round(prop.table(as.matrix(aux.df)),2))
    porcentaje_acc_itinere$total=rowSums(porcentaje_acc_itinere)
    porcentaje_acc_itinere[4,]=colSums(porcentaje_acc_itinere)
    porcentaje_acc_itinere=sapply(porcentaje_acc_itinere, function(x) percent(x, accuracy=1))
    porcentaje_acc_itinere
    obten_tabla(porcentaje_acc_itinere)
    
  }#End_function_
  
  informe=function(df, años){
    
    tamaño=df$PLANTILLAE
    tamaño=cut(tamaño, breaks=c(0,50,249,Inf))
    levels(tamaño)
    levels(tamaño)=c("Pequeña Eª","Mediana Eª","Gran Eª")
    df$tamaño=tamaño
    
    #Conclusiones años 
    
    #1? Selecciono solo los poligonos y años 
    accidentes_en_poligonos=subset(df, !is.na(df$ubicacion))
    aux.marcador=is.element(accidentes_en_poligonos$year,años)
    accidentes_en_poligonos_17_19=subset(accidentes_en_poligonos,aux.marcador==TRUE)
    
    #2? Obtengo la media de trabajadores por empresa o el dato mÃ¡s reciente
    
    aux.acc=accidentes_en_poligonos_17_19 %>% group_by(CIF_EMPRESA)  %>% mutate(nª_trabajadores=mean(PLANTILLAE))
    aux.acc$nª_trabajadores=as.integer(aux.acc$nª_trabajadores)
    aux.acc=aux.acc %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_reciente=PLANTILLAE[which.max(FECHAACCIDENTE)])
    aux.acc$plantilla_reciente=as.integer(aux.acc$plantilla_reciente)
    aux.acc=aux.acc %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT=PLANTILLACT[which.max(FECHAACCIDENTE)])
    aux.acc$plantilla_ACT=as.integer(aux.acc$plantilla_ACT)
    
    # aux.acc=aux.acc  %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT2=PLANTILLACT[which.max(1:n())] )
    # aux.acc$plantilla_ACT2=as.integer(aux.acc$plantilla_ACT2)
    
    aux.acc=aux.acc%>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT_reciente=mean(PLANTILLACT))
    aux.acc$plantilla_ACT_reciente=as.integer(aux.acc$plantilla_ACT_reciente)
    
    #3? Elimino accidentes repetidos y autonomos, y creo tamaño promedio de empresa
    
    accidentes_en_poligonos_17_19=(aux.acc[!duplicated(aux.acc$IPF_MD5,aux.acc$FECHAACCIDENTE),])
    accidentes_en_poligonos_17_19=subset(accidentes_en_poligonos_17_19, SITUACION==1|SITUACION==2)
    
    tamaño_promedio=accidentes_en_poligonos_17_19$nª_trabajadores
    tamaño_promedio=cut(tamaño_promedio, breaks=c(0,50,249,Inf))
    levels(tamaño_promedio)
    levels(tamaño_promedio)=c("Pequeña Eª","Mediana Eª","Gran Eª")
    accidentes_en_poligonos_17_19$tamaño_promedio=tamaño_promedio
    
    #4? Selecciono solo "in itinere"
    accidentes_en_poligonos_17_19_itinere=subset(accidentes_en_poligonos_17_19, itinere==TRUE)
    
    
    #------------------------------------#
    
    tabla_accidentes=table(accidentes_en_poligonos_17_19_itinere$year)
    
    #------------------------------------#
    
    #Elimino las empresas repetidas
    
    #subset_2017_2019=subset(accidentes_en_poligonos_17_19)[!duplicated(accidentes_en_poligonos_17_19$CIF_EMPRESA),]
    subset_2017_2019=subset(accidentes_en_poligonos_17_19[!duplicated(accidentes_en_poligonos_17_19[c("CIF_EMPRESA","ubicacion")]),] )
    ubicacion=subset_2017_2019$ubicacion
    #tamañosubset_2017_2019=subset_2017_2019$tamaño
    tamañosubset_2017_2019=subset_2017_2019$tamaño_promedio
    tabla_2017_2019=tapply(subset_2017_2019$plantilla_ACT, list(tamañosubset_2017_2019, ubicacion), sum, na.rm=TRUE)
    #tabla_2017_2019=tapply(subset_2017_2019$nª_trabajadores, list(tamañosubset_2017_2019, ubicacion), sum, na.rm=TRUE)
    tabla_2017_2019[is.na(tabla_2017_2019)] = 0
    sum(subset_2017_2019$plantilla_ACT[subset_2017_2019$LUGAR==3|subset_2017_2019$LUGAR==2],na.rm=TRUE)
    #tabla_2017_2019=tapply(subset_2017_2019$plantilla_reciente, list(tamañosubset_2017_2019, ubicacion), sum)
    tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
    tabla_2017_2019$total=rowSums(tabla_2017_2019,na.rm=TRUE)
    aux.function=function(x){if(is.numeric(x)) y=sum(x,na.rm=TRUE) else y="Total";return(y)}
    tabla_2017_2019=tabla_2017_2019 %>% bind_rows(summarise_all(., ~aux.function(.x)))
    tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
    row.names(tabla_2017_2019)[4]="Total"
    tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
    tabla_2017_2019
    
    #------------------------------------#
    subset=accidentes_en_poligonos_17_19_itinere
    table(subset$year)
    
    #tabla_17_19_itinere=table(subset$tamaño,subset$ubicacion)
    tabla_17_19_itinere=table(subset$tamaño_promedio,subset$ubicacion)
    tabla_17_19_itinere=as.data.frame.matrix(tabla_17_19_itinere)
    tabla_17_19_itinere$total=rowSums(tabla_17_19_itinere)
    #tabla_17_19_itinere=tabla_17_19_itinere %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
    aux.function=function(x){if(is.numeric(x)) y=sum(x) else y="Total";return(y)}
    tabla_17_19_itinere=tabla_17_19_itinere %>% bind_rows(summarise_all(., ~aux.function(.x)))
    tabla_17_19_itinere=as.data.frame.matrix(tabla_17_19_itinere)
    row.names(tabla_17_19_itinere)[4]="Total"
    tabla_17_19_itinere=tabla_17_19_itinere[c(3,2,1,4),]
    #tabla_17_19_itinere
    
    #------------------------------------#
    n=length(levels(df$ubicacion))+1
    promedio_trabajadores=tabla_2017_2019[c(3,2,1,4),]
    promedio=as.numeric(round((promedio_trabajadores[4,1:n]/promedio_trabajadores[4,n]),2))
    promedio_trabajadores[5,]=label_percent()(promedio)
    #promedio_trabajadores
    #obten_tabla(promedio_trabajadores)
    
    aux.df=(promedio_trabajadores[1:3,1:n])
    aux.df= as.data.frame(sapply(aux.df, as.numeric))
    porcentaje_trabajadores=as.data.frame(round(prop.table(as.matrix(aux.df),2),2))
    porcentaje_trabajadores[4,]=colSums(porcentaje_trabajadores)
    porcentaje_trabajadores=sapply(porcentaje_trabajadores, function(x) percent(x, accuracy=1))
    # porcentaje_trabajadores
    # obten_tabla(porcentaje_trabajadores)
    
    # tabla_17_19_itinere
    # obten_tabla(tabla_17_19_itinere)
    
    aux.df=(tabla_17_19_itinere[1:3,1:(n-1)])
    aux.df= as.data.frame(sapply(aux.df, as.numeric))
    porcentaje_acc_itinere=as.data.frame(round(prop.table(as.matrix(aux.df)),2))
    porcentaje_acc_itinere$total=rowSums(porcentaje_acc_itinere)
    porcentaje_acc_itinere[4,]=colSums(porcentaje_acc_itinere)
    porcentaje_acc_itinere=sapply(porcentaje_acc_itinere, function(x) percent(x, accuracy=1))
    # porcentaje_acc_itinere
    # obten_tabla(porcentaje_acc_itinere)
    
    #------------------------------------#
    
    empr_trab_accs=as.data.frame(as.numeric(t(table(subset_2017_2019$tamaño))))
    row.names(empr_trab_accs)=names((table(subset_2017_2019$tamaño)))
    colnames(empr_trab_accs)=c("Nª_de_empresas")
    empr_trab_accs["Total" ,] = colSums(empr_trab_accs)
    
    porcentaje=round((empr_trab_accs[1:4,1]/empr_trab_accs[4,1]), 2)
    empr_trab_accs$trabajadores_nª_Eªs=percent(porcentaje)
    
    tabla_aux=tabla_2017_2019[c(3,2,1,4),]
    empr_trab_accs$Nª_de_trabajadores=tabla_aux[1:4,5]
    porcentaje=round((empr_trab_accs[1:4,3]/empr_trab_accs[4,3]), 2)
    empr_trab_accs$trabajadores=percent(porcentaje)
    
    tabla_aux=tabla_17_19_itinere[c(3,2,1,4),]
    empr_trab_accs$Nª_de_accidentes=tabla_aux[1:4,5]
    porcentaje=round((empr_trab_accs[1:4,5]/empr_trab_accs[4,5]), 2)
    empr_trab_accs$accidentes=percent(porcentaje)
    
    A=round((empr_trab_accs[1:4,3]/empr_trab_accs[4,3]), 2)
    B=round((empr_trab_accs[1:4,5]/empr_trab_accs[4,5]), 2)
    C=A-B
    empr_trab_accs$accidentes_trabajadores=percent(C)
    
    empr_trab_accs[c(3,2,1,4),]
    obten_tabla(empr_trab_accs[c(3,2,1,4),])
    
    Trabajadores=round((empr_trab_accs[1:4,3]/empr_trab_accs[4,3]), 2)*100
    Accidentes=round((empr_trab_accs[1:4,5]/empr_trab_accs[4,5]), 2)*100
    aux.dat=data.frame(Trabajadores, Accidentes)
    row.names(aux.dat)=row.names(empr_trab_accs)
    aux.dat=as.matrix(aux.dat)
    aux.dat=aux.dat[c(3,2,1,4),]
    
    if (length(años)>1) {
      aux.main=(paste0(años[1]," a ", años[length(años)]))} else {
        aux.main=(paste0(años[1]))}
    
    ylim.sup=max(aux.dat[1:3,1:2],na.rm = TRUE)*1.2
    aux.ylim=c(0,ylim.sup)
    bp=barplot(t(aux.dat[1:3,1:2]), beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), cex.names = 1.5,cex.axis=2, col = c("steelblue","brown2"), main=aux.main )
    #dev.print(pdf, 'Informe_3.11_ESTUDIO_DE_LA_ACCIDENTABILIDAD_POR_TAMAÑO.pdf' ,  height=10, width=10 )
    
    #------------------------------------#
    
    ubic_trab_accs=as.data.frame(as.numeric(t(table(subset_2017_2019$ubicacion))))
    row.names(ubic_trab_accs)=names((table(subset_2017_2019$ubicacion)))
    colnames(ubic_trab_accs)=c("Nª_de_empresas")
    ubic_trab_accs["Total" ,] = colSums(ubic_trab_accs)
    
    porcentaje=round((ubic_trab_accs[1:n,1]/ubic_trab_accs[n,1]), 2)
    ubic_trab_accs$trabajadores_nª_Eªs=percent(porcentaje)
    
    
    ubic_trab_accs$Nª_de_trabajadores=as.numeric((t(tabla_2017_2019))[1:n,4])
    porcentaje=round((ubic_trab_accs[1:n,3]/ubic_trab_accs[n,3]), 2)
    ubic_trab_accs$trabajadores=percent(porcentaje)
    
    ubic_trab_accs$Nª_de_accidentes=as.numeric((t(tabla_17_19_itinere))[1:n,4])
    porcentaje=round((ubic_trab_accs[1:n,5]/ubic_trab_accs[n,5]), 2)
    ubic_trab_accs$accidentes=percent(porcentaje)
    
    A=round((ubic_trab_accs[1:n,3]/ubic_trab_accs[n,3]), 2)
    B=round((ubic_trab_accs[1:n,5]/ubic_trab_accs[n,5]), 2)
    C=A-B
    ubic_trab_accs$accidentes_trabajadores=percent(C)
    
    # ubic_trab_accs
    # obten_tabla(ubic_trab_accs)
    
    Trabajadores=round((ubic_trab_accs[1:n,3]/ubic_trab_accs[n,3]), 2)*100
    Accidentes=round((ubic_trab_accs[1:n,5]/ubic_trab_accs[n,5]), 2)*100
    aux.dat=data.frame(Trabajadores, Accidentes)
    row.names(aux.dat)=row.names(ubic_trab_accs)
    aux.dat=as.matrix(aux.dat)
    
    ylim.sup=max(aux.dat[1:(n-1),1:2],na.rm = TRUE)*1.2
    aux.ylim=c(0,ylim.sup)
    bp=barplot(t(aux.dat[1:(n-1),1:2]), beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)),cex.names = 1.25,cex.axis=2, col = c("steelblue","brown2"), main=aux.main )
    #dev.print(pdf, 'Informe_3.12_ESTUDIO_DE_ACCIDENTABILIDAD_POR_UBICACIÓN.pdf' ,  height=10, width=10 )
    
    #------------------------------------#
    
    aux.dat=accidentes_en_poligonos_17_19_itinere
    aux.dat=(aux.dat[!duplicated(aux.dat$IPF_MD5,aux.dat$FECHAACCIDENTE),])
    aux.dat=aux.dat[,c(19,87,112,113,114,115,116,117,106,118)]
    
    #aux.dat=aux.dat %>% group_by(CIF_EMPRESA) %>% mutate(acc_empr=sum(accidentes))
    aux.dat=aux.dat %>% group_by(CIF_EMPRESA) %>% mutate(acc_empr=sum(itinere))
    aux.dat=subset(aux.dat)[!duplicated(aux.dat$CIF_EMPRESA),]
    
    #aux.dat$promedio=aux.dat$acc_empr/aux.dat$nª_trabajadores*1000/3
    aux.dat$promedio=aux.dat$acc_empr/aux.dat$nª_trabajadores*1000/length(años)
    
    ubicacion=aux.dat$ubicacion     
    #tamaño=aux.dat$tamaño
    tamaño=aux.dat$tamaño_promedio
    promedio=aux.dat$promedio
    subtabla=tapply(promedio, list(tamaño, ubicacion), sum)
    tabla=subtabla/table(tamaño, ubicacion)
    tabla=as.data.frame.matrix(tabla)
    tabla$total= rowSums(subtabla,na.rm = TRUE) / table(tamaño)
    
    library(plyr)
    total= (colSums(subtabla,na.rm = TRUE) / table(ubicacion))
    total=data.frame(rbind(total))
    tabla = rbind.fill(tabla, total)
    tabla[4,n]=sum(subtabla,na.rm = TRUE)/ sum(table(ubicacion),na.rm = TRUE)
    detach("package:plyr", unload = TRUE)
    
    rownames(tabla)=rownames(tabla_2017_2019[c(3,2,1,4),])
    aux.tabla.plot1=tabla
    
    tabla=tabla[c(3,2,1,4),]
    tabla=as.data.frame.matrix(round(tabla ,2 ))
    tabla[] = lapply(tabla, paste0, " \211")
    tabla
    
    tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
    df=( (tabla_17_19_itinere[1:4,1:n]/3) / (tabla_2017_2019[1:4,1:n]) )*1000
    df=as.data.frame(round(df ,2 ))
    aux.tabla.plot2=df
    df[] = lapply(df, paste0, " \211")
    df
    
    plot.tabla=aux.tabla.plot1[c(3,2,1,4),]
    ylim.sup=max(plot.tabla,na.rm = TRUE)*1.2
    aux.ylim=c(0,ylim.sup)
    bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])), yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=1.5 ), cex.names = 1.5, col = c("steelblue","brown2", "olivedrab3", "purple2"), main=aux.main)
    abline(h=seq(1, ylim.sup, by=10), col="gray")
    bp=barplot(t(as.matrix(plot.tabla[1:3,1:(n-1)])),add=TRUE, yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=1.5 ), cex.names = 1.5, col = c("steelblue","brown2", "olivedrab3", "purple2"), main=aux.main)
    axis(2, at = seq(0, ylim.sup, by=20), las = 1, cex.axis=2, col = NA)
    text(bp, t(as.matrix(plot.tabla[1:3,1:(n-1)])), round(t(as.matrix(plot.tabla[1:3,1:(n-1)])), 2),font=2,cex=1,pos=3)
    #dev.print(pdf, 'Informe_3.13_ESTUDIO_POR_RATIO_DE_SINIESTRALIDAD.pdf' ,  height=10, width=10 )
    plot.tabla=round(plot.tabla, 2)
    #------------------------------------#
    
    obten_tabla(promedio_trabajadores)
    obten_tabla(porcentaje_trabajadores)
    obten_tabla(tabla_17_19_itinere)
    obten_tabla(porcentaje_acc_itinere)
    obten_tabla(empr_trab_accs)
    obten_tabla(ubic_trab_accs)
    obten_tabla(plot.tabla)
    
    #------------------------------------#
    
    
    return(list(promedio_trabajadores,porcentaje_trabajadores,tabla_17_19_itinere,porcentaje_acc_itinere, empr_trab_accs,ubic_trab_accs,plot.tabla))
  }#End_function_
  
  informe_ampliacion=function(df, años){
    
    #Conclusiones años 
    
    #1? Selecciono solo los poligonos y años 
    accidentes_en_poligonos=subset(df, !is.na(df$ubicacion))
    aux.marcador=is.element(accidentes_en_poligonos$year,años)
    accidentes_en_poligonos_17_19=subset(accidentes_en_poligonos,aux.marcador==TRUE)
    
    #2? Obtengo la media de trabajadores por empresa o el dato mÃ¡s reciente
    
    aux.acc=accidentes_en_poligonos_17_19 %>% group_by(CIF_EMPRESA)  %>% mutate(nª_trabajadores=mean(PLANTILLAE))
    aux.acc$nª_trabajadores=as.integer(aux.acc$nª_trabajadores)
    aux.acc=aux.acc %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_reciente=PLANTILLAE[which.max(FECHAACCIDENTE)])
    aux.acc$plantilla_reciente=as.integer(aux.acc$plantilla_reciente)
    aux.acc=aux.acc %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT=PLANTILLACT[which.max(FECHAACCIDENTE)])
    aux.acc$plantilla_ACT=as.integer(aux.acc$plantilla_ACT)
    
    # aux.acc=aux.acc  %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT2=PLANTILLACT[which.max(1:n())] )
    # aux.acc$plantilla_ACT2=as.integer(aux.acc$plantilla_ACT2)
    
    aux.acc=aux.acc%>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT_reciente=mean(PLANTILLACT))
    aux.acc$plantilla_ACT_reciente=as.integer(aux.acc$plantilla_ACT_reciente)
    
    #3? Elimino accidentes repetidos y autonomos
    
    accidentes_en_poligonos_17_19=(aux.acc[!duplicated(aux.acc$IPF_MD5,aux.acc$FECHAACCIDENTE),])
    accidentes_en_poligonos_17_19=subset(accidentes_en_poligonos_17_19, SITUACION==1|SITUACION==2)
    
    #4? Selecciono solo "in itinere"
    accidentes_en_poligonos_17_19_itinere=subset(accidentes_en_poligonos_17_19, itinere==TRUE)
    
    
    #------------------------------------#
    
    tabla_accidentes=table(accidentes_en_poligonos_17_19_itinere$year)
    
    #------------------------------------#
    
    #Elimino las empresas repetidas
    
    subset_2017_2019=subset(accidentes_en_poligonos_17_19)[!duplicated(accidentes_en_poligonos_17_19$CIF_EMPRESA),]
    ubicacion=subset_2017_2019$ubicacion
    tamañosubset_2017_2019=subset_2017_2019$tamaño
    tabla_2017_2019=tapply(subset_2017_2019$plantilla_ACT, list(tamañosubset_2017_2019, ubicacion), sum, na.rm = TRUE)
    tabla_2017_2019[is.na(tabla_2017_2019)]=0
    sum(subset_2017_2019$plantilla_ACT[subset_2017_2019$LUGAR==3|subset_2017_2019$LUGAR==2])
    #tabla_2017_2019=tapply(subset_2017_2019$plantilla_reciente, list(tamañosubset_2017_2019, ubicacion), sum)
    tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
    tabla_2017_2019$total=rowSums(tabla_2017_2019)
    aux.function=function(x){if(is.numeric(x)) y=sum(x) else y="Total";return(y)}
    tabla_2017_2019=tabla_2017_2019 %>% bind_rows(summarise_all(., ~aux.function(.x)))
    tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
    row.names(tabla_2017_2019)[4]="Total"
    tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
    tabla_2017_2019
    
    #------------------------------------#
    subset=accidentes_en_poligonos_17_19_itinere
    table(subset$year)
    
    tabla_17_19_itinere=table(subset$tamaño,subset$ubicacion)
    tabla_17_19_itinere=as.data.frame.matrix(tabla_17_19_itinere)
    tabla_17_19_itinere$total=rowSums(tabla_17_19_itinere)
    #tabla_17_19_itinere=tabla_17_19_itinere %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
    aux.function=function(x){if(is.numeric(x)) y=sum(x) else y="Total";return(y)}
    tabla_17_19_itinere=tabla_17_19_itinere %>% bind_rows(summarise_all(., ~aux.function(.x)))
    tabla_17_19_itinere=as.data.frame.matrix(tabla_17_19_itinere)
    row.names(tabla_17_19_itinere)[4]="Total"
    tabla_17_19_itinere=tabla_17_19_itinere[c(3,2,1,4),]
    #tabla_17_19_itinere
    
    #------------------------------------#
    n=length(levels(df$ubicacion))+1
    promedio_trabajadores=tabla_2017_2019[c(3,2,1,4),]
    promedio=as.numeric(round((promedio_trabajadores[4,1:n]/promedio_trabajadores[4,n]),2))
    promedio_trabajadores[5,]=label_percent()(promedio)
    #promedio_trabajadores
    #obten_tabla(promedio_trabajadores)
    
    aux.df=(promedio_trabajadores[1:3,1:n])
    aux.df= as.data.frame(sapply(aux.df, as.numeric))
    porcentaje_trabajadores=as.data.frame(round(prop.table(as.matrix(aux.df),2),2))
    porcentaje_trabajadores[4,]=colSums(porcentaje_trabajadores)
    porcentaje_trabajadores=sapply(porcentaje_trabajadores, function(x) percent(x, accuracy=1))
    # porcentaje_trabajadores
    # obten_tabla(porcentaje_trabajadores)
    
    # tabla_17_19_itinere
    # obten_tabla(tabla_17_19_itinere)
    
    aux.df=(tabla_17_19_itinere[1:3,1:(n-1)])
    aux.df= as.data.frame(sapply(aux.df, as.numeric))
    porcentaje_acc_itinere=as.data.frame(round(prop.table(as.matrix(aux.df)),2))
    porcentaje_acc_itinere$total=rowSums(porcentaje_acc_itinere)
    porcentaje_acc_itinere[4,]=colSums(porcentaje_acc_itinere)
    porcentaje_acc_itinere=sapply(porcentaje_acc_itinere, function(x) percent(x, accuracy=1))
    # porcentaje_acc_itinere
    # obten_tabla(porcentaje_acc_itinere)
    
    #------------------------------------#
    
    empr_trab_accs=as.data.frame(as.numeric(t(table(subset_2017_2019$tamaño))))
    row.names(empr_trab_accs)=names((table(subset_2017_2019$tamaño)))
    colnames(empr_trab_accs)=c("Nª_de_empresas")
    empr_trab_accs["Total" ,] = colSums(empr_trab_accs)
    
    porcentaje=round((empr_trab_accs[1:4,1]/empr_trab_accs[4,1]), 2)
    empr_trab_accs$trabajadores_nª_Eªs=percent(porcentaje)
    
    tabla_aux=tabla_2017_2019[c(3,2,1,4),]
    empr_trab_accs$Nª_de_trabajadores=tabla_aux[1:4,5]
    porcentaje=round((empr_trab_accs[1:4,3]/empr_trab_accs[4,3]), 2)
    empr_trab_accs$trabajadores=percent(porcentaje)
    
    tabla_aux=tabla_17_19_itinere[c(3,2,1,4),]
    empr_trab_accs$Nª_de_accidentes=tabla_aux[1:4,5]
    porcentaje=round((empr_trab_accs[1:4,5]/empr_trab_accs[4,5]), 2)
    empr_trab_accs$accidentes=percent(porcentaje)
    
    A=round((empr_trab_accs[1:4,3]/empr_trab_accs[4,3]), 2)
    B=round((empr_trab_accs[1:4,5]/empr_trab_accs[4,5]), 2)
    C=A-B
    empr_trab_accs$accidentes_trabajadores=percent(C)
    
    empr_trab_accs[c(3,2,1,4),]
    obten_tabla(empr_trab_accs[c(3,2,1,4),])
    
    Trabajadores=round((empr_trab_accs[1:4,3]/empr_trab_accs[4,3]), 2)*100
    Accidentes=round((empr_trab_accs[1:4,5]/empr_trab_accs[4,5]), 2)*100
    aux.dat=data.frame(Trabajadores, Accidentes)
    row.names(aux.dat)=row.names(empr_trab_accs)
    aux.dat=as.matrix(aux.dat)
    aux.dat=aux.dat[c(3,2,1,4),]
    
    ylim.sup=max(aux.dat[1:3,1:2],na.rm = TRUE)*1.2
    aux.ylim=c(0,ylim.sup)
    bp=barplot(t(aux.dat[1:3,1:2]), beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=2.5, inset = c(- 0.05, 0)), cex.names = 2.5,cex.axis=2.5, col = c("steelblue","brown2") )
    #dev.print(pdf, 'Informe_3.11_ESTUDIO_DE_LA_ACCIDENTABILIDAD_POR_TAMAÑO.pdf' ,  height=10, width=10 )
    
    #------------------------------------#
    
    ubic_trab_accs=as.data.frame(as.numeric(t(table(subset_2017_2019$ubicacion))))
    row.names(ubic_trab_accs)=names((table(subset_2017_2019$ubicacion)))
    colnames(ubic_trab_accs)=c("Nª_de_empresas")
    ubic_trab_accs["Total" ,] = colSums(ubic_trab_accs)
    
    porcentaje=round((ubic_trab_accs[1:n,1]/ubic_trab_accs[n,1]), 2)
    ubic_trab_accs$trabajadores_nª_Eªs=percent(porcentaje)
    
    
    ubic_trab_accs$Nª_de_trabajadores=as.numeric((t(tabla_2017_2019))[1:n,4])
    porcentaje=round((ubic_trab_accs[1:n,3]/ubic_trab_accs[n,3]), 2)
    ubic_trab_accs$trabajadores=percent(porcentaje)
    
    ubic_trab_accs$Nª_de_accidentes=as.numeric((t(tabla_17_19_itinere))[1:n,4])
    porcentaje=round((ubic_trab_accs[1:n,5]/ubic_trab_accs[n,5]), 2)
    ubic_trab_accs$accidentes=percent(porcentaje)
    
    A=round((ubic_trab_accs[1:n,3]/ubic_trab_accs[n,3]), 2)
    B=round((ubic_trab_accs[1:n,5]/ubic_trab_accs[n,5]), 2)
    C=A-B
    ubic_trab_accs$accidentes_trabajadores=percent(C)
    
    # ubic_trab_accs
    # obten_tabla(ubic_trab_accs)
    
    Trabajadores=round((ubic_trab_accs[1:n,3]/ubic_trab_accs[n,3]), 2)*100
    Accidentes=round((ubic_trab_accs[1:n,5]/ubic_trab_accs[n,5]), 2)*100
    aux.dat=data.frame(Trabajadores, Accidentes)
    row.names(aux.dat)=row.names(ubic_trab_accs)
    aux.dat=as.matrix(aux.dat)
    
    ylim.sup=max(aux.dat[1:(n-1),1:2],na.rm = TRUE)*1.2
    aux.ylim=c(0,ylim.sup)
    bp=barplot(t(aux.dat[1:(n-1),1:2]), beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=2.5, inset = c(- 0.05, 0)),cex.names = 2,cex.axis=2.5, col = c("steelblue","brown2") )
    #dev.print(pdf, 'Informe_3.12_ESTUDIO_DE_ACCIDENTABILIDAD_POR_UBICACIÓN.pdf' ,  height=10, width=10 )
    
    #------------------------------------#
    
    aux.dat=accidentes_en_poligonos_17_19_itinere
    aux.dat=(aux.dat[!duplicated(aux.dat$IPF_MD5,aux.dat$FECHAACCIDENTE),])
    aux.dat=aux.dat[,c(19,87,112,113,114,115,116,117,106)]
    
    #aux.dat=aux.dat %>% group_by(CIF_EMPRESA) %>% mutate(acc_empr=sum(accidentes))
    aux.dat=aux.dat %>% group_by(CIF_EMPRESA) %>% mutate(acc_empr=sum(itinere))
    aux.dat=subset(aux.dat)[!duplicated(aux.dat$CIF_EMPRESA),]
    aux.dat$promedio=aux.dat$acc_empr/aux.dat$nª_trabajadores*1000/3
    
    ubicacion=aux.dat$ubicacion
    tamaño=aux.dat$tamaño
    promedio=aux.dat$promedio
    subtabla=tapply(promedio, list(tamaño, ubicacion), sum)
    tabla=subtabla/table(tamaño, ubicacion)
    tabla=as.data.frame.matrix(tabla)
    tabla$total= rowSums(subtabla) / table(tamaño)
    
    library(plyr)
    total= (colSums(subtabla) / table(ubicacion))
    total=data.frame(rbind(total))
    tabla = rbind.fill(tabla, total)
    tabla[4,n]=sum(subtabla)/ sum(table(ubicacion))
    detach("package:plyr", unload = TRUE)
    
    rownames(tabla)=rownames(tabla_2017_2019[c(3,2,1,4),])
    aux.tabla.plot1=tabla
    
    tabla=tabla[c(3,2,1,4),]
    tabla=as.data.frame.matrix(round(tabla ,2 ))
    tabla[] = lapply(tabla, paste0, " \211")
    tabla
    
    tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
    df=( (tabla_17_19_itinere[1:4,1:n]/3) / (tabla_2017_2019[1:4,1:n]) )*1000
    df=as.data.frame(round(df ,2 ))
    aux.tabla.plot2=df
    df[] = lapply(df, paste0, " \211")
    df
    
    plot.tabla=aux.tabla.plot1[c(3,2,1,4),]
    ylim.sup=max(plot.tabla,na.rm = TRUE)*1.2
    aux.ylim=c(0,ylim.sup)
    bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])), yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=1.5 ), cex.names = 2, col = c("steelblue","brown2", "olivedrab3", "purple2"))
    abline(h=seq(1, ylim.sup, by=10), col="gray")
    bp=barplot(t(as.matrix(plot.tabla[1:3,1:(n-1)])),add=TRUE, yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=1.5 ), cex.names = 2, col = c("steelblue","brown2", "olivedrab3", "purple2"))
    axis(2, at = seq(0, ylim.sup, by=20), las = 1, cex.axis=2, col = NA)
    text(bp, t(as.matrix(plot.tabla[1:3,1:(n-1)])), round(t(as.matrix(plot.tabla[1:3,1:(n-1)])), 2),font=2,cex=1.5,pos=3)
    #dev.print(pdf, 'Informe_3.13_ESTUDIO_POR_RATIO_DE_SINIESTRALIDAD.pdf' ,  height=10, width=10 )
    plot.tabla=round(plot.tabla, 2)
    #------------------------------------#
    
    obten_tabla(promedio_trabajadores)
    obten_tabla(porcentaje_trabajadores)
    obten_tabla(tabla_17_19_itinere)
    obten_tabla(porcentaje_acc_itinere)
    obten_tabla(empr_trab_accs)
    obten_tabla(ubic_trab_accs)
    obten_tabla(plot.tabla)
    
    #------------------------------------#
    
    
    return(list(promedio_trabajadores,porcentaje_trabajadores,tabla_17_19_itinere,porcentaje_acc_itinere, empr_trab_accs,ubic_trab_accs,plot.tabla))
  }#End_function_
  
  
  
  
  
  sexo_edad=function(aux.df,aux.df2,aux.lang=NULL){
    lang=aux.lang
    if (is.null(lang)){lang="ESP"}  
    if(lang=="ESP"){aux.text="ALT por EDAD"
    aux.text2="ALT por SEXO"
    aux.text3="ALT por SEXO y EDAD"
    aux.text4=c("H", "M")
    aux.text5="Afiliados Seguridad Social por SEXO"
    aux.text6="ALT por SEXO por cada 1000 trabajadores"}
    if(lang=="ENG"){aux.text="WTA by AGE"      
    aux.text2="WTA by GENDER"
    aux.text3="WTA by SEX and AGE"
    aux.text4=c("M", "W")
    aux.text5="Social Security Affiliates by SEX"
    aux.text6="ALT by SEX for every 1000 workers"}
    
    summary(aux.df$edad)
    summary(aux.df$SEXO)
    
    table(aux.df$edad)
    aux.table= table(aux.df$edad)
    aux.table= table(aux.df$edad_5)
    y_max = 1.2*max(aux.table)
    bp=barplot(aux.table,  ylim =c(0,y_max),main=aux.text ,cex.main=2,col=aux.colours[1])
    aux.exp=length(digits( y_max/10 ))-1
    axis(2,at=seq(0,round(y_max/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    axis(4,at=max(aux.table),col.ticks =2,col.axis  =2)
    abline(h=max(aux.table), col=2, lty=2, lwd=2)
    
    table(aux.df$SEXO)
    aux.table= table(aux.df$SEXO)
    y_max = 1.2*max(aux.table)
    bp=barplot(aux.table,  ylim =c(0,y_max),main=aux.text2 ,cex.main=2,col=c("steelblue","brown2"),legend = aux.text4,args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)))
    aux.exp=length(digits( y_max/10 ))-1
    axis(2,at=seq(0,round(y_max/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    text(bp, as.matrix(aux.table), round(as.matrix(aux.table), 1),cex=2,pos=3)
    
    H= sum(seg_social_total[c("h")])
    M= sum(seg_social_total[c("m")])
    aux.table=(as.matrix(data.frame(H,M)))
    y_max = 1.2*max(aux.table)
    bp=barplot(aux.table[1,],  ylim =c(0,y_max),main=aux.text5 ,cex.main=2,col=c("steelblue","brown2"),legend = aux.text4,args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)))
    aux.exp=length(digits( y_max/10 ))-1
    axis(2,at=seq(0,round(y_max/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    text(bp, as.matrix(aux.table), round(as.matrix(aux.table), 1),cex=2,pos=3)
    
    aux.table=table(aux.df$SEXO)/aux.table[1,]*1000
    y_max = 1.2*max(aux.table)
    bp=barplot(aux.table,  ylim =c(0,y_max),main=aux.text6 ,cex.main=2,col=c("steelblue","brown2"),legend = aux.text4,args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)))
    aux.exp=length(digits( y_max/10 ))-1
    axis(2,at=seq(0,round(y_max/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    text(bp, as.matrix(aux.table), round(as.matrix(aux.table), 2),cex=2,pos=3)
    
    aux.table=table(aux.df$SEXO,factor(aux.df$edad,levels=c(0:100)))
    y_max = 1.2*max(aux.table)
    plot(aux.table[1,], ylim =c(0,y_max),main=aux.text2, type = "l",xlab="",ylab="" ,cex.main=2,lwd=2,col=c("steelblue"))
    lines(aux.table[2,], ylim =c(0,y_max),main=aux.text2 ,cex.main=2,lwd=2,col=c("brown2"))
    legend ("topright",legend = aux.text4, lty=1, lwd=2.5, col=c("steelblue","brown2"), cex=1.5,box.lty=0,bty = "n",bg=0, inset=c(0.01,0.005))
    aux.exp=length(digits( y_max/10 ))-1
    axis(2,at=seq(0,round(y_max/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    axis(4,at=c(max(aux.table[1,]),max(aux.table[2,])),col.ticks =2,col.axis  =2)
    abline(h=c(max(aux.table[1,]),max(aux.table[2,])), col=2, lty=2, lwd=2)
    
    aux.table=table(aux.df$SEXO,aux.df$edad_5)
    y_max = 1.2*max(aux.table)
    bp=barplot(aux.table, beside=TRUE, ylim =c(0,y_max),main=aux.text3 ,cex.main=2,col=c("steelblue","brown2"),legend = aux.text4,args.legend = list(bty = "n", x = "right", cex=1.5, inset = c(- 0.05, 0)))
    aux.exp=length(digits( y_max/10 ))-1
    axis(2,at=seq(0,round(y_max/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    axis(4,at=c(max(aux.table[1,]),max(aux.table[2,])),col.ticks =2,col.axis  =2)
    abline(h=c(max(aux.table[1,]),max(aux.table[2,])), col=2, lty=2, lwd=2)
    
    aux.table1=table(aux.df$SEXO,aux.df$edad_5)
    inicio=head(which(aux.table1!=0,arr.ind = T),1)[2]
    aux.levels=length(levels(seg_social_tabla$edad))
    aux.table1=aux.table1[,c(inicio:(inicio+aux.levels))]
    aux.table1[,aux.levels]=rowSums(aux.table1[,c(aux.levels:ncol(aux.table1))])
    aux.table1=aux.table1[,1:aux.levels]
    
    aux.table2=rbind(seg_social_edad$h,seg_social_edad$m)
    colnames(aux.table2)=rownames(seg_social_edad)
    
    aux.table=round(aux.table1/aux.table2*1000,2)
    y_max = 1.2*max(aux.table)
    bp=barplot(aux.table, beside=TRUE, ylim =c(0,y_max),main=aux.text3 ,cex.main=2,col=c("steelblue","brown2"),legend = aux.text4,args.legend = list(bty = "n", x = "right", cex=1.5, inset = c(- 0.05, 0)))
    aux.exp=length(digits( y_max/10 ))-1
    axis(2,at=seq(0,round(y_max/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    axis(4,at=c(max(aux.table[1,]),max(aux.table[2,])),col.ticks =2,col.axis  =2)
    abline(h=c(max(aux.table[1,]),max(aux.table[2,])), col=2, lty=2, lwd=2)
    
    
    
    #args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0))
    
  }#End_function_
  
  ALT_año=function(aux.df, lang=NULL){

    if (is.null(lang)){lang="ESP"}
    if(lang=="ESP"){aux.text="ALT"
    aux.text1="ALT"}
    if(lang=="ENG"){aux.text="WTA"
    aux.text1="ALT"}
    
    length(aux.df$y)
    table(aux.df$y)
    aux.table= table(aux.df$y)
    summary(aux.df$y)
    y_max = 1.2*max(aux.table)
    bp=barplot(aux.table,  ylim =c(0,y_max), main=aux.text ,cex.main=2,col="steelblue")
    aux.exp=length(digits( y_max/10 ))-1
    axis(2,at=seq(0,round(y_max/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    text(bp, as.matrix(aux.table), round(as.matrix(aux.table), 1),cex=2,pos=3)
    
    aux.table=table(aux.df$itinere.key)
    y_max = 1.2*max(aux.table)
    aux.nvar=length(table(aux.time))
    bp=barplot(aux.table,  ylim =c(0,y_max), main=aux.text1 ,cex.main=2,col=aux.colours[1:(aux.nvar)])
    aux.exp=length(digits( y_max/10 ))
    axis(2,at=seq(0,round(y_max/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    text(bp, as.matrix(aux.table), round(as.matrix(aux.table), 1),cex=2,pos=3)
    
    
    
  }#End_function_
  
  grado=function(aux.df, lang=NULL){      
    
    if (is.null(lang)){lang="ESP"}
    if(lang=="ESP"){aux.text="GRADO"}
    if(lang=="ENG"){aux.text="SEVERITY"}
    
    aux.class=table(aux.df$GRADO)
    obten_tabla(aux.class)
    tabla_grado_mes=table(aux.df$GRADO,aux.df$month)
    obten_tabla(tabla_grado_mes)
    ylim.sup=max(tabla_grado_mes)*1.2
    aux.ylim=c(0,ylim.sup)
    aux.nvar=length(table(aux.class))
    barplot(tabla_grado_mes,beside=TRUE, ylim=aux.ylim,legend = TRUE, main=aux.text,cex.main=2, args.legend = list(bty = "n", x = "top",horiz=TRUE),col=aux.colours[1:(aux.nvar)])
    aux.exp=length(digits( ylim.sup/10 ))-1
    axis(2,at=seq(0,round(ylim.sup/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    
    ylim.sup=max(tabla_grado_mes[1,])*1.2
    aux.ylim=c(0,ylim.sup)
    barplot(tabla_grado_mes[1,],beside=TRUE, ylim=aux.ylim,legend = "1",main=aux.text,cex.main=2, args.legend = list(bty = "n", x = "top",horiz=TRUE),col=aux.colours[1])
    aux.exp=length(digits( ylim.sup/10 ))-1
    axis(2,at=seq(0,round(ylim.sup/10^aux.exp,0)*10^aux.exp,by=10^(aux.exp) ))
    
    ylim.sup=max(tabla_grado_mes[-1,])*1.2
    aux.ylim=c(0,ylim.sup)
    barplot(tabla_grado_mes[-1,],beside=TRUE, ylim=aux.ylim,legend = TRUE, main=aux.text,cex.main=2, args.legend = list(bty = "n", x = "top",horiz=TRUE),col=aux.colours[2:(aux.nvar)])
    aux.exp=length(digits( ylim.sup/10 ))-1
    axis(2,at=seq(0,round(ylim.sup/10^aux.exp,0)*10^aux.exp,by=10^(aux.exp-1) ))
    
    
    
  }#End_function_
  
  box_plot_temporal=function(aux.df,aux.time,aux.class,aux.lang=NULL){
    
    lang=aux.lang
    if (is.null(lang)){lang="ESP"}  
    if(lang=="ESP"){aux.text="ALT by MONTH"}
    if(lang=="ENG"){aux.text="ALT por MES"}
    
    
    aux.table=table(aux.class,aux.time)
    
    aux.boxplot=(as.data.frame.matrix(aux.table))
    ylim.sup=max(aux.table)*1.2
    ylim.inf=min(aux.table)-max(aux.table)*0.2
    
    aux.ylim=c(ylim.inf,ylim.sup)
    aux.nvar=length(table(aux.time))
    aux.exp=length(digits( ylim.sup/10 ))-1
    
    aux.table2=table(aux.class,aux.time)
    ylim.sup=max(aux.table2)*1.2
    aux.ylim=c(0,ylim.sup)
    
    if (length(levels(aux.class))>5){aux.ncol=ceiling(length(levels(aux.class))/10)
    }else {aux.ncol=1}
    
    boxplot(aux.boxplot, ylim=aux.ylim,legend = TRUE, main=aux.text,cex.main=2,col=aux.colours[1:(aux.nvar)])
    legend.text=names(table(aux.class))
    legend("topright", ncol=aux.ncol, legend.text, border="black", fill = aux.colours[1:(aux.nvar)])
    axis(2,at=seq(0,round(ylim.sup/10^aux.exp,0)*10^aux.exp,by=10^aux.exp ),labels=NA)
    
  }
  
  descrip_categorica=function(aux.df,aux.time,aux.class.categoricas,lang=NULL){
    for (i in 1:length(aux.class.categoricas)) {
      aux.name_classif=aux.class.categoricas[i]
      #print(aux.name_classif)
      aux.descrip1=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="mean")
      if (is.null(lang)){lang="ESP"}
      if(lang=="ESP"){aux.text="MEDIA, DESVIACIÓN ESTÁNDAR Y QUANTILE 0,9"}
      if(lang=="ENG"){aux.text="MEAN,  STANDAR DEVIATION AND QUANTILE 0,9"}
      
      print(aux.text)
      obten_tabla(t(aux.descrip1))
      
      aux.descrip2=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="sd")
      obten_tabla(t(aux.descrip2))
      
      aux.descrip3=func.discrete.descrip.quantile(aux.df, aux.time, aux.name_classif, FUN="quantile")
      obten_tabla(t(aux.descrip3))
    }
  }#End_function_
  
  acierto_lluvia_mL=function(datos_reales,datos_extraidos,aux.meteo,decalaje=NULL,mL_lluvia,aux.ROC=NULL){
    if (is.null(decalaje)){decalaje=0}
    
    
    # table(bases4$total)
    # bases4$total_max=bases4$total
    #datos_reales$total_max[datos_reales$total_max>mL_lluvia]=mL_lluvia
    # table(datos_reales$total_max>0)
    # table(datos_reales$total_max)
    #datos_reales=datos_reales %>%  relocate(total_max, .after = total)
    
    #Se modifica la base datos que para cada hora-dia-año recoge si ha habido o no accidente y extrae textualmente si en el momento del accidente estaba lloviendo.
    datos_join=full_join(datos_extraidos,datos_reales,by="aux.yday.hour")
    datos_join=datos_join %>% rename( year = year.x, hour= hour.x)
    
    #datos_join = datos_join %>% filter(fecha>= datos_reales$fecha[1])
    datos_join = subset(datos_join, datos_join$fecha >= datos_reales$fecha[1])
    
    #lluvia=datos_join[,c("y","lluvia","lluvia_mue","lluvia_lote","lluvia_zgz","lluvia_galle","total")]
    lluvia=datos_join[,c("y",aux.meteo,colnames(datos_reales[c(1:which(colnames(datos_reales)=="total"))]))]
    
    #lluvia_1_0 =1*(as.data.frame(lluvia[] != 0)) #El multiplicar por 1 pasa de TRUE/FALSE A 1/0
    lluvia_1_0 =as.data.frame(lluvia)
    lluvia_1_0$hour=datos_join$hour
    lluvia_1_0$aux.yday.hour=datos_join$aux.yday.hour
    lluvia_1_0$fecha=datos_join$fecha
    lluvia_1_0=subset(lluvia_1_0, !is.na(fecha))
    lluvia_1_0[is.na(lluvia_1_0)] = 0
    dim(lluvia_1_0)
    
    lluvia_1_0$fecha_hora=as.POSIXct(paste(lluvia_1_0$fecha, lluvia_1_0$hour), format="%Y-%m-%d %H")
    
   #lluvia.decalada=which(lluvia_1_0$total > mL_lluvia)+decalaje
    aux.decalaje=decalaje
    aux.lluvia.decalada= c()   
    for (i in 1:aux.decalaje) {
      out.lluvia.decalada_antes=which(lluvia_1_0$total > mL_lluvia)-(i)
      out.lluvia.decalada_despues=which(lluvia_1_0$total > mL_lluvia)+(i)
      aux.lluvia.decalada=c(aux.lluvia.decalada,out.lluvia.decalada_antes,out.lluvia.decalada_despues)
    }
    
    lluvia.decalada=c(aux.lluvia.decalada,which(lluvia_1_0$total > mL_lluvia))
    lluvia.decalada=sort(unique(lluvia.decalada), decreasing=FALSE)
    aux.p=which(colnames(lluvia_1_0) == "total")  
    
    lluvia.decalada=lluvia.decalada[lluvia.decalada<=length(lluvia_1_0$total)]
    lluvia_1_0$lluvia.decalada=lluvia_1_0$total
    lluvia_1_0$lluvia.decalada[lluvia.decalada]=1
    lluvia_1_0$lluvia.decalada[is.na(lluvia_1_0$lluvia.decalada)]=0
    
    # aux.marcador=(lluvia.decalada<=length(lluvia_1_0$total))
    # lluvia_1_0_decalaje=subset(lluvia_1_0, aux.marcador==TRUE)
    # lluvia_1_0_fuera_rango=subset(lluvia_1_0, aux.marcador==FALSE)
    # 
    # lluvia_1_0_decalaje$total=lluvia_1_0_decalaje$total[aux.marcador==TRUE]+(lluvia_1_0_decalaje$total[aux.marcador==TRUE]==0)*1
    # 
    #lluvia_1_0$total[lluvia.decalada]=case_when ((lluvia.decalada <= length(lluvia_1_0$total) ) ~ ( lluvia_1_0$total[lluvia.decalada]+(lluvia_1_0$total[lluvia.decalada]==0)*1)  )
    # ,(lluvia.decalada > length(lluvia_1_0$total))   ~ ( lluvia_1_0$total[length(lluvia_1_0$total)]+(lluvia_1_0$total[length(lluvia_1_0$total)]==0)*1 )  )
     
    #case_when((lluvia.decalada<=length(lluvia_1_0$total) )  ~( lluvia_1_0$total[lluvia.decalada]+(lluvia_1_0$total[lluvia.decalada]==0)*1)  )
    

    
    
    # lluvia.decalada=which(lluvia_1_0$total >mL_lluvia)+decalaje #Posiciones de la lluvia de las bases
    # aux.p=which(colnames(lluvia_1_0) == "total")
    # hora.decalada=lluvia_1_0[lluvia.decalada,aux.p]
    # #(lluvia_1_0[lluvia.decalada,aux.p])*1
    # lluvia_1_0$total[lluvia.decalada]=lluvia_1_0$total[lluvia.decalada]+(lluvia_1_0$total[lluvia.decalada]==0)*1
    
    #lluvia_bases=1*(lluvia_1_0$total>mL_lluvia) #Recoge el total de lluvia medido por las bases
    lluvia_bases=lluvia_1_0$lluvia.decalada #Recoge el total de lluvia medido por las bases
    lluvia_respuesta=lluvia_1_0[[aux.meteo]] #Recoge la lluvia en el momento del accidente
    y=lluvia_1_0$y
    df=data.frame(lluvia_bases,lluvia_respuesta,y)
    df=df[df$y>0,]
    df=df[,-3]  
    aux.tabla.lluvia=addmargins(table(df))
    
    VN=aux.tabla.lluvia[1,1] #verdadero_negativo
    FN=aux.tabla.lluvia[2,1] #falso_negativo
    FP=aux.tabla.lluvia[1,2] #falso_positivo
    VP=aux.tabla.lluvia[2,2] #verdadero_positivo
    
    aciertos=VN+VP
    fallos=FN+FP
    Sensibilidad=VP/(VP+FN)
    Especificidad=VN/(VN+FP)
    
    porcentaje_acierto=aciertos/(aciertos+fallos)*100
    porcentaje_acierto_lluvia=aux.tabla.lluvia[2,2]/(aux.tabla.lluvia[2,2]+aux.tabla.lluvia[1,2])*100
    
    if (is.null(aux.ROC)){aux.ROC=FALSE}
    
    #Figura ROC
    if (aux.ROC==TRUE){
      prediobj = prediction(as.numeric(lluvia_bases),as.numeric(lluvia_respuesta))
      perf =  performance(prediobj, "tpr","fpr")
      plot(perf,main = "Curva ROC",xlab="Tasa de falsos positivos", ylab="Tasa de verdaderos positivos")
      abline(a=0,b=1,col="blue",lty=2)
      grid()
      auc = as.numeric(performance(prediobj,"auc")@y.values)
      legend("bottomright",legend=paste(" AUC =",round(auc,4)))
    }
    else{}
    
    Clasificacion=c("VN","FN","FP","VP")
    Variable=c("Acierto_global","Acierto_llueve","Sensibilidad","Especificidad")
    b1=c(VN,FN,FP,VP)
    b2=c(porcentaje_acierto,porcentaje_acierto_lluvia,Sensibilidad,Especificidad )
    
    tabla_aciertos1=as.data.frame(Clasificacion)
    tabla_aciertos1$Valor=b1
    tabla_aciertos2=as.data.frame(Variable)
    tabla_aciertos2$Valor=b2  
    
    return(list(aux.tabla.lluvia,tabla_aciertos1,tabla_aciertos2,lluvia_1_0))
  }#End_function_
  
  info_lluvia_mL=function(datos_reales,datos_extraidos, aux.meteo,decalaje=NULL,mL_lluvia){
    if (is.null(decalaje)){decalaje=0}
    
    
    datos_join=full_join(datos_extraidos,datos_reales,by="aux.yday.hour")
    datos_join=datos_join %>% rename( year = year.x, hour= hour.x)
    datos_join = subset(datos_join, datos_join$fecha >= datos_reales$fecha[1])
    lluvia=datos_join[,c("y",aux.meteo,colnames(datos_reales[c(1:which(colnames(datos_reales)=="total"))]))]
    
    lluvia_1_0 =as.data.frame(lluvia)
    lluvia_1_0$hour=datos_join$hour.x
    lluvia_1_0$aux.yday.hour=datos_join$aux.yday.hour
    lluvia_1_0$fecha=datos_join$fecha
    lluvia_1_0=subset(lluvia_1_0, !is.na(fecha))
    lluvia_1_0[is.na(lluvia_1_0)] = 0
    dim(lluvia_1_0)
    
    aux.decalaje=decalaje
    aux.lluvia.decalada= c()
    for (i in 1:aux.decalaje) {
      out.lluvia.decalada=which(lluvia_1_0$total > mL_lluvia)+(i)
      aux.lluvia.decalada=c(aux.lluvia.decalada,out.lluvia.decalada)
    }
    lluvia.decalada=unique(aux.lluvia.decalada)
    aux.p=which(colnames(lluvia_1_0) == "total")  
    lluvia_1_0$total[lluvia.decalada]=case_when (
      (lluvia.decalada <= length(lluvia_1_0$total))   ~ ( lluvia_1_0$total[lluvia.decalada]+(lluvia_1_0$total[lluvia.decalada]==0)*1),
      (lluvia.decalada >= length(lluvia_1_0$total))   ~ ( lluvia_1_0$total[length(lluvia_1_0$total)]+(lluvia_1_0$total[length(lluvia_1_0$total)]==0)*1 )  )
    
    lluvia_1_0$lluvia_1_0=1*(lluvia_1_0$total>mL_lluvia) #Recoge el total de lluvia medido por las bases
    
    nº_accs=as.vector(table(lluvia_1_0$lluvia_1_0,lluvia_1_0$y>0)[, "TRUE"]) 
    nº_hours=as.vector(table(lluvia_1_0$lluvia_1_0))
    aux.df=data.frame(nº_accs,nº_hours)
    aux.df[3,]=colSums(aux.df)
    rownames(aux.df)[rownames(aux.df)=="3"]="Total"
    aux.df$tasa=round(aux.df$nº_accs/aux.df$nº_hours,4)
    
    return(aux.df)
  }#End_function_
  
  auto_model=function(aux.df,aux.y,aux.modelo_base, model.name, aux.variable=NULL,aux.p_valor=NULL){
    
    #Función de retirada automática de variables hasta obtener un modelo con solo variables significativas
    
    if (is.null(aux.p_valor)){aux.p_valor=0.001}         #Establece umbral de aceptación según p-valor

    model.df=data.frame(matrix(ncol = 4))                #Dataframe de los modelos
    colnames(model.df)=c("model.name", "model.formula", "model.deviance", "model.AIC")
    model_list=list(aux.modelo_base)                         #Lista de los modelos
    
    aux.names=as.data.frame(summary(aux.modelo_base)$coefficients)
    lista=rownames(aux.names)
    new_str=gsub(":","*",lista[2:length(lista)])
    new_str = gsub('TRUE',"",new_str)
    aux.formula=paste("(",new_str, ")",collapse = "+")
    
    aux.model_name=paste("modelo_base")                  #Nombra el nuevo modelo y númera
    #Datos del modelo base
    model.df[1,]$model.name=aux.model_name
    model.df[1,]$model.formula=as.character(aux.formula)
    model.df[1,]$model.deviance=round((aux.modelo_base)$deviance,0)
    model.df[1,]$model.AIC=round((aux.modelo_base)$aic,0)
    
    aux.model=aux.modelo_base                                #Entra el modelo base
    p_valor=summary(aux.modelo_base)$coefficients[,4]        #P-valor del modelo base
    
    i=2
    
    if (is.null(aux.variable) ){
    } else {
      aux.model.base=aux.modelo_base
      aux.model_name=paste(model.name,"",(i-1),sep="")       #Nombra el nuevo modelo y númera
      aux.model=adicion_modelo(aux.df,aux.y,aux.model.base,aux.variable,aux.p_valor)
      model.df[i,]$model.name=aux.model_name
      model.df[i,]$model.formula=as.character(((aux.model)$formula)[3])
      model.df[i,]$model.deviance=round((aux.model)$deviance,0)
      model.df[i,]$model.AIC=round((aux.model)$aic,0)
      
      model_list=c(model_list,list(aux.model))           #Guarda el modelo el la lista de los modelos
      
      i=i+1                                              #Continua con la numeración
      print(i)}
    
    p_valor=summary(aux.model)$coefficients[,4]        #P-valor del nuevo modelo

    while( all(p_valor<aux.p_valor)==FALSE   ){          #Mientras que P-valor<aux.p_valor del anterior modelo
      aux.model_name=paste(model.name,"",(i-1),sep="")   #Nombra el nuevo modelo y númera
      aux.model=crear_modelo(aux.df,aux.y,aux.model,aux.p_valor)  #Crea el nuevo modelo
      #Datos del nuevo modelo
      model.df[i,]$model.name=aux.model_name
      model.df[i,]$model.formula=as.character(((aux.model)$formula)[3])
      model.df[i,]$model.deviance=round((aux.model)$deviance,0)
      model.df[i,]$model.AIC=round((aux.model)$aic,0)
      
      model_list=c(model_list,list(aux.model))           #Guarda el modelo el la lista de los modelos
      
      p_valor=summary(aux.model)$coefficients[,4]        #P-valor del nuevo modelo
      # aux.terms=as.data.frame(summary(aux.model)$coefficients)
      # lista=(aux.terms %>% filter(aux.terms[,4]<aux.p_valor) %>% rownames())
      aux.except=terminos_inter(aux.model,aux.p_valor)
 
      p_valor=p_valor[ !names(p_valor)%in% aux.except[,1]]
      i=i+1                                              #Continua con la numeración
      print(i)
      
    }
    
    row.names(model.df)=c(1:(i-1))
    
    return(list(model.df ,model_list))                  #Devuelve el dataframe y la lista con los modelos creados
  }
  
  mejor_modelo=function(my_list,modelo.names, aux.delta_AIC=NULL){
    
    #Función de selección de mejor modelo de una lista, estableciendo como límite prreestablecido delta_aic +/-5% y menor cantidad de terminos
    
    if (is.null(aux.delta_AIC)){aux.delta_AIC=5}        #Establece umbral de aceptación según incremento de AIC en %
    my_list=aux.model.list
    modelo.names=modelo.names_list
    model.df=data.frame(matrix(ncol = 5, nrow = length(modelo.names)))
    colnames(model.df)=c("model.name", "model.formula", "model.deviance", "model.AIC", "model.delta_AIC")
    
    aux.modelo_base=my_list[[1]]
    aux.names=as.data.frame(summary(aux.modelo_base)$coefficients)
    lista=rownames(aux.names)
    new_str=gsub(":","*",lista[2:length(lista)])
    new_str = gsub('TRUE',"",new_str)
    aux.formula=paste("(",new_str, ")",collapse = "+")
    
    model.df[1,]$model.name=modelo.names[1]
    model.df[1,]$model.formula=as.character(aux.formula)
    model.df[1,]$model.deviance=round((aux.modelo_base)$deviance,0)
    model.df[1,]$model.AIC=round((aux.modelo_base)$aic,0)
    
    for(i in 2:length(my_list)) { 
      Modelo=my_list[[i]]
      model.df[i,]$model.name=modelo.names[i]
      #model.df[i,]$model.formula=as.character(((Modelo)$formula)[3])
      aux.f=(Modelo$formula)
      terms=attr(terms.formula(aux.f), "term.labels")
      model.df[i,]$model.formula=paste(terms, collapse="+")
      model.df[i,]$model.deviance=round((Modelo)$deviance,0)
      model.df[i,]$model.AIC=round((Modelo)$aic,0)
    }
    
    for(i in 1:length(my_list)) { 
      delta_AIC=((model.df[i,]$model.AIC-model.df[1,]$model.AIC)/ model.df[1,]$model.AIC)*100   
      model.df[i,]$model.delta_AIC=round(delta_AIC,2)
    }
    
    #aux.longitud_formula=nchar(model.df[2:3,]$model.formula)
    aux.variables=(model.df[1:i,]$model.formula)
    n_variables=str_count(aux.variables, pattern = "\\+")
    model.df$n_variables=n_variables
    
    #aux.marcador=(  (model.df$model.delta_AIC<=5 ) & ( nchar(model.df$model.formula)==min(aux.longitud_formula) )  )
    aux.marcador=(  (abs(model.df$model.delta_AIC)<=aux.delta_AIC ) & (  model.df$n_variables==min(n_variables[2:i]) )  ) #Si no se cumple la selección anterior al menos devuleve el modelo con menor cantidad de terminos y
    if(!any(aux.marcador==TRUE)){
      aux.marcador=(model.df$n_variables==min(n_variables[2:i]) )
      print("No cumple %AIC<5%, el modelo obtenido contendrá solo terminos significativos")}
    aux.n_modelo=which(aux.marcador==TRUE)
    best.model.df=subset(model.df,aux.marcador==TRUE)   
    
    cat(paste("Model",sep = ""),best.model.df$model.name,sep="&",fill=TRUE)
    cat(paste("AIC",sep = ""),round(best.model.df$model.AIC),sep="&",fill=TRUE)
    
    return(list(best.model.df,my_list[[aux.n_modelo]]))
  }#End_function_ 
  
  plot_model.df=function(aux.model){
    
    ggplot.modelos.df=modelos.df[!duplicated(modelos.df$model.name), ]
    aux.aic=ggplot(ggplot.modelos.df, aes(x=reorder(model.name,model.AIC), y=model.AIC)) +
      geom_point( color="blue", size=4, alpha=0.6)+
      geom_segment( aes(x=model.name, xend=model.name, y=0, yend=model.AIC), 
                    color='skyblue')+xlab('Modelo')+ylab('AIC')+theme_light() +scale_y_continuous(trans="log10")+
      coord_flip()+theme (axis.text.x = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
    
    aux.variables=(ggplot.modelos.df[1:nrow(ggplot.modelos.df),]$model.formula)
    ggplot.modelos.df$n_variables=str_count(aux.variables, pattern = "\\+")
    
    aux.n_variables=ggplot(ggplot.modelos.df, aes(x=reorder(model.name,model.AIC), y=n_variables)) +
      geom_point( color="blue", size=4, alpha=0.6)+
      geom_segment( aes(x=model.name, xend=model.name, y=0, yend=n_variables), 
                    color='skyblue')+xlab('Modelo')+ylab('n_variables')+theme_light() +
      coord_flip()+theme (axis.text.x = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
    return(list(aux.aic,aux.n_variables))
  }#End_function_  
  
  box_plot_temporal=function(aux.df,aux.time,aux.class,aux.lang=NULL,orden.horiz=NULL){
    
    lang=aux.lang
    if (is.null(lang)){lang="ESP"}  
    if(lang=="ESP"){aux.text=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" )}
    if(lang=="ENG"){aux.text=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec" )}
    
    if (is.null(orden.horiz)){
      orden.horiz=FALSE
    }    
    if (orden.horiz==TRUE){
      orden_dias=c(1,2,3,7,8,9,4,5,6,10,11,12)
    }    
    
    if (orden.horiz==FALSE){
      orden_dias=c(1:12)
    }
    
    aux.time2=aux.df$day_year
    aux.table=table(aux.class,aux.time2)
    aux.table=table(aux.time2[aux.time==1],aux.class[aux.time==1])
    aux.boxplot.df=as.data.frame.matrix(aux.table)
    aux.boxplot.df$month=month(as.POSIXct(row.names(aux.boxplot.df), format="%j"))
    
    aux.df$day_month=mday(aux.df$dia_hora)
    year=aux.df$year
    month=aux.df$month
    edad=aux.df$edad
    edad_10=aux.df$edad_10
    day_month=aux.df$day_month
    y=aux.df$y
    
    # aux.year=tapply(!is.na(year), list(month, day_month, edad), mean)
    # aux.month=tapply(month, list(month, day_month, edad), mean)
    # aux.day_month=tapply(day_month, list(month, day_month, edad), mean)
    # #aux.edad_10=tapply(edad_10, list(month, day_month, edad_10), mean)
    # aux.edad=tapply(edad, list(month, day_month, edad), mean)
    # aux.y=tapply(y, list(month, day_month, edad), sum)
    # prueba=as.data.frame(cbind(aux.year,aux.month,aux.day_month,aux.edad,aux.y))
    # 
    # prueba$aux.edad=cut(prueba$aux.edad, c(seq(0,100,15)),labels = c(seq(15,100,15)))
    # prueba=na.omit(prueba)
    # 
    # ggplot(prueba , aes(x = as.factor(aux.edad), y = aux.y, group = aux.edad)) +
    #   geom_boxplot(alpha = 0.5) + theme(axis.text.x=element_text(angle=60, hjust=1)) +
    #   facet_wrap(~aux.month, nrow = 1, scales = "free_x")
    
    
    
    
    aux.basura=table(month,edad_10,day_month)
    almacen.lista=list()
    aux.max=c()
    for(i.month in 1:12){
      
      aux.marcador=is.element(month,i.month)
      basura=as.data.frame.matrix(table(day_month[aux.marcador],edad_10[aux.marcador]))
      max.lista=max(basura)
      aux.max=c(aux.max,max.lista)
      almacen.lista=c(almacen.lista,list(basura))
    }
    
    par(mfrow=c(2,3),mar=c(5, 4, 4, 1))
    
    aux.nvar=length(table(aux.class))
    y.max=max(aux.max)
    ylim.sup=y.max*1.2
    ylim.inf=0
    aux.ylim=c(ylim.inf,ylim.sup)
    
    aux.exp=length(digits( ylim.sup/10 ))-1
    
    for(i.month in 1:12){
      
      aux.i.month=orden_dias[i.month]
      boxplot((almacen.lista[[aux.i.month]]), ylim=aux.ylim,legend = TRUE,axes = FALSE, main=aux.text[aux.i.month],cex.main=2,col=aux.colours[1:(aux.nvar)])
      axis(2)
      # if (aux.i.month==1){}
      # else{}
      
    }
    mar.default=c(5, 4, 4, 2) + 0.1
    par(mfrow=c(1,1),mar=mar.default)
    
    
    
  }#End_function_ 
  
  map_ALT=function(aux.df,aux.y,aux.variable,aux.com=NULL,aux.prov=NULL,aux.lang=NULL){
    
    #Función de visualización de mapas de Aragón y sus provincias a nivel de municipio
    #Nº de accidentes total, sobre la población del municipio o sobre la afiliación a la Seg.Soc del municipio
    
    lang=aux.lang
    if (is.null(lang)){lang="ESP"}  
    if(lang=="ESP"){aux.text1=paste("ALT en", aux.com)
                    aux.text2=paste("ALT en", aux.prov)}
    if(lang=="ENG"){aux.text1=paste("ALT in", aux.com)
                    aux.text2=paste("ALT in", aux.prov)}
    
     if (is.null(aux.com)){
      aux.com="Aragon"
      grafico="municipio"} 
    if (is.null(aux.prov)){
      aux.prov="Zaragoza"
      grafico="comunidad"
    }else{grafico="ambos"}
    
    comunidad <- esp_get_munic_siane(region = aux.com) %>%
      # Datos de ejemplo: Población INE
      left_join(mapSpain::pobmun19, by = c("cpro", "cmun"))%>%
      # Datos de ejemplo: Seguridad social
      left_join(seg_social_total, by = c("LAU_CODE"))
    
    munic <- esp_get_munic(region = aux.prov) %>%
      # Datos de ejemplo: Población INE
      left_join(mapSpain::pobmun19, by = c("cpro", "cmun"))%>%
      # Datos de ejemplo: Seguridad social
      left_join(seg_social_total, by = c("LAU_CODE"))
    
    aux.LAU_CODE=aux.df$MUNICIPIOCT
    delta.LAU=as.data.frame(tapply(aux.y, list(aux.LAU_CODE), sum))
    colnames(delta.LAU)=c("y")
    delta.LAU$LAU_CODE=row.names(delta.LAU)
    row.names(delta.LAU)=(1:length(delta.LAU$y))  
    
    comunidad$y=delta.LAU$y[match(comunidad$LAU_CODE, delta.LAU$LAU_CODE)]
    comunidad[is.na(comunidad)]=0
    
    munic$y=delta.LAU$y[match(munic$LAU_CODE, delta.LAU$LAU_CODE)]
    munic$sexo=delta.LAU$SEXO[match(munic$LAU_CODE, delta.LAU$LAU_CODE)]
    munic[is.na(munic)]=0
    
    comunidad$y_pob=comunidad$y/comunidad$pob19*1000
    munic$y_pob=munic$y/munic$pob19*1000
    comunidad$y_seg_soc=comunidad$y/comunidad$total*1000
    munic$y_seg_soc=munic$y/munic$total*1000
    
    poblacion=list(comunidad,munic)
  
    plot_com=ggplot(comunidad) +  geom_sf(aes(fill = .data[[aux.variable]]), alpha = 0.9, color = NA) +
      scale_fill_gradientn(    colors = hcl.colors(1000, "YlOrRd"),trans = 'reverse',
                               n.breaks = 10,    labels = scales::label_comma(),    guide = guide_legend()  ) +
      labs(    fill = "Nº ALT",  title = aux.text1,    subtitle = ""
      ) +  theme_void() +  theme(    plot.background = element_rect("grey80"),
                                     text = element_text(face = "bold"),    plot.title = element_text(hjust = .5),
                                     plot.subtitle = element_text(hjust = .5)
      )
    
    plot_mun=ggplot(munic) +  geom_sf(aes(fill = .data[[aux.variable]]), alpha = 0.9, color = NA) +
      scale_fill_gradientn(    colors = hcl.colors(1000, "YlOrRd"),trans = 'reverse',
                               n.breaks = 10,    labels = scales::label_comma(),    guide = guide_legend()  ) +
      labs(    fill = "Nº ALT",  title = aux.text2,    subtitle = ""
      ) +  theme_void() +  theme(    plot.background = element_rect("grey80"),
                                     text = element_text(face = "bold"),    plot.title = element_text(hjust = .5),
                                     plot.subtitle = element_text(hjust = .5)
      )
    
    if (grafico=="comunidad"){plot_mun=NULL } 
    if (grafico=="municipio"){plot_com=NULL} 
    else{}

    return(list(plot_com,plot_mun))
    
  }#End_function_ 
  
  map_worst=function(aux.df,aux.y,aux.com=NULL,aux.prov=NULL,aux.lang=NULL){
    
    #Función para observar los 10 peores municipios de Aragón según el número de ALT

    lang=aux.lang
    if (is.null(lang)){lang="ESP"}  
    if(lang=="ESP"){aux.text1=paste("ALT en", aux.com)
    aux.text2=paste("ALT en", aux.prov)}
    if(lang=="ENG"){aux.text1=paste("ALT in", aux.com)
    aux.text2=paste("ALT in", aux.prov)}
    
    if (is.null(aux.com)){
      aux.com="Aragon"} 
    if (is.null(aux.prov)){
      aux.prov="Zaragoza"
    }else{aux.com="Aragon"
      aux.prov="Zaragoza"}
    
    comunidad <- esp_get_munic_siane(region = aux.com) %>%
      # Datos de ejemplo: Población INE
      left_join(mapSpain::pobmun19, by = c("cpro", "cmun"))%>%
      # Datos de ejemplo: Seguridad social
      left_join(seg_social_total, by = c("LAU_CODE"))
    
    munic <- esp_get_munic(region = aux.prov) %>%
      # Datos de ejemplo: Población INE
      left_join(mapSpain::pobmun19, by = c("cpro", "cmun"))%>%
      # Datos de ejemplo: Seguridad social
      left_join(seg_social_total, by = c("LAU_CODE"))
    
    aux.LAU_CODE=aux.df$MUNICIPIOCT
    delta.LAU=as.data.frame(tapply(aux.y, list(aux.LAU_CODE), sum))
    colnames(delta.LAU)=c("y")
    delta.LAU$LAU_CODE=row.names(delta.LAU)
    row.names(delta.LAU)=(1:length(delta.LAU$y))  
    
    comunidad$y=delta.LAU$y[match(comunidad$LAU_CODE, delta.LAU$LAU_CODE)]
    comunidad$cp=aux.df$CODPOSTALCT[match(comunidad$LAU_CODE, aux.df$MUNICIPIOCT)]
    
    comunidad[is.na(comunidad)]=0
    
    munic$y=delta.LAU$y[match(munic$LAU_CODE, delta.LAU$LAU_CODE)]
    munic$sexo=delta.LAU$SEXO[match(munic$LAU_CODE, delta.LAU$LAU_CODE)]
    munic[is.na(munic)]=0
    
    comunidad$y_pob=comunidad$y/comunidad$pob19*1000
    munic$y_pob=munic$y/munic$pob19*1000
    comunidad$y_seg_soc=round(comunidad$y/comunidad$total*1000, 2)
    munic$y_seg_soc=round(munic$y/munic$total*1000, 2)
    
    poblacion=list(comunidad,munic)
    name_poblacion=c(aux.com,aux.prov)
    
    worst=list()
    for (i in 1:2) {
      
      #print(name_poblacion[i])
      aux.poblacion=as.data.frame(poblacion[i])
      
      aux.10=(sort(aux.poblacion$y_seg_soc, decreasing = TRUE))[1:10]
      aux.marcador=is.element(aux.poblacion$y_seg_soc, aux.10)
      aux.worst_10=subset(aux.poblacion, aux.marcador==TRUE)
      worst_10=data.frame(aux.worst_10$municipio,aux.worst_10$y_seg_soc)
      colnames(worst_10)=c("Municipio","y_seg_soc")
      worst_10=worst_10[order(worst_10$y_seg_soc,decreasing=TRUE),]

      aux.marcador=(aux.poblacion$total>=1000)
      aux.poblacion_1000=subset(aux.poblacion, aux.marcador==TRUE)
      aux.10_1000=(sort(aux.poblacion_1000$y_seg_soc, decreasing = TRUE))[1:10]
      aux.marcador=is.element(aux.poblacion_1000$y_seg_soc, aux.10_1000)
      aux.worst_10_1000=subset(aux.poblacion_1000, aux.marcador==TRUE)
      worst_10_1000=data.frame(aux.worst_10_1000$municipio,aux.worst_10_1000$total,aux.worst_10_1000$y_seg_soc)
      colnames(worst_10_1000)=c("Municipio","Afiliados_SEG_SOC","y_seg_soc")
      worst_10_1000=worst_10_1000[order(worst_10_1000$y_seg_soc,decreasing=TRUE),]
      
      aux.worst=list(worst_10,worst_10_1000)
      worst=c(worst,aux.worst)
      
      } 
    
    # aux.cp=CP_POLIGONOS
    # aux.marcador=is.element(comunidad$cp, aux.cp)
    # poligonos_map=subset(comunidad, aux.marcador==TRUE)
    
    
    return(worst)
      
      
    }  

  df_to_latex=function(list.df){
    #Función transformación dataframes a formato tabla de LÁTEX
    
    if (class(list.df)=="list"){
      for (k in 1:length(list.df)) {
        df=as.data.frame(list.df[k])
        cat("",colnames(df),sep="&")
        for (i in 1:nrow(df)) {
          cat(sep="\n")
          for (j in 1:ncol(df)) {
            cat("",df[i,j],sep="&")
          }
        }
        cat(sep="\n")
        cat(sep="\n")
      }
      
    }else{
          cat("",colnames(df),sep="&")
          for (i in 1:nrow(df)) {
            cat(sep="\n")
            for (j in 1:ncol(df)) {
              cat("",df[i,j],sep="&")
            }
          }
      }
  }#End_function_ 
  
}    

#BLOQUE DELTA  

  {  #setwd("C:/Users/Inigo/Desktop/TFG")
     setwd("D:/Users/Íñigo/Desktop/TFG")
     rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
    
  #------------------------------------#
  
  #LECTURA
  
  # delta_21=read_xlsx("2021_TRAFICO.xlsx", skip=2, col_names = FALSE)
  # colnames(delta_21)=colnames(read_xlsx("2021_TRAFICO.xlsx", col_names = TRUE))
  # dim(delta_21)
  # Poligonos=read.csv("Poligonos.csv")
  
  # delta=read_xlsx("TRAFICO_2009_2022.xlsx", skip=2, col_names = FALSE)
  # colnames(delta)=colnames(read_xlsx("TRAFICO_2009_2022.xlsx", col_names = TRUE))
  # dim(delta)
  
    
    delta=read_xlsx("TRAFICO_2009_2023.xlsx", skip=2, col_names = FALSE)
    colnames(delta)=colnames(read_xlsx("TRAFICO_2009_2023.xlsx", col_names = TRUE))
    dim(delta)
    
  #------------------------------------#
  
  #GRAVEDAD Y ACCIDENTADOS
  
  delta$muertos=is.element(delta$GRADO,c("4"))
  sum(delta$GRADO==4,na.rm = TRUE)
  delta$muertos=as.numeric(delta$muertos)
  delta$gravedad=is.element(delta$GRADO,c("2","3","4"))
  delta$gravedad=as.numeric(delta$gravedad)
  
  delta$accidentes=delta$MULTIPLES
  delta$accidentes=is.element(delta$accidentes,c("1"))
  delta$accidentes=as.numeric(delta$accidentes)
  delta$accidentes=delta$accidentes+1
  delta$y=delta$accidentes
  
  #VARIABLES TEMPORALES
  
  delta$wday=wday(delta$FECHAACCIDENTE)
  delta$day_of_week=weekdays(delta$FECHAACCIDENTE)
  delta$week=week(delta$FECHAACCIDENTE)
  delta$month=month(delta$FECHAACCIDENTE)
  delta$year=year(delta$FECHAACCIDENTE)
  delta$day_year=yday(delta$FECHAACCIDENTE)
  delta$hour=delta$HORA
  delta$dia_hora=as.POSIXct(paste(delta$FECHAACCIDENTE, delta$hour), format="%Y-%m-%d %H")
  
  año_actual=max(delta$year, na.rm = TRUE)
  delta$edad= año_actual-year(delta$FECHANACIMIENTO)
  delta$edad_5=cut(delta$edad, c(seq(0,100,5)),labels = c(seq(5,100,5)))
  
  
  #UBICACION POLIGONOS
  CP_POLIGONOS=c(50197,50410,50830,50639)
  
  
  delta$plaza=is.element(delta$COPOSTALE ,c(50197)) #Codigo de la empresa contratante
  delta$plaza.ocurrencia=is.element(delta$CODPOSTALCT ,c(50197)) #Codigo de la empresa donde ha ocurrido el accidente
  table(delta$plaza, delta$plaza.ocurrencia)
  
  
  #delta$cuarte=is.element(delta$COPOSTALE ,c(50410, 50411, 50420, 50430,50019)) #Codigo de la empresa contratante
  #delta$cuarte.ocurrencia=is.element(delta$CODPOSTALCT ,c(50410, 50411, 50420, 50430,50019)) #Codigo de la empresa donde ha ocurrido el accidente
  delta$cuarte=is.element(delta$COPOSTALE ,c(50410)) #Codigo de la empresa contratante
  delta$cuarte.ocurrencia=is.element(delta$CODPOSTALCT ,c(50410)) #Codigo de la empresa donde ha ocurrido el accidente
  table(delta$cuarte, delta$cuarte.ocurrencia)
  
  
  #delta$villanueva=is.element(delta$COPOSTALE ,c(50830, 50820, 50020)) #Codigo de la empresa contratante
  #delta$villanueva.ocurrencia=is.element(delta$CODPOSTALCT ,c(50830, 50820, 50020)) #Codigo de la empresa donde ha ocurrido el accidente
  delta$villanueva=is.element(delta$COPOSTALE ,c(50830)) #Codigo de la empresa contratante
  delta$villanueva.ocurrencia=is.element(delta$CODPOSTALCT ,c(50830)) #Codigo de la empresa donde ha ocurrido el accidente
  table(delta$villanueva, delta$villanueva.ocurrencia)
  
  delta$figueruelas=is.element(delta$COPOSTALE ,c(50639)) #Codigo de la empresa contratante
  delta$figueruelas.ocurrencia=is.element(delta$CODPOSTALCT ,c(50639)) #Codigo de la empresa donde ha ocurrido el accidente
  table(delta$figueruelas, delta$figueruelas.ocurrencia)
  
  #VARIABLES CATEGORICAS
  
  aux.variable.selected  <- as.matrix( delta[  , 60]  )
  aux.name.variable <-  'lluvia'
  aux.words.selected <- c('lluvia', 'nieve', 'hielo', 'niebla', 'precipitacion')
  aux.words.selected.wide <- paste(aux.words.selected, collapse = '|')
  delta[[as.name(aux.name.variable)]] <-  grepl(aux.words.selected.wide , aux.variable.selected, perl=TRUE, ignore.case = T)
  
  names(delta)
  head(delta[, c(60,104)])
  table(delta$lluvia)
  delta$lluvia = as.integer(delta$lluvia == "TRUE")
  
  aux.marcador=is.element(delta$lluvia,c(1))
  aux.marcador=delta$dia_hora[aux.marcador]
  delta$lluvia=ifelse(is.element(delta$dia_hora,aux.marcador),1,0)
  
  aux.variable.selected  <- as.matrix( delta[  , 60]  )
  aux.name.variable <-  'lluvia_pura'
  aux.words.selected <- c('lluvia', 'precipitacion')
  aux.words.selected.wide <- paste(aux.words.selected, collapse = '|')
  delta[[as.name(aux.name.variable)]] <-  grepl(aux.words.selected.wide , aux.variable.selected, perl=TRUE, ignore.case = T)
  
  names(delta)
  head(delta[, c(60,105)])
  table(delta$lluvia_pura)
  delta$lluvia_pura = as.integer(delta$lluvia_pura == "TRUE")
  
  aux.marcador=is.element(delta$lluvia_pura,c(1))
  aux.marcador=delta$dia_hora[aux.marcador]
  delta$lluvia_pura=ifelse(is.element(delta$dia_hora,aux.marcador),1,0)
  
  aux.variable.selected  <- as.matrix( delta[  , 60]  )
  aux.name.variable <-  'niebla'
  aux.words.selected <- c('niebla', 'neblina')
  aux.words.selected.wide <- paste(aux.words.selected, collapse = '|')
  delta[[as.name(aux.name.variable)]] <-  grepl(aux.words.selected.wide , aux.variable.selected, perl=TRUE, ignore.case = T)
  
  names(delta)
  head(delta[, c(60,106)])
  table(delta$niebla)
  delta$niebla = as.integer(delta$niebla == "TRUE")
  
  aux.marcador=is.element(delta$niebla,c(1))
  aux.marcador=delta$dia_hora[aux.marcador]
  delta$niebla=ifelse(is.element(delta$dia_hora,aux.marcador),1,0)
  
  aux.name.variable <-  'itinere'
  delta$itinere=delta$LUGAR==3
  table(delta$itinere)
  delta$itinere = as.integer(delta$itinere == "TRUE")
  
  aux.variable.selected  <- as.matrix( delta[  , 60]  )
  aux.name.variable <-  'alcance'
  aux.words.selected <- c('alcance', 'trasero', 'choque','colisión')
  aux.words.selected.wide <- paste(aux.words.selected, collapse = '|')
  delta[[as.name(aux.name.variable)]] <-  grepl(aux.words.selected.wide , aux.variable.selected, perl=TRUE, ignore.case = T)
  
  head(delta[, c(60,107)])
  table(delta$alcance)
  delta$alcance = as.integer(delta$alcance == "TRUE")
  
  #------------------------------------#
  
  aux.marcador=is.element(delta$itinere,c(1))&(!is.na(delta$itinere))
  y1=delta$y[aux.marcador]
  hour1=delta$hour[aux.marcador]
  wday1=delta$wday[aux.marcador]
  
  aux_collapse1=tapply(y1, list(hour1, wday1), sum, na.rm=TRUE)
  aux_collapse1=as.data.frame(aux_collapse1)
  aux_collapse1[is.na(aux_collapse1)] = 0
  
  y=delta$y
  hour=delta$hour
  wday=delta$wday
  month=delta$month
  day_year=delta$day_year
  
  aux_collapse=tapply(y, list(hour, wday), sum, na.rm=TRUE)
  aux_collapse=as.data.frame(aux_collapse)
  aux_collapse[is.na(aux_collapse)] = 0
  
  
  #------------------------------------#
  
  #UBICACIÓN Y TAMAÑO
  
  plaza=delta$plaza
  #plaza=delta$plaza.ocurrencia
  plaza=as.factor(plaza)
  levels(plaza)=c(NA,"PLAZA")
  delta$PLAZA=plaza
  
  figueruelas=delta$figueruelas
  #figueruelas=delta$figueruelas.ocurrencia
  figueruelas=as.factor(figueruelas)
  levels(figueruelas)=c(NA,"FIGUERUELAS")
  delta$FIGUERUELAS=figueruelas
  
  cuarte=delta$cuarte
  #cuarte=delta$cuarte.ocurrencia
  cuarte=as.factor(cuarte)
  levels(cuarte)=c(NA,"CUARTE")
  delta$CUARTE=cuarte
  
  villanueva=delta$villanueva
  #villanueva=delta$villanueva.ocurrencia
  villanueva=as.factor(villanueva)
  levels(villanueva)=c(NA,"VILLANUEVA")
  delta$VILLANUEVA=villanueva
  
  prueba=delta %>% mutate (mycol4 = coalesce(PLAZA,FIGUERUELAS,CUARTE,VILLANUEVA)) 
  #prueba=cbind(prueba, mycol4 = na.omit(unlist(data[-1])))
  prueba=subset(prueba, (!is.na(prueba$mycol4)))
  delta_P=prueba
  colnames(delta_P)[which(names(delta_P) == "mycol4")] ="ubicacion"
  delta_poligonos=delta_P
  
  tamaño=delta_poligonos$PLANTILLAE
  tamaño=cut(tamaño, breaks=c(0,50,249,Inf))
  levels(tamaño)
  levels(tamaño)=c("Pequeña Eª","Mediana Eª","Gran Eª")
  delta_poligonos$tamaño=tamaño
  
  #------------------------------------#
  
  #Para mostrar repeticiones
  
  aux.acc=delta_poligonos
  aux.df=table(aux.acc$IPF_MD5)
  aux.df2=aux.df[aux.df>1]
  
  aux.df3=aux.acc[is.element(aux.acc$IPF_MD5,names(aux.df2)),]
  aux.df4=aux.df3[order(aux.df3$IPF_MD5,aux.df3$FECHAACCIDENTE),]
  head(aux.df4[1:10,c(5,7,53,83)])#REPETICIONES
  head(aux.df4[1:10,c(5,7,24,51)])#REPETICIONES
  sum(duplicated(aux.acc$IPF_MD5,aux.acc$FECHAACCIDENTE))
  
  #delta_poligonos=(aux.acc[!duplicated(aux.acc$IPF_MD5,aux.acc$FECHAACCIDENTE),])
  
  
  
  #------------------------------------#
  
  #PASOS PREVIOS A LOS MODELOS 2021#
  
  # delta_21=subset(delta, year==2021)
  # y=delta_21$y
  # hour=delta_21$hour
  # wday=delta_21$wday
  # month=delta_21$month
  # day_year=delta_21$day_year
  # 
  # aux.yday.hour = round((delta_21$day_year + (delta_21$hour-1)/24),6)
  # respuesta_2021=tapply(y, list(aux.yday.hour ), sum, na.rm=TRUE)
  # respuesta_2021=as.data.frame(respuesta_2021)
  # respuesta_2021[is.na(respuesta_2021)] = 0
  # names(respuesta_2021)=c('y')
  # 
  # respuesta_2021$day_year=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta_2021$hour=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta_2021$wday=tapply(wday, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta_2021$month=tapply(month, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta_2021$arm_dia=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta_2021$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
  # 
  # respuesta_2021$lluvia=(tapply(delta_21$lluvia, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  # respuesta_2021$itinere=tapply(delta_21$itinere, list(aux.yday.hour), sum, na.rm=TRUE)
  # respuesta_2021$alcance=(tapply(delta_21$alcance, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  # respuesta_2021$fin_de_semana=is.element(respuesta_2021$wday,c(6,7))
  # 
  # dim(respuesta_2021)
  # respuesta_2021=rownames_to_column(respuesta_2021, "aux.yday.hour")
  # respuesta_2021$aux.yday.hour=as.numeric(respuesta_2021$aux.yday.hour)
  # 
  # aux.vday_year=1:365
  # aux.vday_hour=1:24
  # aux.v = expand.grid(aux.vday_year , (aux.vday_hour-1)/24)
  # aux.v$aux.yday.hour=round((aux.v$Var1+aux.v$Var2), 6)
  # 
  # prueba=merge(respuesta_2021, aux.v, all = TRUE)
  # prueba[is.na(prueba)] = 0
  # prueba$Var1=NULL
  # prueba$Var2=NULL
  # respuesta_2021=prueba
  # 
  # respuesta_2021$c1d=cos(2*pi* respuesta_2021$arm_dia/365)
  # respuesta_2021$s1d=sin(2*pi*respuesta_2021$arm_dia/365)
  # respuesta_2021$c2d=cos(4*pi*respuesta_2021$arm_dia/365)
  # respuesta_2021$s2d=sin(4*pi*respuesta_2021$arm_dia/365)
  # respuesta_2021$c3d=cos(6*pi*respuesta_2021$arm_dia/365)
  # respuesta_2021$s3d=sin(6*pi*respuesta_2021$arm_dia/365)
  # respuesta_2021$c4d=cos(8*pi*respuesta_2021$arm_dia/365)
  # respuesta_2021$s4d=sin(8*pi*respuesta_2021$arm_dia/365)
  # respuesta_2021$c5d=cos(10*pi*respuesta_2021$arm_dia/365)
  # respuesta_2021$s5d=sin(10*pi*respuesta_2021$arm_dia/365)
  # 
  # respuesta_2021$c1h=cos(2*pi*respuesta_2021$arm_hora/24)
  # respuesta_2021$s1h=sin(2*pi*respuesta_2021$arm_hora/24)
  # respuesta_2021$c2h=cos(4*pi*respuesta_2021$arm_hora/24)
  # respuesta_2021$s2h=sin(4*pi*respuesta_2021$arm_hora/24)
  # respuesta_2021$c3h=cos(6*pi*respuesta_2021$arm_hora/24)
  # respuesta_2021$s3h=sin(6*pi*respuesta_2021$arm_hora/24)
  
}
  
#BLOQUE BASES METEREOLÓGICAS y SEGURIDAD SOCIAL
  
  {  
  {
    # muela15=read.csv("muela15.csv", check.names = F, sep = ";",skip=15) 
    # colnames(muela15)=c("fecha","lluvia_mue")
    # #muela15$fecha=format(as.Date(muela15$fecha), "%Y-%m-%d")
    # 
    # loteta15=read.csv("loteta15.csv", check.names = F, sep = ";",skip=15)
    # colnames(loteta15)=c("fecha","lluvia_lote")
    # #loteta15$fecha=format(as.Date(loteta15$fecha), "%Y-%m-%d")
    # 
    # zgz15=read.csv("zgz15.csv", check.names = F, sep = ";",skip=15)
    # colnames(zgz15)=c("fecha","lluvia_zgz")
    # #zgz15$fecha=format(as.Date(zgz15$fecha), "%Y-%m-%d %H:%M") 
    # 
    # gallego15=read.csv("gallego15.csv", check.names = F, sep = ";",skip=15)
    # colnames(gallego15)=c("fecha","lluvia_galle")
    # #gallego15$fecha=format(as.Date(gallego15$fecha), "%Y-%m-%d")   
    
    muela15=read.csv("muela15_2023.csv", check.names = F, sep = ";",skip=15)
    loteta15=read.csv("loteta15_2023.csv", check.names = F, sep = ";",skip=15)
    zgz15=read.csv("zgz15_2023.csv", check.names = F, sep = ";",skip=15)
    gallego15=read.csv("gallego15_2023.csv", check.names = F, sep = ";",skip=15)
    
    
    colnames(muela15)=c("fecha","hour","lluvia_mue")
    colnames(loteta15)=c("fecha","hour","lluvia_lote")
    colnames(zgz15)=c("fecha","hour","lluvia_zgz")
    colnames(gallego15)=c("fecha","hour","lluvia_galle")
    
    muela15$fecha=paste(muela15$fecha,muela15$hour)
    loteta15$fecha=paste(loteta15$fecha,loteta15$hour)
    zgz15$fecha=paste(zgz15$fecha,zgz15$hour)
    gallego15$fecha=paste(gallego15$fecha,gallego15$hour)
    
    muela15=muela15[,-2]
    loteta15=loteta15[,-2]
    zgz15=zgz15[,-2]
    gallego15=gallego15[,-2]
    
    
    df_list = list(muela15, loteta15, zgz15, gallego15)
    base_lluvia=df_list %>% reduce(full_join, by='fecha')
    
    # base_lluvia=muela15
    # base_lluvia[3]=loteta15$lluvia_lote
    # base_lluvia[4]=zgz15$lluvia_zgz
    # base_lluvia[5]=gallego15$lluvia_galle
    colnames(base_lluvia)=c("fecha","lluvia_mue","lluvia_lote","lluvia_zgz","lluvia_galle")
    
    base_lluvia$lluvia_mue=as.numeric(gsub(",", ".", gsub("\\.", "", base_lluvia$lluvia_mue)))
    base_lluvia$lluvia_lote=as.numeric(gsub(",", ".", gsub("\\.", "", base_lluvia$lluvia_lote)))
    base_lluvia$lluvia_zgz=as.numeric(gsub(",", ".", gsub("\\.", "", base_lluvia$lluvia_zgz)))
    base_lluvia$lluvia_galle=as.numeric(gsub(",", ".", gsub("\\.", "", base_lluvia$lluvia_galle)))
    base_lluvia$total=rowSums((base_lluvia[,c(2:5)]), na.rm=TRUE) 
    base_lluvia=na.omit(base_lluvia)#Solo me quedo con las filas que contengan informaciÃ³n en las 3 bases
    base_lluvia$fecha_hora=as.POSIXct(base_lluvia$fecha,format="%d/%m/%Y %H:%M:%S")
    base_lluvia$fecha_dia=as.POSIXct(base_lluvia$fecha,format="%d/%m/%Y")
    # base_lluvia$fechaprueba=as.Date(base_lluvia$fecha, format = "%Y/%d/%m")
    # strptime(base_lluvia$fecha[1],format="%d/%m/%Y: %H:%M:%S")
    
    base_lluvia$year=year(base_lluvia$fecha_hora)
    base_lluvia$yday=yday(base_lluvia$fecha_hora)
    base_lluvia$day=day(base_lluvia$fecha_hora)
    base_lluvia$month=month(base_lluvia$fecha_hora)
    base_lluvia$hour=hour(base_lluvia$fecha_hora)
    base_lluvia$aux.yday.hour=(base_lluvia$year*100000+base_lluvia$yday*100+base_lluvia$hour)
    
    prueba=tapply(base_lluvia$lluvia_mue, (base_lluvia$aux.yday.hour), sum, na.rm=TRUE)
    prueba=as.data.frame(prueba)
    colnames(prueba)=("lluvia_mue")
    prueba$lluvia_lote=tapply(base_lluvia$lluvia_lote, (base_lluvia$aux.yday.hour), sum, na.rm=TRUE)
    prueba$lluvia_zgz=tapply(base_lluvia$lluvia_zgz, (base_lluvia$aux.yday.hour), sum, na.rm=TRUE)
    prueba$lluvia_galle=tapply(base_lluvia$lluvia_galle, (base_lluvia$aux.yday.hour), sum, na.rm=TRUE)
    prueba$total=tapply(base_lluvia$total, (base_lluvia$aux.yday.hour), sum, na.rm=TRUE)
    prueba$aux.yday.hour=tapply(base_lluvia$aux.yday.hour, (base_lluvia$aux.yday.hour), mean, na.rm=TRUE)
    
    prueba$day=tapply(base_lluvia$day, (base_lluvia$aux.yday.hour), mean, na.rm=TRUE)
    prueba$month=tapply(base_lluvia$month, (base_lluvia$aux.yday.hour), mean, na.rm=TRUE)
    prueba$year=tapply(base_lluvia$year, (base_lluvia$aux.yday.hour), mean, na.rm=TRUE)
    
    bases4=prueba #Lluvia recogida por las 4 estaciones metereologicas
    bases4$hour=bases4$aux.yday.hour-round(bases4$aux.yday.hour/100,0)*100
    bases4$hour=bases4$aux.yday.hour-round(bases4$aux.yday.hour/100,0)*100
    bases4$yday=(bases4$aux.yday.hour-(bases4$year*100000+bases4$hour))/100
    bases4$fecha=make_date(year = bases4$year, month = bases4$month, day = bases4$day)
    
    bases4=bases4[which.max(bases4$hour == "0") : nrow(bases4), ]#Elimino los primeros dÃ­as incompletos
    
    
  } #Se construye la base de datos con las 4 bases y se le da formato
  head(bases4)
  dim(bases4)
  table(bases4$year) 
  
  table(bases4[,5])
  
  {muela15=read.csv("muela15.csv", check.names = F, sep = ";",skip=15)
    colnames(muela15)=c("fecha","lluvia_mue")
    #muela15$fecha=format(as.Date(muela15$fecha), "%Y-%m-%d")
    
    zgz15=read.csv("zgz15.csv", check.names = F, sep = ";",skip=15)
    colnames(zgz15)=c("fecha","lluvia_zgz")
    #zgz15$fecha=format(as.Date(zgz15$fecha), "%Y-%m-%d %H:%M") 
    
    gallego15=read.csv("gallego15.csv", check.names = F, sep = ";",skip=15)
    colnames(gallego15)=c("fecha","lluvia_galle")
    #gallego15$fecha=format(as.Date(gallego15$fecha), "%Y-%m-%d")   
    
    
    # df_list = list(muela15, loteta15, zgz15, gallego15)      
    # base_lluvia=df_list %>% reduce(full_join, by='fecha')
    base_lluvia=muela15
    base_lluvia[3]=zgz15$lluvia_zgz
    base_lluvia[4]=gallego15$lluvia_galle
    colnames(base_lluvia)=c("fecha","lluvia_mue","lluvia_zgz","lluvia_galle")
    
    base_lluvia$lluvia_mue=as.numeric(gsub(",", ".", gsub("\\.", "", base_lluvia$lluvia_mue)))
    base_lluvia$lluvia_zgz=as.numeric(gsub(",", ".", gsub("\\.", "", base_lluvia$lluvia_zgz)))
    base_lluvia$lluvia_galle=as.numeric(gsub(",", ".", gsub("\\.", "", base_lluvia$lluvia_galle)))
    base_lluvia$total=rowSums((base_lluvia[,c(2:4)]), na.rm=TRUE) 
    base_lluvia=na.omit(base_lluvia)#Solo me quedo con las filas que contengan informaciÃ³n en las 3 bases
    base_lluvia$fecha_hora=as.POSIXct(base_lluvia$fecha,format="%d/%m/%Y %H:%M:%S")
    base_lluvia$fecha_dia=as.POSIXct(base_lluvia$fecha,format="%d/%m/%Y")
    # base_lluvia$fechaprueba=as.Date(base_lluvia$fecha, format = "%Y/%d/%m")
    # strptime(base_lluvia$fecha[1],format="%d/%m/%Y: %H:%M:%S")
    
    base_lluvia$year=year(base_lluvia$fecha_hora)
    base_lluvia$yday=yday(base_lluvia$fecha_hora)
    base_lluvia$day=day(base_lluvia$fecha_hora)
    base_lluvia$month=month(base_lluvia$fecha_hora)
    base_lluvia$hour=hour(base_lluvia$fecha_hora)
    base_lluvia$aux.yday.hour=(base_lluvia$year*100000+base_lluvia$yday*100+base_lluvia$hour)
    
    prueba=tapply(base_lluvia$lluvia_mue, (base_lluvia$aux.yday.hour), sum, na.rm=TRUE)
    prueba=as.data.frame(prueba)
    colnames(prueba)=("lluvia_mue")
    prueba$lluvia_zgz=tapply(base_lluvia$lluvia_zgz, (base_lluvia$aux.yday.hour), sum, na.rm=TRUE)
    prueba$lluvia_galle=tapply(base_lluvia$lluvia_galle, (base_lluvia$aux.yday.hour), sum, na.rm=TRUE)
    prueba$total=tapply(base_lluvia$total, (base_lluvia$aux.yday.hour), sum, na.rm=TRUE)
    prueba$aux.yday.hour=tapply(base_lluvia$aux.yday.hour, (base_lluvia$aux.yday.hour), mean, na.rm=TRUE)
    
    prueba$day=tapply(base_lluvia$day, (base_lluvia$aux.yday.hour), mean, na.rm=TRUE)
    prueba$month=tapply(base_lluvia$month, (base_lluvia$aux.yday.hour), mean, na.rm=TRUE)
    prueba$year=tapply(base_lluvia$year, (base_lluvia$aux.yday.hour), mean, na.rm=TRUE)
    
    bases3=prueba #Lluvia recogida por las 4 estaciones metereologicas
    bases3$hour=bases3$aux.yday.hour-round(bases3$aux.yday.hour/100,0)*100
    bases3$hour=bases3$aux.yday.hour-round(bases3$aux.yday.hour/100,0)*100
    bases3$yday=(bases3$aux.yday.hour-(bases3$year*100000+bases3$hour))/100
    bases3$fecha=make_date(year = bases3$year, month = bases3$month, day = bases3$day)
    
    bases3=bases3[which.max(bases3$hour == "0") : nrow(bases3), ]#Elimino los primeros dÃ­as incompletos
    
    
  } #Se construye la base de datos con las 3 bases y se le da formato (se elimina la loteta, ya que limita las fechas, debido a la posterior construcciÃ³n del pantano al inicio de la base de datos de los accidentes)
  head(bases3)
  dim(bases3)
  table(bases3$year)  
  
  {seg_social_sexo_año=read_xlsx("seg_social_sexo_año.xlsx", skip=12, col_names = TRUE)
    #colnames(seg_social)=c("month_year","CP","municipio", "sexo", "numero")
    aux.names=c("LAU_CODE","municipio","edad")
    aux.orden=c()
    for (i in 1:12){
      aux.inicio=i*3+1
      aux.fin=i*3+3
      aux.names[aux.inicio:aux.fin]=paste(colnames(seg_social_sexo_año[1,aux.inicio]),seg_social_sexo_año[1,aux.inicio:aux.fin])
      aux.orden[i]=paste(aux.inicio)
    }
    aux.orden=as.numeric(aux.orden)
    
    colnames(seg_social_sexo_año)=aux.names
    seg_social_sexo_año=seg_social_sexo_año[-1,]
    seg_social_sexo_año = seg_social_sexo_año %>%  fill(LAU_CODE, .direction = "down")
    seg_social_sexo_año = seg_social_sexo_año %>%  fill(municipio, .direction = "down")
    seg_social_sexo_año=na.omit(seg_social_sexo_año)
    
    seg_social_sexo_año[4:length(aux.names)] <- sapply(seg_social_sexo_año[4:length(aux.names)],as.numeric)
    seg_social_sexo_año$total=ceiling(rowSums(seg_social_sexo_año[,as.numeric(aux.orden)])/12)
    seg_social_sexo_año$h=ceiling(rowSums(seg_social_sexo_año[,aux.orden+1])/12)
    seg_social_sexo_año$m=ceiling(rowSums(seg_social_sexo_año[,aux.orden+2])/12)
    
    seg_social_sexo_año=seg_social_sexo_año[,c(1:3, (length(aux.names)+1):(length(aux.names)+3))]
    seg_social_sexo_año$edad=as.factor(seg_social_sexo_año$edad) 
    #seg_social_sexo_año = droplevels(seg_social_sexo_año[!seg_social_sexo_año$edad == 'Sin clasificar ',])
    
    seg_social_sexo_año=seg_social_sexo_año[-which(seg_social_sexo_año$edad == 'Sin clasificar',),]
    seg_social_total=(seg_social_sexo_año[seg_social_sexo_año$edad == 'Total',])
    seg_social_tabla=droplevels(seg_social_sexo_año[!seg_social_sexo_año$edad == c('Total'),])
    levels(seg_social_tabla$edad)=c(seq(20,75,5))
    
    seg_social_edad=as.data.frame(tapply(seg_social_tabla$total,seg_social_tabla$edad, sum))
    colnames(seg_social_edad)="total"
    seg_social_edad$h=tapply(seg_social_tabla$h,seg_social_tabla$edad, sum)
    seg_social_edad$m=tapply(seg_social_tabla$m,seg_social_tabla$edad, sum)
    
    
  }#Seguridad social
  
  
  }

#BLOQUE BASE DELTA 2009_2022
  
  {  años=c(2009:2022)
  delta_09_22=subset(delta, year %in% años)
  delta_09_22=(delta_09_22[!duplicated(delta_09_22$IPF_MD5,delta_09_22$FECHAACCIDENTE),])
  y=delta_09_22$y
  hour=delta_09_22$hour
  wday=delta_09_22$wday
  month=delta_09_22$month
  day_year=delta_09_22$day_year
  
  # delta_09_22$dias_años=as.numeric(difftime(as.Date(delta_09_22$FECHAACCIDENTE), as.Date("2008-12-31"), unit="days"))
  # dias_años=delta_09_22$dias_años
  
  aux.yday.hour=(delta_09_22$year*100000+delta_09_22$day_year*100+delta_09_22$hour)
  delta_09_22$aux.yday.hour=aux.yday.hour
  #aux.yday.hour = round((delta_09_22$dias_años + (delta_09_22$hour-1)/24),6)
  
  delta_09_22$entrada=1*(delta_09_22$HORATRABAJO==0)
  delta_09_22$salida=1*(delta_09_22$HORATRABAJO==99)
  delta_09_22$jornada=1-((delta_09_22$HORATRABAJO==0)|(delta_09_22$HORATRABAJO==99)) 
  
  #building dataframe response.hourly.itinere, every row is the number of injuries in an hour and in a situation defined by in mission, in itinere-goto, in itenere-return
  #Se obtienen las fechas completas (año, mes, dia y hora) para el periodo de años desde el 2009 a 2022.
  #itenere.key is 0 for mission, is 1 for itinere-goto , is 2 for itinere-return
  delta_09_22$itinere.key = delta_09_22$itinere
  delta_09_22$itinere.key[ (delta_09_22$itinere==1) & is.element( delta_09_22$HORATRABAJO   ,c( 0) ) ] = 1
  delta_09_22$itinere.key[ (delta_09_22$itinere==1) & is.element( delta_09_22$HORATRABAJO   ,c( 99) ) ] = 2
  }
  
#BLOQUE BASE DELTA 2024
  
  {#BASES DELTA LÍMITADA POR:
    #-INICIO TEMPORAL DEL 05-2011 (BASE LA LOTETA) 
    #-FIN TEMPORAL DEL 06-2023 (DATOS DELTA) 
    #-PROVINCIA DE ZARAGOZA (COD_PROV 50) 

    
    
    
  cod_prov=c("22","44","50") #Huesca, Teruel y Zaragoza
  aux.prov=floor(delta_09_22$MUNICIPIOCT/1000)
  aux.prov=factor(aux.prov, levels=cod_prov)
  delta_09_22$prov=aux.prov
  
  aux.marcador=is.element(delta_09_22$prov,"50")          #Solo provincia de Zgz
  delta_09_22_zgz=subset(delta_09_22,aux.marcador==TRUE)  
  
  decalaje=1
  mL_lluvia=0.0
  datos_reales=bases4
  datos_extraidos=delta_09_22_zgz
  aux.meteo="lluvia_pura"
  df_lluvia=acierto_lluvia_mL(datos_reales, datos_extraidos, aux.meteo, decalaje, mL_lluvia,aux.ROC=TRUE)[[4]]
  aux.df_lluvia=df_lluvia[,c(3:7,9,12)]
  delta_2024=merge(delta_09_22,aux.df_lluvia,by="aux.yday.hour") 
  
  #saveRDS(delta_2024, file = "delta_2024.rds")
  }
  
#BLOQUE MODELO RESPUESTA 
 
  {
  #df_respuesta=delta_09_22
  df_respuesta=delta_2024
    
  aux.yday.hour=df_respuesta$aux.yday.hour
  y=df_respuesta$y
  respuesta=tapply(y, list(aux.yday.hour ), sum, na.rm=TRUE)
  respuesta=as.data.frame(respuesta)
  respuesta[is.na(respuesta)] = 0
  names(respuesta)=c('y')
  
  #respuesta$y se depuran los datos para evitar que accidentes con multiples personas involucradoas como el autobus del 30-nov-2010, den lugar a un nÃºmero de accidentes sobrerepresentado
  respuesta$y[respuesta$y==75]=1
  #respuesta$y[is.element(respuesta$year,c(2010))&is.element(respuesta$day_year,c(330))&is.element(respuesta$hour,c(6))]=1
  respuesta$y[respuesta$y==19]=1
  #respuesta$y[respuesta$y>=15]=1
  # respuesta$dias_años=tapply(dias_años, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$hour=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$wday=tapply(wday, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$month=tapply(month, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$arm_dia=tapply(dias_años, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
  
  respuesta$lluvia=(tapply(df_respuesta$lluvia, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  respuesta$lluvia_pura=(tapply(df_respuesta$lluvia_pura, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  respuesta$niebla=(tapply(df_respuesta$niebla, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  respuesta$total=(tapply(df_respuesta$total, list(aux.yday.hour), sum, na.rm=TRUE))
  respuesta$lluvia.decalada=(tapply(df_respuesta$lluvia.decalada, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  
  respuesta$itinere=tapply(df_respuesta$itinere, list(aux.yday.hour), sum, na.rm=TRUE)
  respuesta$alcance=(tapply(df_respuesta$alcance, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  #respuesta$fin_de_semana=is.element(respuesta$wday,c(6,7))
  respuesta$leve=(tapply(df_respuesta$GRADO==1, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  grave1=(tapply(df_respuesta$GRADO==2, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  grave2=(tapply(df_respuesta$GRADO==3, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  respuesta$grave=grave1+grave2
  respuesta$muertos=(tapply(df_respuesta$GRADO==4, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  
  #Variables IDA_VUELTA
  respuesta$HORATRABAJO=(tapply(df_respuesta$HORATRABAJO, list(aux.yday.hour), sum, na.rm=TRUE))
  respuesta$entrada=(tapply(df_respuesta$entrada, list(aux.yday.hour), sum, na.rm=TRUE))
  respuesta$salida=(tapply(df_respuesta$salida, list(aux.yday.hour), sum, na.rm=TRUE))
  respuesta$jornada=(tapply(df_respuesta$jornada, list(aux.yday.hour), sum, na.rm=TRUE))
  
  dim(respuesta)
  respuesta=rownames_to_column(respuesta, "aux.yday.hour")
  respuesta$aux.yday.hour=as.numeric(respuesta$aux.yday.hour)
  
  aux.date=seq(ymd('2009-01-01'),ymd('2022-12-31'), by = 'days')
  aux.date=seq(as.POSIXct("2009-01-01 0","%Y-%m-%d %H", tz="UTC"),to=as.POSIXct("2022-12-31 23", "%Y-%m-%d %H", tz="UTC"), by="hour")
  head(aux.date)
  aux.date=cbind(aux.date,year(aux.date),yday(aux.date),hour(aux.date))
  aux.date=cbind(aux.date,aux.date[,2]*100000+aux.date[,3]*100+aux.date[,4])
  aux.v=aux.date[,2:5]
  colnames(aux.v)=c("year","day_year","hour","aux.yday.hour")
  #aux.yday.hour=round((delta$year*100000+delta$day_year*100+delta$hour), 6)
  
  #aux.vday_year=1:(365*length(años)+sum(leap_year(años)))
  # aux.vday_year=aux.date[,4]
  # aux.vday_hour=1:24
  # aux.v = expand.grid(aux.vday_year , (aux.vday_hour-1)/24)
  # aux.v$aux.yday.hour=round((aux.v$Var1+aux.v$Var2), 6)
  
  prueba=merge(respuesta, aux.v, all = TRUE)
  prueba[is.na(prueba)] = 0
  # prueba$Var1=NULL
  # prueba$Var2=NULL
  respuesta=prueba
  
  aux.yday.hour=respuesta$aux.yday.hour
  day_year=respuesta$day_year
  hour=respuesta$hour
  respuesta$arm_dia=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
  respuesta$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
  
  # respuesta$day_years=respuesta$day_year+365*(respuesta$year-2009)+sum(leap_year(respuesta$year-2009))
  # day_years=respuesta$day_years
  # respuesta$arm_dia=tapply(day_years, list(aux.yday.hour), mean, na.rm=TRUE)
  
  #dias=365*length(años)+sum(leap_year(años))
  respuesta$c1d=cos(2*pi*respuesta$arm_dia/366)
  respuesta$s1d=sin(2*pi*respuesta$arm_dia/366)
  respuesta$c2d=cos(4*pi*respuesta$arm_dia/366)
  respuesta$s2d=sin(4*pi*respuesta$arm_dia/366)
  respuesta$c3d=cos(6*pi*respuesta$arm_dia/366)
  respuesta$s3d=sin(6*pi*respuesta$arm_dia/366)
  respuesta$c4d=cos(8*pi*respuesta$arm_dia/366)
  respuesta$s4d=sin(8*pi*respuesta$arm_dia/366)
  respuesta$c5d=cos(10*pi*respuesta$arm_dia/366)
  respuesta$s5d=sin(10*pi*respuesta$arm_dia/366)
  
  
  respuesta$c1h=cos(2*pi*respuesta$arm_hora/24)
  respuesta$s1h=sin(2*pi*respuesta$arm_hora/24)
  respuesta$c2h=cos(4*pi*respuesta$arm_hora/24)
  respuesta$s2h=sin(4*pi*respuesta$arm_hora/24)
  respuesta$c3h=cos(6*pi*respuesta$arm_hora/24)
  respuesta$s3h=sin(6*pi*respuesta$arm_hora/24)
  
  #------------------------------------#
  
  respuesta$dia_año=respuesta$day_year+respuesta$year*1000
  i=(as.Date("15-03-2020", "%d-%m-%Y"))
  f=(as.Date("21-06-2020", "%d-%m-%Y"))
  f_est_alar=(as.Date("9-05-2021", "%d-%m-%Y"))
  inicio=yday(i)+year(i)*1000
  final=yday(f)+year(f)*1000
  final_estado_alarma=yday(f_est_alar)+year(f_est_alar)*1000
  
  respuesta$conf=as.numeric(is.element(respuesta$dia_año,inicio:final))
  respuesta$covid=as.numeric(is.element(respuesta$dia_año,final:final_estado_alarma))
  
  aux.df=respuesta
  d = with(aux.df, paste(year, day_year))
  d = as.Date(d, "%Y %j")
  aux.df$dia=d
  h=aux.df$hour
  d = paste(d, paste(h))
  d = as.POSIXct(d, format ="%Y-%m-%d %H")
  aux.df$fecha=d
  aux.df$day=day(aux.df$fecha)
  aux.df$month=month(aux.df$fecha)
  respuesta=aux.df
  
  #------------------------------------#
  
  aux.yday.hour.itinere = (df_respuesta$year*1000000+df_respuesta$day_year*1000+df_respuesta$hour*10) + df_respuesta$itinere.key
  response.hourly.itinere = tapply(df_respuesta$y, list(aux.yday.hour.itinere ), sum, na.rm=TRUE)
  response.hourly.itinere = as.data.frame(response.hourly.itinere)
  #to built the response 
  response.hourly.itinere [is.na(response.hourly.itinere)] =  0
  names(response.hourly.itinere) = c('y')
  
  #respuesta$y se depuran los datos para evitar que accidentes con multiples personas involucradoas como el autobus del 30-nov-2010, den lugar a un nÃºmero de accidentes sobrerepresentado
  #respuesta$y[is.element(respuesta$year,c(2010))&is.element(respuesta$day_year,c(330))&is.element(respuesta$hour,c(6))]=1
  response.hourly.itinere$y[response.hourly.itinere$y==72]=1
  response.hourly.itinere$y[response.hourly.itinere$y==18]=1
  
  #to built the key
  response.hourly.itinere$itinere.key  = tapply(df_respuesta$itinere.key, list(aux.yday.hour.itinere), mean, na.rm=TRUE)
  #to built the key
  response.hourly.itinere$aux.yday.hour.itinere  = tapply(aux.yday.hour.itinere, list(aux.yday.hour.itinere), mean, na.rm=TRUE)
  
  response.hourly.itinere$lluvia=(tapply(df_respuesta$lluvia, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
  response.hourly.itinere$lluvia_pura=(tapply(df_respuesta$lluvia_pura, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
  response.hourly.itinere$niebla=(tapply(df_respuesta$niebla, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
  response.hourly.itinere$total=(tapply(df_respuesta$total, list(aux.yday.hour.itinere), sum, na.rm=TRUE))
  response.hourly.itinere$lluvia.decalada=(tapply(df_respuesta$lluvia.decalada, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
  
   response.hourly.itinere$itinere=tapply(df_respuesta$itinere, list(aux.yday.hour.itinere), mean, na.rm=TRUE)
  response.hourly.itinere$alcance=(tapply(df_respuesta$alcance, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
  response.hourly.itinere$fin_de_semana = (tapply( is.element(df_respuesta$wday,c(6,7))  , list(aux.yday.hour.itinere), sum, na.rm=TRUE) >0)
  
  response.hourly.itinere$leve=(tapply(df_respuesta$GRADO==1, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
  grave1=(tapply(df_respuesta$GRADO==2, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
  grave2=(tapply(df_respuesta$GRADO==3, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
  response.hourly.itinere $grave=grave1+grave2
  response.hourly.itinere $muertos=(tapply(df_respuesta$GRADO==4, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
  
  #to build a dataframe with a row for every hour of every day of every year and in situation of mission, itinere go, itinere return
  
  aux.date=seq(ymd('2009-01-01'),ymd('2022-12-31'), by = 'days')
  aux.date=seq(as.POSIXct("2009-01-01 0","%Y-%m-%d %H", tz="UTC"),to=as.POSIXct("2022-12-31 23", "%Y-%m-%d %H", tz="UTC"), by="hour")
  head(aux.date)
  aux.date=cbind(aux.date,year(aux.date),yday(aux.date),hour(aux.date))
  aux.number.hours = dim(aux.date)[1]
  #to include a row in every day for mission, itinere-togo, itinere-return
  #to include the itinere.key
  aux.itinere.key = c(rep( 0, aux.number.hours ) , rep( 1, aux.number.hours ), rep( 2, aux.number.hours )) 
  aux.date = rbind(aux.date ,aux.date, aux.date) 
  aux.date = cbind(aux.date, aux.itinere.key )
  aux.date=cbind(aux.date, aux.date[,2]*1000000+aux.date[,3]*1000+aux.date[,4]*10+ aux.date[,5] )
  aux.v=aux.date[,2:6]
  colnames(aux.v)=c("year","day_year","hour", "itinere.key"  , "aux.yday.hour.itinere" )
  
  prueba=merge(response.hourly.itinere, aux.v, all = TRUE)
  
  #la variable lluvia debe entenderse como LLUVIA DECLARADA, por eso se asocia 0 a las horas sin accidntes, 
  prueba$lluvia[is.na(prueba$lluvia)] = 0
  
  #Se decalara 0 los valores que corresponden a horas sin accidentes
  prueba$y[is.na(prueba$y)] = 0
  prueba$alcance[is.na(prueba$alcance)] = 0
  prueba$muertos[is.na(prueba$muertos)] = 0
  prueba$leve[is.na(prueba$leve)] = 0
  prueba$grave[is.na(prueba$grave)] = 0
  
  #to built the harmonic
  day_year=prueba$day_year
  hour=prueba$hour
  prueba = prueba[  prueba$day_year>0 ,  ]
  
  #  prueba$arm_dia=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
  # prueba$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
  prueba$arm_dia = (prueba$day_year-1)
  prueba$arm_hora = (prueba$hour)
  prueba$c1d=cos(2*pi*prueba$arm_dia/366)
  prueba$s1d=sin(2*pi*prueba$arm_dia/366)
  prueba$c2d=cos(4*pi*prueba$arm_dia/366)
  prueba$s2d=sin(4*pi*prueba$arm_dia/366)
  prueba$c3d=cos(6*pi*prueba$arm_dia/366)
  prueba$s3d=sin(6*pi*prueba$arm_dia/366)
  prueba$c4d=cos(8*pi*prueba$arm_dia/366)
  prueba$s4d=sin(8*pi*prueba$arm_dia/366)
  prueba$c5d=cos(10*pi*prueba$arm_dia/366)
  prueba$s5d=sin(10*pi*prueba$arm_dia/366)
  
  prueba$c1h=cos(2*pi*prueba$arm_hora/24)
  prueba$s1h=sin(2*pi*prueba$arm_hora/24)
  prueba$c2h=cos(4*pi*prueba$arm_hora/24)
  prueba$s2h=sin(4*pi*prueba$arm_hora/24)
  prueba$c3h=cos(6*pi*prueba$arm_hora/24)
  prueba$s3h=sin(6*pi*prueba$arm_hora/24)
  
  #effect of COVID19
  prueba$dia_año=prueba$day_year+prueba$year*1000
  i=(as.Date("15-03-2020", "%d-%m-%Y"))
  f=(as.Date("21-06-2020", "%d-%m-%Y"))
  f_est_alar=(as.Date("9-05-2021", "%d-%m-%Y"))
  inicio=yday(i)+year(i)*1000
  final=yday(f)+year(f)*1000
  final_estado_alarma=yday(f_est_alar)+year(f_est_alar)*1000
  
  #periodo confinamiento
  prueba$conf=as.numeric(is.element(prueba$dia_año,inicio:final)) 
  #estado de alarma
  prueba$covid=as.numeric(is.element(prueba$dia_año,final:final_estado_alarma))
  
  #para guardar el data.frame
  response.hourly.itinere = prueba
  
  
  
  #Creo la variable fecha y el dia de la semana
  
  d = with(response.hourly.itinere, paste(year, day_year))
  d = as.Date(d, "%Y %j")
  response.hourly.itinere$dia=d
  h=response.hourly.itinere$hour
  d = paste(d, paste(h))
  d = as.POSIXct(d, format ="%Y-%m-%d %H")
  response.hourly.itinere$fecha=d
  response.hourly.itinere$wday=wday(response.hourly.itinere$fecha)
  response.hourly.itinere$weekday=weekdays(response.hourly.itinere$fecha)
  
  response.hourly.itinere$finde=1*( (response.hourly.itinere$wday==1)|(response.hourly.itinere$wday==7) )
  response.hourly.itinere$month=month(response.hourly.itinere$fecha)
  response.hourly.itinere$mday=mday(response.hourly.itinere$fecha)
  
  #VARIABLES SOLARES
  
  lat_zgz=010015	
  lon_zgz=413938
  
  suavizado=0.75
  aux.df=response.hourly.itinere
  
  resultados_luz_solar=datos_luz_solar_modelo(lat_zgz,lon_zgz, aux.df,suavizado)
  response.hourly.itinere=resultados_luz_solar[[1]]
  response.hourly.itinere$salida_0_15=1*(0<=(response.hourly.itinere$altitud)&(response.hourly.itinere$altitud)<=15)
  response.hourly.itinere$puesta_0_15=1*(-15<=(response.hourly.itinere$altitud)&(response.hourly.itinere$altitud)<0)
  response.hourly.itinere$noche=1*( (response.hourly.itinere$luz==FALSE) & (response.hourly.itinere$salida_0_15!=1) & (response.hourly.itinere$puesta_0_15!=1))
  response.hourly.itinere$banda_15_50=response.hourly.itinere$altitud
  
  #------------------------------------#
  subset_respuesta=subset(respuesta,respuesta$dia_año>0)
  
  # prob_acc=tapply(subset_respuesta$y, list(subset_respuesta$dia_año), sum, na.rm=TRUE)
  # summary(prob_acc)
  
  prob_leve=tapply(subset_respuesta$leve, list(subset_respuesta$dia_año), sum, na.rm=TRUE)
  prob_grave=tapply(subset_respuesta$grave, list(subset_respuesta$dia_año), sum, na.rm=TRUE)
  prob_muerte=tapply(subset_respuesta$muertos, list(subset_respuesta$dia_año), sum, na.rm=TRUE)
  
  #prob_leve=tapply(subset_respuesta$leve, list(subset_respuesta$day_year), sum, na.rm=TRUE)/13
  #prob_grave=tapply(subset_respuesta$grave, list(subset_respuesta$day_year), sum, na.rm=TRUE)/13
  #prob_muerte=tapply(subset_respuesta$muertos, list(subset_respuesta$day_year), sum, na.rm=TRUE)/13
  
  p_leve=summary(prob_leve)[4]
  p_grave=summary(prob_grave)[4]
  p_muerte=summary(prob_muerte)[4]
  
  cat(p_leve,p_grave,p_muerte,sep="&",fill=TRUE)
  
  
  
  
  basura=table(respuesta$alcance,respuesta$grave)
  basura[2,]/apply(basura,2,sum)
  
  #saveRDS(response.hourly.itinere, file = "response.hourly.itinere.rds")
  
}
  
#BLOQUE MODELO RESPUESTA POLÍGONOS
  
  {años=c(2009:2022)
  
  aux.plaza=frame_poligono(delta_poligonos,"plaza",años)
  aux.figueruelas=frame_poligono(delta_poligonos,"figueruelas",años)
  aux.cuarte=frame_poligono(delta_poligonos,"cuarte",años)
  aux.villanueva=frame_poligono(delta_poligonos,"villanueva",años)
  
  aux.plaza$ubicacion="plaza"
  aux.figueruelas$ubicacion="figueruelas"
  aux.cuarte$ubicacion="cuarte"
  aux.villanueva$ubicacion="villanueva"
  aux.frame.poligonos=rbind(aux.plaza,aux.figueruelas,aux.cuarte,aux.villanueva)
  
  aux.y=aux.frame.poligonos$ubicacion
  aux.y=as.factor(aux.y)
  levels(aux.y)=c(3,2,1,4)
  #table(aux.frame.poligonos$ubicacion, aux.y)
  aux.frame.poligonos$ubicacion=aux.y
  
  respuesta_poligono=aux.frame.poligonos
  respuesta_poligono$ubic_plaza=0+1*(aux.frame.poligonos$ubicacion==1)
  respuesta_poligono$ubic_figueruelas=0+1*(aux.frame.poligonos$ubicacion==2)
  respuesta_poligono$ubic_cuarte=0+1*(aux.frame.poligonos$ubicacion==3)
  respuesta_poligono$ubic_villanueva=0+1*(aux.frame.poligonos$ubicacion==4)
  
  
  #------------------------------------#
  respuesta_poligono$dia_año=respuesta_poligono$day_year+respuesta_poligono$year*1000
  
  subset_respuesta_poligono=subset(respuesta_poligono,respuesta_poligono$dia_año>0)
  
  # prob_acc_poligono=tapply(subset_respuesta_poligono$y, list(subset_respuesta_poligono$dia_año), sum, na.rm=TRUE)
  # summary(prob_acc_poligono)
  
  prob_leve_poligono=tapply(subset_respuesta_poligono$leve, list(subset_respuesta_poligono$dia_año), sum, na.rm=TRUE)
  prob_grave_poligono=tapply(subset_respuesta_poligono$grave, list(subset_respuesta_poligono$dia_año), sum, na.rm=TRUE)
  prob_muerte_poligono=tapply(subset_respuesta_poligono$muertos, list(subset_respuesta_poligono$dia_año), sum, na.rm=TRUE)
  
  p_leve_P=summary(prob_leve_poligono)[4]
  p_grave_P=summary(prob_grave_poligono)[4]
  p_muerte_P=summary(prob_muerte_poligono)[4]
  
  aux.df=respuesta_poligono
  d = with(aux.df, paste(year, day_year))
  d = as.Date(d, "%Y %j")
  aux.df$dia=d
  h=aux.df$hour
  d = paste(d, paste(h))
  d = as.POSIXct(d, format ="%Y-%m-%d %H")
  aux.df$aux.fecha=d
  aux.df$day=day(aux.df$aux.fecha)
  aux.df$month=month(aux.df$aux.fecha)
  respuesta_poligono=aux.df
  
  cat(p_leve_P,p_grave_P,p_muerte_P,sep="&",fill=TRUE)
  
  
}  
  
  # Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre7') # for 32-bit version


  
  