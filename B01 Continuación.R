

#------------------------------------#

# #Predict 

    {
# 
M_R_todos=glm(y~(c1d+c1h+c2h+c3h+s3d+s3h+ubic_cuarte+ubic_cuarte*s3h+ubic_figueruelas+ubic_figueruelas*c2h*c1d+ubic_figueruelas*c3h+ubic_figueruelas*c4d+ubic_figueruelas*s3d+ubic_figueruelas*s3h+ubic_villanueva),data=respuesta_poligono, family = "poisson",control = glm.control(maxit=100))
# 
# 
# aux.data=respuesta_poligono
# aux.marcador=(aux.data$hour=8)&(aux.data$day=2) 
# modelo=M_R_todos
# modelo_glm=glm_model(y~(c1d+c1h+c2h+c3h+s3d+s3h+ubic_cuarte+ubic_cuarte*s3h+ubic_figueruelas+ubic_figueruelas*c2h*c1d+ubic_figueruelas*c3h+ubic_figueruelas*c4d+ubic_figueruelas*s3d+ubic_figueruelas*s3h+ubic_villanueva), family = "poisson",control = glm.control(maxit=100))
# 
predict_model=function(aux.data,aux.marcador,modelo, modelo_glm){
# 
#   aux.predict1=predict(modelo, newdata, type="response")
#   head(aux.predict1)
# 
#   fitted_model=fit(modelo_glm,newdata)
#   aux.predict2=predict(fitted_model, simulate_pi = FALSE, uncertain = FALSE, alpha = 0.1)
#   head(aux.predict2)
#   
#   aux.x=newdata$day_year
#   aux.y=modelo
#     
#   # p1 = ggplot(aux.predict2, aes(aux.x, aux.y)) +
#   # geom_point(size = 1) +
#   # geom_line(aes(y = estimate), linewidth = 1.2) +
#   # geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
#   # geom_ribbon(aes(ymin = `lower_ci`, ymax = `upper_ci`), data = aux.predict2, alpha = 0.4) +
#   # ggtitle("Poisson regression with prediction intervals and no uncertainty in parameters", 
#   #         subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)") +
#   # coord_cartesian(ylim=c(0, 30))
# 
#   p1 = ggplot(aux.predict2, aes(aux.x, aux.y)) +
#     geom_point(size = 1) +
#     geom_line(aes(y = estimate), linewidth = 1.2) +
#     geom_ribbon(aes(ymin = `lower_ci`, ymax = `upper_ci`), data = aux.predict2, alpha = 0.4) +
#     ggtitle("Poisson regression with prediction intervals and no uncertainty in parameters", 
#             subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)") +
#     coord_cartesian(ylim=c(0, 30))
#   
# #return(head(aux.predict1))
# return(list(head(aux.predict1),head(aux.predict2),p1))
}
# 
# predict_model(aux.data,aux.marcador,modelo, modelo_glm)
# 
# aux.predict1=predict(modelo, newdata, type="response")
# head(aux.predict1)
# 
# fitted_model=fit(modelo_glm,newdata)
# aux.predict2=predict(fitted_model, simulate_pi = FALSE, uncertain = FALSE, alpha = 0.1)
# head(aux.predict2)
# 
# aux.x=newdata$day_year
# aux.y=aux.predict1
# 
# p1 = ggplot(newdata, aes(aux.x, aux.y)) +
#   geom_point(size = 1) +
#   geom_line(aes(y = y), linewidth = 1.2) +
#   geom_ribbon(aes(ymin = `lower_ci`, ymax = `upper_ci`), data = aux.predict2, alpha = 0.4) +
#   ggtitle("Poisson regression with prediction intervals and no uncertainty in parameters", 
#           subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)") +
#   coord_cartesian(ylim=c(0, 30))
# 
# 
# 
# #----------------------------------
# 
# 
# 
# df=delta
# 
# df$c1d=cos(2*pi*df$day_year/365)
# df$s1d=sin(2*pi*df$day_year/365)
# df$c2d=cos(4*pi*df$day_year/365)
# df$s2d=sin(4*pi*df$day_year/365)
# df$c3d=cos(6*pi*df$day_year/365)
# df$s3d=sin(6*pi*df$day_year/365)
# df$c4d=cos(8*pi*df$day_year/365)
# df$s4d=sin(8*pi*df$day_year/365)
# df$y=df$accidentes
# 
# aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_of_week, data=df,family = "poisson")
# summary(aux_glm)
# aux.x=df$day_year
# 
# 
# aux.predict1=predict(aux_glm, df, type="response")
# head(aux.predict1)
# 
# aux_model_glm= glm_model(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_of_week,family = "poisson")
# fitted_model=fit(aux_model_glm,df)
# aux.predict2=predict(fitted_model, simulate_pi = FALSE, uncertain = FALSE, alpha = 0.1)
# head(aux.predict2)
# 
# min=min(aux.predict2$lower_ci,na.rm = TRUE)
# max=max(aux.predict2$upper_ci,na.rm = TRUE)
# 
# 
# aux.y=summary(aux_glm)$coef[2,1]*df$c1d+
#   summary(aux_glm)$coef[3,1]*df$s1d+
#   summary(aux_glm)$coef[4,1]*df$c2d+
#   summary(aux_glm)$coef[5,1]*df$s2d+
#   summary(aux_glm)$coef[6,1]*df$c3d+
#   summary(aux_glm)$coef[7,1]*df$s3d+
#   summary(aux_glm)$coef[8,1]*df$c4d+
#   summary(aux_glm)$coef[9,1]*df$s4d
# aux.y=exp(aux.y)
# 
# aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_year, data=df,family = "poisson")
# aux.predict1=predict(aux_glm, df, type="response")
# 
# p1 = ggplot(df, aes(aux.x, aux.predict1)) +
#   geom_point(size = 1) +
#   geom_ribbon(aes(ymin = `lower_ci`, ymax = `upper_ci`), data = aux.predict2, alpha = 0.4) +
#   ggtitle("Poisson regression with prediction intervals and no uncertainty in parameters",
#           subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)")
# p1

#MEJOR SOLUCIÃ“N


df=delta
df$c1d=cos(2*pi*df$day_year/365)
df$s1d=sin(2*pi*df$day_year/365)
df$c2d=cos(4*pi*df$day_year/365)
df$s2d=sin(4*pi*df$day_year/365)
df$c3d=cos(6*pi*df$day_year/365)
df$s3d=sin(6*pi*df$day_year/365)
df$c4d=cos(8*pi*df$day_year/365)
df$s4d=sin(8*pi*df$day_year/365)

aux.x=df$day_year
df$y=df$accidentes


# aux.y=summary(aux_glm)$coef[2,1]*df$c1d+
#   summary(aux_glm)$coef[3,1]*df$s1d+
#   summary(aux_glm)$coef[4,1]*df$c2d+
#   summary(aux_glm)$coef[5,1]*df$s2d+
#   summary(aux_glm)$coef[6,1]*df$c3d+
#   summary(aux_glm)$coef[7,1]*df$s3d+
#   summary(aux_glm)$coef[8,1]*df$c4d+
#   summary(aux_glm)$coef[9,1]*df$s4d
# aux.y=exp(aux.y)

aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_year, data=df,family = "poisson")
aux_model_glm= glm_model(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_year,family = "poisson")


plot_glm_confidence=function(aux.x,aux.y,df,aux_glm,aux_model_glm){

   
  aux.predict1=predict(aux_glm, df, type="response")

  fitted_model=fit(aux_model_glm,df)
  aux.predict2=predict(fitted_model, simulate_pi = FALSE, uncertain = FALSE, alpha = 0.1)
  head(aux.predict2)
  
  min=min(aux.predict2$lower_ci,na.rm = TRUE)
  max=max(aux.predict2$upper_ci,na.rm = TRUE) 

  p1 = ggplot(df, aes(aux.x, aux.predict1)) +
    geom_point(size = 1) +
    geom_ribbon(aes(ymin = `lower_ci`, ymax = `upper_ci`), data = aux.predict2, alpha = 0.4) +
    ggtitle("Poisson regression with prediction intervals and no uncertainty in parameters",
            subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)")
  
  return(list(head(aux.predict2),p1))
}
  
plot_glm_confidence(aux.x,aux.y,df,aux_glm,aux_model_glm)



#CREANDO FUNCIÃ“N

aux.data=respuesta_poligono

        #Creo la variable fecha y el dia de la semana
aux.data$year=as.integer(aux.data$aux.yday.hour/100000)
aux.data$day_year=as.integer(aux.data$aux.yday.hour/100)-as.integer(aux.data$aux.yday.hour/100000)*1000
d = with(aux.data, paste(year, day_year))
d = as.Date(d, "%Y %j")
aux.data$dia=d
h=aux.data$hour
d = paste(d, paste(h))
d = as.POSIXct(d, format ="%Y-%m-%d %H")
aux.data$fecha=d
aux.data$weekday=wday(aux.data$fecha)

plot_conf_int=function(aux.df,aux.x){
  

# aux.frame=newdata %>% group_by(ubicacion,day_year) %>% summarise_each(mean, -aux.yday.hour)
# newdata=aux.frame

# aux_glm=glm(y~(c1d+c1h+c2h+c3h+s3d+s3h+ubic_cuarte+ubic_cuarte*s3h+ubic_figueruelas+ubic_figueruelas*c2h*c1d+ubic_figueruelas*c3h+ubic_figueruelas*c4d+ubic_figueruelas*s3d+ubic_figueruelas*s3h+ubic_villanueva),data=newdata, family = "poisson",control = glm.control(maxit=100))
# aux_model_glm=glm_model(y~(c1d+c1h+c2h+c3h+s3d+s3h+ubic_cuarte+ubic_cuarte*s3h+ubic_figueruelas+ubic_figueruelas*c2h*c1d+ubic_figueruelas*c3h+ubic_figueruelas*c4d+ubic_figueruelas*s3d+ubic_figueruelas*s3h+ubic_villanueva), family = "poisson",control = glm.control(maxit=100))

# aux_glm=glm(y~(c1d+c1h+c2h+c3h+s3d+s3h+ubic_cuarte+ubic_cuarte),data=newdata, family = "poisson",control = glm.control(maxit=100))
# aux_model_glm=glm_model(y~(c1d+c1h+c2h+c3h+s3d+s3h+ubic_cuarte), family = "poisson",control = glm.control(maxit=100))

aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=aux.df,family = "poisson")
aux_model_glm= glm_model(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d,family = "poisson")

#aux.y=summary(aux_glm)$coef[i,1]*df[i]+aux.y

# efect_0=summary(aux_glm)$coef[1]
# aux.y=0+0*newdata$year+efect_0
# 
# for (i.coeff in 1:4){
#   aux.variable=paste("c",i.coeff,"d",sep = "",collapse = "")
#   aux.position.armonics=grepl(c(aux.variable),rownames(summary(aux_glm)$coef))
#   if(sum(aux.position.armonics)>0){
#     aux.y_c=(summary(aux_glm)$coef[aux.position.armonics,1])[1]*newdata[[aux.variable]]
#     aux.y=aux.y+aux.y_c}
# }
# aux.y=aux.y
# aux.y=exp(aux.y)
# #aux.y.plot_0=exp(tapply(aux.y, aux.x, mean))
# 
# aux.df=0
# for (i in 2:length(newdata)){
#   aux.df[,i]=tapply(newdata[,i], aux.x, mean)
#   
# }
# newdata=prueba

# newdata=tapply(aux.y, list(aux.x), sum, na.rm=TRUE)


aux.predict1=predict(aux_glm, aux.df, type="response")

fitted_model=fit(aux_model_glm,aux.df)
aux.predict2=predict(fitted_model, simulate_pi = FALSE, uncertain = FALSE, alpha = 0.1)
head(aux.predict2)

min=min(aux.predict2$lower_ci,na.rm = TRUE)
max=max(aux.predict2$upper_ci,na.rm = TRUE) 

p1 = ggplot(aux.df, aes(aux.x, aux.predict1)) +
  geom_point(size = 1) +
  geom_ribbon(aes(ymin = `lower_ci`, ymax = `upper_ci`), data = aux.predict2, alpha = 0.4) +
  ggtitle("Poisson regression with prediction intervals",
          subtitle = "Model fit (black line), with parametric intervals (dark gray)")
p1

return(p1)

}

#aux.marcador=(aux.data$hour==10)&(aux.data$weekday==2)&(aux.data$ubicacion==4)
aux.marcador=(aux.data$hour==10)&(aux.data$ubicacion==3)
#aux.marcador=(aux.data$hour==10)&(aux.data$weekday==2)

newdata=aux.data[aux.marcador,]
aux.x=newdata$day_year
aux.y=newdata$y



plot_conf_int(newdata,aux.x)

#dev.print(pdf, 'Graf_INt_Confianza.pdf' ,  height=10, width=10 )












#

# p1 = ggplot(aux.predict2, aes(x, y)) +
#   geom_point(size = 1) +
#   geom_line(aes(y = estimate), size = 1.2)+
#   geom_ribbon(aes(ymin = `lower_ci`, ymax = `upper_ci`), data = aux.predict2, alpha = 0.4) +
#   ggtitle("Poisson regression with prediction intervals and no uncertainty in parameters", 
#           subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)") 
# #+  coord_cartesian(ylim=c(min*0,8, max*1,2))

# aux.predict1=predict(aux_glm, df, type="response")
# p1 = ggplot(df, aes(aux.x, aux.predict1)) +
#   geom_point(size = 1,aes(y=estimate), data = aux.predict2) +
#   geom_ribbon(aes(ymin = `lower_ci`, ymax = `upper_ci`), data = aux.predict2, alpha = 0.4) +
#   ggtitle("Poisson regression with prediction intervals and no uncertainty in parameters",
#           subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)")
# p1



# x=df$day_year
# y=df$y
# df2=data.frame(x,y)
# fit = glm(y ~ x, data=df2 , family = poisson(link = "log"))
# dat1 = add_pi(df2, fit, names = c("lpb", "upb"), alpha = 0.1, nsims = 200)
# 
# p1 = ggplot(dat1, aes(aux.x, y)) +
#   geom_point(size = 1) +
#   geom_line(aes(y = pred), size = 1.2) +  geom_ribbon(aes(ymin = lpb, ymax = upb), alpha = 0.4) +
#   geom_ribbon(aes(ymin = `lower_ci`, ymax = `upper_ci`), data = aux.predict2, alpha = 0.4) +
#   ggtitle("Poisson regression with prediction intervals and no uncertainty in parameters",
#           subtitle = "Model fit (black line), with bootstrap intervals (gray), parametric intervals (dark gray)")

# aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=aux_collapse, family = "poisson")
# aux.x=aux_collapse$w_day
# aux.y=predict(aux_glm,type = "response")
# plot(aux.x, aux.y,cex.axis=2.5)




#FunciÃ³n para cualquier modelo----NO FUNCIONA NECESITO QUE EN LA FORMULA EN VEZ DE a APAREZCAN SUS VALORES

# a=names(M_R_todos$coefficients[-1])
# a=gsub(":", "*", a)
# a=paste(a,collapse = "+")
# modelo_glm=glm_model(y~(a), family = "poisson",control = glm.control(maxit=100))
# 
# newdata=aux.data[aux.marcador]
# fitted_model=fit(modelo_glm,newdata)
# aux.predict2=predict(fitted_model, simulate_pi = FALSE, uncertain = FALSE, alpha = 0.1)
# head(aux.predict2)


}

#------------------------------------#

#LUZ SOLAR



#Mediante la librer?a suncalc se obtienen las condiciones lum?nicas m?s relevantes.
#Para ello se emplea la posici?n georreferenciada de la estaci?n meteorol?gica de Zaragoza
    
#INFORMACI?N RELATIVA A LA ESTACI?N METEREOL?GICA DEL AEROPUERTO DE ZARAGOZA:
    #9434	ZARAGOZA (AEROPUERTO)	010015W	413938	ETRS89	ZARAGOZA	263	890	1941	2015	74	1	1941 2015	 #https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjo5trinJ_9AhWvTKQEHZP7BCUQFnoECBIQAQ&url=https%3A%2F%2Fwww.aragon.es%2Fdocuments%2F20127%2F674325%2Festaciones_meteorologicas.xls%2Fe7bcae4e-965f-f6ea-41de-f17ff0923019&usg=AOvVaw2vzX9INo2G-YKqhvouKw7S
    
  lat_zgz=010015	
  lon_zgz=413938


suavizado=0.75
aux.df=respuesta

resultados_luz_solar=datos_luz_solar(lat_zgz,lon_zgz, aux.df,suavizado)
respuesta=resultados_luz_solar[[1]]

#datos_luz_solar(lat_zgz,lon_zgz, aux.df,suavizado)
#datos_luz_solar_poligonos(lat_zgz,lon_zgz, aux.df,suavizado)

#Se observa como se producen 3312 accidentes en situaciones donde se puede haber llegado un deslumbramiento.


#EN LOS POLIGONOS:

ida_vuelta=respuesta_poligono$hour
ida_vuelta=cut(ida_vuelta, breaks=c(0,1,5,7,9,13,15,17,21,23))
levels(ida_vuelta)
levels(ida_vuelta)=c("SAL","","IDA","SAL","","IDA","SAL","","IDA")
respuesta_poligono$ida_vuelta=ida_vuelta
respuesta_poligono$ida_vuelta=replace(respuesta_poligono$ida_vuelta, is.na(respuesta_poligono$ida_vuelta), "SAL")


datos_luz_solar_poligonos=function(lat_zgz,lon_zgz,aux.df,suaviz) {
#   
#   #Se obtienen las fechas completas (año, mes, dia y hora) para el periodo de años desde el 2009 a 2022.
#   aux.df$y[aux.df$y>=30]=1
#   
#   aux.df$year=as.integer(aux.df$aux.yday.hour/100000)
#   aux.df$day_year=as.integer(aux.df$aux.yday.hour/100)-as.integer(aux.df$aux.yday.hour/100000)*1000
#   d = with(aux.df, paste(year, day_year))
#   d = as.Date(d, "%Y %j")
#   aux.df$dia=d
#   h=aux.df$hour
#   d = paste(d, paste(h))
#   d = as.POSIXct(d, format ="%Y-%m-%d %H")
#   aux.df$fecha=d
#   
#   luz_solar=getSunlightTimes(aux.df$dia, lat = lat_zgz, lon = lon_zgz)
#   
#   aux.df$salida=luz_solar$sunrise
#   aux.df$puesta=luz_solar$sunset
#   
#   aux.df$luz=(aux.df$salida < aux.df$fecha) & (aux.df$fecha < aux.df$puesta)
#   # aux.df$un_accidente=aux.df$y
#   # aux.df$un_accidente[aux.df$un_accidente>=1]=1
#   tabla_luz=table(aux.df$y,aux.df$luz)
#   
#   
#   #Se ha tomado como criterio de luz natural suficiente las horas situadas entre la salida y puesta del sol.
#   
#   #Se observa como se obtienen un nÃºmero similar de casos ocurridos bajo condiciones lumÃ­nicas insuficientes, 6535, 
#   #que frente aquellos sucesos donde exist?a luz natural, 7531.
#   #Cabe mecionar que este estudio solo busca determinar las condiciones luminicas naturales, 
#   #es decir, no tiene en consideraciÃ³n otras fuentes de luz artificiales, como bien pueden ser la iluminaciÃ³n pblica, o la del propio vehÃ­culo.
#   
#   
#   #ObtenciÃ³n del deslumbramiento solar
#   
#   #SegÃºn An Empirical Investigation of the Impacts of Sun-Related Glare on Traffic Flow,
#   #el deslumbramiento solar es un suceso relevante en el trÃ¡fico, produciendose principalmente con angulos de 0 a 15Âº.
#   #https://d1wqtxts1xzle7.cloudfront.net/41285459/An_Empirical_Investigation_of_the_Impact20160117-9818-1fsc0k6-libre.pdf?1453033601=&response-content-disposition=inline%3B+filename%3DAn_Empirical_Investigation_of_the_Impact.pdf&Expires=1676734006&Signature=AcpBMWQcQipSh2akKQ0~nkP0-LqrdVb3CuSqL63m7ZfJ6~VJqrcSb8x~Wxn2cb0zVlefFm5jpkQVtVR-j85bncuwz6RRJir4JfHvHRPTJR-FRwaZfDOZb4-JwYUM6rCy~5MhBcMwpxkbaRZNdQkvCJZc-RPB-uLp6rO1huYLYqRtf~DVn1LR1usvaYPrTVzC5VCN2xUeFIavcnASODT~~WwYxpTwVEmsyyj1~gUCtSl5s6JFAVLOv~79NhGIiAMBozWa3~yyzUwGu-HGcZy~C1FPaqnZqp-IpWBghyEOOWig-tGwT2S15B0atNwQUVXAVXWPzm8ztFXA5qdN3wPr3w__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA
#   
#   angulo_solar=getSunlightPosition(aux.df$fecha, lat = lat_zgz, lon = lon_zgz)
#   angulo_solar$altitud=angulo_solar$altitude*180/pi
#   
#   aux.df$altitud=angulo_solar$altitud
#   
#   aux.df$deslumbramiento=(0 <= aux.df$altitud) & (aux.df$altitud <= 15) & (aux.df$luz=TRUE)
#   tabla_deslumb=table(aux.df$y,aux.df$deslumbramiento)
#   
#   acc_deslum=aggregate(aux.df$y , by=list(round(aux.df$altitud,0)) , FUN=sum, na.rm=TRUE)
#   
#   
#   acc_deslum=as.data.frame(acc_deslum)
#   colnames(acc_deslum)=c("Altitud","NÂº_accidentes")
#   shapes = c(16:(15+length(levels(aux.df$ubicacion))))
#   # shapes = shapes[as.numeric(aux.df$ubicacion)]
#   aux.plot.luz=plot(acc_deslum, pch=shapes, col=aux.df$ida_vuelta)
#   legend(x = "topright", legend=levels(aux.df$ida_vuelta), pch=16, col=unique(aux.df$ida_vuelta))
#   
#   
#   x=acc_deslum[,1]
#   y=acc_deslum[,2]
#   
#   if (missing(suaviz)){suaviz=0.75}
#   # En caso de no dar un indice de suavizado se toma por defecto 0.75, equivalente al default de loess.
#   
#   aux.loess.plot=loess(y ~ x,data=acc_deslum, span=suaviz)
#   lines(acc_deslum$Altitud,aux.loess.plot$fitted,col="blue",lwd=3)                         
#   abline(v=c(-15,15), col="red", lwd=2)
#   
#   #Si se representa el angulo solar desde 0 a pi/2
#   
#   acc_deslum_posit=aggregate(aux.df$y , by=list(abs(round(aux.df$altitud,0))), FUN=sum, na.rm=TRUE)
#   #acc_deslum_posit=aggregate(aux.df$y~abs(round(aux.df$altitud,0))+aux.df$ubicacion, FUN=sum, na.rm=TRUE)
#   
#   acc_deslum_posit=as.data.frame(acc_deslum_posit)
#   colnames(acc_deslum_posit)=c("Altitud","NÂº_accidentes")
#   shapes = c(16:(15+length(levels(aux.df$ubicacion))))
#   # shapes = shapes[as.numeric(aux.df$ubicacion)]
#   aux.plot.luz.posit=plot(acc_deslum_posit, pch=shapes, col=aux.df$ida_vuelta)
#   legend(x = "topright", legend=levels(aux.df$ida_vuelta), pch=16, col=unique(aux.df$ida_vuelta))
#   
#   x=acc_deslum_posit[,1]
#   y=acc_deslum_posit[,2]
#   
#   aux.loess.plot=loess(y ~ x,data=acc_deslum_posit, span=suaviz)
#   lines(acc_deslum_posit$Altitud,aux.loess.plot$fitted,col="blue",lwd=3)                         
#   abline(v=15, col="red", lwd=2)
#   
#   return(list("Accidentes con luz natural",tabla_luz,"Accidentes con deslumbramiento",tabla_deslumb,aux.plot.luz,aux.plot.luz.posit))
#   
} 
# 
# aux.df=respuesta_poligono
# datos_luz_solar_poligonos(lat_zgz,lon_zgz, aux.df,suavizado)
datos_luz_solar_CADA_poligono=function(lat_zgz,lon_zgz,aux.df,suaviz,tamaño) {
#   
#   #Se obtienen las fechas completas (año, mes, dia y hora) para el periodo de años desde el 2009 a 2022.
#   aux.df$y[aux.df$y>=30]=1
#   
#   aux.df$year=as.integer(aux.df$aux.yday.hour/100000)
#   aux.df$day_year=as.integer(aux.df$aux.yday.hour/100)-as.integer(aux.df$aux.yday.hour/100000)*1000
#   d = with(aux.df, paste(year, day_year))
#   d = as.Date(d, "%Y %j")
#   aux.df$dia=d
#   h=aux.df$hour
#   d = paste(d, paste(h))
#   d = as.POSIXct(d, format ="%Y-%m-%d %H")
#   aux.df$fecha=d
#   
#   luz_solar=getSunlightTimes(aux.df$dia, lat = lat_zgz, lon = lon_zgz)
#   
#   aux.df$salida=luz_solar$sunrise
#   aux.df$puesta=luz_solar$sunset
#   
#   aux.df$luz=(aux.df$salida < aux.df$fecha) & (aux.df$fecha < aux.df$puesta)
#   # aux.df$un_accidente=aux.df$y
#   # aux.df$un_accidente[aux.df$un_accidente>=1]=1
#   tabla_luz=table(aux.df$y,aux.df$luz)
#   
#   
#   #Se ha tomado como criterio de luz natural suficiente las horas situadas entre la salida y puesta del sol.
#   
#   #Se observa como se obtienen un nÃºmero similar de casos ocurridos bajo condiciones lumÃ­nicas insuficientes, 6535, 
#   #que frente aquellos sucesos donde exist?a luz natural, 7531.
#   #Cabe mecionar que este estudio solo busca determinar las condiciones luminicas naturales, 
#   #es decir, no tiene en consideraciÃ³n otras fuentes de luz artificiales, como bien pueden ser la iluminaciÃ³n pblica, o la del propio vehÃ­culo.
#   
#   
#   #ObtenciÃ³n del deslumbramiento solar
#   
#   #SegÃºn An Empirical Investigation of the Impacts of Sun-Related Glare on Traffic Flow,
#   #el deslumbramiento solar es un suceso relevante en el trÃ¡fico, produciendose principalmente con angulos de 0 a 15Âº.
#   #https://d1wqtxts1xzle7.cloudfront.net/41285459/An_Empirical_Investigation_of_the_Impact20160117-9818-1fsc0k6-libre.pdf?1453033601=&response-content-disposition=inline%3B+filename%3DAn_Empirical_Investigation_of_the_Impact.pdf&Expires=1676734006&Signature=AcpBMWQcQipSh2akKQ0~nkP0-LqrdVb3CuSqL63m7ZfJ6~VJqrcSb8x~Wxn2cb0zVlefFm5jpkQVtVR-j85bncuwz6RRJir4JfHvHRPTJR-FRwaZfDOZb4-JwYUM6rCy~5MhBcMwpxkbaRZNdQkvCJZc-RPB-uLp6rO1huYLYqRtf~DVn1LR1usvaYPrTVzC5VCN2xUeFIavcnASODT~~WwYxpTwVEmsyyj1~gUCtSl5s6JFAVLOv~79NhGIiAMBozWa3~yyzUwGu-HGcZy~C1FPaqnZqp-IpWBghyEOOWig-tGwT2S15B0atNwQUVXAVXWPzm8ztFXA5qdN3wPr3w__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA
#   
#   angulo_solar=getSunlightPosition(aux.df$fecha, lat = lat_zgz, lon = lon_zgz)
#   angulo_solar$altitud=angulo_solar$altitude*180/pi
#   
#   aux.df$altitud=angulo_solar$altitud
#   
#   aux.df$deslumbramiento=(0 <= aux.df$altitud) & (aux.df$altitud <= 15) & (aux.df$luz=TRUE)
#   tabla_deslumb=table(aux.df$y,aux.df$deslumbramiento)
#   
#   acc_deslum=aggregate(aux.df$y, by=list(round(aux.df$altitud,0)) , FUN=sum, na.rm=TRUE)
#   acc_deslum=as.data.frame(acc_deslum)
#   colnames(acc_deslum)=c("Altitud","NÂº_accidentes")  
#   x=acc_deslum[,1]
#   y=acc_deslum[,2]  
#   
#   aux.plot.luz=plot(acc_deslum , pch=16, col=aux.df$ida_vuelta)
#   if (missing(tamaño)){tamaño=1}
#   legend(x = "topright", legend=levels(aux.df$ida_vuelta), pch=16,cex=tamaño, col=unique(aux.df$ida_vuelta))
#   
#   
#   if (missing(suaviz)){suaviz=0.75}
#   # En caso de no dar un indice de suavizado se toma por defecto 0.75, equivalente al default de loess.
#  
#   #Tendencia global
#   aux.loess.plot=loess(y ~ x,data=acc_deslum, span=suaviz)
#   lines(acc_deslum$Altitud,aux.loess.plot$fitted,col="blue",lwd=3)                         
#   abline(v=c(-15,15), col="red", lwd=2)        
#   
#   
#   #Tendencia SALIDA
#   
#   aux.tend=aux.df[aux.df$ida_vuelta == "SAL",]
#   acc_deslum_SALIDA=aggregate(aux.tend$y, by=list(round(aux.tend$altitud,0)) , FUN=sum, na.rm=TRUE)
#   acc_deslum_SALIDA=as.data.frame(acc_deslum_SALIDA)
#   colnames(acc_deslum_SALIDA)=c("Altitud","NÂº_accidentes")  
#   x=acc_deslum_SALIDA[,1]
#   y=acc_deslum_SALIDA[,2]
#   
#   aux.loess.plot=loess(y ~ x,data=acc_deslum_SALIDA, span=0.25)
#   lines(acc_deslum_SALIDA$Altitud,aux.loess.plot$fitted,col="black",lwd=1) 
#  
#   
#   #Tendencia IDA
#   
#   aux.tend=aux.df[aux.df$ida_vuelta == "IDA",]
#   acc_deslum_IDA=aggregate(aux.tend$y, by=list(round(aux.tend$altitud,0)) , FUN=sum, na.rm=TRUE)
#   acc_deslum_IDA=as.data.frame(acc_deslum_IDA)
#   colnames(acc_deslum_IDA)=c("Altitud","NÂº_accidentes")  
#   x=acc_deslum_IDA[,1]
#   y=acc_deslum_IDA[,2]
#   
#   aux.loess.plot=loess(y~ x,data=acc_deslum_IDA , span=suaviz)
#   lines(acc_deslum_IDA$Altitud,aux.loess.plot$fitted,col="green",lwd=1)                         
# 
# 
# #Si se representa el angulo solar desde 0 a pi/2  
#   
#   acc_deslum_posit=aggregate(aux.df$y , by=list(abs(round(aux.df$altitud,0))), FUN=sum, na.rm=TRUE)
#   #acc_deslum_posit=aggregate(aux.df$y~abs(round(aux.df$altitud,0))+aux.df$ubicacion, FUN=sum, na.rm=TRUE)
#   
#   acc_deslum_posit=as.data.frame(acc_deslum_posit)
#   colnames(acc_deslum_posit)=c("Altitud","NÂº_accidentes")
#   aux.plot.luz.posit=plot(acc_deslum_posit, pch=16, col=aux.df$ida_vuelta)
#   legend(x = "topright", legend=levels(aux.df$ida_vuelta), pch=16,cex=tamaño, col=unique(aux.df$ida_vuelta))
#   
#   x=acc_deslum_posit[,1]
#   y=acc_deslum_posit[,2]
#   
#   #Tendencia global
#   
#   aux.loess.plot=loess(y ~ x,data=acc_deslum_posit, span=suaviz)
#   lines(acc_deslum_posit$Altitud,aux.loess.plot$fitted,col="blue",lwd=3)                         
#   abline(v=15, col="red", lwd=2)
#   
#   #Tendencia SALIDA
# 
#   aux.tend=aux.df[aux.df$ida_vuelta == "SAL",]
#   acc_deslum_posit_SALIDA=aggregate(aux.tend$y, by=list(abs(round(aux.tend$altitud,0))) , FUN=sum, na.rm=TRUE)
# 
#   acc_deslum_posit_SALIDA=as.data.frame(acc_deslum_posit_SALIDA)
#   colnames(acc_deslum_posit_SALIDA)=c("Altitud","NÂº_accidentes")
#   x=acc_deslum_posit_SALIDA[,1]
#   y=acc_deslum_posit_SALIDA[,2]
# 
#   aux.loess.plot=loess(y ~ x,data=acc_deslum_posit_SALIDA, span=suaviz)
#   lines(acc_deslum_posit_SALIDA$Altitud,aux.loess.plot$fitted,col="black",lwd=1)
# 
# 
#   #Tendencia IDA
# 
#   aux.tend=aux.df[aux.df$ida_vuelta == "IDA",]
#   acc_deslum_posit_IDA=aggregate(aux.tend$y, by=list(abs(round(aux.tend$altitud,0))) , FUN=sum, na.rm=TRUE)
#   acc_deslum_posit_IDA=as.data.frame(acc_deslum_posit_IDA)
#   colnames(acc_deslum_posit_IDA)=c("Altitud","NÂº_accidentes")
#   x=acc_deslum_posit_IDA[,1]
#   y=acc_deslum_posit_IDA[,2]
# 
#   aux.loess.plot=loess(y~ x,data=acc_deslum_posit_IDA , span=suaviz)
#   lines(acc_deslum_posit_IDA$Altitud,aux.loess.plot$fitted,col="green",lwd=1)
# 
# 
#   
#   
#   return(list("Accidentes con luz natural",tabla_luz,"Accidentes con deslumbramiento",tabla_deslumb,aux.plot.luz,aux.plot.luz.posit))
#   
 } 
# 
# par(mfrow=c(4,2))
# aux.df=respuesta_poligono
# 
# for (i in 1:length(levels(aux.df$ubicacion))) {
#   aux.variable=respuesta_poligono$ubicacion==i
#   aux.df=respuesta_poligono[aux.variable,]
#   
#   tamaño=1/length(levels(aux.df$ubicacion))
#   datos_luz_solar_CADA_poligono(lat_zgz,lon_zgz, aux.df,suavizado)
 #}
# 
# par(mfrow=c(1,1))

par(mfrow=c(1,2))
aux.df=respuesta_poligono

for (i in 1:length(levels(aux.df$ubicacion))) {
  aux.variable=respuesta_poligono$ubicacion==i
  aux.df=respuesta_poligono[aux.variable,]
  
  tamaño=1/length(levels(aux.df$ubicacion))
  datos_luz_solar_ida_salida_poligono(lat_zgz,lon_zgz, aux.df,suavizado)
}

par(mfrow=c(1,1))

    #------------------------------------#

#Creacion 4 variables solares y modelos solares sencillos
  
    {

respuesta$salida_0_15=1*(0<=(respuesta$altitud)&(respuesta$altitud)<=15)
respuesta$puesta_0_15=1*(-15<=(respuesta$altitud)&(respuesta$altitud)<0)
respuesta$noche=1*( (respuesta$luz==FALSE) & (respuesta$salida_0_15!=1) & (respuesta$puesta_0_15!=1))
#respuesta$banda_15_50=1-(respuesta$salida_0_15+respuesta$puesta_0_15+respuesta$noche) #No ya que entonces las 4 variables suman 1 siendo redundantes de cara al estduio de su efecto en conjunto sobre el modelo
respuesta$banda_15_50=respuesta$altitud

#Anteriores modelos

M8= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+itinere+alcance, data=respuesta, family = "poisson",control = glm.control(maxit=100))
IM8= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*itinere*alcance,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#Nuevos modelos con variables solares

M9_solar= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)+(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+(lluvia+itinere+alcance)+(salida_0_15+puesta_0_15+noche+banda_15_50), data=respuesta, family = "poisson",control = glm.control(maxit=100))
#IM8_solar= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(lluvia*itinere*alcance)*(salida_0_15*puesta_0_15*noche*banda_15_50),data=respuesta, family = "poisson",control = glm.control(maxit=100))
#El tiempo de computo es inadecuado se proponen dos soluciones
  
  IM8_solar_a= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+(lluvia*itinere*alcance)+(salida_0_15*puesta_0_15*noche*banda_15_50),data=respuesta, family = "poisson",control = glm.control(maxit=100))
  IM8_solar_b= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(lluvia+itinere+alcance)*(salida_0_15+puesta_0_15+noche+banda_15_50),data=respuesta, family = "poisson",control = glm.control(maxit=100))
  
  my_list_Modelos_solar=list(M8,IM8,M9_solar,IM8_solar_a,IM8_solar_b)
  obten_modelos(my_list_Modelos_solar)
  
  #El modelo con menor AIC es IM8 (75294) seguido de IM8_solar_b (76674), por lo que no se aprecia una mejorÃ­a repecto de los modelos originales
 
#Para observar de mejor forma el efecto de las variables solares se eliminan las variables categoricas
  
  M8_armonicos= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=respuesta, family = "poisson",control = glm.control(maxit=100))

  #Nuevos modelos con variables solares
  
  M9_armonicos_solar= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)+(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+(salida_0_15+puesta_0_15+noche+banda_15_50), data=respuesta, family = "poisson",control = glm.control(maxit=100))
  #IM8_solar= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(lluvia*itinere*alcance)*(salida_0_15*puesta_0_15*noche*banda_15_50),data=respuesta, family = "poisson",control = glm.control(maxit=100))
  #El tiempo de computo es inadecuado se proponen dos soluciones
  
  IM8_armonicos_solar_a= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+(salida_0_15*puesta_0_15*noche*banda_15_50),data=respuesta, family = "poisson",control = glm.control(maxit=100))
  IM8_armonicos_solar_b= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(salida_0_15+puesta_0_15+noche+banda_15_50),data=respuesta, family = "poisson",control = glm.control(maxit=100))
  
  my_list_Modelos_solar=list(M8_armonicos,M9_armonicos_solar,IM8_armonicos_solar_a,IM8_armonicos_solar_b)
  obten_modelos(my_list_Modelos_solar)
  
  #SÃ­ se aprecia una mejorÃ­a en cuanto al AIC ya que el modelo con menor AIC es M9_armonicos_solar(103072), frente al segundo mejor IM8_armonicos_solar_b (110238)
  #Atendiendo a los mejores modelos del anterior apartado,IM8 (110776) e IM8_solar_b (110099), se observa una mejorÃ­a.

  
  #IM_step_solar=step(glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)+(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+(salida_0_15+puesta_0_15+noche+banda_15_50), data=respuesta, family = "poisson",control = glm.control(maxit=100)))

    }

#------------------------------------#

#'[LLUVIA] 

#ANALISIS ADECUACION ANALISIS TEXTUAL LLUVIA

#Se ha procedido a la realizaciÃ³n de una funciÃ³n que nos indique la adecuaciÃ³n del analisis textual;
# en busqueda de obtener los accidentes producidos bajo situaciones atmosfÃ©ricas adversas.

#Solo se emplean los datos de accidentes


aux.lluvia=read.csv("Lluvia_real.txt", skip=22, sep = ",")
datos_reales=aux.lluvia
datos_extraidos=respuesta 
acierto_lluvia_aero(datos_reales,datos_extraidos)
tabla_acierto_lluvia_aero=acierto_lluvia_aero(datos_reales,datos_extraidos)
obten_tabla(tabla_acierto_lluvia_aero[[1]])
obten_tabla(tabla_acierto_lluvia_aero[[2]])


aux.lluvia=read.csv("Lluvia_real.txt", skip=22, sep = ",")
datos_reales=aux.lluvia
poligono=respuesta_poligono$ubicacion==1
datos_extraidos=respuesta_poligono[poligono,]
acierto_lluvia_polig(datos_reales,datos_extraidos)
tabla_acierto_lluvia_polig=acierto_lluvia_polig(datos_reales,datos_extraidos)
obten_tabla(tabla_acierto_lluvia_polig[[1]])
obten_tabla(tabla_acierto_lluvia_polig[[2]])

#No tiene sentido comparar cuando llovia en la estacion metereologica con la base de accidentes, 
#ya que no solo puede ser que en el lugar exacto del accidente no hubiera precipitacion, 
#si no que, solo se recoge si ha llovido si ha habido accidente.

# Si se comparan los accidentes producidos bajo lluvia y si en la estacion metereologica se recoge lluvia, 
#se puede observar como prÃ¡cticamente existen la misma cantidad de casos en los cuales ambas bases coinciden que en los casos que difieren. 
#Este hecho se puede deber a dos motivos: el primero de ellos es una deficiente cumplimentaciÃ³n del campo que recoge el accidente o de del analisis textual. 
#El segundo motivo se debe a la diferencia de localizaciones entre la estaciÃ³n meterelÃ³gica y el lugar del accidente, 
#ya que dada suficiente distancia el fenomeno metereolÃ³gico puede estar localizado en el aeropuerto de Zaragoza, 
#y no darse en el lugar del accidente. 


#AEMET


# browseURL("https://opendata.aemet.es/centrodedescargas/obtencionAPIKey")
# aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJpbmlnb2Vsdmlyb211bm96QGdtYWlsLmNvbSIsImp0aSI6ImJjYjBlMGUzLWFiMzktNDgzNC1iMjk3LWU2ZTlmNTYxM2QxOSIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNjg2MTQ4NDA3LCJ1c2VySWQiOiJiY2IwZTBlMy1hYjM5LTQ4MzQtYjI5Ny1lNmU5ZjU2MTNkMTkiLCJyb2xlIjoiIn0.cQ2aDE-p76E3WfoL29OGb-LfYaRcBqVZX3fmFuPX4J8",install = TRUE)
aemet_api_key("MY API KEY",install = TRUE)
#aemet_api_key("MY API KEY",install = TRUE,overwrite=TRUE)

stations = aemet_stations()
stations[stations$provincia=="ZARAGOZA",]
station = "9434" # Zaragoza Aeropuerto
station = "9434P" # Zaragoza Valdespartera

lluvia_datos_aemet =  aemet_daily_clim(station, start = "2009-01-01", end = "2022-12-31")

    #------------------------------------#


#CONSTRUCCION DE BASE DE DATOS DE LLUVIA HORARIA ENTRE LAS BASES:
  # -Aeropuerto
  # -La Muela
  # -La Loteta

{muela=read.csv("muela_dia.csv", check.names = F, sep = ";")
muela=muela[,c(1,5)]
muela$fecha=format(as.Date(muela$fecha), "%Y-%m-%d")
colnames(muela)=c("lluvia_mue","fecha")

loteta=read.csv("loteta_dia.csv", check.names = F, sep = ";")
loteta=loteta[,c(1,5)]
loteta$fecha=format(as.Date(loteta$fecha), "%Y-%m-%d")
colnames(loteta)=c("lluvia_lote", "fecha")

aeropuerto=aux.lluvia
aeropuerto=aeropuerto[,3:4]
colnames(aeropuerto)=c("fecha","lluvia")
aeropuerto$dia_hora=ymd(aeropuerto$fecha)
colnames(aeropuerto)=c("fecha_num", "lluvia_aero", "fecha")
aeropuerto$fecha=as.character(aeropuerto$fecha)

  # min_mue=muela$fecha[1]
  # min_lote=loteta$fecha[1]
  # min_aero=aeropuerto$fecha[1]
  # inicio=max(min_mue,min_lote,min_aero)
  # fin=("2022-12-31")

df_list = list(muela, loteta, aeropuerto)      
base_lluvia=df_list %>% reduce(full_join, by='fecha')
# base_lluvia[is.na(base_lluvia)] = 0
base_lluvia=na.omit(base_lluvia)#Solo me quedo con las filas que contengan informaciÃ³n en las 3 bases
base_lluvia=base_lluvia[,c(4,2,5,1,3)]
base_lluvia$lluvia_mue=as.numeric(gsub(",", ".", gsub("\\.", "", base_lluvia$lluvia_mue)))
base_lluvia$lluvia_lote=as.numeric(gsub(",", ".", gsub("\\.", "", base_lluvia$lluvia_lote)))
base_lluvia$total=rowSums((base_lluvia[,c(3:5)]), na.rm=TRUE) 

base_lluvia$year=year(base_lluvia$fecha)
base_lluvia$yday=yday(base_lluvia$fecha)
base_lluvia$aux.yday=(base_lluvia$year*1000+base_lluvia$yday)

} #Se construye la base de datos con las 3 bases y se le da formato
head(base_lluvia)
dim(base_lluvia)
table(base_lluvia$year) 

inicio=base_lluvia$aux.yday[1]

#Se construye la funcion que nos indique el acierto de la extraccion textual


datos_reales=base_lluvia
datos_extraidos=respuesta


acierto_lluvia(base_lluvia,datos_extraidos)
tabla_acierto_lluvia=acierto_lluvia(base_lluvia,datos_extraidos)
obten_tabla(tabla_acierto_lluvia[[1]])  
obten_tabla(tabla_acierto_lluvia[[2]])  

  # years=c(2009:2022)
  # aeropuerto=subset(aeropuerto, year %in% years)  
  # aeropuerto$lluvia_real01=as.integer(as.logical(aeropuerto$lluvia))
  # dim(aeropuerto)


    #------------------------------------#

#Mediante las bases 15 minutales

datos_reales=bases4
datos_extraidos=respuesta

acierto_lluvia_horaria(datos_reales,datos_extraidos)
tabla_acierto_lluvia_horaria=acierto_lluvia_horaria(datos_reales,datos_extraidos)
obten_tabla(tabla_acierto_lluvia_horaria[[1]])  
obten_tabla(tabla_acierto_lluvia_horaria[[2]])  

    #------------------------------------#

#Decalaje

decalaje=1 #Tiempo que se extiende la lluvia, por ejemplo si llueve entre las 12-13:00, con decalaje=1 la lluvia durarÃ¡ entre las 12-14:00.
acierto_lluvia_decalada(bases4,respuesta,decalaje)
acierto_lluvia_decalada(bases3,respuesta,decalaje)

tabla_acierto_lluvia_decalada=acierto_lluvia_decalada(bases4,respuesta,decalaje)
obten_tabla(tabla_acierto_lluvia_decalada[[1]])
obten_tabla(tabla_acierto_lluvia_decalada[[2]])  

tabla_acierto_lluvia_decalada=acierto_lluvia_decalada(bases3,respuesta,decalaje)
obten_tabla(tabla_acierto_lluvia_decalada[[1]])  
obten_tabla(tabla_acierto_lluvia_decalada[[2]])  


    #------------------------------------#

#ACIERTO DE LA LLUVIA FRENTE A LAS 4 BASES, CURVAS ROC, (VP, VN, FP, FN)

#Guardar graficos en PDF
#destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\lluvia_mL.pdf'
destination = 'D:\\Users\\??igo\\Desktop\\TFG\\Graficos 2023\\lluvia_mL.pdf'
pdf(file=destination)

decalaje=1
mL_lluvia=0.0
  datos_reales=bases4
  datos_extraidos=respuesta_poligono

  #aux.meteo="lluvia"
  aux.meteo="lluvia_pura"
  #aux.meteo="niebla"
acierto_lluvia_mL(bases4, respuesta_poligono, aux.meteo, decalaje, mL_lluvia,aux.ROC=TRUE)

    #Lluvia segun mL

decalaje=1
mLs_lluvia=seq(from=0,to=10)
#aux.meteo="lluvia"
aux.meteo="lluvia_pura"
mLs=list()
Acierto_global=list()
Acierto_llueve=list()
i=1
for (mL_lluvia in mLs_lluvia) {
  
  mL_lluvia=mL_lluvia/10
  print(mL_lluvia)
  acierto_mL_lluvia=as.data.frame((acierto_lluvia_mL(bases4, respuesta_poligono, aux.meteo, decalaje, mL_lluvia,aux.ROC=FALSE)[3]))
  mLs[i]=mL_lluvia
  Acierto_global[i]=(acierto_mL_lluvia)[1,2]
  Acierto_llueve[i]=(acierto_mL_lluvia)[2,2]  
  i=i+1
  mL_lluvia=mL_lluvia*10
}

#mL = as.data.frame(cbind(mLs,Acierto_global,Acierto_llueve))
mL = as.data.frame(cbind(as.numeric(mLs),as.numeric(Acierto_global),as.numeric(Acierto_llueve)))
colnames(mL)=c("mLs","Acierto_global","Acierto_llueve")
mL
obten_tabla(mL)

aux.min=Acierto_llueve
aux.max=Acierto_global
aux.ylim=c(0.8*Reduce(min, aux.min), 1.2*Reduce(max, aux.max))
plot(mL$mLs,mL$Acierto_llueve, type = "b",pch = 19, col = aux.colours[1], lty = 1, lwd=2.5, xlab = "> mLs", ylab = "% Acierto",ylim=aux.ylim,main="% Acierto VS >mLs")
lines(mL$mLs,mL$Acierto_global, type = "b", pch = 19, col = aux.colours[2], lty = 2, lwd=2.5)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 2)
legend("topleft", legend=c("Acierto_global", "Acierto_llueve"),  col=aux.colours[1:2], lty = 1:2, lwd=3, cex = 1.2, bg ="white")


decalaje=1
mL_lluvia=0.0
acierto_lluvia_mL(bases4, respuesta_poligono, aux.meteo, decalaje, mL_lluvia,aux.ROC=TRUE)
acierto_lluvia_mL(bases4, respuesta, aux.meteo, decalaje, mL_lluvia,aux.ROC=FALSE)

tabla_acierto=acierto_lluvia_mL(bases4, respuesta_poligono, aux.meteo, decalaje, mL_lluvia,aux.ROC=TRUE)
as.data.frame(tabla_acierto[1])
for (i in 2:(length(tabla_acierto))-1) {
  obten_tabla(as.data.frame(tabla_acierto[i]))
  
}

aux.min=Acierto_llueve
aux.max=Acierto_global
aux.ylim=c(0.8*Reduce(min, aux.min), 1.2*Reduce(max, aux.max))
plot(mL$mLs,mL$Acierto_llueve, type = "b",pch = 19, col = aux.colours[1], lty = 1, lwd=2.5, xlab = "> mLs", ylab = "% Acierto",ylim=aux.ylim,main="% Acierto VS >mLs")
lines(mL$mLs,mL$Acierto_global, type = "b", pch = 19, col = aux.colours[2], lty = 2, lwd=2.5)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 2)
legend("topleft", legend=c("Acierto_global", "Acierto_llueve"),  col=aux.colours[1:2], lty = 1:2, lwd=3, cex = 1.2, bg ="white")

decalaje=1
mL_lluvia=0.0
acierto_lluvia_mL(bases4, respuesta_poligono, aux.meteo, decalaje, mL_lluvia,aux.ROC=TRUE)


 

    #Nivel de acierto de cada base de forma individual

nombres_bases=c("La Muela","La Loteta","Zaragoza","Gallego")
bases_indiv=bases4[ , -which(colnames(bases4)=="total")]
aux.list=list()
for (i in 1:4) {
  bases1=bases_indiv[,c(i,5:ncol(bases_indiv))]
  print(nombres_bases[i])
  colnames(bases1)[1]="total"
  
  decalaje=1
  mL_lluvia=0.1
  datos_reales=bases1
  datos_extraidos=respuesta_poligono
  aux.list[[i]] =acierto_lluvia_mL(datos_reales, datos_extraidos, aux.meteo, decalaje, mL_lluvia,aux.ROC=TRUE)  
}

for (j in 1:3) {
  for (i in 1:4) {
    #print(nombres_bases[i])
    obten_tabla(unlist((aux.list[[i]])[[j]]))
    
  }  
}

    #Info_lluvia_mL

decalaje=1
mL_lluvia=0.0
#aux.meteo="lluvia"
aux.meteo="lluvia_pura"
datos_reales=bases4
datos_extraidos=respuesta_poligono

aux_lluvia_mL=info_lluvia_mL(datos_reales,datos_extraidos, aux.meteo,decalaje,mL_lluvia)
obten_tabla(aux_lluvia_mL)

#Cerrar PDF
dev.off()

#------------------------------------#


#VERANO

acierto_verano=acierto_lluvia_decalada_verano(bases4,respuesta,decalaje)
obten_tabla(acierto_verano[[1]])  
obten_tabla(acierto_verano[[2]])

decalaje=1
mL_lluvia=0.0
aux.verano=c(7,8)
datos_reales=bases4[bases4$month==aux.verano,]
datos_extraidos=respuesta_poligono[respuesta_poligono$month==aux.verano,]
aux.meteo="lluvia_pura"
acierto_lluvia_mL(datos_reales, datos_extraidos, aux.meteo, decalaje, mL_lluvia,aux.ROC=FALSE)

aux.resto_año=c(1:6,9:12)
datos_reales=bases4[bases4$month==aux.resto_año,]
datos_extraidos=respuesta_poligono[respuesta_poligono$month==aux.resto_año,]
acierto_lluvia_mL(datos_reales, datos_extraidos, aux.meteo, decalaje, mL_lluvia,aux.ROC=FALSE)


#------------------------------------#

#ANALISIS TEXTUAL

{
  
#Errores del analisis textual 
  
    {
#fechas donde se producen error:
 
 #2011-05-07 09:00:00
 delta$DESCRIPCIONACC[delta$dia_hora=="2011-05-07 09:00:00"] 
 #DescripciÃ³n: una furgoneta se salto un semaforo en la avda.de las torres, a la altura de jorge cocci y dio en el lateral izquierdo del coche de la trabajadora, qiue iba de camino al trabajo."
                "una furgoneta se selto un semaforo en la avda.de las torres, a la altura de jorge cocci y dio en lateral izquierdo del coche de la trabajadora, que iba de camino al trabajo."    
                "una furgoneta se salto un semaforo en la avda. de las torres, a la altura de jorge cocci y dio en lateral izquierdo del coche de la trabajadora, que iba de camino al trabajo"
     
                 #No se recoge el efecto de la lluvia, y en esos momentos se recoge lluvia en 3 bases
 
 #2011-07-19 06:00:00
 delta$DESCRIPCIONACC[delta$dia_hora=="2011-07-19 06:00:00"] 
 #DescripciÃ³n:"circulando por la carretera en su vehÃ­culo, se vieron sorprendidos por otro vehÃ­culo que aparecÃ­a reÃ¨ntinamente desde un cruce tras saltarse un stop. como consecuencia se produjo una colisiÃ³n entre ambos vehÃ­culos"                                                          
                "saliendo de fÃ¡brica, tras trabajar en el turno de nochem en el giro a la izquierda de incorporaciÃ³n a la carretera nacional, se despistas con otros coches que entraban a saica y al hacer la incorporaciÃ³n es arrolado por un vehÃ­culo que seguÃ­a recto hacia fuentes de ebro."
                "al llegar a un cruce, un coche se saltÃ³ el stop, situÃ¡ndose en la trayectoria que llevaba el vehÃ­culo de los trabajadores afectados causando choque entre los vehÃ­culos." 
                
                #No se recoge el efecto de la lluvia, y en esos momentos se recoge lluvia en 4 bases
                
 #2012-03-21 08:00:00
 delta$DESCRIPCIONACC[delta$dia_hora=="2012-03-21 08:00:00"] 
 #DescripciÃ³n: al ir a trabajar, parada en su coche en semaforo recibe golpe por detras"                                 
                "al acudir a trabajar en vehÃ­culo particular colisiona con otro vehÃ­culo, produciÃ©ndose contusiÃ³n cervical"
                
                #No se recoge el efecto de la lluvia, y en esos momentos se recoge lluvia en 4 bases
                
 #2016-02-10 19:00:00
 delta$DESCRIPCIONACC[delta$dia_hora=="2016-02-10 19:00:00"] 
 #DescripciÃ³n: "accidente in itinere al salir de trabajar: se produjo una retencion de vehiculos y al frenar los coches, una furgoneta impactÃ³ por detras con el vehiculo de la trabajadora."
                "al estar parada en un semaforo es golpeada por otro vehiculo"
 
                #No se recoge el efecto de la lluvia, y en esos momentos se recoge lluvia en 4 bases
                
 #2016-04-04 08:00:00
 delta$DESCRIPCIONACC[delta$dia_hora=="2016-04-04 08:00:00"] 
 #DescripciÃ³n: "cuando se dirigia al trabajo en madrid, un vehiculo le colisiono por detras"                                                                                                                                                                              
              "cuando se dirigia al trabajo en madrid, un vehiculo le colisiono por detras"                                                                                                                                                                              
              "el trabajador se dirige con su motocicleta a su puesto de trabajo en inicio de jornada siguiendo el trayecto habitual. en la c/ juslibol esquina con la c/ monte perdido, pasa con su vehiculo por un desnivel que habia en el firme, cayendose al suelo."
              "circulaba por la carretera que comunica la almunia (municipio de residencia) con alpartir (municipio de trabajo) cuando perdio control del vehiculo que conducia, dando vueltas de campana"                                                               
              "viniendo a trabajar a realizado un stop con su vehiculo y un coche por detras de la dado un golpe produciendole este dolor cervical"
                
              #No se recoge el efecto de la lluvia, y en esos momentos se recoge lluvia en 4 bases
                
}
  
  #------------------------------------#

#LIBRERIAS CORPUS             
              
    {text=delta$DESCRIPCIONACC             
 frequent_terms = freq_terms(text, 30)             
 
 text_source = VectorSource(text)
 text_corpus = VCorpus(text_source)
 text_corpus[[15]]
 text_corpus[[15]][1]
 str(text_corpus[[15]])
 
 stopwords("sp")
 texto_limpio=removeWords(text, stopwords("sp"))
 frequent_terms = freq_terms(texto_limpio, 30)             
 
 
 #'lluvia', 'nieve', 'hielo', 'niebla', 'precipitacion'
 
 raiz_lluvia = c("lluvia", "llovia", "lloviendo")
 stem_lluvia = stemDocument(raiz_lluvia)
 comp_dict_lluvia = ("lluvia")
 complete_text_lluvia = stemCompletion(stem_lluvia, comp_dict_lluvia)
 complete_text_lluvia
 
 # text_data = text
 # rm_punc = removePunctuation(text_data)
 # n_char_vec = unlist(strsplit(rm_punc, split = ' '))
 # stem_doc = stemDocument(n_char_vec)
 # complete_doc = stemCompletion(stem_doc, comp_dict_lluvia) 
 
 
 clean_corp =clean_corpus(text_corpus)
 clean_corp[[227]][1]
 
 texto_dtm = DocumentTermMatrix(clean_corp)
 texto_dtm
 texto_m = as.matrix(texto_dtm)
 dim(texto_m)
 texto_m[1:20, 10:50]
 
 texto_tdm = TermDocumentMatrix(clean_corp)
 texto_tdm
 texto_m = as.matrix(texto_tdm)
 dim(texto_m)
 texto_m[148:158, 126:138]
 
 texto_tdm_rm_sparse = removeSparseTerms(texto_tdm, 0.99)
 texto_tdm_rm_sparse
 texto_m = as.matrix(texto_tdm_rm_sparse)
 dim(texto_m)
 texto_m[14:28, 10:22]
 }
  
}

#------------------------------------#
              
#IDA-VUELTA (I_V)

    {
   #ITINERE
 
   df=delta[delta$itinere==1, ]
   I_V(df)   
   I_V_itinere=I_V(df)[[1]] 
   
   
   plot_turno(I_V_itinere)
   #plot_turno_proporciones(I_V_itinere)
   
  #Primer año completo 2009, ultimo año completo 2022
   
   #par(mfrow=c(1,2))
   
   df=delta[delta$itinere==1 & delta$year=="2009" , ]
   itinere_2009=I_V(df)[[1]] 
   plot_turno(itinere_2009)
   #plot_turno_proporciones(itinere_2009)
   
   df=delta[delta$itinere==1 & delta$year=="2022" , ]
   itinere_2022=I_V(df)[[1]]  
   plot_turno(itinere_2022)
   #plot_turno_proporciones(itinere_2022)
   
  #Cuatro poligonos
   par(mfrow=c(2,2))
   
   levels(delta_poligonos$ubicacion)  #PolÃ­gonos y orden de plots
   for (i in 1:length(levels(delta_poligonos$ubicacion))) {
     
     aux_ubicacion=levels(delta_poligonos$ubicacion)[i]
     df=delta_poligonos[delta_poligonos$itinere==1 & delta_poligonos$ubicacion==aux_ubicacion, ]
     itinere_POLIGONO=I_V(df)[[1]]  
     plot_turno(itinere_POLIGONO)
   }
   
  #MISION
 
   df=delta[delta$itinere==0, ]
   I_V(df)   
   I_V_mision=I_V(df)[[1]] 
   mapa_I_V_HORA=heatmap(I_V_mision[,0:8], Rowv = NA, Colv = NA, xlab = "Hora_Turno", ylab = "Hora_Real") #Horas de turnos mÃ¡s relevantes
   
 #Primer año completo 2009, ultimo año completo 2022
  
   par(mfrow=c(1,2))
   df=delta[delta$itinere==0 & delta$year=="2009" , ]
   aux_plot_1=I_V(df)[[2]]
   df=delta[delta$itinere==0 & delta$year=="2022" , ]
   I_V(df) 
   aux_plot_2=I_V(df)[[2]]
   aux_plot_1
   
   table(delta$year,delta$HORATRABAJO)
   
 #Cuatro poligonos
   par(mfrow=c(2,2))
   
   levels(delta_poligonos$ubicacion)  #PolÃ­gonos y orden de plots
   for (i in 1:length(levels(delta_poligonos$ubicacion))) {
     
     aux_ubicacion=levels(delta_poligonos$ubicacion)[i]
     df=delta_poligonos[delta_poligonos$itinere==0 & delta_poligonos$ubicacion==aux_ubicacion, ]
     I_V(df)
   }
  
   par(mfrow=c(1,1))
   
 #Entre semana vs fin de semana
   
   delta$fin_de_semana=1*ifelse((delta$wday>=2 & delta$wday<=6),FALSE,TRUE) #Fin de semana 1, entre semana 0
   table(delta$fin_de_semana)
   df=delta[delta$itinere==0 & delta$fin_de_semana==1, ]
   I_V(df) 

 }


#------------------------------------#

#' [MODELIZACION]   

 #Creando variable Ida_Vuelta 
   
   {
  table(respuesta$HORATRABAJO[respuesta$y==1]) #Purgar hora de trabajo 3567
   table(respuesta$y==1)
   table(respuesta$y>1)
   
   respuesta$entrada=1*(respuesta$entrada>0)#Variables binarias
   respuesta$salida=1*(respuesta$salida>0)
   
   table((respuesta$entrada+respuesta$salida)==1)
   
   respuesta$IDA=respuesta$entrada
   respuesta$VUELTA=respuesta$salida
   aux_IDA_VUELTA=(respuesta$entrada+respuesta$salida)
   respuesta$IDA[aux_IDA_VUELTA>1]=NA
   respuesta$VUELTA[aux_IDA_VUELTA>1]=NA
  } 
 
 #Anteriores modelos incluyeNdo IDA y VUELTA
  
   {
   M8= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+itinere+alcance, data=respuesta, family = "poisson",control = glm.control(maxit=100))
   IM8= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*itinere*alcance,data=respuesta, family = "poisson",control = glm.control(maxit=100))
   
 #Para observar de mejor forma el efecto de las variables solares se eliminan las variables categoricas
   
   M8_armonicos= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=respuesta, family = "poisson",control = glm.control(maxit=100))

 #Nuevos modelos con variables solares
  
   M9_armonicos_IV= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)+(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+(IDA+VUELTA), data=respuesta, family = "poisson",control = glm.control(maxit=100))
   #IM8_IV= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(lluvia*itinere*alcance)*(IDA*VUELTA),data=respuesta, family = "poisson",control = glm.control(maxit=100))
   #El tiempo de computo es inadecuado se proponen dos soluciones
   
   IM8_armonicos_IV_a= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+(IDA*VUELTA),data=respuesta, family = "poisson",control = glm.control(maxit=100))
   IM8_armonicos_IV_b= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(IDA+VUELTA),data=respuesta, family = "poisson",control = glm.control(maxit=100))
   
   IM8_itinere_IV= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*itinere*(IDA+VUELTA),data=respuesta, family = "poisson",control = glm.control(maxit=100))
   
   my_list_Modelos_IV=list(M8, IM8, M8_armonicos,M9_armonicos_IV,IM8_armonicos_IV_a,IM8_armonicos_IV_b,IM8_itinere_IV)
   obten_modelos(my_list_Modelos_IV)
   
   #Se aprecia una importante mejora respecto al AIC donde los modelos que incluyen las interacciones de la Ida_vuelta con los armonicos obtienen un valor de 60093, frente a M8_armonicos con 110776
  
   #Incluyo el resto de variables categoricas y realizo step para obtener las variables m?s relevantes del modelo
   
   IM8_categoricas_IV=glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(itinere+lluvia+alcance)*(IDA+VUELTA),data=respuesta, family = "poisson",control = glm.control(maxit=100))
      #AIC=56023
   #step(IM8_categoricas_IV)
   #IM_step=step(glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(itinere)*(IDA+VUELTA),data=respuesta, family = "poisson"),control = glm.control(maxit=50))

 }

    #------------------------------------#
   
 #Building dataframe response.hourly.itinere, every row is the number of injuries in an hour and in a situation defined by in mission, in itinere-goto, in itenere-return
   
              {
 #itenere.key is 0 for mission, is 1 for itinere-goto , is 2 for itinere-return
   # #delta_09_23$itinere.key = delta_09_23$itinere + (delta_09_23$itinere==1) *( is.element( delta_09_23$HORATRABAJO   , 0 ) ) +2*  (delta_09_23$itinere==1) *( is.element( delta_09_23$HORATRABAJO   , 99 ) ) 
   # 
   # delta_09_23$itinere.key <- delta_09_23$itinere
   # delta_09_23$itinere.key[ (delta_09_23$itinere==1) & is.element( delta_09_23$HORATRABAJO   ,c( 0) ) ] <- 1
   # delta_09_23$itinere.key[ (delta_09_23$itinere==1) & is.element( delta_09_23$HORATRABAJO   ,c( 99) ) ] <- 2
   # 
   # 
   # aux.yday.hour.itinere =(delta_09_23$year*1000000+delta_09_23$day_year*1000+delta_09_23$hour*10) + delta_09_23$itinere.key
   # 
   # response.hourly.itinere =tapply(y, list(aux.yday.hour.itinere ), sum, na.rm=TRUE)
   # response.hourly.itinere =as.data.frame(response.hourly.itinere)
   # response.hourly.itinere [is.na(response.hourly.itinere)] = 0
   # names(response.hourly.itinere)=c('y')
   # 
   # response.hourly.itinere$itinere.key =tapply(delta_09_23$itinere.key, list(aux.yday.hour.itinere), mean, na.rm=TRUE)
} 
      #Obsoleto se crean las variables con anterioridad 

   #BLOQUE MODELO RESPUESTA
   
   { response.hourly.itinere$lluvia=(tapply(delta_09_23$lluvia, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
     #response.hourly.itinere$itinere=tapply(delta_09_23$itinere, list(aux.yday.hour.itinere), sum, na.rm=TRUE)
     response.hourly.itinere$alcance=(tapply(delta_09_23$alcance, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
     #response.hourly.itinere$fin_de_semana=is.element(response.hourly.itinere$wday,c(6,7))
     response.hourly.itinere$leve=(tapply(delta_09_23$GRADO==1, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
     grave1=(tapply(delta_09_23$GRADO==2, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
     grave2=(tapply(delta_09_23$GRADO==3, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
     response.hourly.itinere$grave=grave1+grave2
     response.hourly.itinere$muertos=(tapply(delta_09_23$GRADO==4, list(aux.yday.hour.itinere), sum, na.rm=TRUE)>0)
     
     
     #Variables IDA_VUELTA
     response.hourly.itinere$HORATRABAJO=(tapply(delta_09_23$HORATRABAJO, list(aux.yday.hour.itinere), sum, na.rm=TRUE))
     
     delta_09_23$entrada=1*(delta_09_23$HORATRABAJO==0)
     delta_09_23$salida=1*(delta_09_23$HORATRABAJO==99)
     delta_09_23$jornada=1-((delta_09_23$HORATRABAJO==0)|(delta_09_23$HORATRABAJO==99)) 
     
     response.hourly.itinere$entrada=(tapply(delta_09_23$entrada, list(aux.yday.hour.itinere), sum, na.rm=TRUE))
     response.hourly.itinere$salida=(tapply(delta_09_23$salida, list(aux.yday.hour.itinere), sum, na.rm=TRUE))
     response.hourly.itinere$jornada=(tapply(delta_09_23$jornada, list(aux.yday.hour.itinere), sum, na.rm=TRUE))
     
     
     dim(response.hourly.itinere)
     response.hourly.itinere=rownames_to_column(response.hourly.itinere, "aux.yday.hour.itinere")
     response.hourly.itinere$aux.yday.hour.itinere=as.numeric(response.hourly.itinere$aux.yday.hour.itinere)
     
     aux.date=seq(ymd('2009-01-01'),ymd('2022-12-31'), by = 'days')
     aux.date=seq(as.POSIXct("2009-01-01 0","%Y-%m-%d %H", tz="UTC"),to=as.POSIXct("2022-12-31 23", "%Y-%m-%d %H", tz="UTC"), by="hour")
     head(aux.date)
     
     aux.date=cbind(aux.date,year(aux.date),yday(aux.date),hour(aux.date))
     aux.number.row=dim(aux.date)[1]
     aux.date=rbind(aux.date,aux.date,aux.date)
     aux.date=cbind(aux.date, c(rep(0,aux.number.row),rep(1,aux.number.row),rep(2,aux.number.row)))
     aux.date=cbind(aux.date,aux.date[,2]*1000000+aux.date[,3]*1000+aux.date[,4]*10+aux.date[,5])
     aux.v=aux.date[,2:6]
     colnames(aux.v)=c("year","day_year","hour","itinere.key","aux.yday.hour.itinere")
     #aux.yday.hour.itinere=round((delta$year*100000+delta$day_year*100+delta$hour), 6)
     
     #aux.vday_year=1:(365*length(años)+sum(leap_year(años)))
     # aux.vday_year=aux.date[,4]
     # aux.vday_hour=1:24
     # aux.v = expand.grid(aux.vday_year , (aux.vday_hour-1)/24)
     # aux.v$aux.yday.hour.itinere=round((aux.v$Var1+aux.v$Var2), 6)
     
     prueba=merge(response.hourly.itinere, aux.v, all = TRUE)
     prueba[is.na(prueba)] = 0
     # prueba$Var1=NULL
     # prueba$Var2=NULL
     response.hourly.itinere=prueba
     
     aux.yday.hour.itinere=response.hourly.itinere$aux.yday.hour.itinere
     day_year=response.hourly.itinere$day_year
     hour=response.hourly.itinere$hour
     response.hourly.itinere$arm_dia=tapply(day_year, list(aux.yday.hour.itinere), mean, na.rm=TRUE)
     response.hourly.itinere$arm_hora=tapply(hour, list(aux.yday.hour.itinere), mean, na.rm=TRUE)
     
   
     # response.hourly.itinere$day_years=response.hourly.itinere$day_year+365*(response.hourly.itinere$year-2009)+sum(leap_year(response.hourly.itinere$year-2009))
     # day_years=response.hourly.itinere$day_years
     # response.hourly.itinere$arm_dia=tapply(day_years, list(aux.yday.hour), mean, na.rm=TRUE)
     
     #dias=365*length(años)+sum(leap_year(años))
     response.hourly.itinere$c1d=cos(2*pi*response.hourly.itinere$arm_dia/366)
     response.hourly.itinere$s1d=sin(2*pi*response.hourly.itinere$arm_dia/366)
     response.hourly.itinere$c2d=cos(4*pi*response.hourly.itinere$arm_dia/366)
     response.hourly.itinere$s2d=sin(4*pi*response.hourly.itinere$arm_dia/366)
     response.hourly.itinere$c3d=cos(6*pi*response.hourly.itinere$arm_dia/366)
     response.hourly.itinere$s3d=sin(6*pi*response.hourly.itinere$arm_dia/366)
     response.hourly.itinere$c4d=cos(8*pi*response.hourly.itinere$arm_dia/366)
     response.hourly.itinere$s4d=sin(8*pi*response.hourly.itinere$arm_dia/366)
     response.hourly.itinere$c5d=cos(10*pi*response.hourly.itinere$arm_dia/366)
     response.hourly.itinere$s5d=sin(10*pi*response.hourly.itinere$arm_dia/366)
     
     
     response.hourly.itinere$c1h=cos(2*pi*response.hourly.itinere$arm_hora/24)
     response.hourly.itinere$s1h=sin(2*pi*response.hourly.itinere$arm_hora/24)
     response.hourly.itinere$c2h=cos(4*pi*response.hourly.itinere$arm_hora/24)
     response.hourly.itinere$s2h=sin(4*pi*response.hourly.itinere$arm_hora/24)
     response.hourly.itinere$c3h=cos(6*pi*response.hourly.itinere$arm_hora/24)
     response.hourly.itinere$s3h=sin(6*pi*response.hourly.itinere$arm_hora/24)
     
     #------------------------------------#
     
     response.hourly.itinere$dia_año=response.hourly.itinere$day_year+response.hourly.itinere$year*1000
     i=(as.Date("15-03-2020", "%d-%m-%Y"))
     f=(as.Date("21-06-2020", "%d-%m-%Y"))
     f_est_alar=(as.Date("9-05-2022", "%d-%m-%Y"))
     inicio=yday(i)+year(i)*1000
     final=yday(f)+year(f)*1000
     final_estado_alarma=yday(f_est_alar)+year(f_est_alar)*1000
     
     response.hourly.itinere$conf=as.numeric(is.element(response.hourly.itinere$dia_año,inicio:final))
     response.hourly.itinere$covid=as.numeric(is.element(response.hourly.itinere$dia_año,final:final_estado_alarma))
     
     #------------------------------------#
     subset_response.hourly.itinere=subset(response.hourly.itinere,response.hourly.itinere$dia_año>0)
     
     # prob_acc=tapply(subset_response.hourly.itinere$y, list(subset_response.hourly.itinere$dia_año), sum, na.rm=TRUE)
     # summary(prob_acc)
     
     prob_leve=tapply(subset_response.hourly.itinere$leve, list(subset_response.hourly.itinere$dia_año), sum, na.rm=TRUE)
     prob_grave=tapply(subset_response.hourly.itinere$grave, list(subset_response.hourly.itinere$dia_año), sum, na.rm=TRUE)
     prob_muerte=tapply(subset_response.hourly.itinere$muertos, list(subset_response.hourly.itinere$dia_año), sum, na.rm=TRUE)
     
     #prob_leve=tapply(subset_response.hourly.itinere$leve, list(subset_response.hourly.itinere$day_year), sum, na.rm=TRUE)/13
     #prob_grave=tapply(subset_response.hourly.itinere$grave, list(subset_response.hourly.itinere$day_year), sum, na.rm=TRUE)/13
     #prob_muerte=tapply(subset_response.hourly.itinere$muertos, list(subset_response.hourly.itinere$day_year), sum, na.rm=TRUE)/13
     
     p_leve=summary(prob_leve)[4]
     p_grave=summary(prob_grave)[4]
     p_muerte=summary(prob_muerte)[4]
     
     cat(p_leve,p_grave,p_muerte,sep="&",fill=TRUE)
     
     
     
     
     basura=table(response.hourly.itinere$alcance,response.hourly.itinere$grave)
     basura[2,]/apply(basura,2,sum)
     
     #------------------------------------#
     
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
     
     
     y=response.hourly.itinere$y
   } 
   
   #------------------------------------#
   
   #Modelos basicos
   
   {
   #M1 BASE-Armonicos 
   
   M1= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #M2 = M1 + LLUVIA
   
   M2= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia, data=response.hourly.itinere,family = "poisson",control = glm.control(maxit=100))
   
   #M3 = M1 + ITINERE
   
   M3= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+((itinere.key==1)+(itinere.key==2)), data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #M4 = M1 + ALCANCE
   
   M4= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+alcance, data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #M5 = M1 + LLUVIA + ITINERE
   
   M5= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+((itinere.key==1)+(itinere.key==2)), data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #M6 = M1 + LLUVIA + ALCANCE
   
   M6= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+alcance, data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #M7 = M1 + ITINERE + ALCANCE
   
   M7= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+((itinere.key==1)+(itinere.key==2))+alcance, data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #M8 = M1 + LLUVIA + ITINERE + ALCANCE
   
   M8= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+((itinere.key==1)+(itinere.key==2))+alcance, data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #M9 = TODAS LAS VARIABLES 
   
   M9= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)+(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+lluvia+((itinere.key==1)+(itinere.key==2))+alcance,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #------------------------------------#
   
   #Interacciones
   
   #IM1 = Armonicos * UBICACION 
   
   IM1= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #IM2 = BASE * LLUVIA 
   
   IM2= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #IM3 = BASE * ITINERE
   
   IM3= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2)),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #IM4 = BASE * ALCANCE
   
   IM4= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*alcance,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #IM5 = LLUVIA * ITINERE
   
   IM5= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*((itinere.key==1)+(itinere.key==2)),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #IM6 = LLUVIA * ALCANCE
   
   IM6= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*alcance,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #IM7 = ITINERE* ALCANCE
   
   IM7= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2))*alcance,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #IM8 = LLUVIA  * ITINERE* ALCANCE
   
   IM8= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*((itinere.key==1)+(itinere.key==2))*alcance,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #IM9 = TODAS LAS VARIABLES INTERACCIONAN
   
   #IM9= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*((itinere.key==1)+(itinere.key==2))*alcance,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   #?Warning message: glm.fit: algorithm did not convergEÂª
   
   IM9= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2)),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   obten_modelos(IM9)
   terminos(IM9)
   summary(IM9)
   
   
   #------------------------------------#
   
   my_list_M=list(M1,M2,M3,M4,M5,M6,M7,M8,M9)
   my_list_IM=list(IM1,IM2,IM3,IM4,IM5,IM6,IM7,IM8,IM9)
   obten_modelos(my_list_M)
   terminos(my_list_IM)
   }
   
   #------------------------------------#
   
   #Modelos var categoricas: Base, Lluvia, Alcance, Fin de semana, y Combinaciones
   
      #Itinere IDA e Itinere VUELTA
  
  #Itinere 1
      
   {
   
   aux.df=response.hourly.itinere
   y=response.hourly.itinere$y  
   aux.y=y
   modelos.df=data.frame()
   
   #Base 
       
   {
   #IM9=glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2)),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   #aux.model=IM9
   #Se incluye sobre IM9 el confinamiento sucedido en el año 2020
   aux.base0=glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2))+((itinere.key==1)+(itinere.key==2))+conf,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   aux.model=aux.base0
   summary(aux.base0) #El modelo aux.base0 es el modelo del cual se construir?n el resto mediante :
                          # 1) Pimer modelo creado: Modelo de adici?n de las variables de inter?s - Funci?n crear_modelo (Incluida en Funci?n auto.modelo )
                          # 2) Siguientes modelos: Modelos con las variables de inter?s y significativas - Funci?n auto.modelo   
                          # 3) Mejor modelos: Selecci?n del mejor modelo de entre los anteriores   
   
   aux.model.name="model_base"
   aux.modelo_base=aux.base0
   #auto_model(aux.modelo_base, aux.model.name)
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name)
   
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_base=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_base=mejor_modelo_base[[2]]
   }
     
   #Lluvia
   
   {
   var.adicional="lluvia"
   aux.model.name="model_lluvia"
   aux.modelo_base=modelo_base
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_lluvia=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_lluvia=mejor_modelo_lluvia[[2]]
   
   }
   
   #Alcance
   
   {
   var.adicional="alcance"
   aux.model.name="model_alcance"
   aux.modelo_base=modelo_base
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_alcance=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_alcance=mejor_modelo_alcance[[2]]   
     
     
     
     
     }
   
   #Fin de semana
   
   {
   var.adicional="finde"
   aux.model.name="model_wday"
   aux.modelo_base=modelo_base
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_wday=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_wday=mejor_modelo_wday[[2]]   
   
   
   }
   
   #Mezcla modelos variables significativas (Lluvia, Alcance, Fin de semana)
   
   {
   aux.lluvia=terminos(modelo_lluvia)
   aux.alcance=terminos(modelo_alcance)
   aux.wday=terminos(modelo_wday)
   aux.list=rbind(aux.lluvia,aux.alcance,aux.wday)   
   model_mezcla=mezcla_modelo(aux.df,aux.y,aux.list)
     
   aux.model.name="model_mezcla"
   aux.modelo_base=model_mezcla
   #auto_model(aux.modelo_base, aux.model.name)
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name)
   
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_mezcla=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_mezcla=mejor_modelo_mezcla[[2]]
   
   
   
   }
     
   #Modelos a partir de la inclusi?n de variables solares en el modelo mezcla
   
   {
   #Bloque solar (salida_0_15+noche+puesta_0_15+banda_15_50)   
      
   aux.modelo_base=modelo_mezcla
   var.adicional="(salida_0_15+noche+puesta_0_15+banda_15_50)" 
   aux.model.name="model_global"
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_global=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_global=mejor_modelo_global[[2]]   
   
   }     
   
   }
      
          {#obten_confint(modelo_global)
   #riesgo_confint(modelo_global)
   
   #Guardar grÃ¡ficos en PDF
   #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\itinere_1.pdf'
   destination = 'D:\\Users\\??igo\\Desktop\\TFG\\Graficos 2023\\itinere_1.pdf'
   pdf(file=destination)
   
   plot_importancia(modelo_global)
   plot_model(modelo_global,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
   plot_model.df(modelos.df)
   
   #Cerrar PDF 
   dev.off()} #Guardar graficos
   
   #------------------------------------#
  
  #Itinere 2
      
   {
   
   aux.df=response.hourly.itinere
   y=response.hourly.itinere$y  
   aux.y=y
   modelos.df=data.frame()
   
   #Base 
       
   {
   #IM9=glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2)),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   #aux.model=IM9
   #Se incluye sobre IM9 el confinamiento sucedido en el año 2020
   aux.base0=glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2))+((itinere.key==1)+(itinere.key==2))+conf,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   aux.model=aux.base0
   summary(aux.base0) #El modelo aux.base0 es el modelo del cual se construir?n el resto mediante :
                          # 1) Pimer modelo creado: Modelo de adici?n de las variables de inter?s - Funci?n crear_modelo (Incluida en Funci?n auto.modelo )
                          # 2) Siguientes modelos: Modelos con las variables de inter?s y significativas - Funci?n auto.modelo   
                          # 3) Mejor modelos: Selecci?n del mejor modelo de entre los anteriores   
   
   aux.model.name="model_base"
   aux.modelo_base=aux.base0
   #auto_model(aux.modelo_base, aux.model.name)
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name)
   
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
     
     auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
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
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_wday=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_wday=mejor_modelo_wday[[2]]   
   
   
   }
   
   #Lluvia
   
   {
   var.adicional="lluvia"
   aux.model.name="model_lluvia"
   aux.modelo_base=modelo_wday
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_lluvia=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_lluvia=mejor_modelo_lluvia[[2]]
   
   }
   
   #Alcance
   
   {
   var.adicional="alcance"
   aux.model.name="model_alcance"
   aux.modelo_base=modelo_lluvia
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_alcance=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_alcance=mejor_modelo_alcance[[2]]   
   
   }
   
   modelo_global=modelo_alcance
   view(modelos.df)
   }
    
      
         {#obten_confint(modelo_global)
      #riesgo_confint(modelo_global)
      
      #Guardar grÃ¡ficos en PDF
      #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\itinere_2.pdf'
      destination = 'D:\\Users\\??igo\\Desktop\\TFG\\Graficos 2023\\itinere_2.pdf'
      pdf(file=destination)
      
      plot_importancia(modelo_global)
      plot_model(modelo_global,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
      plot_model.df(modelos.df)
      
      #Cerrar PDF 
      dev.off()} #Guardar graficos
      
   #------------------------------------#
   
     #MISION
      
  #Mision 1
      
   { 
   aux.marcador=is.element(response.hourly.itinere$itinere.key,0)
   aux.df=response.hourly.itinere[aux.marcador,]
   y=aux.df$y  
   aux.y=y   
   modelos.df=data.frame()
   
   #Base 
    
   {
    #IM9=glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2)),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
    #aux.model=IM9
    #Se incluye sobre IM9 el confinamiento sucedido en el año 2020
        
    aux.mision0=glm(y~((c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+conf),data=aux.df, family = "poisson",control = glm.control(maxit=100))
    aux.model=aux.mision0
    summary(aux.mision0) 
        
    aux.model.name="model_base"
    aux.modelo_base=aux.mision0
    #auto_model(aux.modelo_base, aux.model.name)
    p_valor=0.001
    auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name,aux.variable=NULL,p_valor)
        
    aux.model.df=auto.modelo[[1]]
    aux.model.list=auto.modelo[[2]]
    modelo.names_list=aux.model.df$model.name
    modelos.df=rbind(modelos.df,aux.model.df)
    
    mejor_modelo_mision=mejor_modelo(aux.model.list,modelo.names_list)
    modelo_mision=mejor_modelo_mision[[2]]
   } 
      
   #Lluvia
   
   {
   var.adicional="lluvia"
   aux.model.name="model_lluvia"
   aux.modelo_base=modelo_mision
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_lluvia=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_lluvia=mejor_modelo_lluvia[[2]]
   
   }
   
   #Alcance
   
   {
   var.adicional="alcance"
   aux.model.name="model_alcance"
   aux.modelo_base=modelo_mision
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_alcance=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_alcance=mejor_modelo_alcance[[2]]   
     
     }
   
   #Fin de semana
   
   {
   var.adicional="finde"
   aux.model.name="model_wday"
   aux.modelo_base=modelo_mision
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_wday=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_wday=mejor_modelo_wday[[2]]   
   
   }
   
   #Mezcla modelos variables significativas (Lluvia, Alcance, Fin de semana)
   
   {
   aux.lluvia=terminos(modelo_lluvia)
   aux.alcance=terminos(modelo_alcance)
   aux.wday=terminos(modelo_wday)
   aux.list=rbind(aux.lluvia,aux.alcance,aux.wday)   
   model_mezcla=mezcla_modelo(aux.df,aux.y,aux.list)
     
   aux.model.name="model_mezcla"
   aux.modelo_base=model_mezcla
   #auto_model(aux.modelo_base, aux.model.name)
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name)
   
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_mezcla=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_mezcla=mejor_modelo_mezcla[[2]]
   
   }
     
   #Modelos a partir de la inclusi?n de variables solares en el modelo mezcla
   
   { 
   var.adicional="(salida_0_15+noche+puesta_0_15+banda_15_50)" 
   aux.model.name="model_global"
   aux.modelo_base=modelo_mezcla
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_global=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_global=mejor_modelo_global[[2]]   
   
   }     
    
   
   }
      
         {#obten_confint(modelo_global)
        #riesgo_confint(modelo_global)
        
        #Guardar grÃ¡ficos en PDF
        #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\mision_1.pdf'
        destination = 'D:\\Users\\??igo\\Desktop\\TFG\\Graficos 2023\\mision_1.pdf'
        pdf(file=destination)
        
        plot_importancia(modelo_global)
        plot_model(modelo_global,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
        plot_model.df(modelos.df)
        
        #Cerrar PDF 
        dev.off()} #Guardar graficos
        
   #------------------------------------#
    
  #Mision 2
   
   {
   
   aux.marcador=is.element(response.hourly.itinere$itinere.key,0)
   aux.df=response.hourly.itinere[aux.marcador,]
   y=aux.df$y  
   aux.y=y
   modelos.df=data.frame()
   
   #Base 
       
   {
   #IM9=glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2)),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   #aux.model=IM9
   #Se incluye sobre IM9 el confinamiento sucedido en el año 2020
   aux.base0=glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+conf,data=aux.df, family = "poisson",control = glm.control(maxit=100))
   aux.model=aux.base0
   summary(aux.base0) #El modelo aux.base0 es el modelo del cual se construir?n el resto mediante :
                          # 1) Pimer modelo creado: Modelo de adici?n de las variables de inter?s - Funci?n crear_modelo (Incluida en Funci?n auto.modelo )
                          # 2) Siguientes modelos: Modelos con las variables de inter?s y significativas - Funci?n auto.modelo   
                          # 3) Mejor modelos: Selecci?n del mejor modelo de entre los anteriores   
   
   aux.model.name="model_base"
   aux.modelo_base=aux.base0
   #auto_model(aux.modelo_base, aux.model.name)
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name)
   
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
     
     modelo_base=aux.modelo_base
     model.name=aux.model.name
     aux.variable=var.adicional
     
     auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
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
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_wday=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_wday=mejor_modelo_wday[[2]]   
   
   
   }
   
   #Lluvia
   
   {
   var.adicional="lluvia"
   aux.model.name="model_lluvia"
   aux.modelo_base=modelo_wday
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_lluvia=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_lluvia=mejor_modelo_lluvia[[2]]
   
   }
   
   #Alcance
   
   {
   var.adicional="alcance"
   aux.model.name="model_alcance"
   aux.modelo_base=modelo_lluvia
   modelo_base=modelo_lluvia
   model.name=aux.model.name
   aux.variable=var.adicional
   
   auto.modelo=auto_model(aux.df,aux.y,aux.modelo_base, aux.model.name, var.adicional)
   aux.model.df=auto.modelo[[1]]
   aux.model.list=auto.modelo[[2]]
   modelo.names_list=aux.model.df$model.name
   modelos.df=rbind(modelos.df,aux.model.df)
   
   mejor_modelo_alcance=mejor_modelo(aux.model.list,modelo.names_list)
   modelo_alcance=mejor_modelo_alcance[[2]]   
   
   }
   
   modelo_global=modelo_alcance
   view(modelos.df)
   }    
      
         {#obten_confint(modelo_global)
      #riesgo_confint(modelo_global)
      
      #Guardar grÃ¡ficos en PDF
      #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\mision_2.pdf'
      destination = 'D:\\Users\\??igo\\Desktop\\TFG\\Graficos 2023\\mision_2.pdf'
      pdf(file=destination)
      
      plot_importancia(modelo_global)
      plot_model(modelo_global,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
      plot_model.df(modelos.df)
      
      #Cerrar PDF 
      dev.off()} #Guardar graficos
      
    
   #------------------------------------#
        
   #Nuevo modelo a partir de variables significativas a partir de los modelos (M_todos_itinere) obtenidos en el dataframe respuesta 
      #Se emplea el mejor modelo obtenudo con el df respuesta y sin diferenciar la tipolog?a de "in itinere"
      #para ahora emplear el df response.hour.itinere y observar la diferenciaci?n
 
   { aux.df=response.hourly.itinere
   aux.y=response.hourly.itinere$y
   M_todos_itinere_12=glm(y~(alcance+c1d+c1d*alcance+c1d*((itinere.key==1)+(itinere.key==2))+c1d*((itinere.key==1)+(itinere.key==2))*alcance+c2d+c2d*((itinere.key==1)+(itinere.key==2))+
                               c2d*((itinere.key==1)+(itinere.key==2))*alcance+c3d+c3d*alcance+c3d*((itinere.key==1)+(itinere.key==2))+c3d*((itinere.key==1)+(itinere.key==2))*alcance+c4d+
                               c4d*alcance+c4d*((itinere.key==1)+(itinere.key==2))+c4d*((itinere.key==1)+(itinere.key==2))*alcance+c4d*lluvia*((itinere.key==1)+(itinere.key==2))+((itinere.key==1)+(itinere.key==2))+
                               ((itinere.key==1)+(itinere.key==2))*alcance+lluvia+lluvia*((itinere.key==1)+(itinere.key==2))+s1d+s1d*((itinere.key==1)+(itinere.key==2))+s2d*((itinere.key==1)+(itinere.key==2))+
                               s2d*((itinere.key==1)+(itinere.key==2))*alcance+s3d+s3d*((itinere.key==1)+(itinere.key==2))+s3d*((itinere.key==1)+(itinere.key==2))*alcance+s4d+s4d*((itinere.key==1)+(itinere.key==2))+
                               s4d*((itinere.key==1)+(itinere.key==2))*alcance),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   
   aux.model=M_todos_itinere_12
   aux.model_terminos=crear_modelo(aux.df,aux.y,aux.model)
   
   aux.model_list=list(aux.model,aux.model_terminos)
   obten_modelos(aux.model_list)  
   summary(aux.model_terminos)}
      
   #------------------------------------#
   
  #Modelo solar a apartir de los mejores modelos (M_todos_itinere) obtenidos en el dataframe respuesta 
   
  { M9_expandido= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)+(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+lluvia+((itinere.key==1)+(itinere.key==2))+alcance+(salida_0_15+puesta_0_15+noche+banda_15_50) ,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
   IM8_expandido= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*(salida_0_15+puesta_0_15+noche+banda_15_50) ,data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))
  
   M_expandido=list(M9_expandido,IM8_expandido)
   obten_modelos(M_expandido)  
   
   summary(M9_expandido)
   terminos(M9_expandido)
   
   aux.df=response.hourly.itinere
   aux.y=y
   aux.model_solar=M9_expandido
   aux.model_terminos_solar=crear_modelo(aux.df,aux.y,aux.model_solar)
   
   aux.model_list_solar=list(aux.model_solar,aux.model_terminos_solar)
   obten_modelos(aux.model_list_solar)  }
   
   #------------------------------------#
   
   #Modelo global a apartir de los mejores modelos (M_todos_itinere) obtenidos en el dataframe respuesta 
   
  { aux.df=response.hourly.itinere
   aux.y=y
   aux.a=terminos(aux.model_terminos)
   aux.b=terminos(aux.model_terminos_solar)
   aux.list=rbind(aux.a,aux.b)
   
   aux.model_mezcla=mezcla_modelo(aux.df,aux.y,aux.list)
   
   aux.model_list_mezcla=list(aux.model_terminos,aux.model_terminos_solar,aux.model_mezcla)
   obten_modelos(aux.model_list_mezcla)
   
   summary(aux.model_mezcla)}

#------------------------------------#

   #Zero inflated
   
   {aux.formula.y = formula(y~( s1d+c1d + +s2d +c2d+ s3d +c3d )*itinere+ c4d+ s4d + lluvia*itinere+( s1h +c1h +s2h+ c2h ) *itinere )
   
   #same structure in Poisson and binary
   aux.model.hurdle.1  = hurdle( aux.formula.y   ,  data=respuesta, dist = "poisson")
   
   aux.model  = hurdle( y ~ (s1d + c1d + +s2d + c2d + s3d + c3d) * itinere + c4d + s4d +     lluvia * itinere + (s1h + c1h + s2h + c2h) * itinere| s1h+c1h + s2h+ c2h ,  data=respuesta, dist = "poisson")
   summary(aux.model)
   AIC(aux.model)
   aux.model.hurdle = aux.model
   
   waldtest(aux.model.hurdle.1, aux.model.hurdle)
   #lrtest(aux.model.hurdle.1, aux.m.hurdle)
   
   #zeroinfl.Poisson model 
   #same linear predictor for Poisson and binary
   aux.model.zeroinf.1= zeroinfl( y ~ (s1d + c1d + +s2d + c2d + s3d + c3d) * itinere + c4d + s4d +     lluvia * itinere + (s1h + c1h + s2h + c2h) * itinere   ,  data=respuesta, dist = "poisson") 
   
   aux.model = zeroinfl( y ~ (s1d + c1d + +s2d + c2d + s3d + c3d) * itinere + c4d + s4d +     lluvia * itinere + (s1h + c1h + s2h + c2h) * itinere| s1h+c1h + s2h+ c2h ,  data=respuesta, dist = "poisson")
   summary(aux.model)
   aux.model.zeroinf = aux.model
   waldtest(aux.model.zeroinf.1, aux.model.zeroinf)
   lrtest(aux.model.zeroinf.1, aux.model.zeroinf)
   
   
   #backward stepwise variable selection with significance level alpha=0.01.
   aux.model  = be.zeroinfl(aux.model.zeroinf, data=respuesta , dist="poisson", alpha=0.01, trace=FALSE)
   summary(aux.model)}

   
    #------------------------------------#   
   
   aux.df=response.hourly.itinere
   aux.y=y
   aux.a=terminos(aux.model_terminos)
   aux.b=terminos(aux.model_terminos_solar)
   aux.list=rbind(aux.a,aux.b)
   
   modelo_0=crear_modelo_zero(aux.df,aux.y,aux.list)
   
   summary(modelo_0)
   
#------------------------------------#
  
   #Pruebas Intervalos de confianza {IM9= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*((itinere.key==1)+(itinere.key==2)),data=response.hourly.itinere, family = "poisson",control = glm.control(maxit=100))

   aux.marcador=is.element(response.hourly.itinere$year,2020)&is.element(response.hourly.itinere$day_year,290)&is.element(response.hourly.itinere$hour,8)
   aux.df= response.hourly.itinere[aux.marcador,]
   aux.model=IM9
   
   aux.df=response.hourly.itinere
   aux.marcador=is.element(response.hourly.itinere$year,2020)&is.element(response.hourly.itinere$day_year,290)&is.element(response.hourly.itinere$hour,8)
   aux.model=IM9
   int.df=as.data.frame((int_conf(aux.df,aux.marcador,aux.model))[1])
   #solapa=as.data.frame((int_conf(aux.df,aux.marcador,aux.model))[2])   #Â¿Solapa el int conf de la primera con la segunda y tercera variablEÂª, Â¿y la segunda con la tercera?...
   int.conf.df=(int_conf(aux.df,aux.marcador,aux.model))[2]
   
   
   #Diario 
   
   aux.df=response.hourly.itinere
   #aux.model=IM9
   aux.model=aux.base0
   #aux.marcador=is.element(response.hourly.itinere$year,2020)&is.element(response.hourly.itinere$hour,18)
   aux.marcador=is.element(response.hourly.itinere$year,2022)&is.element(response.hourly.itinere$hour,18)
   plot_con_int_diario(aux.df,aux.marcador,aux.model)
   
   aux.model=modelo_global3
   aux.marcador=is.element(response.hourly.itinere$year,2020)&is.element(response.hourly.itinere$hour,8)
   plot_con_int_diario(aux.df,aux.marcador,aux.model)
   
  #Horario 
   
   aux.df=response.hourly.itinere
   #aux.model=IM9
   aux.model=aux.base0
   #aux.marcador=is.element(response.hourly.itinere$year,2020)&is.element(response.hourly.itinere$hour,18)
   aux.marcador=is.element(response.hourly.itinere$year,2022)&is.element(response.hourly.itinere$day_year,64)
   plot_con_int_horario(aux.df,aux.marcador,aux.model)
   
   #int_conf_dias_2022_completo.pdf
   {
     #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\int_conf_dias_2022_completo_2.pdf'
     destination = 'D:\\Users\\??igo\\Desktop\\TFG\\Graficos 2023\\int_conf_dias_2022_completo_2.pdf'
     pdf(file=destination)
     
     dias_selec=c(1:365)
     aux.df=response.hourly.itinere
     aux.model=aux.base0
     aux.año=2022
     
     plot_con_int_horario(aux.df,aux.año,dias_selec,aux.model)
     
     #Cerrar PDF 
     dev.off() }
   
   
   aux.df=response.hourly.itinere 
   aux.y=response.hourly.itinere$y
   aux.model=modelo_global
   modelo_global_sin_NA=crear_modelo_sin_NA(aux.df,aux.y,aux.model) 
   aux.model=modelo_global_sin_NA
   modelo_global_sin_NA=crear_modelo_sin_NA(aux.df,aux.y,aux.model)  
  

#------------------------------------#   

#' [INFORME ISSLA]      
      {
        
# ANÃLISIS año 2023
      
  {
  
  #Año individual en estudio 2023 
     
  año=c(2023)
  aux.marcador=is.element(delta_09_23$year,año)
  delta_año=subset(delta_09_23,aux.marcador==TRUE)
  aux.df=delta_año
  aux.lang="ESP"  

  #Guardar grÃƒÂ¡ficos en PDF
  #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\año_2022.pdf'
  destination = 'D:\\Users\\Íñigo\\Desktop\\TFG\\Graficos 2023\\año_2023.pdf'
  pdf(file=destination)
  
  aux.y=aux.df$y    
  obten_tabla(summary(aux.y))
  aux.variable1=("y")
  aux.variable2=("y_pob")
  aux.variable3=("y_seg_soc")
  aux.com=("Aragon")
  aux.prov=("Zaragoza")
  aux.map.y=map_ALT(aux.df,aux.y,aux.variable1,aux.com,aux.prov,aux.lang)
  aux.map.y_pob=map_ALT(aux.df,aux.y,aux.variable2,aux.com,aux.prov,aux.lang)
  aux.map.y_seg_soc=map_ALT(aux.df,aux.y,aux.variable3,aux.com,aux.prov,aux.lang)
  c(aux.map.y,  aux.map.y_pob,  aux.map.y_seg_soc)
  
  tabla_worst=map_worst(aux.df,aux.y,aux.com=NULL,aux.prov=NULL,aux.lang=NULL)
  df_to_latex(tabla_worst)
  
  

  {
    
    aux.time=aux.df$month
    ALT_año(aux.df,aux.time, lang=aux.lang)

    sexo_edad(aux.df,aux.lang)
    descrip_categorica(aux.df,aux.time,"SEXO",aux.lang)
    #descrip_categorica(aux.df,aux.time,"edad_5",aux.lang)
    
    # aux.time=aux.df$month
    # aux.df$edad_10=cut(aux.df$edad, c(seq(0,100,10)),labels = c(seq(10,100,10)))
    # aux.class=aux.df$edad_10
    # box_plot_temporal2(aux.df,aux.time,aux.class,aux.lang,orden.horiz=TRUE)
  
    # aux.sexo=names(table(aux.df[["SEXO"]]))
    # for (i in 1:length(aux.sexo)){
    # aux.df2=aux.df[aux.df[["SEXO"]]==aux.sexo[i],]
    # print(aux.sexo[i])
    # aux.time=aux.df2$month
    # descrip_categorica(aux.df2,aux.time,"edad_5",aux.lang)
    # }
    
    aux.time=aux.df$day_of_week
    aux.name_classif="y"
    aux.descrip=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="mean")
    obten_tabla(t(aux.descrip))
  
    aux.time=aux.df$month
    aux.descrip=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="mean")
    obten_tabla(t(aux.descrip))
   
    grado(aux.df, lang=aux.lang)
    
    aux.df=delta_año
    aux.time=aux.df$month
    aux.class.categoricas=c("lluvia","alcance","itinere","itinere.key")
    descrip_categorica(aux.df,aux.time,aux.class.categoricas,aux.lang)
    
    aux.name_classif="lluvia"
    aux.df=delta_año
    aux.class.time="month"

    año=c(2023)
    aux.marcador=is.element(bases4$year,año)
    aux.lluvia.real=subset(bases4,aux.marcador==TRUE)
    aux.descrip=func.tasa.discrete.descrip.quantile(aux.df, aux.class.time, aux.name_classif,aux.lluvia.real,aux.lang, FUN="quantile")
    obten_tabla(t(aux.descrip))
    
    aux.classif="accidentes"
    accidentes_dia_semana(delta_año,aux.classif, orden.horiz=TRUE, aux.lang)  
    itinere.key_dia_semana(delta_año,aux.classif,max.indiv=FALSE, orden.horiz=TRUE, aux.lang)  
    itinere.key_dia_semana(delta_año,aux.classif,max.indiv=TRUE, orden.horiz=TRUE, aux.lang)
  
    accidentes_todos_los_dias_semana_color(delta_año,aux.classif)  
    ida_vuelta_todos_los_dias_semana_color(delta_año,aux.classif,acc.indiv=TRUE)  
    
    aux.df=delta_año
    efecto_hora(aux.df,aux.classif)
    accidentes_año(aux.df,aux.classif,aux.lang)
    efecto_dia_año(aux.df,aux.classif,aux.superposition=FALSE,aux.lang)
    
    años=c(2021:2023)
    aux.marcador=is.element(delta_09_23$year,años)
    aux.df=subset(delta_09_23,aux.marcador==TRUE)
    efecto_dia_año(aux.df,aux.classif,aux.superposition=TRUE,aux.lang)
  }
  
  #Análisis en conjunto de años
  
    año=c(2021:2023)
    aux.marcador=is.element(delta_09_23$year,año)
    delta_año=subset(delta_09_23,aux.marcador==TRUE)  
    for (i in 1:length(año)){
      aux.año=año[i]
      print(aux.año)
      aux.marcador=is.element(delta_año$year, aux.año)
      aux.df=subset(delta_año, aux.marcador==TRUE)
      print(table(aux.df$year))
      sexo_edad(aux.df,aux.lang)
      #ALT_año(aux.df, lang=aux.lang)
    }
    
  
    aux.marcador.año=c(2019,2021:2023)
    
    aux.marcador=is.element(delta_09_23$year,aux.marcador.año)
    delta_año=subset(delta_09_23,aux.marcador==TRUE) 
    edades=seq(5,75,5)
    aux.df=delta_año
    aux.sexo=c("H","M")
    for (i in 1:length(aux.sexo)) {
      aux.marcador.sexo=aux.sexo[i]
      print(aux.marcador.sexo)
      ALT_sec_soc=ecdf_sexo_edad(delta_año,aux.marcador.año,aux.marcador.sexo,aux.lang=NULL)
      obten_tabla(ALT_sec_soc[[2]])
    }
    
  
    for (i in 1:length(aux.marcador.año)) {
      año=aux.marcador.año[i]
      aux.marcador=is.element(ALT_sec_soc$year,año)
      df_ALT_sec_soc=subset(ALT_sec_soc,aux.marcador==TRUE) 
      df_to_latex(df_ALT_sec_soc)
    }
    
    aux.marcador.año=c(2022,2023)
    aux.marcador=is.element(delta_09_23$year,aux.marcador.año)
    aux.df=subset(delta_09_23,aux.marcador==TRUE) 
    aux.y="y"
    tabla_all=tabla_all_prov(aux.df,aux.y,aux.prov,aux.lang=NULL)
    df_to_latex(tabla_all)
    
  #Cerrar PDF 
  dev.off() 
  
  #Evolución total 2009-2023
  

  df=delta_09_23
  aux.marcador.año=names(table(df$year))
  #aux.categorica="GRADO"
  aux.categorica="itinere.key"
  df$aux.grado=as.factor(df[[aux.categorica]])
  grado.df=data.frame()
  for (i in 1:length(aux.marcador.año)) {
    año=aux.marcador.año[i]
    aux.marcador=is.element(df$year,año)
    delta_año=subset(df,aux.marcador==TRUE)
    grado.año=as.matrix.data.frame(t(table(delta_año$aux.grado)))
    rownames(grado.año)=año
    grado.df=rbind(grado.df,grado.año)
  }
  colnames(grado.df)=names(table(df$aux.grado))
  obten_tabla(grado.df)
  
  }
        
      #------------------------------------# 
      
#COMPARATIVA 2021-2023, FRENTE A 2017-2019
   
   {
   #años=c(2009:2016)
   #años=c(2017:2019)
   #años=c(2020:2022)
   años=c(2021:2023)
   
   #df=delta_poligonos
   
   #Guardar grÃ¡ficos en PDF
   #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\graficos_21_23.pdf'
   destination = 'D:\\Users\\Íñigo\\Desktop\\TFG\\Graficos 2023\\graficos_20_23.pdf'
   pdf(file=destination)
   
   analisis_general(delta_poligonos,años)
   informe(delta_poligonos,años)

   #Cerrar PDF 
   dev.off() 
   
   #ANÃLISIS INDIVIDUALIZADO POR AÃ‘OS
   
   
   #Guardar grÃ¡ficos en PDF
   #destination = 'C:\\Users\\inigo\\Desktop\\TFG\\Graficos 2023\\graficos_individualizados_23.pdf'
   destination = 'D:\\Users\\Íñigo\\Desktop\\TFG\\Graficos 2023\\graficos_individualizados23.pdf'
   pdf(file=destination)
   
   periodo=c(2021:2023)
   for (i in 1:length(periodo)) {
     años=periodo[i]
     analisis_general(delta_poligonos,años)
     informe(delta_poligonos,años)  
   }
   
   #Cerrar PDF 
   dev.off() 
   
   }   
   
      } 
      
#------------------------------------#   
   
   
#MALPICA
   
   {
#El cÃ³digo postal de Poligono Industrial Malpica en Zaragoza es 50016.
#https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjVhOyuvJ_9AhXeg_0HHWqGCCkQFnoECBEQAw&url=https%3A%2F%2Fwww.codigo-postal.info%2Fzaragoza%2Fzaragoza%2Fpoligono-industrial-malpica%23%3A~%3Atext%3DEl%2520c%25C3%25B3digo%2520postal%2520de%2520Poligono%2520Industrial%2520Malpica%2520en%2520Zaragoza%2520es%252050016.&usg=AOvVaw0v9bXOO6JhQDhThUnm4KYs

delta$malpica=is.element(delta$COPOSTALE ,c(50016)) #Codigo de la empresa contratante
delta$malpica.ocurrencia=is.element(delta$CODPOSTALCT ,c(50016)) #Codigo de la empresa donde ha ocurrido el accidente
table(delta$malpica, delta$malpica.ocurrencia)

malpica=delta$malpica
#malpica=delta$malpica.ocurrencia
malpica=as.factor(malpica)
levels(malpica)=c(NA,"MALPICA")
delta$MALPICA=malpica

prueba=delta %>% mutate (mycol4 = coalesce(PLAZA,FIGUERUELAS,CUARTE,VILLANUEVA, MALPICA)) 
#prueba=cbind(prueba, mycol4 = na.omit(unlist(data[-1])))
prueba=subset(prueba, (!is.na(prueba$mycol4)))
delta_P2=prueba
colnames(delta_P2)[which(names(delta_P2) == "mycol4")] ="ubicacion"
delta_poligonos_ampliacion=delta_P2

tamaño=delta_poligonos_ampliacion$PLANTILLAE
tamaño=cut(tamaño, breaks=c(0,50,249,Inf))
levels(tamaño)
levels(tamaño)=c("PequeÃ±a EÂª","Mediana EÂª","Gran EÂª")
delta_poligonos_ampliacion$tamaño=tamaño

años=c(2009:2016)
#años=c(2017:2019)
años=c(2020:2022)

informe_ampliacion(delta_poligonos_ampliacion,años)
informe(delta_poligonos,años)

  
}

#------------------------------------#

#Effects

M_R_todos=glm(y~(c1d+c1h+c2h+c3h+s3d+s3h+ubic_cuarte+ubic_cuarte*s3h+ubic_figueruelas+ubic_figueruelas*c2h*c1d+ubic_figueruelas*c3h+ubic_figueruelas*c4d+ubic_figueruelas*s3d+ubic_figueruelas*s3h+ubic_villanueva),data=respuesta_poligono, family = "poisson",control = glm.control(maxit=100))
plot_model(M_R_todos, type = "pred", terms = "ubic_cuarte")

lluvia_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia, data=respuesta,family = "poisson",control = glm.control(maxit=100))
plot_model(lluvia_glm, type = "pred", terms = "lluvia")

respuesta_polig_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+ubicacion, data=respuesta_poligono,family = "poisson",control = glm.control(maxit=100))
plot_model(respuesta_polig_glm, type = "pred", terms = c("ubicacion","lluvia"))



#------------------------------------#


#------------------------------------#













#------------------------------------#


