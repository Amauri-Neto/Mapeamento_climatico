#carregar os pacotes
pacman::p_load('raster','ggplot2','ggspatial', 'tidyverse',"TSA","tseries", "astsa" , "forecast","sf")

#carregar o shapefile
municipios_cariri = st_read("C:/Users/Amauri/Desktop/estatistica pastas/Artigos_Leituras/shapes/shape_municipios_cariri.shp")
municipios_cariri <- st_transform(municipios_cariri, crs = 4326)
#definir o local dos arquivos.
setwd('C:/Users/Amauri/Desktop/dados wordclim/wc2.1_cruts4.06_2.5m_Tmin_1980-1989')

#ler os rasters da pasta.
lista = dir(pattern = 'tif')
lista
#carregar os rasters
dados =raster::stack(lista)


#cortar e redimencionar os rasters de acordo com o shape
raster_cortado <- crop(dados, municipios_cariri)
raster_mascarado <- mask(raster_cortado,municipios_cariri) 

#extrair os valores de cada ponto no raster
pontos = rasterToPoints(raster_mascarado)

#calcular a media por mês
media_mes = c()

for(i in 3:122) {
  
  media= mean(pontos[,i])
  media_mes[i]= media                     
}                 


mean(pontos[,3])                   

media_mes = media_mes[3:length(media_mes)]
media_ano= mean(media_mes[1:12])


par(mfrow=c(1,1))

#criar graficos
# Carregar o raster
raster_data <- raster_mascarado

# Carregar o shapefile
shapefile_data <- municipios_cariri

# Converter o raster em um data frame
raster_decada = mean(stack(raster_mascarado))
raster_decada = mean(stack(raster_mascarado))
raster_df <- as.data.frame(raster_decada, xy = TRUE)

#escala de cores
cores_intermediarias <- c("blue", "yellow", "orange", "red")
#grafico espacial


mapa1 <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = shapefile_data, fill = "transparent", color = "black",linewidth = 0.8) +
  coord_sf() +
  annotation_north_arrow(style = north_arrow_nautical)+
  scale_fill_gradientn(colours = cores_intermediarias, na.value = NA) +
  labs(x = "Longitude", y = "Latitude", fill = "temperatura") +
  ggtitle("Temperatura mínima no cariri paraibano entre 1980 e 1989") +
  annotation_scale(location = 'br') +
   theme_minimal()






#grafico dos pontos
serie_historica=data.frame(valores= media_mes,mes= 1:length(media_mes))

#linha=ggplot(data = serie_precipitação,aes(x=mes,y=valores))+
#geom_point()+
# geom_line(color='blue',size=1)+
# theme_bw()+
#ggtitle('série temporal da precipitação no cariri ocidental')+
#labs(x='meses', y= 'chuva em milimetros')


#transformar em serie temporal

serie_precipitacao = ts(media_mes,frequency = 12,start = c(1980,1))
ts.plot(serie_precipitacao)


serie_df <- data.frame(data = (time(serie_precipitacao_total4)), valor = as.numeric(serie_precipitacao_total4))

# Plotar a série temporal com informações de datas
ggplot(data = serie_df, aes(x = data, y = valor)) +
  geom_line(color='blue', size= 0.5) +
  labs(x = "Data", y = "Precipitação", title = "Série Temporal de Precipitação")+
  theme_bw()





#mapa espacial por decada


raster_decada = mean(stack(raster_mascarado))







#para 1990
#definir o local dos arquivos.
setwd('C:/Users/Amauri/Desktop/dados wordclim/wc2.1_cruts4.06_2.5m_Tmin_1990-1999')

#ler os rasters da pasta.
lista2 = dir(pattern = 'tif')
lista2
#carregar os rasters
dados2 =raster::stack(lista2)


#cortar e redimencionar os rasters de acordo com o shape
raster_cortado2 <- crop(dados2, municipios_cariri)
raster_mascarado2 <- mask(raster_cortado2,municipios_cariri) 

#extrair os valores de cada ponto no raster
pontos2 = rasterToPoints(raster_mascarado2)

#calcular a media por mês
media_mes2 = c()

for(i in 3:122) {
  
  media= mean(pontos2[,i])
  media_mes2[i]= media                     
}                 


mean(pontos2[,3])                   

media_mes2 = media_mes2[3:length(media_mes2)]
media_ano2= mean(media_mes2[1:12])
media_mes_total= c(media_mes,media_mes2)




serie_precipitacao_total = ts(media_mes_total,frequency = 12,start = c(1980,1))
ts.plot(serie_precipitacao_total)

#grafico espacial 1990-1999
# Converter o raster em um data frame
raster_decada2 = mean(stack(raster_mascarado2))

raster_df2 <- as.data.frame(raster_decada2, xy = TRUE)

#escala de cores
#cores_intermediarias <- c("red", "yellow", "orange", "blue")
#grafico espacial
mapa2 = ggplot() +
  geom_tile(data = raster_df2, aes(x = x, y = y, fill = as.numeric(layer))) +  # Converte para numérico
  geom_sf(data = shapefile_data, fill = "transparent", color = "black", linewidth = 0.8) +
  #scale_fill_gradient(low = "lightblue", high = "darkred", na.value = "white")  +
  scale_fill_gradientn(colours = cores_intermediarias , na.value = NA) + 
  labs(x = "Longitude", y = "Latitude", fill = "temperatura") +  # Adiciona rótulos aos eixos e à legenda de cores
  ggtitle("Temperatura minima no cariri paraibano entre 1990 e 1999",) +
  annotation_north_arrow(style = north_arrow_nautical)+
  annotation_scale(location= 'br')+
  coord_sf() +  # Para manter a proporção dos eixos
  theme_minimal()  # Remover todos os elementos do tema


#para 2000
#definir o local dos arquivos.
setwd('C:/Users/Amauri/Desktop/dados wordclim/wc2.1_cruts4.06_2.5m_Tmin_2000-2009')

#ler os rasters da pasta.
lista3 = dir(pattern = 'tif')
lista3
#carregar os rasters
dados3 =raster::stack(lista3)


#cortar e redimencionar os rasters de acordo com o shape
raster_cortado3 <- crop(dados3, municipios_cariri)
raster_mascarado3 <- mask(raster_cortado3,municipios_cariri) 

#extrair os valores de cada ponto no raster
pontos3 = rasterToPoints(raster_mascarado3)

#calcular a media por mês
media_mes3 = c()

for(i in 3:122) {
  
  media= mean(pontos3[,i])
  media_mes3[i]= media                     
}                 


mean(pontos3[,3])                   

media_mes3 = media_mes3[3:length(media_mes3)]
media_ano3= mean(media_mes3[1:12])
media_mes_total= c(media_mes,media_mes2, media_mes3)




serie_precipitacao_total2 = ts(media_mes_total,frequency = 12,start = c(1980,1))
ts.plot(serie_precipitacao_total2)



#grafico espacial 1990-1999
# Converter o raster em um data frame
raster_decada3 = mean(stack(raster_mascarado3))

raster_df3 <- as.data.frame(raster_decada3, xy = TRUE)

#escala de cores
#cores_intermediarias <- c("red", "yellow", "orange", "blue")
#grafico espacial
mapa3 = ggplot() +
  geom_tile(data = raster_df3, aes(x = x, y = y, fill = as.numeric(layer))) +  # Converte para numérico
  geom_sf(data = shapefile_data, fill = "transparent", color = "black",linewidth = 0.8)+
  #scale_fill_gradient(low = "lightblue", high = "darkred", na.value = "white")  +
  scale_fill_gradientn(colours = cores_intermediarias , na.value = NA) + 
  labs(x = "Longitude", y = "Latitude", fill = "temperatura") +  # Adiciona rótulos aos eixos e à legenda de cores
  ggtitle("Temperatura minima no cariri paraibano entre 2000 e 2009",) +
  annotation_north_arrow(style = north_arrow_nautical)+
  annotation_scale(location= 'br')+
  coord_sf() +  # Para manter a proporção dos eixos
  theme_minimal()  # Remover todos os elementos do tema





#para 2010
#definir o local dos arquivos.
setwd('C:/Users/Amauri/Desktop/dados wordclim/wc2.1_cruts4.06_2.5m_Tmin_2010-2019')

#ler os rasters da pasta.
lista4 = dir(pattern = 'tif')
lista4
#carregar os rasters
dados4 =raster::stack(lista4)


#cortar e redimencionar os rasters de acordo com o shape
raster_cortado4 <- crop(dados4, municipios_cariri)
raster_mascarado4 <- mask(raster_cortado4,municipios_cariri) 

#extrair os valores de cada ponto no raster
pontos4 = rasterToPoints(raster_mascarado4)

#calcular a media por mês
media_mes4 = c()

for(i in 3:122) {
  
  media= mean(pontos4[,i])
  media_mes4[i]= media                     
}                 


mean(pontos4[,3])                   

media_mes4 = media_mes4[3:length(media_mes4)]
media_ano4= mean(media_mes4[1:12])
media_mes_total= c(media_mes,media_mes2, media_mes3,media_mes4)




serie_precipitacao_total4 = ts(media_mes_total,frequency = 12,start = c(1980,1))
ts.plot(serie_precipitacao_total4)
mean(media_mes4)


#grafico espacial 2010-2020
# Converter o raster em um data frame
raster_decada4 = mean(stack(raster_mascarado4))

raster_df4 <- as.data.frame(raster_decada4, xy = TRUE)

#escala de cores
cores_intermediarias <- c("blue", "yellow", "orange", "red")
#grafico espacial
mapa4 = ggplot() +
  geom_tile(data = raster_df4, aes(x = x, y = y, fill = as.numeric(layer))) +  # Converte para numérico
  geom_sf(data = shapefile_data, fill = "transparent", color = "black", linewidth = 0.8)+
  #scale_fill_gradient(low = "lightblue", high = "darkred", na.value = "white")  +
  scale_fill_gradientn(colours = cores_intermediarias , na.value = NA) + 
  labs(x = "Longitude", y = "Latitude", fill = "temperatura") +  # Adiciona rótulos aos eixos e à legenda de cores
  ggtitle("temperatura minima no cariri paraibano entre 2010 e 2020",) +
  annotation_north_arrow(style = north_arrow_nautical)+
  annotation_scale(location= 'br')+
  coord_sf() +  # Para manter a proporção dos eixos
  theme_minimal()  # Remover todos os elementos do tema


serie_df <- data.frame(data = (time(serie_precipitacao_total4)), valor = as.numeric(serie_precipitacao_total4))
lm(serie_df$valor ~ c(1:480))
linha=ggplot(data = serie_df,aes(x=data,y=valor))+
  #geom_point()+
  geom_line(color='blue',size=0.6)+
  geom_smooth(method = 'lm', col= 'red')+
  theme_bw()+
  ggtitle('série temporal das temperaturas minimas no cariri ocidental')+
  labs(x='Tempo', y= 'chuva em milimetros')

#salvando os graficos

setwd('C:/Users/Amauri/Desktop/arquivos pibic')
ggsave("tmin_1980.png", plot = mapa1, width = 10, height = 6, dpi = 300)
ggsave("tmin_1990.png", plot = mapa2, width = 10, height = 6, dpi = 300)
ggsave("tmin_2000.png", plot = mapa3, width = 10, height = 6, dpi = 300)
ggsave("tmin_2010.png", plot = mapa4, width = 10, height = 6, dpi = 300)
ggsave("serie_tmin_reg.png", plot = linha, width = 10, height = 6, dpi = 300)
