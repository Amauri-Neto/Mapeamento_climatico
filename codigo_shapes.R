
library(raster)


setwd('C:/Users/Amauri/Desktop/estatistica pastas/Artigos_Leituras/shapes')

par(mfrow=c(1,1))

regiao <- shapefile(x = 'PB_Microrregioes_2022/PB_Microrregioes_2022.shp')
plot(regiao)
regiao$NM_MICRO
regiao$CD_MICRO
regiao$SIGLA_UF
regiao@data
cariri_ocidental = regiao[10,]
plot(cariri_ocidental, main= 'cariri ocidental',axes=T, graticule= T,col= 'gray')
 shapefile(cariri_ocidental,filename= 'shape_cariri_ocidental.shp')
 axis(2)
 axis(1)
 
 paraiba <- shapefile(x = 'PB_municipios_2022/PB_municipios_2022.shp')
 plot(paraiba)
 paraiba@data
  plot()     
sf(pmascara)
  
paraiba@data
municipios_cariri = paraiba[c(19,10,51,63,65,106,124,133,134,152,184,194,198,201,212,214,223),]


shapefile(municipios_cariri,filename= 'shape_municipios_cariri.shp',overwrite=TRUE)
plot(municipios_cariri)
par(mfrow=c(1,2))


seu_shapefile <- st_read("PB_municipios_2022/PB_municipios_2022.shp")

# Verificar a estrutura do arquivo
str(seu_shapefile)



ggplot() +
  geom_sf(data = seu_shapefile , fill = ifelse(NM_MUN== lista, 'red', 'gray')) +
  theme_void()

ggplot() +
  geom_sf(data = municipios, fill= 'red' , size = 20 ) +
  theme_void()



write.csv2(municipios, "nome_do_arquivo.csv", row.names = FALSE)

lista =  municipios$NM_MUN

seu_dataframe <- st_drop_geometry(seu_shapefile)

# Salvar como CSV
write.csv2(seu_dataframe, "nome_do_arquivo.csv", row.names = FALSE)



ggplot() +
  geom_sf(data = seu_shapefile) +  # Plotar todos os polígonos do shapefile
  geom_sf(data = municipios, fill = "red") +  # Destacar apenas os polígonos das cidades desejadas
  labs(title = "") +
  theme_void()


