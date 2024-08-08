#carregar os pacotes
pacman::p_load('raster','ggplot2','ggspatial', 'tidyverse',"TSA","tseries", "astsa" , "forecast","viridis","psych","summarytools","gridExtra","gganimate")

#carregar o shapefile
municipios_cariri = shapefile(x='C:/Users/Amauri/Desktop/estatistica pastas/Artigos_Leituras/shapes/shape_municipios_cariri.shp')

#definir o local dos arquivos.
setwd('C:/Users/Amauri/Desktop/dados cobertura vegetal')

#ler os rasters da pasta.
lista = dir(pattern = 'tif')
lista
#carregar os rasters
dados =raster::stack(lista[37])

tempo <- c()
tempo[1] = system.time(raster_cortado <- crop(dados, municipios_cariri))

tempo[2] = system.time(raster_mascarado <- mask(raster_cortado,municipios_cariri)) 

rm(dados)
rm(raster_cortado)
#converter para raster
raster_df <- as.data.frame(raster_mascarado$brasil_coverage_2022, xy = TRUE)

#nomeando o shape
shapefile_data <- municipios_cariri

#alterando os nomes das variaveis
names(raster_df) <- c("x", "y", "valor")
raster_df -> dados_raster

# Definir cores para cada valor
cores <- c("brown", "blue", "green")
#codificando as cores
dados_raster$cor <- ifelse(dados_raster$valor == 33, cores[2],
                           ifelse(dados_raster$valor %in% c(1, 3, 4,5,49,10,12,50), cores[3], cores[1]))
#na.omit(dados_raster)


#dev.new()

# Plot com cores fixas
mapa = ggplot(dados_raster, aes(x = x, y = y, fill = cor)) +
  geom_raster() +
  scale_fill_manual(values = setNames(cores, cores),na.value = NA,labels= c('corpo de água', 'sem vegetação', 'vegetação')) +
  theme_minimal()+ 
  geom_polygon(data = shapefile_data, aes(x = long, y = lat, group = group), fill = "transparent", color = "black",size= 1,) +
  #geom_path(data = shapefile_data, aes(x = long, y = lat, group = group), color = "black", size = 0.7) +
  # Remove o tema para uma melhor visualização
  annotation_north_arrow()+
  labs(x = "Longitude", y = "Latitude", fill = "Densidade vegetal") +  # Adiciona rótulos aos eixos e à legenda de cores
  ggtitle("Densidade vegetal do cariri ocidental paraibano em 2022",)




# Calcular frequências
frequencias <- table(dados_raster$cor)

# Transformar frequências em um data frame
dados_df <- data.frame(categoria = names(frequencias), frequencia = as.numeric(frequencias))
dados_df <- transform(dados_df, percentual = paste0(round(frequencia/sum(frequencia)*100, 1), "%"))

# Criar o gráfico de pizza com ggplot2
grafico_pizza <- ggplot(dados_df, aes(x = "", y = frequencia, fill = categoria)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start=0) +
  labs(title = "Gráfico de Pizza", fill = "Categoria", y = "Frequência") +
  geom_text(aes(label = percentual), position = position_stack(vjust = 0.5),color="black") + # Adiciona as porcentagens
  scale_fill_manual(values = c("blue" = "blue", "green" = "green", "brown" = "brown"), labels= c("água", "aréa sem vegetação", "vegetação")) + # Define as cores
  theme_minimal() +
  labs(x="",y="",title = "")+
  theme(legend.position = "bottom")+
theme(legend.position = "none") +  # Remove a legenda
  theme(plot.margin = margin(0.2, 0.2,0.2 , 0.2, "cm"))  # Define margens para ajustar o tamanho do gráfico


# Mostrar o gráfico
#print(grafico_pizza)



# Exibir os dois gráficos lado a lado
grafico = grid.arrange(mapa,grafico_pizza, ncol = 2, widths = c(0.7, 0.3))
ggsave("grafico_2022.png", plot =grafico, width = 12, height = 10, dpi = 450)





vetor_agua <- c(0.8,0.9,mean(c(0.9,0.7)),0.7,0.7,0.6,0.6,0.6,0.5,0.4,0.5,0.5,0.4,0.3,0.3,0.3,0.3,0.4,0.4,1,0.9,0.9,0.9,1,1,0.9,0.9,0.7,0.4,0.3,0.3,0.2,0.3,0.5,0.4,0.6,0.5,0.6)
vetor_terra<- c(42.9,43,mean(c(43,46.1)),46.1,47,48.7,48.6,48.6,49,48.5,48,47.8,47.3,46.5,46.8,46.8,46.9,47.7,48.6,47.5,45.8,43.1,41.8,40.7,40,40.8,41.3,42.7,43.7,43.9,44.2,44.2,44.6,44.8,45,44.9,45,44.9)
vetor_vegetacao <- c(56.3,56.1,mean(c(56.1,53.2)),53.2,52.3,50.7,50.7,50.7,50.6,51.1,51.5,51.8,52.3,53.2,52.9,52.8,52.8,51.9,51,51.6,53.3,56,57.3,58.3,59,58.3,57.8,56.5,55.9,55.8,55.6,55.6,55.1,54.7,54.6,54.5,54.5,54.5)

dados_serie <- data.frame( 'agua'=vetor_agua, 'terra'= vetor_terra,'vegetacao'= vetor_vegetacao,'ano'= seq(1985,2022,1))

write.csv2(dados_serie,"dados_vegetação.csv")


max(dados_serie$terra)
min(dados_serie$terra) 



grafico_series=ggplot(data = dados_serie, aes(x = ano)) +
  geom_line(aes(y = terra, color = "Terra")) +
  geom_line(aes(y = agua, color = "Água")) +
  geom_line(aes(y = vegetacao, color = "Vegetação")) +
  geom_point(aes(y = terra, color = "Terra")) +
  geom_point(aes(y = agua, color = "Água")) +
  geom_point(aes(y = vegetacao, color = "Vegetação")) +
  labs(title = "Variação ao longo do tempo",
       x = "Ano",
       y = "Valor (%)") +  # Mudança no nome do eixo y
  scale_color_manual(values = c("Terra" = "brown", "Água" = "blue", "Vegetação" = "green"),
                     name = "Variável") +  # Mudança no nome da legenda
  guides(color = guide_legend(override.aes = list(shape = c(4, 5, 6)))) +
  scale_y_continuous(limits = c(0, 100)) +  # Escala de 0 a 100 em porcentagem
  theme_bw()

ggsave("grafico_series.png", plot =grafico_series, width = 12, height = 10, dpi = 450)

serie_agua =ggplot(data = dados_serie, aes(x = ano)) +
  geom_line(aes(y = agua, color = "água"))+
  labs(title = "Variação ao longo do tempo",
       x = "Ano",
       y = "Valor (%)") +
  scale_color_manual(values = 'blue',name= "Variável")+
  theme_bw()

ggsave("serie_agua.png", plot =serie_agua, width = 12, height = 10, dpi = 450)

