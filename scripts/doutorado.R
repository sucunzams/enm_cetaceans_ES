##Analises capitulo 2 da tese de doutorado -- modelagem de nicho ecologico de cetaceos no litoral do ES##

#linha teste github



#lendo pacotes####

library("spThin")
library("dplyr")
library("sp")
library("sf")
library("ggspatial")
library("ggplot2")
library("dplyr")
library("tidyr")
library("forcats")
library("stringr")
library("patchwork")
library("janitor")
library("sdmpredictors")
library("terra")
library("raster")

#visualizando os dados####

analises %>% dplyr::group_by(year,season, species) %>% dplyr::summarise(n_sightings = n())

df<-
  dir(path = "./dados/", pattern = "*.csv", ignore.case = TRUE, full.names = TRUE) %>% 
  purrr::map_df(~readr::read_csv2(., col_types = readr::cols(.default = "c"))) %>%
  janitor::clean_names()

df<-df[,c(1:18)]

df<-df[!is.na(df$data),]

df$data<-as.POSIXct(df$data,format="%d/%b/%y")

df<-df %>% mutate(month=lubridate::month(data),
                  year=lubridate::year(data),.after = data) %>% 
  mutate(season=case_when(
    month==1|month==2~"summer",
    month==8|month==9~"winter"
    ))

df$especie<-janitor::clean_names(df$especie)



##trabalhando com a planilha consolidada de cetaceos e lendo dados espaciais####


cetaceans<-read.csv2(file="./data/cetaceans.csv")

str(cetaceans)

numeric_cols <- c("latitude", "longitude", "group_size", "calves", "beaufort", "year")

cetaceans[numeric_cols] <- lapply(cetaceans[numeric_cols], as.numeric)

factor_cols <- c("season", "species")

cetaceans[factor_cols] <- lapply(cetaceans[factor_cols], as.factor)

rm("factor_cols", "numeric_cols")

cetaceans <- cetaceans[, c("longitude", "latitude", "species")]
table(cetaceans$species)
cetaceans<- cetaceans [,-3]


pontoporia<- cetaceans[c(cetaceans$species=="pontoporia"),]  
pontoporia<- pontoporia [,-3]

sotalia<- cetaceans[c(cetaceans$species=="sotalia"),]  

sp_thin <- thin(
  loc.data = pontoporia,
  lat.col = "latitude",
  long.col = "longitude",
  spec.col = "species",
  thin.par = 5, # espacialização em km
  reps = 100,
  locs.thinned.list.return = TRUE,
  write.files = FALSE,
  write.log.file = FALSE
)


#selecionando apenas as avistagens de toninha no data frame cetaceans

pont_summer<-cetaceans[c(cetaceans$species=="pontoporia" & cetaceans$season=="summer"),c("season","latitude", "longitude", "species")]
str(pont_summer)
head(pont_summer)

pont_summer<-pont_summer[,-c(1,4)]


#usando a funcao coordinates do pacote sp para transformar o data frame x em um arquivo spatial points
coordinates(pontoporia)<- ~longitude + latitude
coordinates(cetaceans)<- ~longitude + latitude

plot(cetaceans)
str(pontoporia)
class(pontoporia) 
class(cetaceans)
head(pontoporia)
rm(bat_es_pontoporia)

#conferindo qual o N total de cada sp
table(cetaceans$species)

#lendo shp brasil estadual
brazil<-read_sf("./shp/brasilestadual.shp")

#plotando o shp 
plot(brazil)


#criando o mapa base####

brazil_base_map <- 
  ggplot(data = brazil) + 
  geom_sf(color = "black", fill = "lightgrey") + 
  coord_sf(xlim = c(-38, -41), ylim =  c(-18,-21)) + 
  theme_bw()+
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering(),
                                    location = "br", 
                                    pad_x = unit(0.25, "cm"), pad_y = unit(0.55, "cm"),
                                    height = unit(0.8, "cm"), width = unit(0.8, "cm"))



#Plotando o data frame pont_summer no mapa do brasil####

library("maps")

world <- map_data("world") #carregando o mapa mundi no objeto world
head(world)

#mapa do mundo

ggplot(data= pont_summer)+
  geom_polygon(data= world, aes(x= long, y= lat, group= group), fill= "grey", alpha = 1) #alpha controla a transparencia da figura

#delimitando a area: funcao coord_quickmap estabelece os limites da area a partir do mapa maior (world)

ggplot(data= pont_summer)+
  geom_polygon(data= world, aes(x= long, y= lat, group= group), fill= "grey", alpha = 1) +
  coord_quickmap(xlim = c(-42, -38), ylim =  c(-22, -18))+
  labs(title="", x="", y="") + theme_bw()+
  geom_point(aes(x= longitude, y= latitude))

#formatando os eixos e mudando o tema: funcao scale delimita os eixos x e y, funcao labs para colocar titulo no mapa e/ou nos eixos, funcao theme muda o fundo do mapa em comparacao ao mapa anterior

ggplot(data= pont_summer)+
  geom_polygon(data= world, aes(x= long, y= lat, group= group), fill= "grey", alpha = 1) +
  coord_quickmap(xlim = c(-50, -35), ylim =  c(-25, -15))+
  scale_x_continuous(breaks = seq(-50, -35, by= 5), label= c("50°", "45°", "40°", "35°W"))+
  scale_y_continuous(breaks = seq(-25, -15, by= 5), label= c("25°", "20°", "15°N"))+
  labs(title="", x="", y="") + theme_bw()

#formatando o tema: funcao theme formata o estilo das legendas dos eixos x e y, se eu quiser formatar apenas um dos eixos eu preciso indicar na funcao; funcao panel.grid altera as linhas de grade do mapa, .minor e element_blank em branco indica que quero eliminar a linha de grade menor  

ggplot(data= pont_summer)+
  geom_polygon(data= world, aes(x= long, y= lat, group= group), fill= "grey", alpha = 1) +
  coord_quickmap(xlim = c(-50, -35), ylim =  c(-25, -15))+
  scale_x_continuous(breaks = seq(-50, -35, by= 5), label= c("50°", "45°", "40°", "35°W"))+
  scale_y_continuous(breaks = seq(-25, -15, by= 5), label= c("25°", "20°", "15°N"))+
  labs(title="", x="", y="") + theme_bw() +
  theme(axis.text = element_text(size= 14, colour = "gray32"),
        panel.grid.minor = element_blank())

#adicionando pontos: a funcao geom_point adiciona os pontos da planilha indicada em data

ggplot(data= pont_summer)+
  geom_polygon(data= world, aes(x= long, y= lat, group= group), fill= "grey", alpha = 1) +
  coord_quickmap(xlim = c(-50, -35), ylim =  c(-25, -15))+
  scale_x_continuous(breaks = seq(-50, -35, by= 5), label= c("50°", "45°", "40°", "35°W"))+
  scale_y_continuous(breaks = seq(-25, -15, by= 5), label= c("25°", "20°", "15°N"))+
  labs(title="", x="", y="") + theme_bw() +
  theme(axis.text = element_text(size= 14, colour = "gray32"),
        panel.grid.minor = element_blank()) +
  geom_point(aes(x= longitude, y= latitude))


#criando mapa de tamanho de grupo por estacao para cada especie####

map_grp_by_season_facetgrid <-
  brazil_base_map +
  ## Bubbles
  geom_point(data = cetaceans[!is.na(cetaceans$group_size),],
             aes(x = longitude, y = latitude,
                 size = group_size, fill = season),
             shape = 21, alpha = 0.5) +
  
  # scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9")) +
  scale_fill_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9")) +
  scale_size_binned(range = c(1, 10), breaks = c(1,5,10,15,25), name = "group_size") +
  ## Facet
  facet_grid(cols = vars(season), rows = vars(species)) +
  guides(colour = "none", fill = "none") +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(vjust = 1, hjust=1, angle = 45))

#exportando o mapa gerado

ggsave(map_grp_by_season_facetgrid,
       filename = "./results/Exploratory_Data_Analysis/map_grps_by_season.pdf",
       height = 20, width = 15, units = "cm", dpi = 300)


#frequencia de ocorrencia e frequencia numerica####

funs <- list(freq_occ = ~ sum(.x >= 1)/n() *100,
             freq_num = ~ sum(.x)/sum(dplyr::pick(total_counts)) *100)

spp_cols<- unique(cetaceans$species)

data_species_fo_nf <-
  cetaceans %>% 
  dplyr::group_by(date, season, species) %>%
  dplyr::summarise(n = sum(group_size), .groups = "drop") %>%
  pivot_wider(id_cols = c(date, season),
              names_from = "species",
              values_from = "n",
              values_fill = 0) %>% 
  dplyr::mutate(delphinidae=tidyr::replace_na(delphinidae,0),
                sotalia=tidyr::replace_na(sotalia,0),
                pontoporia=tidyr::replace_na(pontoporia,0),
                megaptera=tidyr::replace_na(megaptera,0),
                tursiops=tidyr::replace_na(tursiops,0)) %>% 
  dplyr::mutate(total_counts = rowSums(across(all_of(spp_cols)))) %>% 
  dplyr::group_by(season) %>%
  dplyr::summarise(across(all_of(spp_cols), .fns = funs)) %>%
  tidyr::pivot_longer(cols = !season, 
                      names_to = "species_freq",
                      values_to = "value") %>%
  dplyr::mutate(value = round(value, digits = 2)) %>% 
  
  # split name into variables
  tidyr::separate(species_freq, 
                  into = c("species", "freq"),
                  sep = -8)%>% 
  # remove an extra underline
  dplyr::mutate(species = stringr::str_sub(species, end = -2))
                               

plot_freq_occ <-
  data_species_fo_nf %>% 
  dplyr::filter(freq == "freq_occ") %>% 
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species), value), 
                y = value, 
                fill = season)) + 
  geom_col() +
  scale_fill_manual(values = c("summer" = "#E69F00", 
                               "winter" = "#56B4E9")) +
  facet_grid(~ season) +
  ylab("Frequency of occurrence (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 9))

plot_freq_num <-
  data_species_fo_nf %>% 
  dplyr::filter(freq == "freq_num") %>% 
  ggplot(., aes(x = forcats::fct_reorder(as.factor(species), value), 
                y = value, 
                fill = season)) + 
  geom_col() +
  scale_fill_manual(values = c("summer" = "#E69F00", 
                               "winter" = "#56B4E9")) +
  facet_grid(~ season) +
  ylab("Relative abundance (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 9))

freqs_occ_num <-
  plot_freq_occ / plot_freq_num

ggsave(freqs_occ_num,
       filename = "./results/Exploratory_Data_Analysis/spp_frqs-occ-num.pdf",
       width = 15, height = 15, units = "cm", dpi = 300)

table(cetaceans$species, cetaceans$season, cetaceans$year)
table(cetaceans$species, cetaceans$year)

options(digits=2)
prop.table(table(cetaceans$species, cetaceans$season))


#funcao histogram do pacto lattice
histogram(cetaceans$species)


#baixando as variaveis ambientais e extraindo os valores de cada avistagem####

#mostra somente as camadas marinhas dentro do banco de dados
list_layers(marine=T)

#cria um novo objeto com a variaveis selecionadas (usando os nomes obtidos na funcao anterior)

bathymetry<- load_layers("MS_bathy_5m")


#cria um objeto que vai ser o "molde" para o corte das camadas
molde<-extent(-50, -33, -24, -15)

#Teste para ver se a area criada anteriormente "molde" esta correta para cortar
plot(bathymetry)
plot(molde,add=T)

#corta todas as camadas salvas no objeto bathymetry com base no molde, e salva no objeto bathymetry_es
bathymetry_es<-crop(bathymetry,molde)
plot(bathymetry_es)

#definindo datum -- conferir isso pois quando deu certo eu nao fiz esse passo
crdref <- CRS('+proj=longlat +datum=WGS84')


#transformar de .csv para spatial points
cetaceans<-SpatialPoints(cetaceans,proj4string = crdref)

#usando a funcao coordinates do pacote sp para transformar o data frame x em um arquivo spatial points
coordinates(pontoporia)<- ~longitude + latitude
coordinates(cetaceans)<- ~longitude + latitude

#visualizacao dos pontos
plot(pontoporia)
plot(cetaceans)

#para extrair os valores das variaveis para cada registro de avistagem, neste exemplo estou extraindo especificamente os valores de batimetria; a planilha de pontos tem que ser convertida de .csv para spatial points, o que foi feito nos passos anteriores

bat_es_cetaceans<-extract(bathymetry_es, cetaceans)

table(bat_es_cetaceans)
class(bat_es_cetaceans)
table(is.na(bat_es_cetaceans))
any(is.na(bat_es_cetaceans))

as.data.frame(bat_es_cetaceans)
class(bat_es_cetaceans)

cetaceans$bathymetri <- bat_es_cetaceans
mean(cetaceans$bathymetri)

mean(bat_es_cetaceans)
mean(bat_es_pontoporia)

sd(bat_es_pontoporia)
summary(bat_es_pontoporia)
hist(bat_es_pontoporia)

#plotando o raster bathymetry_es e plotando os pontos cetaceans sob o raster

plot(bathymetry_es)
points(cetaceans, pch=1, cex= 0.75)

