library(tidyverse)
library(lubridate)
library(gganimate)
library(plotly)
library(purrr)
library(showtext)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(animation)
library(ggforce)
library(patchwork)
library(scales)

# Cria paleta de cor de climate stripes
pal_strip <- c(
  "#00264d",
  "#2a486f",
  "#3f6ca6",
  "#8cbbd9",
  "#c3e4ef",
  "#eaf7fa",
  "#fedc81",
  "#fda64e",
  "#f25626",
  "#cc0000",
  "#990024",
  "#660018"
)

# Trabalhando com dados NASA --------------------------------------------------------

# Tirados daqui: https://svs.gsfc.nasa.gov/4975

# Lendo
url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"
temperature <- read_csv(url, skip = 1, col_types = cols(.default = "n")) %>% janitor::clean_names()

# Transformando
temperature <- temperature %>% 
  filter(!year==2023) %>% 
  select(1:13) %>% 
  pivot_longer(2:13,names_to='month') %>%
  mutate(mes_index = case_when(month=='jan' ~ 1,
                   month=='feb' ~ 2,
                   month=='mar' ~ 3,
                   month=='apr' ~ 4,
                   month=='may' ~ 5,
                   month=='jun' ~ 6,
                   month=='jul' ~ 7,
                   month=='aug' ~ 8,
                   month=='sep' ~ 9,
                   month=='oct' ~ 10,
                   month=='nov' ~ 11,
                   month=='dec' ~ 12,))

# Tirando a média anual para usar no climate stripes
temperature_anual <- temperature %>% 
  group_by(year) %>% summarise(value=mean(value))

temperature_anual %>% 
  ggplot() +
  aes(x=year, y=1, fill=value) +
  geom_tile() +
  scale_fill_gradientn(colours = pal_strip)

# Extrai a correspondência entre códigos de cor e valores de temperatura para handoff para desenvolvimento
# Criando a paleta de cores para a visualização das bolinhas
pal_strip <- c("#08306b", "#08519c", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#deebf7", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d")

# Rescale os valores de temperatura para o intervalo [0,1]
temp_values <- rescale(temperature_anual$value)

# Criar uma função de cores
color_func <- grDevices::colorRampPalette(colors = pal_strip)

# Mapear os valores de temperatura para as cores correspondentes na paleta de cores
hex_codes <- color_func(length(temp_values))

# Crie a tabela de correspondência
correspondence_table <- data.frame(year = temperature_anual$year,
                                   value = round(temperature_anual$value,2), 
                                   hex_code = hex_codes
                                   ) 

# Trabalhando com dados do Living Planet Index --------------------------------------

lpi <- read_csv("C:/Users/rodol/Downloads/LivingPlanetIndexDatabase_2023-03-27_20.05.48/LivingPlanetIndex_2022_PublicData/LPD2022_public.csv", 
                na = "NULL", trim_ws = FALSE)
lpi <- lpi %>% 
  pivot_longer(33:103, names_to='ano')
lpi$ano <- as.double(lpi$ano)
lpi$Binomial <- str_replace_all(lpi$Binomial,"_", " ")

# CICLO 3 ---------------------------------------------------------------------------

# Cria uma checagem para ver quais os estudos com os maiores time ranges (maior sum de ano_preenchido)
lpi %>% 
  mutate(ano_preenchido = case_when(is.na(value) ~ 0,
                             TRUE ~ 1)) -> lpi_timetest
lpi_timetest %>% 
  group_by(ID, Citation, Class, Common_name, Method, Country) %>% 
  summarise(timerange = sum(ano_preenchido), populacao_mediana = median(value, na.rm=T))

# Cria lista de espécies selecionadas dali a partir do ID (seleção feita manualmente considerando diversidade geográfica, de classe, métodos de população via indivíduos e grandes timeranges)
selecionadas <- c(2092, 
                  1415, 
                  8957, 
                  19301, 
                  19129, 
                  27913,
                  10734,
                  27735,
                  7838,
                  13304, 
                  632,
                  2037,
                  5425,
                  27919,
                  2086
)

# Prepara DF temperature, tira a média das medições mensais, junta com os hexadecimais de cor
temperature %>% 
  select(year, value) %>% 
  rename(temp_anomaly_celsius = value) %>% 
  filter(year >= 1950 & year <= 2020) %>%
  group_by(year) %>% 
  summarise(temp_anomaly_celsius=mean(temp_anomaly_celsius)) -> j_temperature

left_join(j_temperature, correspondence_table, 'year') %>% 
  select(year, temp_anomaly_celsius, hex_code) -> j_temperature

# Prepara DF lpi_timetest e filtra espécies
lpi_timetest %>% 
  select(ID, Binomial, Common_name, Class, Method, Units, value, ano, Country) %>% 
  rename(id = ID, name = Binomial, common_name = Common_name, class = Class, method = Method, units = Units, population_count = value, year = ano, country=Country) %>% 
  filter(id %in% selecionadas) -> j_lpi

# Usa método de LOCF para preencher NAs que interrompem a série e remove os NAs das pontas
j_lpi <- data.frame(j_lpi) %>%  
  group_by(id)
j_lpi <- j_lpi %>% 
 tidyr::fill(population_count) %>% 
  filter(!is.na(population_count))

# JUNTA DFs
petri <- left_join(j_temperature, j_lpi, 'year')

# Faz normalização min-max com limite de 5000 pontos
max_value <- max(petri$population_count)
min_value <- min(petri$population_count)

petri$points <- (petri$population_count - min_value) / (max_value - min_value) * 5000

# Arredonda valores de indivíduos e de pontos
petri <- petri %>% 
  mutate(population_count = round(population_count, 0), points = round(points, 0))

# CHECAGENS -------------------------------------------------------------------------

# Cria uma checagem da diversidade geográfica entre as espécies selecionadas
petri %>%
  count(country, class, common_name, sort=T)

# Cria uma checagem de métodos de coleta entre as espécies selecionadas
petri %>%
  count(method, sort=T)

# CRIA PLOT PARA VER RANGE TEMPORAL DAS ESPECIES SELECIONADAS
petri %>%
  ggplot() +
  aes(x=year, y=common_name, fill=population_count) +
  geom_tile() -> p
ggplotly(p)  

# CRIA PLOT DO NÚMERO DE PONTOS POR ESPECIE
petri %>% 
  ggplot() +
  aes(x=year, y=population_count, color=common_name) +
  geom_point() -> p
ggplotly(p)


# PREPARANDO SAÍDA ------------------------------------------------------------------

# REORGANIZA TABELA PARA ARIEL
petri_untidy <-  petri %>% 
  mutate(pop_year = paste("POP",year,sep=''), point_year = paste("POINT",year,sep='')) %>% 
  select(name, common_name, year, population_count, pop_year, points, point_year) 

petri_untidy_pop <- petri_untidy %>% 
  pivot_wider(id_cols = c(name, common_name), names_from = pop_year, values_from = population_count)

petri_untidy_point <- petri_untidy %>% 
  pivot_wider(id_cols = c(name, common_name), names_from = point_year, values_from = points)

left_join(petri_untidy_pop, petri_untidy_point, by=c('name','common_name')) -> petri_untidy

# ESCREVE CSVs PARA ARIEL
write.csv(petri_untidy, 'petri_sample.csv')
write.csv(correspondence_table, 'temperaturas_cor.csv')

