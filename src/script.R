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
correspondence_table <- data.frame(value = temperature_anual$value, 
                                   hex_code = hex_codes, 
                                   year = temperature_anual$year)

# Trabalhando com dados do Living Planet Index --------------------------------------

lpi <- read_csv("C:/Users/rodol/Downloads/LivingPlanetIndexDatabase_2023-03-27_20.05.48/LivingPlanetIndex_2022_PublicData/LPD2022_public.csv", 
                na = "NULL", trim_ws = FALSE)
lpi <- lpi %>% 
  pivot_longer(33:103, names_to='ano')
lpi$ano <- as.double(lpi$ano)

# CICLO 3 ---------------------------------------------------------------------------

# Cria uma checagem para ver quais os estudos com os maiores time ranges (maior sum de ano_preenchido)
lpi %>% 
  mutate(ano_preenchido = case_when(is.na(value) ~ 0,
                             TRUE ~ 1)) -> lpi_timetest
lpi_timetest %>% 
  group_by(ID, Citation, Class, Common_name, Method, Country) %>% 
  summarise(timerange = sum(ano_preenchido))

# Cria lista de espécies selecionadas dali a partir do ID (seleção feita manualmente considerando diversidade geográfica, de classe, métodos de população via indivíduos e grandes timeranges)
selecionadas <- c(19301,
                  2098,
                  19313,
                  25466,
                  27918,
                  19282,
                  10392,
                  27913,
                  10734,
                  1415,
                  5425,
                  6828,
                  2092,
                  8957,
                  2037,
                  7838,
                  19528,
                  1162,
                  1565)

# Prepara DF temperature, tira a média das medições mensais, junta com os hexadecimais de cor
temperature %>% 
  select(year, value) %>% 
  rename(temp_anomaly_celsius = value) %>% 
  filter(year >= 1950 & year <= 2020) %>%
  group_by(year) %>% 
  summarise(temp_anomaly_celsius=mean(temp_anomaly_celsius)) -> j_temperature

left_join(j_temperature, correspondence_table, 'year') %>% 
  select(year, temp_anomaly_celsius, hex_code) -> 

# Prepara DF lpi_timetest e filtra espécies
lpi_timetest %>% 
  select(ID, Common_name, Class, Method, Units, value, ano, Country) %>% 
  rename(id = ID, common_name = Common_name, class = Class, method = Method, units = Units, population_count = value, year = ano, country=Country) %>% 
  filter(id %in% selecionadas) -> j_lpi

# Usa método de LOCF para preencher NAs que interrompem a série e remove os NAs das pontas
j_lpi <- data.frame(j_lpi) %>%  
  group_by(id)
j_lpi <- j_lpi %>% 
 tidyr::fill(population_count) %>% 
  filter(!is.na(population_count))

# JUNTA DFs
petri <- left_join(j_temperature, j_lpi, 'year')

# Cria coluna com número de pontos
petri <- petri %>% 
  mutate(points = population_count/1000)

# Cria uma checagem da diversidade geográfica entre as espécies selecionadas
petri %>%
  count(country, common_name, sort=T)

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

# ESCREVE CSV
write.csv(petri, 'petri_sample.csv')









# LEGADO (NAO USADO) ----------------------------------------------------------------

# CICLO 1 ---------------------------------------------------------------------------

# Carrega dados
url <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_summary.txt"
global_warming_raw <- read_tsv(url, skip = 20)
paleo <- read_csv("D:\\Profissional\\Pesquisa\\Dissertação\\PROJ Heating scales\\data\\Temp.csv")
anage <- read_csv("D:\\Profissional\\Pesquisa\\Dissertação\\PROJ Heating scales\\data\\anage.csv") %>% 
  janitor::clean_names()

# Limpa dados
global_warming <- global_warming_raw %>% 
  rename("year anomaly anomaly_unc. five_year_anomaly five_year_unc." = `% Year, Annual Anomaly, Annual Unc., Five-year Anomaly, Five-year Unc.`) %>% 
  mutate(`year anomaly anomaly_unc. five_year_anomaly five_year_unc.` = str_squish(`year anomaly anomaly_unc. five_year_anomaly five_year_unc.`)) %>% 
  separate(`year anomaly anomaly_unc. five_year_anomaly five_year_unc.`,
           into = c("year", "anomaly", "anomaly_unc", "five_year_anomaly", "five_year_unc."), 
           sep = " ") %>% 
  select(year, anomaly) %>% 
  mutate(across(where(is.character), parse_number)) %>% 
  arrange(desc(year)) %>% 
  mutate(anosantes=2020-year)


# Seleciona espécies representativas
especies <- c("Townsend's chipmunk",
              "Geoffroy's marmoset",
              "Gang-gang cockatoo",
              "Brown-headed spider monkey",
              "Human",
              "Blue whale",
              "Eastern box turtle",
              "Red sea urchin",
              "Greenland shark")


# Longevidade AnAge -------------------------------------------------------

# Plota concentricos
anage %>% 
  filter(maximum_longevity_yrs<400) %>% 
  filter(common_name %in% especies) %>% 
  ggplot(aes(0,0,size=maximum_longevity_yrs, colour=common_name)) +
  geom_point(shape=21, show.legend = T) +
  scale_radius(range = c(1, 100)) +
  guides(size="none") +
  theme_void()

# Aquecimento Berkeley ----------------------------------------------------

# Plota concentricos
global_warming %>%
  ggplot(aes(0, 0, colour = anomaly, size = year, label=year)) + 
  geom_point(shape=21, show.legend = T, stroke=2) + 
  scale_radius(range = c(1, 110)) +
  theme_void() +
  scale_colour_gradientn(colours = pal_strip) +
  guides(size="none") +
  transition_time(year) +
  shadow_mark() -> p

# Plota animado e exporta frames
animate(p, fps = 27, duration = 10, width=1080, height=1080,
        device = "png", renderer = file_renderer("~/Desktop/frames", prefix = "gganim_plot", overwrite = TRUE))

# Plota linha
global_warming %>% 
  ggplot(aes(y = anomaly, x = year, color=anomaly)) + 
  geom_line(show.legend = F, size=1) + 
  scale_radius(range = c(1, 110)) +
  theme_minimal() +
  scale_colour_gradientn(colours = pal_strip)

# Plota tiles
global_warming %>% 
  ggplot(aes(y=400, x = year, fill=anomaly)) + 
  geom_tile(show.legend = T) + 
  scale_radius(range = c(1, 110)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_gradientn(colours = pal_strip)  


# NOAA Paleoclima ---------------------------------------------------------

# Plota concentricos
paleo %>%
  ggplot(aes(0, 0, colour = temp, size = year, label=year)) + 
  geom_point(shape=21, show.legend = T, stroke=2) + 
  scale_radius(range = c(1, 110)) +
  theme_void() +
  guides(size="none") +
  scale_colour_gradientn(colours = pal_strip) +
  transition_time(year) +
  shadow_mark() -> p
animate(p, fps = 27, duration = 10, width=1080, height=1080,
        device = "png", renderer = file_renderer("~/Desktop/frames/paleoclima/", prefix = "gganim_plot", overwrite = TRUE))
ggplot(aes(0, 0, colour = temp, size = year, label=year)) + 
  geom_point(show.legend = F) + 
  scale_radius(range = c(1, 110)) +
  scale_colour_gradientn(colours = pal_strip) 

# Plota linha
paleo %>%
  ggplot(aes(x=year, y=temp, colour = temp)) + 
  geom_line() + 
  scale_radius(range = c(1, 500)) +
  theme_minimal() +
  scale_colour_gradientn(colours = pal_strip)

# Plota tiles
paleo %>%
  ggplot(aes(x=year, y=400, colour = temp)) + 
  geom_tile() + 
  scale_radius(range = c(1, 500)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_colour_gradientn(colours = pal_strip)

# https://stackoverflow.com/questions/32177132/moving-circles-randomly-created-using-d3-js




# CICLO 2 ---------------------------------------------------------------------------

font_add_google("Space Mono", "Space Mono")
# Definir a função vis_points_inside_circle
vis_points_inside_circle <- function(n, year) {
  
  # part 1 -- set up empty circle
  df_empty_circle <-
    data.frame(x = 0,
               y = 0, 
               r = 1)
  
  p_empty_circle <-
    ggplot(df_empty_circle) +
    geom_circle(aes(x0 = x, y0 = y, r = r)) + 
    coord_fixed() +
    theme_void() +
    ggtitle(paste(year)) +
    theme(plot.title = element_text(family = "Space Mono", size = rel(1.1),
                                    hjust = 0.5, vjust = 1,
                                    margin = margin(t = 10, b = 10))) +
    annotate("text", x = 0, y = -1.2, label = paste(n, "indivíduos"), family = "Space Mono", size = 3)
  
  # part 2 -- set up points scatter
  r <- runif(n)
  th <- runif(n, 0, 2 * pi)
  
  df_circular_points_scatter <- 
    data.frame(x = sqrt(r) * cos(th), 
               y = sqrt(r) * sin(th))
  
  # part 3 -- combine circle and points
  p_empty_circle +
    geom_point(data = df_circular_points_scatter, 
               aes(x = x, y = y), alpha = 0.8, size = 0.7)
}

# Criar lista com os plots
plots <- lapply(seq_along(coho_salmon$ano), function(i) {
  vis_points_inside_circle(coho_salmon$value[i]/100, coho_salmon$ano[i])
})

# Criar grid com os plots
grid <- plot_grid(plotlist = plots, ncol = 8)

# Criar animação com o grid
ani.options(interval = 0.5)

saveGIF({
  for (i in seq_along(plots)) {
    print(plots[[i]])
  }
}, movie.name = "coho_salmon.gif", width = 1500, height = 1500)

# COPIA PLOTS DO PREVIEW PRO DESK
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:\\Users\\rodol\\OneDrive\\Desktop")

# Plotando
temperature %>% 
  filter(year >= 1954) %>% 
  mutate(date = make_date(year, mes_index)) %>% 
  ggplot() +
  aes(x=date, y=1, fill=value) +
  geom_tile() +
  scale_fill_gradientn(colours = pal_strip) +
  theme_void() +
  transition_time(date) +
  view_follow(fixed_y = TRUE) +
  enter_fade() +
  exit_fade() -> tiles

date <- as.Date(tiles$data$date)


ggplot(tiles, aes(x = 0, y = 0, width = 1, height = 1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colours = pal_strip) +
  transition_time(date) +
  labs(title = "Climate Stripes Animation") +
  theme_void() +
  view_follow(fixed_y = TRUE) +
  enter_fade() +
  exit_fade()