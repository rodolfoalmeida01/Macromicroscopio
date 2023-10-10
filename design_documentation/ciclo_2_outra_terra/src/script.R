library(tidyverse)
library(terra)
library(geodata)
library(maps)

(shrew <- terra::vect("C:\\Users\\rodol\\Downloads\\IUCN_Sorex_alpinus\\IUCN_Sorex_alpinus.shp"))

map('world',xlim=c(5,30), ylim=c(40,55))
plot(shrew, col='red', add=T)







# CIANOBACTERIAS --------------------------------------------------------------------

library(tidyverse)
library(maps)
library(sf)

cyano <- read_delim("C:/Users/rodol/Downloads/0105109-230224095556074/0105109-230224095556074.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)


cyano_coord <- cyano %>% filter(!is.na(decimalLatitude))
my_sf <- st_as_sf(cyano_coord, coords = c('decimalLongitude', 'decimalLatitude'))
my_sf <- st_set_crs(my_sf, crs = 53032)

#Plot it:
ggplot(my_sf) + 
  geom_sf(aes(color = '#4ef9cc', size=0.2)) +
  facet_wrap(~month) +
  theme_void()


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