#### Pacotes utilizados ####

if(!require(gganimate)){install.packages("gganimate"); require(gganimate)} # construção gráficos animados
if(!require(tidyverse)){install.packages("tidyverse"); require(tidyverse)} # manipulação de dados
if(!require(gapminder)){install.packages("gapminder"); require(gapminder)} # banco de dados
if(!require(av)){install.packages("av"); require(av)} # renderizar no formato de vídeo
if(!require(rsvg)){install.packages("rsvg"); require(rsvg)} # modificar device para svg
if(!require(ggmap)){install.packages("ggmap"); require(ggmap)} # theme_nothing para animação ilusão de óptica
if(!require(grid)){install.packages("grid"); require(grid)} # animação ilusão de óptica

options(gganimate.device='svg')

# Gráficos gapminder ----

## Gráfico de Bolhas Expectativa de Vida versus PIB per capita (pacote gapminder)

ggm <- ggplot(gapminder,aes(x = gdpPercap, y=lifeExp, size = pop, colour = continent)) +
  geom_point(show.legend = TRUE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "PIB per capita", y = "Expectativa de vida")+ 
  geom_jitter(alpha = 1/3, size = 3)+
  transition_time(year) +
  labs(title = "Ano: {frame_time}") +
  view_follow(fixed_y = TRUE)

anim_save("Gifs/animggm.gif",ggm)

# Gráficos airquality ----

airquality <- airquality %>% mutate(Month = as.factor(airquality$Month)) %>%
  rename(Mês = Month)

## Gráfico de Linhas Temperatura ao longo dos dias do mês (pacote airquality)

gt1 <- ggplot(airquality,aes(Day, Temp, color = Mês)) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Dia do Mês", y = "Temperatura") +
  theme(legend.position = "top") + geom_point(aes(group = seq_along(Day))) +
  transition_reveal(Day)

anim_save("Gifs/animgt1.gif",gt1)
anim_save("Gifs/animgt1png.gif",animate(device='png',gt1))

## Gráfico de Barras Temperatura Média por Mês (pacote airquality)

airquality_temp_med <- airquality%>%
  group_by(Mês) %>%
  summarise(Temperatura = mean(Temp))

gt2 <- ggplot(airquality_temp_med,aes(x=Mês,y=Temperatura,fill=Temperatura))+ 
  geom_col()+ 
  transition_states(Mês, wrap = F) +
  shadow_mark() +
  enter_grow() +
  enter_fade()

anim_save("Gifs/animgt2.gif",gt2)

# Gráfico de Linhas Vento ao longo dos dias do mês (pacote airquality) / saída em vídeo

gv <- ggplot(airquality,aes(Day, Wind, color = Mês)) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Dia do Mês", y = "Vento") +
  theme(legend.position = "top") + geom_point(aes(group = seq_along(Day))) +
  transition_reveal(Day)

anim_save("Videos/animgv.mp4",animate(gv,device = 'png',renderer = av_renderer()))

anim_save("Gifs/animgv.gif",gv)



############## ANIMAÇÕES CURIOSAS ##############


## Ilusão de Óptica ----

# Coordinates for 10 squares
d <- data.frame(
  x1 = 1:10, x2 = 2:11,
  y1 = 4, y2 = 5,
  t = 1:10
)

# Creating the gradient background
g <- rasterGrob(
  t(colorRampPalette(c("#000000", "#FFFFFF"))(1000)),
  width = unit(1, "npc"), height = unit(1, "npc")
)

# Creating the animation

gio <- ggplot() +
  annotation_custom(g, -Inf, Inf, -Inf, Inf) +
  geom_rect(
    data = d,
    mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
    color = "black", fill = "#7E7E7E"
  ) +
  ylim(c(1, 8)) +
  theme_nothing() +
  transition_time(t)

anim_save("Gifs/anim_io.gif",animate(gio,dev='png'))


## Fogos de artifício ----

colours <- c(
  'lawngreen',
  'gold',
  'white',
  'orchid',
  'royalblue',
  'yellow',
  'orange'
)
# Produce data for a single blast
blast <- function(n, radius, x0, y0, time) {
  u <- runif(n, -1, 1)
  rho <- runif(n, 0, 2*pi)
  x <- radius * sqrt(1 - u^2) * cos(rho) + x0
  y <- radius * sqrt(1 - u^2) * sin(rho) + y0
  id <- sample(.Machine$integer.max, n + 1)
  data.frame(
    x = c(x0, rep(x0, n), x0, x),
    y = c(0, rep(y0, n), y0, y),
    id = rep(id, 2),
    time = c((time - y0) * runif(1), rep(time, n), time, time + radius + rnorm(n)),
    colour = c('white', rep(sample(colours, 1), n), 'white', rep(sample(colours, 1), n)),
    stringsAsFactors = FALSE
  )
}
# Make 20 blasts
n <- round(rnorm(20, 30, 4))
radius <- round(n + sqrt(n))
x0 <- runif(20, -30, 30)
y0 <- runif(20, 40, 80)
time <- runif(20, max = 100)
fireworks <- Map(blast, n = n, radius = radius, x0 = x0, y0 = y0, time = time)
fireworks <- dplyr::bind_rows(fireworks)

fogosartificio <- ggplot(fireworks) + 
  geom_point(aes(x, y, colour = colour, group = id), size = 0.5, shape = 20) + 
  scale_colour_identity() + 
  coord_fixed(xlim = c(-65, 65), expand = FALSE, clip = 'off') +
  theme_void() + 
  theme(plot.background = element_rect(fill = 'black', colour = NA), 
        panel.border = element_blank()) + 
  # Here comes the gganimate code
  transition_components(time, exit_length = 20) + 
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_wake(0.05, size = 3, alpha = TRUE, wrap = FALSE, 
              falloff = 'sine-in', exclude_phase = 'enter') + 
  exit_recolour(colour = 'black')

anim_save("Gifs/fogosartificio.gif",fogosartificio)
