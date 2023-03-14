library(tidyverse)
library(ggplot2)
library(patchwork)# para poner varios gráficos en una sola imagen

getwd()
setwd('C:/Users/Public/proyecto_youtube_api')

canales_tv <- read_csv('canales_tv_stats.csv')

p1 <- filter(canales_tv) %>%
  ggplot(aes(totalVideos, subscribers, color = channelName)) +
  geom_point(size = 3) + 
  scale_color_discrete(name = "Nombre del Canal de TV") + 
  xlab("Número total de videos") +
  ylab("Número de subscriptores")


p2 <- filter(canales_tv) %>%
  ggplot(aes(totalVideos, views, color = channelName)) +
  geom_point(size = 3) + 
  scale_color_discrete(name = "Nombre del Canal de TV") + 
  xlab("Número total de videos") +
  ylab("Número de videos vistos")


p3 <- filter(canales_tv) %>%
  ggplot(aes(subscribers, views, color = channelName)) +
  geom_point(size = 3) + 
  scale_color_discrete(name = "Nombre del Canal de TV") + 
  xlab("Número de subscriptores") +
  ylab("Número total de videos vistos")

# para poner varios gráficos en una sola imagen
p1 + p2 + p3 + 
  plot_layout(ncol = 2)
