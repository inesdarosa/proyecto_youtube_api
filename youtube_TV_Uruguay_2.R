library(tidyverse)
library(ggplot2)
library(patchwork)# para poner varios gráficos en una sola imagen

videos_comments <- read_csv('videos_tv_comments_1.csv')

head(videos_comments)

# Graficos de densidad de la variable viewCount

canales = c("TV Ciudad", "Canal 4", "Canal 10 Uruguay")

diez_cuatro_ciudad <- filter(videos_comments, channelTitle %in% canales) %>% 
  ggplot(aes(viewCount, y = ..count..)) + 
  geom_density(fill="blue") + 
  facet_wrap(~channelTitle) + 
  scale_x_continuous(trans = "log2") +
  xlab("Número de vistas por video") +
  ylab("Número de observaciones") 


canales_1 = c("Teledoce", "Medios Públicos Uruguay")

cinco_doce <- filter(videos_comments, channelTitle %in% canales_1) %>% 
  ggplot(aes(viewCount, y = ..count..)) + 
  geom_density(fill="blue") + 
  facet_wrap(~channelTitle) + 
  scale_x_continuous(trans = "log2") + 
  xlab("Número de vistas por video") +
  ylab("Número de observaciones")


vtv <- filter(videos_comments, channelTitle == "Vtv Uruguay") %>% 
  ggplot(aes(viewCount, y = ..count..)) + 
  geom_density(fill="blue") + 
  facet_wrap(~channelTitle) + 
  scale_x_continuous(trans = "log2") + 
  xlab("Número de vistas por video") +
  ylab("Número de observaciones") 

diez_cuatro_ciudad + cinco_doce + vtv + 
  plot_layout(nrow = 3)


# Graficos de densidad de la variable likeCount

diez_cuatro_ciudad_like <- filter(videos_comments, channelTitle %in% canales) %>% 
  ggplot(aes(likeCount, y = ..count..)) + 
  geom_density(fill="blue") + 
  facet_wrap(~channelTitle) + 
  scale_x_continuous(trans = "log2") +
  xlab("Número de likes por video") +
  ylab("Número de observaciones") 


cinco_doce_like <- filter(videos_comments, channelTitle %in% canales_1) %>% 
  ggplot(aes(likeCount, y = ..count..)) + 
  geom_density(fill="blue") + 
  facet_wrap(~channelTitle) + 
  scale_x_continuous(trans = "log2") + 
  xlab("Número de likes por video") +
  ylab("Número de observaciones") 


vtv_like <- filter(videos_comments, channelTitle == "Vtv Uruguay") %>% 
  ggplot(aes(likeCount, y = ..count..)) + 
  geom_density(fill="blue") + 
  facet_wrap(~channelTitle) + 
  scale_x_continuous(trans = "log2") + 
  xlab("Número de likes por video") +
  ylab("Número de observaciones") 

diez_cuatro_ciudad_like + cinco_doce_like + vtv_like + 
  plot_layout(nrow = 3)

# Graficos de densidad de la variable commentCount

diez_cuatro_ciudad_comment <- filter(videos_comments, channelTitle %in% canales) %>% 
  ggplot(aes(commentCount, y = ..count..)) + 
  geom_density(fill="blue") + 
  facet_wrap(~channelTitle) + 
  scale_x_continuous(trans = "log2") +
  xlab("Número de comentarios por video") +
  ylab("Número de observaciones") 


cinco_doce_comment <- filter(videos_comments, channelTitle %in% canales_1) %>% 
  ggplot(aes(commentCount, y = ..count..)) + 
  geom_density(fill="blue") + 
  facet_wrap(~channelTitle) + 
  scale_x_continuous(trans = "log2") + 
  xlab("Número de comentarios por video") +
  ylab("Número de observaciones") 


vtv_comment <- filter(videos_comments, channelTitle == "Vtv Uruguay") %>% 
  ggplot(aes(commentCount, y = ..count..)) + 
  geom_density(fill="blue") + 
  facet_wrap(~channelTitle) + 
  scale_x_continuous(trans = "log2") + 
  xlab("Número de comentarios por video") +
  ylab("Número de observaciones") 

diez_cuatro_ciudad_comment + cinco_doce_comment + vtv_comment + 
  plot_layout(nrow = 3)

#################################################################

# Plot, relación entre variables

plot_view_like <- videos_comments %>% ggplot(aes(viewCount, likeCount, color=channelTitle)) + 
  geom_point()

plot_view_comment <- videos_comments %>% ggplot(aes(viewCount, commentCount, color=channelTitle)) + 
  geom_point()

plot_like_comment <- videos_comments %>% ggplot(aes(likeCount, commentCount, color=channelTitle)) + 
  geom_point()

plot_view_comment + plot_view_like + plot_like_comment + 
  plot_layout(nrow = 3)

#################################################################

# Plot, relación entre variables por canal, número de visualizaciones menor a 
# 20000.

filter(videos_comments, viewCount < 20000) %>% 
  ggplot(aes(viewCount, commentCount, color=channelTitle)) + 
  geom_point() + 
  facet_wrap(~channelTitle)

filter(videos_comments, viewCount < 20000) %>% 
  ggplot(aes(viewCount, likeCount, color=channelTitle)) + 
  geom_point() + 
  facet_wrap(~channelTitle)

# Plot, relación entre commentCount y likeCount, con el número de commentCount
# menor a 500.

filter(videos_comments, commentCount < 500) %>% 
  ggplot(aes(commentCount, likeCount, color=channelTitle)) + 
  geom_point() + 
  facet_wrap(~channelTitle)





