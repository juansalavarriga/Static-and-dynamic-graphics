# Packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gganimate")
install.packages("gifski")
install.packages("hrbrthemes")
install.packages("plotly")
install.packages("av")

library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)
library(hrbrthemes)
library(plotly)
library(av)


# Data

covid<- read.csv("https://raw.githubusercontent.com/jmcastagnetto/covid-19-peru-data/main/datos/covid-19-peru-data.csv")

str(covid)

covid$date <- as.Date(covid$date)

table(covid$region)

# Only Lima

covid_Lima <- covid %>%
  group_by(date) %>%
  mutate(Lima_total_casos= sum(confirmed[region=="Lima"],
                               confirmed[region=="Lima Metropolitana"],
                               confirmed[region=="Lima Región"],na.rm = TRUE)) %>%
  summarize(Casos_confirmados_Lima= mean(Lima_total_casos))


# Static

covid_Lima %>%
  ggplot(aes(x=date, y=Casos_confirmados_Lima))+
  geom_line()+
  theme_ipsum()

# Missings

covid_Lima <- covid_Lima[covid_Lima$Casos_confirmados_Lima !=0,]

# Interactive

covid_Lima %>%
  plot_ly(x= ~date, y= ~Casos_confirmados_Lima  ) %>%
  add_lines()

# Animate

Covid_Lima_plot<-covid_Lima %>%
  ggplot(aes(x=date, y = Casos_confirmados_Lima))+
  geom_line() +
  theme_ipsum()+
  transition_reveal(date)

# Another example

#Scatter plot

covid_regiones<- covid %>%
  filter(region != "Lima" & region != "Lima Metropolitana" & region != "Lima Región") %>% 
  filter( !is.na(region))

# Interactive

covid_plot_inter<-ggplot(covid_regiones, aes(total_tests, confirmed, size=deaths, color=region))+
  geom_point()+
  theme_bw()

ggplotly(covid_plot_inter)

# Animate

covid_plot<-ggplot(covid_regiones, aes(total_tests, confirmed, size=deaths,color=region))+
  geom_point()+
  theme_bw()+
  labs(title = "d?a:{frame_time}")+
  transition_time(date)

animate(covid_plot,duration=15,renderer = gifski_renderer())

# Save 
anim_save("covid_plot.gif",covid_plot)


###### Barplot animate #####

fifa<-read.csv("https://github.com/cnc8/fifa-world-ranking/raw/master/fifa_ranking-2020-12-10.csv")


str(fifa)

fifa$rank_date <- as.Date(fifa$rank_date)

# Rank

fifa_ranking<- fifa %>%
  group_by(rank_date) %>%
  mutate(ranking=rank(-total_points,ties.method="first"))

#Filter the top ten since 2015

fifa_ranking<-fifa_ranking %>%
  group_by(rank_date) %>%
  filter(ranking<=10 & rank_date>as.Date("2015-01-01")) %>%
  ungroup()


plot_ranking<-ggplot(fifa_ranking, aes(ranking, total_points, group= country_abrv,
                                       fill=as.factor(country_abrv), color=as.factor(country_abrv))) +
    geom_col()+
    coord_flip(clip="off")+
  scale_x_reverse()+
  guides(color=FALSE, fill=FALSE)+
  labs(title = "{closest_state}", x="", y ="FIFA POINTS")+
  geom_text(aes(y=0, label=country_full,hjust=1))+
  theme(axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        panel.background = element_blank())+
  transition_states(rank_date, transition_length = 4, state_length = 1)+
  enter_fade()+
  exit_fade()

#Animated
plot_ranking_a<-animate(plot_ranking, width=700, height= 432, fps=11,duration=22, renderer=gifski_renderer())

#Video
plot_ranking_a<-animate(plot_ranking, width=700, height= 432, fps=11,duration=22, renderer=av_renderer())


#Save

anim_save("plot_ranking_a.avi", plot_ranking_a)


