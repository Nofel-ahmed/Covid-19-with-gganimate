library(tidyverse)
library(lubridate)
library(readr)
library(reshape2)
library(gganimate)

covid_data <- read_csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv")
# write_csv(covid_data, "data/covid_data.csv")
# covid_data <- read_csv("data/covid_data.csv")
names(covid_data)[2] <- "country"

formated_covid <- melt(covid_data, id.vars = 1:4, variable.name = "date") %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(stringr::str_split(date, "/"))) %>% 
  group_by(country, date) %>% 
  summarise(total = sum(value)) %>% 
  mutate(new_cases = ifelse(!is.na(lag(total)), total - lag(total), total)) %>% 
  ungroup()

total_daily <- formated_covid %>% 
  select(date, new_cases) %>% 
  group_by(date) %>% 
  summarise(td = sum(new_cases)) %>% 
  mutate(w.cases = cumsum(td)) %>% 
  ungroup()

formated_covid <- formated_covid %>% 
  right_join(total_daily, by = "date")

covid_tidy <- formated_covid %>% 
  group_by(date) %>% 
  mutate(rank = rank(-total, ties.method = "first")) %>% 
  arrange(date, rank) %>% 
  filter(rank <= 10) %>% 
  arrange(rank) %>% 
  ungroup()



staticplot <- covid_tidy  %>% #filter(date %in% seq(as.Date("2020-7-25")-10, as.Date("2020-7-25"), by = "days")) %>% 
  ggplot(aes(rank, group = country, fill = country)) +
  geom_tile(aes(y = total/2, height = total, width = .9), alpha = .8, colour = "black") +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse("") +
  geom_text(aes(y = 0, label = paste(country, " "), hjust = 1), size = 10) +
  geom_text(aes(y = total, label = paste0(scales::comma(total))), hjust = 0, size = 10) +
  guides(fill = FALSE) +
  geom_text(aes(x = 7,y = 3500000, label = paste0("Total cases: \n", scales::comma(w.cases))), size = 10, color = "grey31") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.title.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = .2, color = "grey"),
        panel.grid.minor.x = element_line(size = .1, color = "grey"),
        plot.title=element_text(size=30, hjust=0.5, face="bold", colour="grey31"),
        plot.subtitle=element_text(size=28, hjust=0.5, face="italic", color="grey31"),
        axis.text.x = element_text(size = 25),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(2,8,2,8, "cm"))

anim <- staticplot + transition_states(date, transition_length = 5,
                                       state_length = .5) +
  labs(title = "Covid-19 world wide total cases",
                                  subtitle = 'Month: {lubridate::month(closest_state, label = TRUE, abbr = F)}')

animate(anim, nframes = 1800, width = 1200, fps = 30, height = 1000, 
        renderer = gifski_renderer("covid_anim.gif"))

