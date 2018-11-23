library(magick)
library(data.table)
library(ggplot2)
library(gganimate)
library(lubridate)
library(magrittr)
# source("~/RELAMPAGO/check_tweets.R")
library(hrbrthemes)
theme_set(theme_ipsum_rc())

map <- rnaturalearth::ne_states("Argentina", returnclass = "sf")

type_lab <- c(electric = "Actividad eléctrica", 
              pp = "Lluvia", 
              severe = "Eventos severos",
              impacts = "Inundación")

tweets <- readRDS("data/tweets.Rds")
aggregate <- "5 minutes"
N <- 300

tw_series <- tweets[] %>% 
  .[, time := round_date(created_at, aggregate) - hours(3)] %>%
  # .[ time < round_date(now(), "1 hour")] %>% 
  .[, .(time, electric, pp, severe, impacts)] %>% 
  melt(id.vars = "time", variable.name = "type") %>% 
  .[, .(N = sum(value)*60/5), by = .(type, time)] %>% 
  .[, maxn := sum(N), by = .(type)] %>% 
  .[, type := reorder(type, -maxn)]  %>% 
  .[time != max(time)] %>% 
  .[day(time) >= 22]

ggplot(tw_series, aes(time, N, color = type)) +
  # geom_vline(aes(xintercept = time, color = type), 
  # size = 0.3, alpha = 0.7) +
  geom_line() +
  # geom_smooth(se = FALSE, method = "gam", formula = y ~ s(as.numeric(x))) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_x_datetime("Día\n(Noviembre)", date_breaks = "24 hours", 
                   date_labels = "%d", 
                   expand = c(0, 0)) +
  scale_y_continuous("Tweets por hora \n (sin RT)", 
                     breaks = function(x) pretty(x, 2)) +
  facet_wrap(~type, ncol = 1, scales = "free_y", labeller = labeller(type = type_lab)) +
  # transition_reveal(type, time) +
  theme(plot.margin = unit(rep(1, 4), "lines"), 
        panel.spacing = unit(0.5, "lines"))
