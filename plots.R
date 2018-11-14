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

type_lab <- c(electric = "Electric activity", 
              pp = "Rain", 
              severe = "Severe events",
              impacts = "Flooding")

tweets <- readRDS("data/tweets.Rds")
aggregate <- "2 hours"
N <- 300

tw_series <- tweets[] %>% 
  .[, time := round_date(created_at, aggregate) - hours(3)] %>%
  # .[ time < round_date(now(), "1 hour")] %>% 
  .[, .(time, electric, pp, severe, impacts)] %>% 
  melt(id.vars = "time", variable.name = "type") %>% 
  .[, .(N = sum(value)/2), by = .(type, time)] %>% 
  .[, maxn := sum(N), by = .(type)] %>% 
  .[, type := reorder(type, -maxn)] 

  # .[time %in% unique(time)[1:3]] %>%
series <- ggplot(tw_series, aes(time, N, color = type)) +
  # geom_vline(aes(xintercept = time, color = type), 
             # size = 0.3, alpha = 0.7) +
  geom_line(aes(x = time2), color = "gray", alpha = 0.8, size = 0.3, 
            data = tw_series[, .(N, type, time2 = time)]) +
  geom_line() +
  # geom_smooth(se = FALSE, method = "gam", formula = y ~ s(as.numeric(x))) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_x_datetime("Day\n(November)", date_breaks = "24 hours", 
                   date_labels = "%d", 
                   expand = c(0, 0)) +
  scale_y_continuous("Hourly tweets \n(no RT)", 
                     breaks = function(x) pretty(x, 2)) +
  facet_wrap(~type, ncol = 1, scales = "free_y", labeller = labeller(type = type_lab)) +
  transition_reveal(type, time) +
  theme(plot.margin = unit(rep(1, 4), "lines"), 
        panel.spacing = unit(0.5, "lines"))

series <- animate(series, nframes = N, duration = 30, width = 480, height = 340)

tweet_map <- tweets %>% 
  .[!is.na(lat)] %>%
  .[, time := round_date(created_at, aggregate) - hours(3)] %>%
  # .[ time < round_date(now(), "1 hour")] %>% 
  .[, .(lng, lat, time, electric, pp, severe, impacts)] %>% 
  melt(id.vars = c("lng", "lat", "time"), variable.name = "type") %>% 
  .[, .(N = sum(value/2)), by = .(lng, lat, time, type)] %>%
  tidyr::complete(tidyr::nesting(lng, lat, type), time, fill = list(N = 0)) %>%
  as.data.table() %>% 
  .[, maxn := sum(N), by = .(type)] %>% 
  .[, type := reorder(type, -maxn)] %>% 
  .[order(N), ] %>% 
  # .[time %in% unique(time)[1:3]] %>%
  # .[N < 10, N := 0] %>%
  ggplot() +
  geom_sf(data = map) +
  geom_jitter(aes(lng, lat, size = N, color = type, group = interaction(lng, lat))) +
  coord_sf(ylim = c(-35, -27), xlim = c(-70, -55),
           label_axes = "----") + 
  scale_size_area(guide = "none", max_size = 8) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  # scale_color_viridis_c(guide = "none") +
  scale_color_brewer(palette = "Set1", guide = "none") +
  theme(axis.ticks = element_blank(), plot.margin = unit(rep(1, 4), "lines")) +
  transition_time(time) +
  labs(title = "Twitter and the storm") +
  NULL

tweet_map <- animate(tweet_map, nframes = N, duration = 30, width = 480, height = 340)

a_mgif <- image_read(tweet_map)
b_mgif <- image_read(series)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 2:N){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}
save_animation(new_gif, "map-time.gif")
