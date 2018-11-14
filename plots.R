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

tw <- readRDS("data/tweets.Rds")

MakeTimeline <- function(tweets, aggregate = 5, unit = "minutes", upload = FALSE) {
  aggregate  <- paste0(aggregate, " ", unit)
  g <- tweets[] %>% 
    .[, time := round_date(created_at, aggregate) - hours(3)] %>%
    # .[ time < round_date(now(), "1 hour")] %>% 
    .[, .(time, electric, pp, severe, impacts)] %>% 
    melt(id.vars = "time", variable.name = "type") %>% 
    .[, .(N = sum(value)), by = .(type, time)] %>% 
    .[, maxn := sum(N), by = .(type)] %>% 
    .[, type := reorder(type, -maxn)] %>% 
    ggplot(aes(time, N, color = type)) +
    geom_line() +
    geom_smooth(se = FALSE, method = "gam", formula = y ~ s(as.numeric(x))) +
    scale_color_brewer(palette = "Set1", guide = "none") +
    scale_x_datetime("Time") +
    scale_y_continuous("Number of tweets (no RT)") +
    facet_wrap(~type, scales = "free_y", labeller = labeller(type = type_lab)) +
    labs(title = "Twitter and the storm")
  
  if (interactive()) print(g)
  ggsave("twit_timeline.png", g, height = 340/72, width = 480/72)
  
  if (isTRUE(upload)) googledrive::drive_upload("twit_timeline.png", "twit_timeline.png")
}

MakeTimeline(tw, 30)


# tw <- readRDS("2018-11-11_tweets.Rds")
# tw[, placing := ifelse(is.na(lat), "location" , "tweet")] %>%
#   .[is.na(lat) & !is.na(location),
#     c("lat", "lng") := nominatim_osm(location)]
# tw <- tw[!is.na(lat)]

# tw %>% 
#   ggplot() +
#   geom_sf(data = map) +
#   geom_point(aes(lng, lat)) 
  
MakeAnimation <- function(tweets, aggregate = 5, unit = "minutes", upload = FALSE) {
  aggregate  <- paste0(aggregate, " ", unit)
  g <- tweets %>% 
    .[!is.na(lat)] %>% 
    .[, .N, by = .(lng, lat, time = round_date(created_at, aggregate) - hours(3))] %>%
    tidyr::complete(tidyr::nesting(lng, lat), time, fill = list(N = 0)) %>%
    ggplot() +
    geom_sf(data = map) +
    geom_point(aes(lng, lat, size = N, color = N, group = interaction(lng, lat))) +
    coord_sf(ylim = c(-35, -27), xlim = c(-70, -55),
             label_axes = "----") + 
    scale_size_area(guide = "none", max_size = 8) +
    scale_x_continuous("") +
    scale_y_continuous("") +
    # scale_color_viridis_c() +
    scale_color_distiller(palette = "YlOrRd", direction = 1, guide = "none") +
    theme(axis.ticks = element_blank()) +
    transition_time(time) +
    labs(title = "Twitter and the storm",
         subtitle = "{frame_time}") +
    NULL
  
  
  
  
  anim_save("twitstorm.gif", 
            animate(g, nframes = 300, duration = 10, width = 480, height = 340))
  if (isTRUE(upload)) googledrive::drive_upload("twitstorm.gif", "twitstorm.gif")
}

MakeAnimation(tw, 15)

tweets <- tw
aggregate <- "1 hour"
series <- tweets[] %>% 
  .[, time := round_date(created_at, aggregate) - hours(3)] %>%
  # .[ time < round_date(now(), "1 hour")] %>% 
  .[, .(time, electric, pp, severe, impacts)] %>% 
  melt(id.vars = "time", variable.name = "type") %>% 
  .[, .(N = sum(value)), by = .(type, time)] %>% 
  .[, maxn := sum(N), by = .(type)] %>% 
  .[, type := reorder(type, -maxn)] %>% 
  ggplot(aes(time, N, color = type)) +
  # geom_vline(aes(xintercept = time, color = type), 
             # size = 0.3, alpha = 0.7) +
  geom_line() +
  # geom_smooth(se = FALSE, method = "gam", formula = y ~ s(as.numeric(x))) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_x_datetime("Time") +
  scale_y_continuous("Number of tweets (no RT)") +
  facet_wrap(~type, ncol = 1, scales = "free_y", labeller = labeller(type = type_lab)) +
  # labs(title = "Twitter and the storm") +
  # transition_time(time) +
  transition_reveal(type, time)
  
tweet_map <- tweets %>% 
  .[!is.na(lat)] %>%
  .[, time := round_date(created_at, aggregate) - hours(3)] %>%
  # .[ time < round_date(now(), "1 hour")] %>% 
  .[, .(lng, lat, time, electric, pp, severe, impacts)] %>% 
  melt(id.vars = c("lng", "lat", "time"), variable.name = "type") %>% 
  .[, .(N = sum(value)), by = .(lng, lat, time, type)] %>%
  tidyr::complete(tidyr::nesting(lng, lat, type), time, fill = list(N = 0)) %>%
  as.data.table() %>% 
  .[, maxn := sum(N), by = .(type)] %>% 
  .[, type := reorder(type, -maxn)] %>% 
  .[order(N), ] %>% 
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
  theme(axis.ticks = element_blank()) +
  transition_time(time) +
  labs(title = "Twitter and the storm") +
  NULL

N <- 100

series <- animate(series, nframes = N, duration = 30, width = 480, height = 340)

tweet_map <- animate(tweet_map, nframes = N, duration = 30, width = 480, height = 340)

a_mgif <- image_read(tweet_map)
b_mgif <- image_read(series)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 2:N){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}
save_animation(new_gif, "map-time.gif")
