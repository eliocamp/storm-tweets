library(rtweet)
library(data.table)
library(ggplot2)
library(gganimate)
library(lubridate)
library(magrittr)
# source("~/RELAMPAGO/check_tweets.R")
library(hrbrthemes)
theme_set(theme_ipsum_rc())

map <- rnaturalearth::ne_states("Argentina", returnclass = "sf")

 source("get_tweets.R")


keywords <- list(electric = c("rayo", "rayos", "trueno", "truenos", "tormenta", "tormentas", "relámpago", "relámpagos"),
                 pp = c("lluvia", "lluvias", "llueve"),
                 severe = c("tornado", "granizo", "graniza"),
                 impacts = c("inundado", "inundacion"))
removes <- c("alerta", "aviso", "river", "boca", "partido", "libertadores",
             "partidos", 
             "conmebol", "suspendido", "suspende", "suspendió", "superclásico")


buildq <- function(key, remove = removes) {
  yes <- paste0(unlist(key), collapse = " OR ")
  no <- paste0("-", remove, collapse = " ")
  paste0(yes, " ", no)
}


tw <- search_tweets(buildq(keywords), n = 20000, 
                    include_rts = FALSE, 
                    retryonratelimit = TRUE,
                    geocode = geocode) 


tw <- lat_lng(tw) %>% 
  as.data.table() %>% 
  # .[lat %between% bbox[c(2, 3)] & lng %between% bbox[c(1, 4)]] %>% 
  identity()

type_lab <- c(electric = "Electric activity", 
              pp = "Rain", 
              severe = "Severe events",
              impacts = "Flooding")

tw[, names(keywords) := lapply(keywords, function(k) detect(tolower(text), k))]
tw[, irrelevant := detect(tolower(text), removes)]

tw[, is_report := !irrelevant & (electric | pp | severe | impacts)]


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
