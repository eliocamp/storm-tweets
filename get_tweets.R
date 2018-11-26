library(data.table)
library(rtweet)

detect <- function(strings, patterns) {
  r <- vapply(patterns, function(p) stringi::stri_detect(strings, fixed = p), rep(TRUE, length(strings)))
  apply(r, 1, any)
}

# googledrive::drive_download("tweet_stream.json", "data/stream-1.json", overwrite = TRUE)

# double_ssh <- "ssh elio.campitelli@portal.cima.fcen.uba.ar 'ssh elio.campitelli@pikachu.cima.fcen.uba.ar \"cat /home/elio.campitelli/RELAMPAGO_tweets/data/stream-1.json\"' > ~/Documents/RELAMPAGO/data/stream-1.json"

# system(double_ssh)

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

bbox <- c(-70, -32, -58, -28)
geocode <- "-32.171,-63.814,200mi"

tw <- search_tweets(buildq(keywords), n = 18000, 
                    include_rts = FALSE, 
                    retryonratelimit = TRUE,
                    geocode = geocode) %>% 
  lat_lng() %>% 
  as.data.table() 

tw_old <- readRDS("data/tweets.Rds")

max_id <- max(tw_old$status_id)

tw <- tw[as.numeric(status_id) > as.numeric(max_id)]

message("Found ", nrow(tw), " new tweets")

tw[, names(keywords) := lapply(keywords, function(k) detect(tolower(text), k))]
tw[, irrelevant := detect(tolower(text), removes)]

tw[, is_report := !irrelevant & (electric | pp | severe | impacts)]

tw <- rbind(tw_old, tw)

saveRDS(tw, "data/tweets.Rds")
