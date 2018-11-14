library(rtweet)

bbox <- c(-70, -32, -58, -28)
dir <- "data"
file_name <- "stream"
tws <- stream_tweets2(q = bbox,
  file_name = file_name,
                     timeout = Inf,
                     parse = FALSE, 
                     append = TRUE,
                     dir = dir)
