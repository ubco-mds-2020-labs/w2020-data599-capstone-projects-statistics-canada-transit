library(dplyr)

allblocks <- read.csv("../../data/score_sets/long_score.csv")
allblocks$pop <- as.numeric(allblocks$pop)
allblocks$id <- jitter(allblocks$fromId)
allblocks$population <- jitter(allblocks$pop)
allblocks$latitude <- jitter(allblocks$lat)
allblocks$longitude <- jitter(allblocks$lon)
allblocks$score <- jitter(allblocks$value)

row.names(allblocks) <-seq.int(nrow(allblocks))


cleantable <- allblocks %>%
  select(
    Id = id,
    Score = score,
    Population = population,
    Lat = latitude,
    Long = longitude
  )
