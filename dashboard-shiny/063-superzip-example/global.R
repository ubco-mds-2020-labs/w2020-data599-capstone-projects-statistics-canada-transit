library(dplyr)

allblocks <- read.csv("../../data/score_sets/vancouver_db_details.csv")
allblocks$pop <- as.numeric(allblocks$pop)
allblocks$id <- jitter(allblocks$id)
allblocks$population <- jitter(allblocks$pop)
allblocks$latitude <- jitter(allblocks$lat)
allblocks$longitude <- jitter(allblocks$lon)
allblocks$score <- jitter(allblocks$score)
row.names(allblocks) <- allblocks$id

cleantable <- allblocks %>%
  select(
    Id = id,
    Score = score,
    Population = population,
    Lat = latitude,
    Long = longitude
  )
