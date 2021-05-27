library(dplyr)

allblocks <- read.csv("../../data/clean/vancouver_db_allscores.csv")
allblocks$pop <- as.numeric(allblocks$pop)
allblocks$id <- jitter(allblocks$fromId)
allblocks$population <- jitter(allblocks$pop)
allblocks$latitude <- jitter(allblocks$lat)
allblocks$longitude <- jitter(allblocks$lon)

###EDITED
allblocks$score <- jitter(allblocks$nearest1_gallery_scores)
row.names(allblocks) <- allblocks$fromId

cleantable <- allblocks %>%
  select(
    fromId = fromId,
    Score = score,
    Population = population,
    Lat = latitude,
    Long = longitude
  )
