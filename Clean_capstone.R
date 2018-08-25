str(athlete_events)
library(dplyr)
colnames(athlete_events)[colSums(is.na(athlete_events)) > 0]
athlete_events <- mutate(athlete_events, Medal = ifelse(is.na(Medal), "No Medal earned", Medal))
athlete_events$Age_Sport <- paste(athlete_events$Age,athlete_events$Sport)
athlete_events <- mutate(athlete_events, Age = ifelse(is.na(Age) & Sport == "Basketball", mean(Age,na.rm = TRUE), Age))