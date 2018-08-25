str(athlete_events)
library(dplyr)
colnames(athlete_events)[colSums(is.na(athlete_events)) > 0]
athlete_events <- mutate(athlete_events, Medal = ifelse(is.na(Medal), "No Medal earned", Medal))
athlete_events$Age_Sport <- paste(athlete_events$Age,athlete_events$Sport)
athlete_events_filter <- filter(athlete_events, is.na(Age))
athlete_events_clean_age <- mutate(athlete_events, Age = ifelse(is.na(Age) & Sport == "Gymnastics", mean(Age,na.rm = TRUE), Age))
athlete_events_clean_age <- mutate(athlete_events, Age = ifelse(is.na(Age) & Sport == "Boxing", mean(Age,na.rm = TRUE), Age))
athlete_events_35_yrs_up<- filter(athlete_events, Age >= 35)
athlete_events_Age <- filter(athlete_events, complete.cases(Age))
library(ggplot2)
ggplot(athlete_events_35_yrs_up, aes(x=Age, y=Sport)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
ggplot(athlete_events, aes(x=Age, y=Sport)) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4)     # 1/4 opacity
ggplot(athlete_events, aes(x=Age, y=Sport, color >= 35(Age))) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4, aes(color = Team))     # 1/4 opacity
ggplot(athlete_events, aes(x=Sport, y=Team, color >= 35(Age))) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4, aes(color = Age))     # 1/4 opacity
ggplot(athlete_events_Age, aes(x=Team, y=Sport)) +
  geom_point(aes(color = Age))
ggplot(athlete_events_Age, aes(x=Year, y=Sport)) +
  geom_point()
ggplot(athlete_events_Age, aes(x=Year, y=Team)) +
  geom_point()
plyr::ddply(athlete_events_Age, .(Year), summarize, num = length(Year), totalSport = sum(Sport))
sum(Sport[athlete_events_Age])
sum(athlete_events_Age, as.character('Sport'))
which( colnames(athlete_events_Age)=="Sport" )
count(athlete_events_Age, "Speed Skating")
athlete_events_Age$Sport_Year <- paste(athlete_events_Age$Sport,athlete_events_Age$Year)
count(athlete_events_Age, "Sport_Year")
table(athlete_events_Age$Sport_Year)
table(athlete_events_Age$Sport)
athlete_events_Age_test <- filter(athlete_events_Age, Sport == c("Alpine Skiing", "Archery"))
athlete_events_Age <- filter(athlete_events_Age, Sport == c("Alpine Skiing", " Athletics ", "Basketball", "Biathlon", "Boxing", "Canoeing", "Cross Country Skiing", "Cycling", "Equestriansm","Fencing", "Football", "Gymnastics", "Hockey", "Ice Hockey", "Rowing", "Sailing", "Shooting", "Speed Skating", "Swimming", "Wrestling"))