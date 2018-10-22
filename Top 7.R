library(dplyr)
library(ggplot2)

load('athlete_event_whole.RData')
#Athlete_event_whole <- mutate(athlete_events, Medal = ifelse(is.na(Medal), "No Medal earned", Medal))

Athlete_event_whole <- mutate(Athlete_event_whole, Medal = ifelse(is.na(Medal), "No Medal earned", Medal))

# Which events in your dataset have the highest proportion of participants aged over 30?
Athlete_event_whole %>%
  group_by(Sport) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))
#Which countries have the highest proportion of participants aged over 30? 
Athlete_event_whole %>%
  group_by(NOC) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))
#the highest proportion of participants aged over 30 looking at Rowing and Equestrian
Compare_Row_Equest_all %>%
  group_by(Sport) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))
#the highest proportion of participants aged over 30 for top 7 countries
Top_7_Team %>%
  group_by(Team) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))

# Which events in your dataset have the highest proportion of participants aged over 30 with medals?
Athlete_event_whole %>%
  group_by(Medal) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))

#create binary results for Age and Medal
Athlete_event_whole$Age_over_30 <- ifelse(Athlete_event_whole$Age >= 30,1,0)
Athlete_event_whole$Medal_value <- ifelse(Athlete_event_whole$Medal == "No Medal earned", 0,1)

#Remove columns that are not needed
Athlete_event_whole$Height = NULL
Athlete_event_whole$Weight = NULL
Athlete_event_whole$Event = NULL

# Split out the Train and Test groups
Train_Equ <- subset(Athlete_event_whole, Sport == "Equestrianism")
Test_Row <- subset(Athlete_event_whole, Sport == "Rowing")

colnames(Athlete_event_whole)[colSums(is.na(Athlete_event_whole)) > 0]

load(athlete_events)
str(athlete_events)
library(dplyr)
#check which columns have NA 
colnames(athlete_events)[colSums(is.na(athlete_events)) > 0]
#Clean up Medal fields with NA
Top_6_Teams <- mutate(athlete_events, Medal = ifelse(is.na(Medal), "No Medal earned", Medal))
#Clean up major country changes of German and Russia to combine
Top_6_Teams <- mutate(Top_6_Teams, Team = ifelse(Team == "Soviet Union", "Russia", Team))
Top_6_Teams <- mutate(Top_6_Teams, Team = ifelse(Team == "East Germany", "Germany", Team))
Top_6_Teams <- mutate(Top_6_Teams, Team = ifelse(Team == "West Germany", "Germany", Team))

#fix age na
Top_6_Teams <- mutate(Top_6_Teams, Age = ifelse(is.na(Age), mean(Age,na.rm = TRUE), Age))
#Keep only rows of top 6 teams
Top_7_Team <- subset(Top_6_Teams, Team == "United States" | Team == "France" | Team == "Italy" | Team =="Great Britain" | 
                       Team == "Germany" | Team == "Russia" | Team == "Japan")
#check for NA in Age
table(Top_7_Team$Age)
table(Top_7_Team$Team)
#fix NA in Age by taking mean
Athlete_event_whole <- mutate(athlete_events, Age = ifelse(is.na(Age), mean(Age,na.rm = TRUE), Age))
Top_7_Team <- mutate(Top_7_Team, Age = ifelse(is.na(Age), mean(Age,na.rm = TRUE), Age))
#create new column to simplify those over / under 30
Top_7_Team$Age_over_30 <- ifelse(Top_7_Team$Age >= 30,'Yes','No')
Compare_Row_Equest_all$Age_over_30 <- ifelse(Compare_Row_Equest_all$Age >= 30,'Yes','No')
str(Top_7_Team)
#build value to medal
Top_7_Team$Medal_value <- ifelse(Top_7_Team$Medal == "Gold", 3, ifelse(Top_7_Team$Medal == "Silver", 2,
                                 ifelse(Top_7_Team$Medal == "Bronze", 1,0)))
#compare rowing vs equestrians if average age has moved up
Compare_Row_Equest <- subset(Top_7_Team, Sport == "Rowing" | Sport == "Equestrianism")
Compare_Row_Equest_all <- subset(Top_6_Teams, Sport == "Rowing" | Sport == "Equestrianism")
table(Compare_Row_Equest$Sport)
table(Compare_Row_Equest_all$Sport)
table(Compare_Row_Equest$Year)

#review the mean age
mean(Top_6_Teams$Age)
mean(Compare_Row_Equest_all$Age)
mean(Compare_Row_Equest_all$Age & Compare_Row_Equest_all$Sport == "Rowing")
mean(Compare_Row_Equest_all$Age & Compare_Row_Equest_all$Sport == "Equestrianism")

#calulate mean per year
#Compare_Row_Equest_all$Year_Age_Mean<- aggregate(Compare_Row_Equest_all[, 4], list(Compare_Row_Equest_all$Year), mean)
#Year_Age_Mean <- aggregate(Compare_Row_Equest_all[, 4:4], list(Compare_Row_Equest_all[, c('Year')]), mean)
#Year_Age_Mean <- names(Compare_Row_Equest_all)[4:4]
#Age.Year.mean <- Compare_Row_Equest_all[,lapply(.SD,mean,na.rm=TRUE),by=Year,.SDcols=Year_Age_Mean]

meanfun <- function(x) {
  c(min = min(x), max =max(x), 
    mean =mean(x), median =median(x), 
    std = sd(x))}

#meanage <- function(x) {
#  c(agr.YR.Mean$mean = mean(x))
#}
Age.Year.mean <- tapply(Compare_Row_Equest_all$Age, Compare_Row_Equest_all$Year, meanfun)
Age_Year_mean<- Mean.Age = c(27.5,26.81,25.34,24.48,27.87,28.36,28.47,29.93,28.08,29.33,29.44,28.32,29.11,27.36,27.28,27.43,27.73,26.47,25.7,27.05,27.72,28.35,29.04,29.76,29.74,30.55,31.28,31.09)
Age_Year_mean<- Year = c(1900,1904,1906, 1908, 1912, 1920, 1924, 1928, 1932, 1936, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)
table(Age.Year.mean)
tapply(Compare_Row_Equest_all$Age, Compare_Row_Equest_all$Year, meanfun)
Mean.Age <- {c(mean = mean(Compare_Row_Equest_all$Age), median=median(Compare_Row_Equest_all$Age))} %% tapply(Compare_Row_Equest_all$Age, Compare_Row_Equest_all$Year)
    table(Mean.Age) 
#group_by(Compare_Row_Equest_all$Year) %%
#group_vars()


library(plyr)
## test plots to see patterns
library(ggplot2)
ggplot(Top_7_Team, aes(x=Age, y=Sport)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line

ggplot(Age.Year.mean, aes(x=Name, y=Mean.Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line

ggplot(Age.Year.mean, aes(x=Age, y=Sport)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line

ggplot(Top_7_Team, aes(x=Age, y=Sport)) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4)     # 1/4 opacity
ggplot(Top_7_Team, aes(x=Age, y=Sport, color >= 35(Age))) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4, aes(color = Team))     # 1/4 opacity
ggplot(Top_7_Team, aes(x=Sport, y=Team, color >= 35(Age))) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4, aes(color = Age))     # 1/4 opacity
ggplot(Top_7_Team, aes(x=Team, y=Sport)) +
  geom_point(aes(color = Age))
ggplot(Top_7_Team, aes(x=Year, y=Sport)) +
  geom_point()
ggplot(Compare_Row_Equest_all, aes(x=Year>1960, y=(mean(Age)))) +
  geom_point()
ggplot(Top_7_Team, aes(x=Year, y=Team)) +
  geom_point()
#create column blending Sport and Year
#Top_6_Teams$Sport_Year <- paste(athlete_events_Age$Sport,athlete_events_Age$Year)
#Table of Sport
table(Top_7_Team$Sport)
#plot to compare sex vs age
ggplot(Top_7_Team, aes(x=Sex, y=Age)) +
  geom_point()
#create new column to simplify those over / under 30
Top_7_Team$Age_over_30 <- ifelse(Top_7_Team$Age >= 30,'Yes','No')
str(Top_7_Team)
#plot over time
ggplot(Top_7_Team, aes(x=Age, y=Year)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 
#plot over time 2
ggplot(Top_7_Team, aes(x=Year, y=Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 

ggplot(Compare_Row_Equest_all, aes(x=Year, y=Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 

ggplot(athlete_events, aes(x=Year, y=Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 

#plot over time 3
ggplot(Top_7_Team, aes(x=Year, y= c(Age >30))) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 
#line plot over Year, Age
ggplot(Top_7_Team, aes(Year, Age)) +
  geom_line()  
ggplot(Top_7_Team, aes(Year, Age, group=Age_over_30)) +
  geom_line(color= "blue")+
  geom_point() 
#bar chart looking at Year by Age
ggplot(Top_7_Team, aes(x = Year, fill = Age)) +
  geom_bar(position = "dodge")
#bar chart to look at Age by team
ggplot(Top_7_Team, aes(x = Age, fill = Team)) +
  geom_bar(position = "dodge")
#bar chart to look at over 30 by team
ggplot(Top_7_Team, aes(x = Age_over_30, fill = Team)) +
  geom_bar(position = "dodge")
#bar chart looking at Age over 30 by sport
ggplot(Top_7_Team, aes(x = Sport, fill = Age_over_30)) +
  geom_bar(position = "dodge")
#bar chart looking at year by age over 30
ggplot(Top_7_Team, aes(x = Year, fill = Age_over_30)) +
  geom_bar(position = "dodge")

ggplot(Compare_Row_Equest_all, aes(x = Year, fill = Age_over_30)) +
  geom_bar(position = "dodge")

#line chart
ggplot(Top_7_Team, aes(Year, Age)) +
  geom_line(aes(linetype=Age_over_30),color = "blue") 


ggplot(Compare_Row_Equest_all, aes(Year, Age)) +
  geom_line(aes(linetype=Age_over_30),color = "blue") 
ggplot(Compare_Row_Equest_all, aes(Year, Age, group=mean(Age))) +
  geom_line(color = "blue") +
  geom_point()



ggplot(Top_7_Team, aes(Year, Team)) +
  geom_line()  
ggplot(Top_7_Team, aes(Year, Age, group=Team)) +
  geom_line(color = "blue")+
  geom_point() 
ggplot(Top_7_Team, aes(Age, fill = factor(Medal))) +
  geom_histogram(binwidth = 1)
#bar plot on metals earned
ggplot(Top_7_Team, aes(Age)) +
  geom_bar(aes(Medal)) 
ggplot(Top_7_Team, aes (x = Year, fill= factor(Age))) +
  geom_histogram(binwidth = 1) +
    facet_grid(Season ~ .)
ggplot(Top_7_Team, aes (x = Year, fill= factor(Age_over_30))) +
  geom_histogram(binwidth = 1) +
  facet_grid(Season ~ .)
#simple scatterplot
ggplot(Top_7_Team, aes(Year, Age)) +
geom_point()
ggplot(Top_7_Team, aes(Age, Team)) +
  geom_point()
ggplot(Top_7_Team, aes(Year, Team, color = Age)) +
  geom_point()
ggplot(Top_7_Team, aes(Medal, Age)) +
  geom_point()
ggplot(Top_7_Team, aes(Year, Age, color = Season)) +
  geom_point(size = 3, alpha = 0.5)
ggplot(Top_7_Team, aes(Year, Age, color = Season), xlim = c(1950, 2016)) +
  geom_point(size = 3, alpha = 0.5)
#plot breakdown by Medal
ggplot(Top_7_Team, aes(Year, Age)) +
  geom_point() +
  facet_grid(Medal ~.)
ggplot(Top_7_Team, aes(Year, mean(Age))) +
  geom_point() +
  facet_grid(Medal ~.)
ggplot(Top_7_Team, aes(Year, Medal)) +
  geom_point() +
  facet_grid(Age_over_30 ~.)
ggplot(Top_7_Team, aes(Year, Medal_value)) +
  geom_point() +
  facet_grid(Age_over_30 ~.)
ggplot(Top_7_Team, aes(Team, Age)) +
  geom_point() +
  facet_grid(Medal ~.)
#scatterplot
posn.jd <- position_jitterdodge(0.5, 0, 0.6)
ggplot(Top_7_Team, aes(x = Year, y = Age, color = Medal)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + facet_grid(.~Team)
#scatterplot 2
ggplot(Top_7_Team, aes(x = Age, y = Year, color = Medal)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + facet_grid(.~Team)
ggplot(Top_7_Team, aes(x = Age, y = Year, color = Medal)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + facet_grid(.~Team)
table(Top_7_Team$Team)
#prob_age <- Top_7_Team$Team / Top_7_Team$Age_over_30
#table(Top_7_Team$Medal_value)
Count_age <- table(Top_7_Team$Age_over_30)
Count_team <- table(Top_7_Team$Team)
#create tables for sport over 30 and medals by sport over 30
table(Top_7_Team$Age_over_30, Top_7_Team$Sport, Top_7_Team$Year >=1980)
Sport_over30 <- table(Top_7_Team$Age_over_30, Top_7_Team$Sport)
Medal_Sport_over30 <- table(Top_7_Team$Age_over_30, Top_7_Team$Sport, Top_7_Team$Medal != "No Medal earned")
Medal_Sport_over30_recent <-table(Top_7_Team$Age_over_30, Top_7_Team$Sport, Top_7_Team$Medal != "No Medal earned", Top_7_Team$Year >= 1980)
table(Top_7_Team$Age_over_30 == "Yes", Top_7_Team$Sport)

qt(c(.025, .975), df=5)   # 5 degrees of freedom
qchisq(.95, df=7)        # 7 degrees of freedom 
pnorm(30, mean=72, sd=15.2, lower.tail=FALSE) 

save(athlete_events, file="athlete_events.RData")
save(Athlete_event_whole, file="athlete_event_whole.RData")
save(Top_7_Team, file="Top_7_Team.RData")
save(Compare_Row_Equest_all, file="Compare_Row_Equest_all.RData")
save(meanfun, file ="meanfun.Rdata")

#linear regression


age.out <- lm(Age >30 ~Sport == "Equestrianism",
               data=Athlete_event_whole)
summary(age.out)
age.out$residuals
SSE = sum(age.out$residuals^2)
SSE

age.out9 <- lm(Age_over_30 ~ Medal_value + NOC + Age,
                 data=Train_Equ)
summary(age.out9)
age.out9$residuals
SSE = sum(age.out9$residuals^2)
SSE


age.out12 <- lm(Age ~ Sport + Medal_value + Sex + Year,
                data=Athlete_event_whole)
summary(age.out12)

age.out12$residuals
SSE = sum(age.out12$residuals^2)
SSE

gage.out7 <- glm(Age_over_30 ~ Medal_value,
               data=Train_Equ)
summary(gage.out7)
coef(summary(gage.out7))

df<- scale(Athlete_event_whole[4])
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)

fit.km <- kmeans(df, 3, nstart=25) 
str(fit.km)
fit.kmCluster = fit.km$cluster
fit.km$centers[3]
str(Athlete_event_whole)

table(fit.kmCluster)
fit.km$centers
distances = dist(Athlete_event_whole[3:14], method = "euclidean")
clusterAge = hclust(distances, method = "ward")
plot(clusterAge)
aggregate(Athlete_event_whole[4], by=list(cluster=fit.km$cluster), mean)

library(cluster)
clusplot(Athlete_event_whole, fit.km$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

# pull out data for top 7 countries

# Assign 0 to under 30 and 1 to over 30
#assign 1 to medal and 0 to no medal

# create the random training(Equestrianism) and test set(Rowing - all countries).
