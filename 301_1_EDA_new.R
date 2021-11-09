# Loading Tidyverse  
library(tidyverse)

# Loading Batting Data
batting = read_csv("data/Batting.csv")
batting_post = read_csv("data/BattingPost.csv")

# Applying filters to show data after 1985
batting <- filter(batting, yearID >= 1985)
batting_post <- filter(batting_post, yearID >= 1985)


salaries = read_csv("data/Salaries.csv")

# Creating new ID key for each player and the year/team they played in
merged_batting <- bind_rows(batting, batting_post) %>% 
  unite('playerseasonID', c("yearID", "playerID", "teamID"), remove = FALSE) %>%
  mutate(
    postseason = if_else(is.na(round), 0, 1) #postseason indicator
  ) 

# Postseason appearance indicator
merged_batting_postseason_app <- merged_batting %>% group_by(playerseasonID) %>%
  summarise(postseason_app = max(postseason)) %>%
  inner_join(merged_batting, by = "playerseasonID") %>%
  mutate(postseason = if_else(postseason == 1, 'Y', 'N')) %>%
  mutate(postseason_app = if_else(postseason_app == 1, 'Y', 'N'))

# Merging with Salary Data and adding new metrics
merged <- salaries %>%
  unite('playerseasonID', c("yearID", "playerID", "teamID"), remove = FALSE) %>%
  select('playerseasonID', 'salary') %>%
  right_join(merged_batting_postseason_app, by = "playerseasonID") %>% 
  mutate(BA = H/AB) %>% # batting average 
  mutate(Kperc = SO/AB) %>% # strikeout percentage
  # reordering columns 
  relocate(salary, .after = last_col()) %>%
  relocate(c(playerseasonID, playerID, yearID, teamID, stint, lgID)) %>%
  relocate(c(postseason, postseason_app, round), .after = lgID) %>%
  relocate(c(BA, Kperc), .after = AB)

merged

  


write.csv(merged, 'data/merged_lahman.csv', row.names=FALSE)



### I. Initial Exploration

df = read_csv('data/merged_lahman.csv')
#### A. Batting Average distribution
df %>%
  ggplot() +
  geom_histogram(mapping = aes(x = BA), binwidth = 0.001)

df %>%
  filter(AB > 100) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = BA), binwidth = 0.001)

df %>%
  filter(AB > 200) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = BA), binwidth = 0.001)

df %>%
  filter(AB > 400) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = BA), binwidth = 0.001)

df %>%
  filter(AB > 100) %>%
  summarise(meanBA = mean(BA, na.rm = T))
df %>%
  filter(AB > 400) %>%
  summarise(meanBA = mean(BA, na.rm = T))

df %>%
  filter(AB > 400) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = BA), binwidth = 0.001) +
  coord_cartesian(xlim = c(0.29, 0.31))

df %>% 
  filter (AB > 400 & BA == 0.300) %>%
  select (BA, AB) %>%
  head()

df %>%
  filter(AB > 400) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = BA), binwidth = 0.001) +
  coord_cartesian(xlim = c(0.24, 0.26))

df %>%
  filter(AB > 400) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = BA), binwidth = 0.001) +
  coord_cartesian(xlim = c(0.19, 0.21))


#### B. Homerun distribution

df %>%
  ggplot() +
  geom_histogram(mapping = aes(x = HR), binwidth = 1)

df %>%
  filter(AB > 400) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = HR), binwidth = 1)


df %>% 
  filter (AB > 400 & BA > 0.3) %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = HR), binwidth = 1)

df %>% 
  filter (AB > 400 & RBI > 100) %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = HR), binwidth = 1)

df %>% 
  filter (AB > 400 & BA > 0.3 & RBI > 100) %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = HR), binwidth = 1)

df %>%
  filter (AB > 400) %>% 
  ggplot() +
  geom_point(aes(x = BA, y = HR), alpha = 1/4)

df %>%
  filter (AB > 400) %>% 
  ggplot() +
  geom_point(aes(x = RBI, y = HR), alpha = 1/4)

df %>%
  filter (AB > 400) %>% 
  ggplot() +
  geom_point(aes(x = BB, y = HR), alpha = 1/4)

df %>%
  filter (AB > 400) %>%
  select(HR, BA) %>%
  cor()
df %>%
  filter (AB > 400) %>%
  select(HR, RBI) %>%
  cor()
df %>%
  filter (AB > 400) %>%
  select(HR, BB) %>%
  cor()

#### C. Salary

df %>%
  ggplot() +
  geom_histogram(mapping = aes(x = salary), binwidth = 100000)

df %>%
  filter (AB > 400) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = salary), binwidth = 100000)

df %>%
  ggplot() + 
  geom_point(mapping = aes(x = yearID, y = salary), alpha = 1/4)


df$yearIDfactor = as.factor(df$yearID)
df %>%
  ggplot(mapping = aes(x = yearIDfactor, y = salary)) +
  geom_boxplot()

df %>%
  group_by(yearID) %>%
  summarise(mean_salaries = mean(salary, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(mapping = aes(x = yearID, y = mean_salaries))


### II. Identifying Predictors

#### A. Predictors for Postseason appearance 

teamsummary <- df %>%
  group_by(yearID, teamID) %>%
  summarise(total_AB = sum(AB, na.rm=TRUE), total_H = sum(H, na.rm=T), 
            total_HR = sum(HR, na.rm = T),  total_R = sum(R, na.rm = T), 
            total_SO = sum(SO, na.rm = T), total_salary = sum(salary, na.rm = T), 
            total_GIDP = sum(GIDP, na.rm = T)) %>%
  mutate(total_BA = total_H/total_AB, 
         total_Kperc = total_SO/total_AB, 
         total_HRperc = total_HR/total_AB,
         total_RRatio = total_R/total_AB,
         total_GIDPperc = total_GIDP/total_AB) %>%
  arrange(yearID, teamID)
head(teamsummary)

teamsPSapp <- df %>% 
  select(teamID, yearID, postseason_app) %>%
  unique()

teamsummary <- left_join(teamsummary, teamsPSapp, by = c("teamID", "yearID")) %>%
  select(yearID, teamID, postseason_app, total_BA, total_Kperc, 
         total_HRperc, total_RRatio, total_GIDPperc, total_salary, everything())%>%
  arrange(yearID, teamID) 
head(teamsummary)

teamsummary$yearIDfactor = as.factor(teamsummary$yearID)
##### Total BA 
teamsummary %>%
  ggplot(mapping = aes(x = total_BA)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 0.003) +
  labs(x = "Season Batting Average", y = "Count", colour = "Postseason Appearance")
##### Total runs
teamsummary %>%
  ggplot(mapping = aes(x = total_R)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 10) +
  labs(x = "Total Runs", y = "Count", colour = "Postseason Appearance")
##### Total runs per BA
teamsummary %>%
  ggplot(mapping = aes(x = total_RRatio)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 0.003) +
  labs(x = "Total Runs per AB", y = "Count", colour = "Postseason Appearance")
##### Total HR
teamsummary %>%
  ggplot(mapping = aes(x = total_HR)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 5) +
  labs(x = "Total Home Runs", y = "Count", colour = "Postseason Appearance")
##### Total HR per BA
teamsummary %>%
  ggplot(mapping = aes(x = total_HRperc)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 0.001) +
  labs(x = "Total Runs per AB", y = "Count", colour = "Postseason Appearance")

##### Total SO
teamsummary %>%
  ggplot(mapping = aes(x = total_SO)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 20) +
  labs(x = "Total SO", y = "Count", colour = "Postseason Appearance")
##### Total SO per AB
teamsummary %>%
  ggplot(mapping = aes(x = total_Kperc)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 0.01) +
  labs(x = "Total SO Rate", y = "Count", colour = "Postseason Appearance")
##### Total GIDP
teamsummary %>%
  ggplot(mapping = aes(x = total_GIDP)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 5) +
  labs(x = "Total GIDP", y = "Count", colour = "Postseason Appearance")
##### Total GIDP per AB
teamsummary %>%
  ggplot(mapping = aes(x = total_GIDPperc)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 0.0007) +
  labs(x = "Total GIDP rate", y = "Count", colour = "Postseason Appearance")

##### Scatterplots
teamsummary %>%
  ggplot(mapping = aes(x = total_R, y = total_Kperc)) +
  geom_point(aes(colour = postseason_app)) +
  labs(x = "Total Runs", y = "Total SO percentage", colour = "Postseason Appearance")

teamsummary %>%
  ggplot(mapping = aes(x = total_R, y = total_GIDPperc)) +
  geom_point(aes(colour = postseason_app))  +
  labs(x = "Total Runs", y = "Total GIDP Rate", colour = "Postseason Appearance")

teamsummary %>%
  ggplot(mapping = aes(x = total_HR, y = total_Kperc)) +
  geom_point(aes(colour = postseason_app)) +
  labs(x = "Total Home Runs", y = "Total SO percentage", colour = "Postseason Appearance")

teamsummary %>%
  ggplot(mapping = aes(x = total_HR, y = total_GIDPperc)) +
  geom_point(aes(colour = postseason_app)) +
  labs(x = "Total Home Runs", y = "Total GIDP Rate", colour = "Postseason Appearance")


##### Total Salary
teamsummary %>%
  ggplot(mapping = aes(x = total_salary)) + 
  geom_freqpoly(mapping = aes(colour = postseason_app), binwidth = 5000000)+
  labs(x = "Team Payroll", y = "Count", colour = "Postseason Appearance")

teamsummary %>%
  ggplot(mapping = aes(x = total_R, y = total_salary)) +
  geom_point(aes(colour = postseason_app)) +
  labs(x = "Team Total Runs", y = "Team Payroll", colour = "Postseason Appearance")

teamsummary %>%
  ggplot(mapping = aes(x = total_HR, y = total_salary)) +
  geom_point(aes(colour = postseason_app)) +
  labs(x = "Team Total Home Runs", y = "Team Payroll", colour = "Postseason Appearance")

