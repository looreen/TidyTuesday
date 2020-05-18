library(tidyverse)
commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")
View(commute_mode)

#Check if all states have the right abbreviation
states_check <- commute_mode %>% 
  group_by(state, state_abb) %>% 
  summarise(n())

#DC as abbreviation for District of Columbia
#State Ca becomes California
#Massachusett becomes Massachusetts

commute_mode <- commute_mode %>% 
  mutate(state=case_when(state=='Ca'~'California',
                         state=='Massachusett'~'Massachusetts',
                         TRUE~state),
         state_abb=case_when(state=='District of Columbia'~'DC',
                             state=='California'~'CA',
                             state=='Massachusetts'~'MA',
                             TRUE~state_abb))

commute_mode %>% 
  mutate(city_size=factor(city_size, levels = c('Small', 'Medium', 'Large'), ordered = T)) %>% 
  group_by(city_size, mode) %>% #state,
  summarise(n_sum=sum(n),
            perc_mean=mean(percent)) %>% 
  ggplot(., aes(x=city_size, y=n_sum, fill=mode))+
  geom_bar(stat = 'identity', position = 'dodge')
