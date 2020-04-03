library(tidyverse)
library('zoo')

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

sf_trees <- sf_trees %>% 
  mutate(year_only=format(sf_trees$date, "%Y"))

min(sf_trees$dbh, na.rm = T)
max(sf_trees$dbh, na.rm = T)

#Check for relationship between age (time of planting) and the DBH
sf_trees %>% 
  filter(dbh<=200&
         !(is.na(date))) %>% 
  ggplot(., aes(date,dbh))+
  geom_point()

#Years complete from 1955 to 2020
year_only_complete <- data.frame(year_only_complete=full_seq(as.numeric(year_trees$year_only),1))

sf_trees$year_only <- as.numeric(sf_trees$year_only)

data_count_trees <- sf_trees %>% 
  filter(dbh<=200&
           !(is.na(date))) %>% 
  group_by(year_only) %>% 
  summarise(count=n()) %>% 
  right_join(year_only_complete, by=c('year_only'='year_only_complete')) 

#Create baseplot
barchart_count_trees <- data_count_trees %>% 
  ggplot(., aes(year_only,count))+
  geom_bar(stat = 'identity')+
  scale_x_continuous(breaks=seq(from=1955, to=2020, by=5), 
                   labels=as.character(seq(from=1955, to=2020, by=5)))+
  coord_flip()

#Get US economics data 
data(economics)

#Add economic data as of the end of the year
data_count_trees_econ <- economics %>% 
  filter(grepl(pattern = '-12-', x=date)) %>% 
  mutate(year_only=format(date, "%Y") %>% as.numeric()) %>% 
  right_join(data_count_trees)

#Create a graph to see a movement between economics and trees planted
data_count_trees_econ %>% 
  ggplot(., aes(year_only,count))+
  geom_bar(stat = 'identity')+
  scale_x_continuous(breaks=seq(from=1955, to=2020, by=5), 
                     labels=as.character(seq(from=1955, to=2020, by=5)))+
  coord_flip()+
  geom_line(aes(year_only, unemploy/10, col='red'), inherit.aes = F)

#Mean and median of trees planted
data_count_trees %>% 
  summarise(mean_trees=mean(count, na.rm = T),
         median_trees=median(count, na.rm = T))

#Make BarChart more beautiful
barchart_count_trees+
  #Add breaks to the y line
  scale_y_continuous(breaks = seq(from=0, to=3000, by=250))+
  #Add the annotation for the 1960s
  annotate(geom='curve', 
           curvature=-0.3, 
           xend = 1962, x=1961, 
           y=500, yend = 0, 
           arrow = arrow(length = unit(3, "mm")))+
  annotate(geom='text', label='In the 1960s, no trees were planted.\nOr no data are available.',
           x=1964, y=500)+
  #Add the annotation for 2008
  annotate(geom = 'curve', 
           curvature=-0.3, 
           x=1995, xend = 2007,
           y=2500, yend = 2600,
           arrow = arrow(length = unit(3, "mm")))+
  annotate(geom='text', 
           label='By far most trees planted in 2008,\nthe year of the financial crash.', 
           x=1992, y=2500)+
  #Add the mean number of trees as a line
  geom_hline(yintercept = mean((data_count_trees %>% filter(year_only>=1970))$count, na.rm = T), 
             alpha=0.6,
             col='red',
             linetype='dotted')+
  annotate(geom='curve', 
           curvature=0.3, 
           x=1978, xend=1985,
           yend = 800, y=1250,
           arrow = arrow(length = unit(3, "mm")), 
           col='red')+
  annotate(geom='text', 
           label='Mean number of trees planted since 1970',
           x=1977, y=1250, 
           col='red')+
  #Clear the plot background
  theme_minimal()+
  #Add correct labels to the axis and the main title 
  labs(title='How many trees have been planted in San Francisco?', 
       x=NULL, 
       y=NULL, 
       subtitle = "(based on data from San Francisco's open data portal)")

#Tree species and DBH
sf_trees %>% 
  filter(dbh<201) %>% 
  group_by(species) %>% 
  summarise(count=n(), mean_dbh=mean(dbh)) %>% 
  arrange(desc(count)) %>% 
  ggplot(., aes(x=count, y=mean_dbh))+
  geom_point()

#Have a closer look at 2008
sf_trees %>% 
  filter(year_only==2008) %>% 
  group_by(species) %>% 
  summarise(count_species=n()) %>% 
  arrange(desc(count_species)) %>% 
  head(5) %>% 
  ggplot(., aes(species,count_species))+
  geom_bar(stat = 'identity')
