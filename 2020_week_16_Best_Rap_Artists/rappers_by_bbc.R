
polls <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

library(tidyverse)

# 
# write.csv(polls,"polls_bbc.csv")
# write.csv(rankings,"rankings_bbc.csv")

polls %>% tbl_df()
#   polls %>% count(artist,sort = T)
polls %>% count(gender)
rankings %>% tbl_df()
rankings %>% arrange(desc(points))

colSums(is.na(polls))
#percentage of missing values in the data frame 
#can get rid of that colum 
polls %>% select(-critic_country2) %>% View()
sum(is.na(polls$critic_country2))/sum(nrow(polls$critic_country2))

rankings %>% select(artist,title,year,points) %>% group_by(year) %>% #then we return 1 row for each group based on points
  top_n(1,points) %>% arrange(desc(year)) %>% mutate(song=paste(artist,"-",title))->top_songs_by_year



top_songs_by_year %>% ggplot(mapping=aes(x=year,y=points,label=song,fill=points))+geom_col()+
  scale_fill_gradient2(high = "cyan2",low = "midnightblue",mid="plum2",midpoint = 60)+
  ggdark::dark_theme_classic()+
  ggtitle("                        HIGHEST RANKED SONGS BY YEAR")->plot1


plotly::ggplotly(plot1,tooltip=c("x","y","label"))



####Top ranked songs with focus on females
text1 <- "MC Lyte - first Hiphop Female artist\n to be nominated for a Grammy\nunder Best Rap Single(Song - 'Rucffneck')"
text2 <- "Queen Latifah - released hiphop\n hit 'UNITY' addressing\ndomestic violence, street harassment ..."
text3 <- "Missy Elliot - Wins her\nFirst Grammy Awards\n[song: Get Ur Freak On]"
text4 <- "Cardi B - Wins several categories\n in BET Awards - Her Single\n 'Bodak Yellow' doesn't win\n Hiphop video [Dj Khaled wins]"
text5 <- "Lauryn Hill's Album\n[The Miseducation of Lauryn Hill#5]\n top-TEN highest first-week\n home market sales"
#rankings %>% filter(stringr::str_detect(gender,"female"),points>7)


rankings %>% group_by(year,gender) %>% summarise(mean_points=mean(points)) %>% arrange(desc(mean_points)) %>% 
  ggplot(aes(factor(year),mean_points,fill=gender))+geom_col()+ggsci::scale_fill_startrek()+
    geom_text(aes(14,55), label = text1, family = 'Gabriola', hjust = -0.05, vjust = 1, size = 5, color = "black")+
  geom_segment(aes(x=14,xend=14,y=55,yend=30), arrow = arrow(length = unit(0.1,'in')), size = 1.5, col = '#bada55')+
  geom_text(aes(19,34),label=text5,family="Papyrus",color="navy")+
  geom_segment(aes(x=19,xend=19,y=25,yend=30),arrow = arrow(length = unit(0.1,"in")),size=1.6,color="navy")+
  geom_text(aes(26.3,30),label=text3,color="orange")+
  geom_segment(aes(x=25,xend=23,y=31,yend=31),arrow = arrow(length = unit(0.1,"in")),size=1.6,color="orange")+
  geom_text(aes(38,26),label=text4,color="grey0")+
  geom_segment(aes(x=38,xend=36,y=26,yend=16),arrow = arrow(length = unit(0.1,"in")),size=1.6,color="grey0")+
  ggthemes::theme_economist()
