## ------------------------------------------------------------------------
library(tidyverse)# data wrangling and plotting 
library(ggdark)#just to customize ggplots 
library(sf)#stands for simple features and primarily for plotting 
options(scipen = 999)# so that numbers do not dispaly in scientific notation
library(ggsci)#again just for more visually appealing fills as colors on plots 


## ------------------------------------------------------------------------
volcano <- readr::read_csv("volcano.csv")
eruptions <- readr::read_csv("eruptions.csv")


## ------------------------------------------------------------------------
# volcano <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv")
# eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
# events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
# tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
# sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')



## ------------------------------------------------------------------------
# volcano %>% write_csv("volcano.csv")
# eruptions %>% write_csv("eruptions.csv")
# events %>% write_csv("event.csv")
# sulfur %>% write_csv("sulfur.csv")
# tree_rings %>% write_csv("tree_rings.csv")


## ------------------------------------------------------------------------
volcano %>% arrange(desc(population_within_100_km)) %>% select(volcano_number:subregion) %>% head(10)


## ------------------------------------------------------------------------
volcano %>% distinct(primary_volcano_type) %>% knitr::kable()


## ------------------------------------------------------------------------
volcano %>% group_by(primary_volcano_type) %>% top_n(n=1,wt=elevation) %>% select(volcano_number:elevation,population_within_100_km,-c(latitude,longitude)) %>% arrange(desc(elevation))


## ------------------------------------------------------------------------
volcano %>% group_by(primary_volcano_type) %>% top_n(n=1,wt=elevation) %>% select(volcano_number:elevation,population_within_100_km,-c(latitude,longitude)) %>% arrange(desc(elevation)) %>% ungroup() %>% 
  mutate(primary_volcano_type=fct_reorder(primary_volcano_type,elevation)) %>% 
  ggplot(aes(x=primary_volcano_type,y=elevation,fill=population_within_100_km))+geom_col()+scale_fill_gradient(low = "blue",high = "red")+ggdark::dark_theme_gray()+theme(axis.text.x = element_text(angle = 90))


## ------------------------------------------------------------------------
volcano %>% count(evidence_category)
volcano %>% mutate(last_eruption_year=as.numeric(last_eruption_year)) %>% 
  ggplot(aes(last_eruption_year,fill=evidence_category))+geom_histogram()+ggsci::scale_fill_simpsons()+
  ggtitle("most erruptions have occured recently and\n the ones that happened in the distant past are the dated ones")+
  ggdark::dark_mode()


## ------------------------------------------------------------------------
volcano %>% mutate(years_ago=2020-as.numeric(last_eruption_year)) %>% 
  ggplot(aes(years_ago+1,fill=evidence_category))+geom_histogram()+ggsci::scale_fill_jama()+
  ggtitle("most erruptions have occured recently and\n the ones that happened in the distant past are the dated ones\n and some even happeed to ahve been observed around 3000 years ago ")+
  ggdark::dark_mode()+scale_x_log10()


## ------------------------------------------------------------------------
library(ggthemes)
volcano %>% ggplot(aes(longitude,latitude))+
  geom_point(color="black")+
  borders()+
  theme_map()+
  ggtitle("one can be able to see the geologically active areas\n around the world ,basically the visual of the tectonic plates")
  


## ------------------------------------------------------------------------
library(ggthemes)
library(RColorBrewer)
volcano %>% mutate(primary_volcano_type=str_remove(primary_volcano_type,"\\(.*\\)") ,
                   primary_volcano_type=fct_lump(primary_volcano_type,10)) %>% #we lump these into 8 categories 
  ggplot(aes(longitude,latitude,color=primary_volcano_type))+
  geom_point()+
  #scale_color_brewer(type = "qual",palette = "Dark2")+
  borders()+
  theme_map()+
  ggtitle("one can be able to see the geologically active areas\n around the world ,basically the visual of the tectonic plates")
  


## ------------------------------------------------------------------------
library(glue)
library(leaflet)

template="<p>{ volcano_name }</p>{ primary_volcano_type }<p></p>"
volcano %>% select(volcano_name,country,latitude,longitude,primary_volcano_type) %>% mutate(html=glue(template))
volcano_html <- volcano %>% mutate(html=glue(template))

leaflet(volcano_html) %>% 
  addTiles() %>% 
  addCircleMarkers(lat = ~latitude,lng = ~longitude,popup = ~html)


## ------------------------------------------------------------------------
library(DT)
volcano %>% filter(region=="Africa and Red Sea") %>% 
  gather(key ,value ,volcano_name,primary_volcano_type,last_eruption_year) %>% mutate(key=str_replace_all(key,"_"," ")) %>% 
  mutate(key=str_to_title(key)) %>% 
  nest(data=c(key,value)) %>% mutate(html=map(data,datatable)) %>% pull(html) %>% pluck(10)


## ------------------------------------------------------------------------
volcano %>% filter(region=="Africa and Red Sea") %>% #just to make it render faster 
   mutate(population=population_within_100_km) %>%
  mutate(pop_trans=log2(population_within_100_km),
         pop_color=colorNumeric(c("green","red"),domain = pop_trans)(pop_trans)) %>%
  gather(key ,value ,volcano_name,primary_volcano_type,last_eruption_year,
         country,population_within_10_km,
         population_within_100_km) %>% 
  mutate(key=str_replace_all(key,"_"," ")) %>% # we are just removing the undescores 
  mutate(key=str_to_title(key)) %>% # just to make it in title type 
  mutate(key=paste0("<b>",key,"</b>")) %>%#just to bolden the 
  replace_na(list(value="Unkown")) %>% 
  nest(data=c(key,value)) %>% mutate(html=map(data,knitr::kable,#perfom the knitr::kable function to every row in the data column 
                                              format="html",
                                              escape=F,
                                              col.names=c(" "," ")))  %>% # this is basically just to remove the column names 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lat = ~latitude,lng = ~longitude,popup = ~html,
                   color = ~pop_color,
                   radius =3) %>% 
  addMeasure()#helps in calculations between two different points in a map 

#colorNumeric(c("red","blue"),domain = volcano$population_within_5_km)(10000) so basically it outputs the different hexadecimal for the provided number 


## ------------------------------------------------------------------------
volcano %>% ggplot(aes(population_within_100_km))+geom_histogram()+scale_x_log10()


## ------------------------------------------------------------------------
eruptions %>% head()


## ----eval=FALSE, include=FALSE-------------------------------------------
## library(cowplot)
## vei <- ggdraw()+draw_image("volcano_explosivity_index.svg")
## plot(vei)
## 


## ------------------------------------------------------------------------
eruptions %>% 
  mutate(volume_of_explosion=.0001*vei^10) %>% 
  ggplot(aes(longitude,latitude))+
  geom_point(aes(size=volume_of_explosion,color=volume_of_explosion))+
  scale_size_continuous(range = c(.1,7),guide = F)+ # small one will be of size .1 while the large ones will be of scale 7
  scale_color_gradient(low = "green",high = "red")+
  borders()+
  ggtitle("VOLCANOES OF THE WORLD IN THEIR SIZES")+theme_map()+
  ggdark::dark_theme_gray()


## ----eval=FALSE, include=FALSE-------------------------------------------
## library(gganimate)
## animation <- eruptions %>% filter(start_year>=1900) %>%
##   mutate(volume_of_explosion=.0001*vei^10) %>%
##   ggplot(aes(longitude,latitude))+
##   geom_point(aes(size=volume_of_explosion,color=volume_of_explosion))+
##   scale_size_continuous(range = c(1,7),guide = F)+ # small one will be of size .1 while the large ones will be of scale 7
##   scale_color_gradient(low = "green",high = "red")+
##   borders()+
##   theme_map()+
##   ggdark::dark_theme_gray()+
##   transition_reveal(start_year)+
##   shadow_trail()+
##   transition_time(start_year)+
##   labs(title = "ERUPTINS:Year{round(frame_time)}")#here there is use of glue
## animate(animation,20,fps=2)# 10 second animation
## anim_save("erruptions.gif")#will save the last animation

