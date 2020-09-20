
# brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
# beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
# brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
# beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')
# 

brewing_materials <- read.csv("brewing_materials.csv",stringsAsFactors = F)
beer_taxed <-read.csv("beer_taxed.csv")
brewer_size <- read.csv("brewer_size.csv")
beer_states <-read.csv("beer_states.csv")

library(tidyverse)
library(magrittr)
glimpse(brewer_size)
attach(brewer_size)
library(forcats)
library(ggsci)
# brewer_size=transform(brewer_size,
#                            brewer_size$brewer_size=as.factor(brewer_size$brewer_size))
# str(brewer_size)

brewer_size$brewer_size <- as.factor(brewer_size$brewer_size)

#distribution of production capacity
#then we lump the brewer size based on the top 6 weight being total barrels
#other is for the small ones 
brewer_size %>%filter(!brewer_size=="Zero Barrels") %>% filter(!brewer_size=="Total") %>% arrange(desc(brewer_size)) %>% 
  mutate(brewer_size=fct_reorder(brewer_size,parse_number(brewer_size))) %>% na.omit() %>%
  mutate(brewer_size=fct_lump(brewer_size,6,w=total_barrels)) %>% 
  ggplot(aes(year,total_barrels,fill=brewer_size))+geom_col()+ggsci::scale_fill_lancet()+scale_y_continuous(labels = scales::comma)+
  labs(x=NULL,y="beer production by size of brewery",caption = "most of the beer produced is by large breweries")+
  ggtitle("                                      BEER PRODUCTION DISTRIBUTION OVER SIZE OF BREWERIES")+
  theme(plot.background = element_rect(fill = "azure"))
  
  
 #brewer_size %>% mutate(parse_number(brewer_size))
  
  
brewer_size %>%    group_by(brewer_size) %>% tally(.,wt=n_of_brewers,sort = TRUE,name = "brewers_in_category") ->beer_production
beer_production#this shows that large breweries are small in number 
beer_production %>%  ggplot(aes(brewer_size,brewers_in_category))+
  geom_col(fill="red4")+theme(axis.text.x =element_text(angle = 90,hjust = 1))->plot1
#levels(brewer_size$brewer_size)
#plot of beer produced shipped but not taxed 
brewer_size %>% group_by(brewer_size) %>% filter(!brewer_size=="Zero Barrels") %>% filter(!brewer_size=="Total") %>%
  ggplot(aes(x=brewer_size,y=total_shipped))+geom_col(fill="red4")+facet_wrap(~year)+
  scale_y_continuous(labels = scales::comma)+theme_classic()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),plot.background = element_rect("cornflowerblue"))->plot2
brewer_size %>%
  na.omit() %>% # 2019 under 1 barrel has missing total_barrels
  add_count(brewer_size, name = "brewer_total_barrels", wt = total_barrels)%>%
  mutate(size_left = gsub(",|Under", "", brewer_size)%>%
           str_split(" ") %>% 
           map_chr(., 1),
         size_left = ifelse(size_left == "", 0, as.numeric(size_left)),
         brewer_size = fct_lump(brewer_size, 7, bs_total_barrels),
         brewer_size = fct_reorder(brewer_size, size_left)) ->transformed_barrel_size
  
transformed_barrel_size %>% filter(!brewer_size=="Total") %>%  ggplot(aes(x = year, y = total_barrels, fill = brewer_size)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = seq(2009, 2019, 1), minor_breaks = NULL) +
  scale_fill_simpsons()+
  labs(x = "Year", y = "Amount of total barrels produced", fill = "Brewery size")+coord_flip()+
  ggtitle("Market share of different sized breweries over time",subtitle = "reducing market share of large breweries")->plot3

cum_plot <- cowplot::plot_grid(plot1,plot2,plot3)
ggsave("brewer_size.png",cum_plot,dpi = 400,width = 10,height = 10,units = "in")
## beer taxed 
glimpse(beer_taxed)
#Convert all data frame character columns to factors
beer_taxed[sapply(beer_taxed, is.character)] <- lapply(beer_taxed[sapply(beer_taxed, is.character)], 
                                       as.factor)
beer_taxed %>% select_if(is.factor) %>% skimr::skim()

#let us see which month brings more taxes 
beer_taxed %>% group_by(month) %>% mutate(n=n()) %>% tail(n=10) %>% filter(ytd_current>0)

##ggdark package can be used to conveert regular ggplot themes into dark version
library(dplyr)


brewer_size %>% mutate(brewer_size=case_when(brewer_size %in% c("Zero Barrels","Under 1 Barrel",
                                                                "1 to 1,000 Barrels","1,001 to 7,500 Barrels",
                                                                "7,501 to 15,000 Barrels")~"small-breweries",
                                             brewer_size %in% c("15,001 to 30,000 Barrels","30,001 to 60,000 Barrels",
                                                                "60,001 to 100,000 Barrels","100,001 to 500,000 Barrels",
                                                                "500,001 to 1,000,000 Barrels")~"medium-size-breweries",
                                             brewer_size %in% c("1,000,000 to 6,000,000 Barrels","1,000,001 to 1,999,999 Barrels",
                                                                "2,000,000 to 6,000,000 Barrels","1,000,001 to 6,000,000 Barrels")~"Regional-breweries",
                                             brewer_size %in% c("6,000,001 Barrels and Over")~"Large-breweries")) %>% 
  #summarise duplicates for 2019 
  group_by(year,brewer_size) %>% summarise(n_of_brewers=sum(n_of_brewers),
                                           total_barrels=sum(total_barrels,na.rm = T),
                                           taxable_removals=sum(taxable_removals),
                                           total_shipped=sum(total_shipped,na.rm = T)) %>% na.omit() %>% 
  ggplot(aes(as.factor(year),y=total_barrels,group=fct_reorder(brewer_size,total_barrels,.desc=T),
             color=fct_reorder(brewer_size,total_barrels,.desc=T)))+geom_point(size=3)+geom_line(alpha=.5,size=1.45)+
  scale_y_continuous(labels = scales::comma)+labs(x=NULL,y="Total barrels produced ")+dark_theme_gray(base_size=12)+
  theme(plot.background = element_rect(fill = "grey10"),legend.position = c(0.85,0.52),legend.title =element_blank())->plot_major
library(ggdark)
#ggsave("trend_of_breweries.png",plot = plot_major,dpi = 400,width = 10,height = 8,units = "in")



#####exploring taxed
beer_taxed %>% count(type)



























##########################beer materials 
library(gganimate)
brewing_materials %>% count(material_type)
brewing_materials %>% count(type)
brewing_materials %>% filter(!type %in%c("Total Used","Total Grain products","Total Non-Grain products"))
brewing_materials$Date <- zoo::as.yearmon(paste(brewing_materials$year,brewing_materials$month), "%Y %m")
# df$month_pct = 0
# for (i in 1:length(unique(df$Date))){
#   for(j in 1:length(unique(df$type))) {
#     df$month_pct[((df$type==unique(df$type)[j])&(df$Date==unique(df$Date)[i]))] <- df$month_current[((df$type==unique(df$type)[j])&(df$Date==unique(df$Date)[i]))] / df$month_current[((df$type==unique(df$type)[10])&(df$Date==unique(df$Date)[i]))]
#   }
# }
# df$month_pct = df$month_pct*100
brewing_materials %>% filter(material_type=="Total Used") %>%
  ggplot(aes(x=month,y=month_current,color=as.factor(year)))+geom_line()+expand_limits(y=0)#the trend within each year shows that they are always lower in
#november and peak at summer
brewing_materials$Date <- as.Date(brewing_materials$Date)
brewing_materials  %>% tally(.,wt=month_current,name = "materials_per_mon",sort = T) %>% ungroup()














#############################################################by state 
glimpse(beer_states)
beer_states %>% count(type,wt=barrels)#bottle and cans sre the majorly produced 
beer_states %>% group_by(year) %>% summarise(bar=sum(barrels,na.rm=T)) %>% ggplot(aes(year,bar))+geom_col()


##who consumes beer on premise(the states) in 2019
beer_states %>% filter(str_detect(type,"On Premises"),year==max(year),!state=="total") %>% group_by(state) %>% count(state,sort = T,wt=barrels)
#demographics also play a part
#like carlifonia is the most populuous no wonder is at the top and somehow north carolina haas many beer 
beer_states %>% group_by(state) %>% mutate(percent=(barrels/sum(barrels))*100) %>% arrange(desc(percent)) %>%
  filter(type=="On Premises") %>% filter(year==max(year))->on_premises

#### and so maybe we want to map it
library(maps)
library(sf)    
# set.seed(123)
# mydata <- sample_n(beer_production,10134,replace=T)
# shapiro.test(mydata$brewers_in_category)
# ks.test(mydata$brewers_in_category,4000)
# library("ggpubr")
# ggdensity(mydata$brewers_in_category, 
#           main = "Density plot of breweries",
#           xlab = "whatever")
states <- st_as_sf(map("state",plot = F,fill = T))
beer_states
state.abb
on_premises %>% mutate(ID=str_to_lower( state.name[match(state,state.abb)])) %>% #kindof similar with leftmerge but it is missing district columbia 
inner_join(states,by="ID") %>% 
  ggplot(aes(geometry=geom,fill=percent))+geom_sf()+scale_fill_gradient2(low="gold",high="navy",midpoint = .4,labels=scales::percent)+ggthemes::theme_map()+
  ggtitle("                                               STATES WITH MOST BEER CONSUMED ON PREMISE")



#create an animation of beer production in state by state 
beer_states %>% count(type)
#to get the state names 




beer_states %>% filter(state %in% top_10_beer$state) %>% filter(str_detect(type,"On Premises")) %>% group_by(state,year) %>% 
  summarise(barrels=sum(barrels)) %>% ungroup() %>% ggplot(aes(x=year,y=barrels,fill=state))+geom_col()+ggsci::scale_fill_simpsons()







beer_states %>%filter(str_detect(type,"On Premises")) %>% filter(!str_detect(state,"total")) %>%
  group_by(state) %>% summarise(barrels=sum(barrels,na.rm=T)) %>% top_n(10,barrels) %>% ungroup()->top_10_beer

library(ggdark)
beer_states %>% filter(state %in% top_10_beer$state)%>% filter(str_detect(type,"On Premises")) %>% 
  group_by(state,year) %>% 
  summarise(barrels=sum(barrels)) %>% ungroup()%>%
  mutate(ID=str_to_lower( state.name[match(state,state.abb)])) %>% 
  ggplot(aes(x=year,y=barrels,color=ID))+geom_line(size=1.5)+labs(y="Barrels of Beer")+
  ggtitle("TOP 10 SATES FOR ON PREMISES BEER DRINKING OVER THE YEARS")+scale_y_continuous(labels=scales::comma)+
  scale_x_continuous(breaks = seq(from=2008,to=2019,by=2))+
  facet_wrap(~ID)+dark_theme_bw()->beer_by_state_plot

ggsave("top_ten_on_premises.png",beer_by_state_plot,dpi = 400,width = 10,height = 7,units = "in")











####then we turn this into an animated plot 
library(gganimate)
library(gifski)
beer_states %>% filter(state %in% top_10_beer$state)%>% filter(str_detect(type,"On Premises")) %>% 
  group_by(state,year) %>% 
  summarise(barrels=sum(barrels)) %>% ungroup()%>%
  mutate(ID=str_to_lower( state.name[match(state,state.abb)])) %>% group_by(year) %>% mutate(rank = rank(-barrels),
                                            Value_rel = barrels/barrels[rank==1],
                                            Value_lbl = round(barrels, 0))->formated_beer
static_plot <- ggplot(formated_beer, aes(rank, group = ID, 
                           fill = as.factor(ID), color = as.factor(ID))) +
  geom_tile(aes(y = barrels/2,
                height = barrels,
                width = 0.9), alpha = 0.8, color = NA)+
  geom_text(aes(y = 0, label = paste(state, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=barrels,label = Value_lbl, hjust=0))+
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ), panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),plot.margin = margin(2,2, 2, 4, "cm"))
#we then animate the static plot
anim = static_plot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Year : {closest_state}',  
       subtitle  =  "Top 10 States for on premises beer drinking in breweries by barrels",
       caption  = "Data from TidyTuesday 2020-03-31")
animate(anim, 200, fps = 15,  width = 1200, height = 1000, 
        renderer = gifski_renderer("beer_bar_race.gif"))
