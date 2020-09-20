# tbi_age <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
# tbi_year <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
# tbi_military <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')
# 
# write.csv(tbi_age,"tbi_age.csv")
# write.csv(tbi_year,"tbi_year.csv")
# write.csv(tbi_military,"tbi_military.csv")
tbi_age <- read.csv(file.choose())
tbi_year <- read.csv(file.choose())
tbi_millitary <- read.csv(file.choose())
# library(dplyr)
# library(ggplot2)
# library(tidyr)
library(magrittr)
library(ggthemes)
library(GGally)
library(skimr)
library(forcats)
library(stringr)
library(fishualize)#Implementation of color palettes based on fish species.
library(tidyverse)
library(ggrepel)
library(ggsci)#for changing color pallettes inspired by scienttific journals
# library(scales)
library(bbplot)#for styling plots bbc style using theeir customized fonts
#number_est is the estimated observed cases each year
#rate_est is the rate/100,000 in 2014
###data description
distribution_of_age<- tbi_age %>%mutate(age_group=factor(age_group,levels = age_group_lvls)) %>% na.omit(age_group) %>% 
  group_by(age_group) %>%
  summarise(y = sum(number_est)) %>%
  arrange(desc(y))
#distribution_of_age
DataExplorer::plot_str(tbi_age)
DataExplorer::plot_missing(tbi_age)# this package simplifies edaa

ggplot(distribution_of_age,aes(x=age_group,y=y,fill="navy"))+
  geom_col()+scale_y_continuous(labels = scales::comma)+scale_fill_economist()

str(tbi_age)
tbi_age %>% skim()
#the missing values are in total 22 #not so bad ommitting them
colSums(is.na(tbi_age))
tbi_age %<>%drop_na() 
unique(tbi_age$age_group)
#there is an overlap in age as those classified as under 17 are stil under 15 or 4 so we create new leves
age_group_lvls <- c('0-4', '5-14', '15-24', '25-34', '35-44', 
                    '45-54', '55-64', '65-74', '75+')
new_tbi_age <- tbi_age %>% filter(!age_group %in% c("0-17","Total"))%>%
  #if age group is not in the set of these to remain with it
                                group_by(age_group,injury_mechanism)%>% 
                              ungroup() %>% 
  mutate(age_group= factor(age_group,levels = age_group_lvls),age_group=fct_rev(age_group))
#fct_rev reverses the order of factors
#let us plot the unintentional causes 
new_tbi_age %>% filter(str_detect(injury_mechanism,"[Un]intentional",negate = FALSE)) %>% 
  ggplot(aes(x=age_group,y=number_est))+geom_bar(stat = "identity",aes(color=age_group,fill=age_group))+
  facet_wrap(~injury_mechanism)+scale_y_continuous(labels = scales::comma)+
  theme(plot.background = element_rect("azure"),
        axis.text.x = element_text(angle = 90,size =10,colour = "RED"))+
  ggtitle("TRAUMATIC BRAIN INJURY BY AGE",subtitle = "unintentional causes")+
  labs(x="Age group",y="ESTIMATED NUMBER OF DEATHS")
#then let us plot the intentional causes 
new_tbi_age %>% filter(!str_detect(injury_mechanism,"[Un]intentional")) %>% 
  ggplot(aes(x=age_group,y=number_est))+geom_bar(stat = "identity",aes(color=age_group,fill=age_group))+
  facet_wrap(~paste("cause of injury ::",injury_mechanism))+theme_few()+theme(plot.background = element_rect("grey"))+
  ggtitle("TRAUMATIC BRAIN INJURY BY AGE GROUP",subtitle = "intentional causes or others")->PLOT2
PLOT2+scale_fill_fish_d()+labs(x="AGE GROUP",y="ESTIMATED NUMBER OF DEATHS")
# ggsave("traumatic_brain_injury_by_age_intentional_causes_and_others.png",plot = PLOT2,dpi = 800,width = 10,height = 9,units = "in")
# ggsave("tbi_unintentional_causes.png",plot = plot1,width = 11,height = 9,units = "in",dpi = 600)


#unique(tbi_age)
new_tbi_age %>% group_by(injury_mechanism) %>% filter(injury_mechanism!="Other or no mechanism specified") %>% 
      filter(injury_mechanism!="Other unintentional injury, mechanism unspecified")->only_specified_injuries

# the above can be achieved alternatively by 
new_tbi_age %>% group_by(injury_mechanism) %>% 
filter(injury_mechanism!="Other or no mechanism specified"&
         injury_mechanism!="Other unintentional injury, mechanism unspecified") ->specified

tbi_deaths <- new_tbi_age %>% filter(type=="Deaths")
tbi_emergency <- new_tbi_age %>% filter(type=="Emergency Department Visit")

tbi_hos <- new_tbi_age %>% filter(type=="Hospitalizations")

# levels(new_tbi_age$type)
ggplot(only_specified_injuries,aes(only_specified_injuries$injury_mechanism,number_est,fill=type))+
  geom_bar(stat="identity",position = "stack")+scale_y_continuous(labels = scales::comma)+
  facet_wrap(~paste("age_gorup",age_group))+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 45,hjust=1,colour = "navy",size = 10),legend.title = element_blank(),
        legend.background = element_rect("cyan"),
        text = element_text(size = 8))+
  labs(x="only specified injuries",y="number of estimated injuries")+scale_fill_simpsons()
#dev.new()
ggplot(tbi_deaths, aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity")+
  labs(title = "            INJURY RESULTED IN DEATH", x = "Ages", y = "Estimated # of Injuries",
       fill = "Injury Mechanism") +
  theme(legend.position = "bottom",
    plot.title = element_text(size = 19, family = "bold", hjust = 1),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme_economist()
#dev.off()
#those who went to the er 

ggplot(tbi_emergency, aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity") +
  labs(title = "                     INJURY RESULTED IN ER VISIT", x = "Ages", y = "ESTIMATED # OF INJURIES",
       fill = "Injury Mechanism")+
  theme_economist()+theme(plot.background = element_rect("grey"))+facet_wrap(~injury_mechanism)
ggplot(tbi_emergency, aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity") +
  labs(title = "                     INJURY RESULTED IN ER VISIT", x = "Ages", y = "ESTIMATED # OF INJURIES", 
       fill = "Injury Mechanism")+
  theme_economist()+theme(plot.background = element_rect("gold4"))

























#### Basic Plot of All Data

BrainInjury.Plot <- ggplot(tbi_age, aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity",position = "stack") +
  facet_wrap(~ type) +
  theme_classic() +
  scale_fill_simpsons()+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

BrainInjury.Plot


# #### Failplot.1: Circular Bar Chart ====
# data <- tbi_age
# 
# # Set a number of 'empty bar'
# empty_bar <- 10
# 
# # Add lines to the initial dataset
# to_add <- matrix(NA, empty_bar, ncol(data))
# colnames(to_add) <- colnames(data)
# data <- rbind(data, to_add)
# data$id <- seq(1, nrow(data))
# 
# # Get the name and the y position of each label
# label_data <- data
# number_of_bar <- nrow(label_data)
# angle <- 90 - 360 * (label_data$id-0.5) /number_of_ba
# I substract 0.5 because the letter must have the angle of the center of the bars.
#Not extreme right(1) or extreme left (0)
# label_data$hjust <- ifelse( angle < -90, 1, 0)
# label_data$angle <- ifelse(angle < -90, angle+180, angle)
# 
# # Plot
# p <- ggplot(data, aes(x = as.factor(id), y = number_est, fill = age_group)) +
#   # This add the bars with a blue color
#   geom_bar(stat="identity", alpha = 0.5) +
#   ylim(-100,120) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = unit(rep(-1,4), "cm") 
#   ) +
#   coord_polar() + 
#   geom_text(data=label_data, aes(x=id, y= number_est + 10, label= age_group , hjust=hjust), 
# color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE)
# data %>% add_count(column)
# p
# 
# 
# #### Failplot.2: Nyssa's circular Bar chart ====
# 
# ## select a few countries to label
# labels_data <- tbi_age %>%mutate(age_group=factor(age_group,levels = age_group_lvls)) %>% na.omit(age_group) %>% 
#   group_by(age_group) %>%
#   summarise(y = sum(number_est)) %>%
#   arrange(desc(y))
# 
# labels_data
# 
# ## Plot this mofo
# p2 <- ggplot(tbi_age2) +
#   # Add the stacked bar
#   geom_bar(aes(x = age_group, y = number_est, fill = injury_mechanism), stat="identity", alpha=0.5) +
#   # Add text showing the value of each 100/75/50/25 lines
#   geom_hline(yintercept = c(400, 800, 1200), lty = 2, color = "grey" )+
#   annotate("text", x = c(10,10,10), y = c(225, 425, 625), 
#            label = c("200","400",expression(paste("1200 * 10", "2"^-1))), angle = 340) +
#   scale_fill_uchicago() +
#   geom_label_repel(data = labels_data, 
#                    mapping = aes(x = injury_mechanism, y = y, label = injury_mechanism), 
#                    segment.size = .5, nudge_y = 70, nudge_x = 2)+
#   labs(caption = "Brain Injuries by age & mechanism")+
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     plot.caption = element_text(hjust = 0.5, size = 16, vjust = 10),
#     legend.title = element_blank()
#   ) +coord_polar()
# 
# #####################################just testing scaling
# 
# # dataframe_me <- data.frame(x=rnorm(10)*1000,y=seq(0,1,length.out = 20))
# # 
# # 
# # p2 <- ggplot(dataframe_me, aes(x, y)) + geom_point()
# # p2+scale_y_continuous(labels=scales::percent)\

new_tbi_age %>% na.omit() %>% ggplot(aes(x=age_group,y=number_est,color=injury_mechanism))+
  geom_point()+geom_path(aes(group=1))+
  facet_wrap(~injury_mechanism,scales="free_y",ncol=2)+
  theme_minimal()+geom_hline(yintercept = 0,size=1,color="black")+
  scale_y_continuous(breaks = pretty_breaksx)

#############################################################################################################
# WE NOW EXPLORE THE MILLITARY DATA 
DataExplorer::plot_str(tbi_millitary)
DataExplorer::plot_missing(tbi_millitary)# the percentage of missing values is very minimal
tbi_millitary %<>%na.omit(tbi_millitary) 
#the diagnosed variable means the number diagnosed
levels(tbi_millitary$service)
levels(tbi_millitary$component)
levels(tbi_millitary$severity)
data_millitary <- tbi_millitary %>% group_by(service,severity,component) %>% 
  summarise(total_diagnosed=sum(diagnosed)) %>% 
  mutate(percentage=total_diagnosed/sum(total_diagnosed)) %>% ungroup()
# data_millitary %<>% filter(component!="Reserve")
data_millitary %<>% mutate(severity=fct_reorder(as.character(severity),percentage,.desc = TRUE))
data_millitary
data_millitary%>% ggplot(aes(x=reorder(severity,total_diagnosed),y=total_diagnosed,fill=service))+
  geom_bar(stat="identity", position = "dodge")+coord_flip()+
  theme(legend.position = "bottom")->millitary_plot
#lets do so with colors of kenya
millitary_plot+scale_fill_manual(values=c("darkgreen","navy", "red2", "black"))+theme_minimal()+
  theme(plot.background = element_rect("lightslateblue"),
        axis.text.x = element_text(size = 10,colour = "blue"),
        axis.text.y = element_text(size = 14,colour = "white",
                                   axis.t)
        )+labs(x="SEVERITY",y="TOTAL DIAGNOSED",
               caption = "Data Source: https://dvbic.dcoe.mil/dod-worldwide-numbers-tbi | graphics: @abiyugiday")






##############################Making dynamic gggplots
mtcars %>% split(.$cyl) %>% imap(~ggplot(.,aes(mpg,wt))
                                +geom_point()+
                         
                                           ggtitle(.$cyl))
mtcars %>% split(.$cyl) %>% 
  map(function(x){
  plot1 <- ggplot(x,aes(mpg,wt))+geom_point()
  plot2 <-ggplot(x,aes(drat,wt))+geom_point() 
 }
)

mpg %>% split(.$class) %>% 
  map(function(x){
    p <- ggplot(x,aes(cty,hwy))+geom_point()
    print(plotly::plotly_build(p))
  })


library(plotly)

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + geom_smooth()

fig <- p %>%
  ggplotly(layerData = 2, originalData = F) %>%
  add_fun(function(fig) {
    fig %>% slice(which.max(se)) %>%
      add_segments(x = ~x, xend = ~x, y = ~ymin, yend = ~ymax) %>%
      add_annotations("Maximum uncertainty", ax = 60)
  })
fig
















## new tricks learned today
#to see the total cases
tbi_millitary %>% group_by(.$severity) %>% 
  tally(.$severity,wt=diagnosed,name ="name") %>% ungroup()->severitydata
attach(tbi_millitary)
severitydata %>% ggplot2::ggplot(aes(x=severitydata$`.$severity`,y=name,fill=name))+geom_col()
data=tribble(~column,~x,~number,"a",4,5,"a",66,78,"a",67,88,"b",45,66,"b",67,88,"c",56,78)


data %>% group_by(column) %>% mutate(n=n()) %>% ungroup()
?n()
#the above can just be done in one step using the add_count()function
data %>% add_count(column)

#to get instances more than 1
data %>% add_count(column) %>% filter(n>1)



#to do , use the broom package to visualize multiple models from chapter 25 in r for ds



#what is your favourite plot?
#a bar plot that is sorted 

#tour forcats 
#scale_log10 normalises the distribution
glimpse(tbi_millitary)
#reveal the observations, number of rows, and what data type exists on each column, along with a mini-preview of the data.