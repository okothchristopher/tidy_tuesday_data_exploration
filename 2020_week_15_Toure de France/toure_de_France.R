#install the package with the datsets 
# devtools::install_github("alastairrushworth/tdf")
# library(tdf)
# 
# rm(data)
# 
#since the above has failed let us just load the data manually 
# tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
# devtools::install_github("thebioengineer/tidytuesdayR")
# tuesdata <- tidytuesdayR::tt_load('2020-04-07')
# #tuesdata <- tidytuesdayR::tt_load(2020, week = 15)
# object.size(tuesdata)
# tuesdata$stage_data
# tuesdata$tdf_stages
# write.csv(tdf_winners,"tdf_winners.csv")
# write.csv(tuesdata$stage_data,"stage_data.csv")
# #to get the cdocumentation 

tdf_stages <- read.csv(file.choose())



library(tidyverse)
#attach(tuesdata)
stage_data %>% count(stage_results_id)
glimpse(tdf_stages)
tdf_stages %>% count(Winner_Country) %>% arrange(desc(n))
#most winners come from France 
tdf_stages %>% count(Stage)
tdf_winners %>% count(winner_team)

tdf_winners %>% count(birth_country,sort = T) %>% mutate(birth_country=fct_reorder(birth_country,n)) %>%
  ggplot(aes(birth_country,y=n))+geom_col()+coord_flip()
tdf_winners %>% count(winner_team,sort = T) 

tdf_winners %>%  mutate(winner_team=fct_lump(winner_team,7,other_level = "Other"))%>% 
        ggplot(aes(winner_team,fill=winner_team))+geom_bar(stat="count")

#alternatively 
#tdf_winners %>% count(winner_team,sort = T) %>% filter(n>2)->the_winning_teams

#tdf_winners %>% filter(winner_team%in%the_winning_teams$winner_team)

library(ggdark)
tdf_winners %>% count(birth_country,sort = T) %>%mutate(birth_country=fct_reorder(birth_country,n)) %>%  ggplot(aes(birth_country,n,fill=birth_country))+geom_col()+
  ggsci::scale_fill_simpsons()+dark_theme_grey()+coord_flip()+scale_y_continuous(breaks = seq(1,40,2))+ggtitle("where do most countries originate")
#WHO ARE THE winners 
tdf_winners %>% count(full_name) %>% arrange(desc(n)) %>% na.omit() %>% tbl_df()

#from which countries 


#how has the age,weight and heigt dist changed over time 

tdf_winners %>% ggplot(aes(start_date,y=age,color=age))+geom_line()

#to group by decades 
library(lubridate)
tdf_winners %>% mutate(year=year(start_date)) %>% mutate(point=year%/%10) %>% group_by(decade=point*10) %>%  
  summarise(winner_age=mean(age),winner_height=mean(height,na.rm = T),winner_weight=mean(weight,na.rm = T)) %>% 
  ggplot(aes(decade,winner_age,color=winner_age))+geom_line(size=3,color="red")+expand_limits(y=5)+theme_dark()

tdf_winners %>% mutate(year=year(start_date)) %>% mutate(point=year%/%10) %>% group_by(decade=point*10) %>%  
  summarise(winner_age=mean(age),winner_height=mean(height,na.rm = T),winner_weight=mean(weight,na.rm = T)) %>% ungroup() %>% 
  ggplot(aes(as.factor(decade),winner_age,color=winner_age))+geom_boxplot()
#in the 1910 and 1920s the metrics for height were not being taken 

#has time margins changed 
tdf_winners %>% mutate(year=year(start_date)) %>% mutate(point=year%/%10) %>% group_by(decade=point*10) %>%  
  summarise(winner_age=mean(age),winner_height=mean(height,na.rm = T),winner_weight=mean(weight,na.rm = T),winner_margin=mean(time_margin)) %>% 
  ggplot(aes(decade,winner_margin))+geom_line(size=1.4,color="cyan")+dark_mode()

#let us get those which top 10 winners  have led the most stages 
tdf_winners %>% count(winner_name,sort = T) %>% top_n(.,n=9,wt=n)->top_candidates
#tdf_winners %>% filter(winner_name %in% top_candidates$winner_name) %>%tally(wt=stages_led)
#who has coveres the most max distance
tdf_winners %>% filter(winner_name %in% top_candidates$winner_name) %>% group_by(winner_name) %>% summarise(avg_distance=mean(distance)) %>%
  arrange(desc(avg_distance))

#obtain a speed of the winners and see whether they have been getting faster or slower overally 
tdf_winners %>% mutate(speed=distance/time_overall) %>% mutate(year=year(start_date)) %>% mutate(point=year%/%10) %>% 
  group_by(decade=point*10) %>%summarise(mean_speed=mean(speed)) %>% ggplot(aes(x = decade,y=mean_speed))+geom_line(size=1.8,color="magenta")+dark_mode()+expand_limits(y=0)
#they have been gettinig faster over time , maybe the bikes have been getting faster and the participanct perhaps more trained 


# can we see the aveerage life expecatancy of the winners 
# this will require survival analysis and we have to keep in mind this is a right censored data in that most of the participants are still alive 
copy=tdf_winners
tdf_winners$died <- ifelse(is.na,tdf_winners$died,today())


tdf_winners %>% distinct(winner_name,.keep_all = T) %>% transmute(winner_name,born_year=year(born),died_year=year(died),dead=as.integer(!is.na(died_year)),
                                                                  died_year=coalesce(died_year,2020),age_at_death=died_year-born_year) %>% 
  tbl_df()

library(survival)
tdf_winners %>% distinct(winner_name,.keep_all = T) %>% transmute(winner_name,born_year=year(born),died_year=year(died),dead=as.integer(!is.na(died_year)),
                                                                  died_year=coalesce(died_year,2020),age_at_death=died_year-born_year) %>% 
  survfit(Surv(age_at_death,dead)~1,data = .)->surv_model
#they are all dead by early 90s the median life expectancy is 77 , we could cohort the data 


# table(tdf_stages$Type,tdf_stages$Winner_Country)
# # this is how you deal with such 




tdf_stages %>% tbl_df()
tdf_stages %>% glimpse() 
tdf_stages %>% count(Winner_Country,sort = T)
tdf_stages %>% count(Type)
# tdf_stages_new %>% mutate(date=mdy(Date)) %>% tbl_df()
stage_data %>% tbl_df()
#when it got parsed the time ended up with seconds alone so we better not use time 
colSums(is.na(stage_data))
#let us use janitor  to change the names in the tdf stages to lowercase
tdf_stages %>% janitor::clean_names() %>% tbl_df()
#now we can join the two together 
tdf_stages %>%janitor::clean_names() %>%mutate(year=year(date)) %>% tbl_df()->tdf_stages_n
tdf_stages_n %<>% mutate(stage=as.character(stage))
#we are grabbing the number(in this case anything based on that regex) from the stage and naming that column stage 
stages_joined <- stage_data %>% tidyr::extract(stage_results_id,"stage","stage-(.*)",convert=F) %>% #the convert =t converts the resultant result to an integer 
  inner_join(tdf_stages_n,by=c("year","stage")) %>% mutate(rank=as.integer(rank)) %>% add_count(year,stage,name = "competitors")
#to get those who finnished 
stages_joined %>% group_by(year,stage) %>% summarise(finnishers=sum(rank)) %>% ungroup()


 
stages_joined %>% group_by(winner_country) %>% summarise(stages=n(),med_rank=median(rank,na.rm = T)) %>% arrange(desc(stages))
#stages_joined %>% count(year,stage) %>% ggplot(aes(n))+geom_histogram()
total_points <- stages_joined %>% group_by(year,rider) %>% summarise(point=sum(points,na.rm = T))%>% mutate(final_rank=percent_rank(point))

total_points %>% tbl_df()


stages_joined %>% group_by(year,rider) %>% summarise(point=sum(points,na.rm = T))%>% mutate(final_rank=percent_rank (point))


#does the winner of the first stage predict their final rank?
stages_joined %>% filter(stage=="1") %>% inner_join(total_points,by=c("year","rider")) %>% select(year,rider,points,rank_first_stage=rank,
                                                                                                  points_first_stage=points,final_rank)
 #  of the first 13000 plus people how does their points in the first stage predict their final rank 

stages_joined %>% filter(stage=="1") %>% inner_join(total_points,by=c("year","rider")) %>% select(year,rider,points,rank_first_stage=rank,
                                                                                                  points_first_stage=points,final_rank) %>% 
  ggplot(aes(rank_first_stage,final_rank))+geom_point(alpha=0.2)



#let us now animate the race using gganimate for some of the top players 
library(gganimate)
library(tidytext)
total_points %>% filter(year==2017) %>% top_n(10)->latest_winners

stages_joined %>% filter(year==max(year)) %>% semi_join(latest_winners,by="rider") %>% mutate(stage=as.integer(stage)) %>% 
  mutate(points=replace_na(points,0)) %>% 
  group_by(rider) %>% mutate(cum_sum=cumsum(points)) %>% ungroup()  %>% 
  ggplot(aes(y=cum_sum,x=rider,fill=rider))+geom_col()+theme(axis.text.x = element_text(angle = 90,hjust = 1))+transition_time(stage)+
  labs(title="The 2017 tour de France race .Stage:{frame_time}")->plc
anim_save("tourdefrance in 2017.gif")

#let us now make them shift positions as they move 
# 
# stages_joined %>% filter(year==max(year)) %>% semi_join(latest_winners,by="rider") %>% mutate(stage=as.integer(stage)) %>% 
#   mutate(points=replace_na(points,0)) %>% 
#   group_by(rider) %>% mutate(cum_sum=cumsum(points)) %>% ungroup()  %>% mutate(rider=reorder_within(rider,cum_sum,stages)) %>% 
#   ggplot(aes(y=cum_sum,x=rider,fill=rider))+geom_col()+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
#   scale_y_reorderd+ transition_time(stage)+
#   labs(title="The 2017 tour de France race .Stage:{frame_time}")
  #so how did they fair on over the course of the race
