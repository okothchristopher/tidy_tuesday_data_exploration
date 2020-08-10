
# loading the data  -------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2020, week = 24)
firsts <- tuesdata$firsts
science <- tuesdata$science

# this removes things from the environment except the specifies list 
rm(list= ls()[! (ls() %in% c('tuesdata'))])
 

# alternatively load data 

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

# packages ----------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(ggsci)
library(ggdark)
library(plotly)
library(magrittr)
firsts %>% View()


firsts %>% 
  count(gender)


# africa american achievements by category per gender ---------------------
firsts %>% 
  group_by(category,gender) %>% 
  count(category,gender) %>% 
  ungroup() %>% 
  mutate(category=fct_reorder(category,n)) %>% 
  mutate(gender=case_when(str_detect(gender,"African-American Firsts")~"Male",
                          TRUE~"Female"))  %>% 
  ggplot(aes(category,n,fill=gender))+
  geom_col(position = "dodge")+
  # geom_text(check_overlap = F,color="black",size=5)+
  scale_fill_manual(values = c("navy","skyblue"))+
  labs(caption = "Achievements by category and gender",
       y='number',
       x='category',
       title = 'AFRICAN-AMERICAN ACHIEVEMENTS BY GEDNER')+
  theme(plot.caption = element_text(face = "bold"))+
  ggthemes::theme_economist()+
  coord_flip()
ggsave("achievemnts by gender.png",dpi = 600,height = 8,width = 10,units = "in")#save the plot
 

# achievements since 1738 of African-American -----------------------------

plot<- firsts %>% 
  mutate(PERSON_ACCOMPLISHMENT=paste(person,accomplishment,sep = "----")) %>% 
  add_count(category) %>% 
  ggplot(aes(year,n,color=category,label=PERSON_ACCOMPLISHMENT))+
  geom_point(size=3)+
  theme(axis.text.y = element_blank())+
  scale_x_continuous(breaks = seq(1738,2050,30))+
  labs(y="number of category accomplishments")+
  ggtitle('ACCOMPLISHMENTS OF AFRICAN AMERICANS FROM 1738 TO 2019')+
  ggdark::dark_theme_gray()

my_plt <- ggplotly(plot,tooltip = c("year","category","PERSON_ACCOMPLISHMENT"))
### save the file 
htmlwidgets::saveWidget(my_plt,"black_accomplishments.html")

# Achievements by category ------------------------------------------------
firsts %>% 
  mutate(person_achievement=paste(person,accomplishment,sep = "----")) %>% 
  add_count(category) %>% 
  ggplot(aes(year,n,color=category,label=person_achievement))+
  geom_point(size=2)+
  scale_x_continuous(breaks = seq(1738,2050,30))+
  ggsci::scale_color_simpsons()+
  ggdark::dark_theme_grey()

# firsts %>% 
#   mutate(person_achievement=paste(person,accomplishment,sep = "----")) %>%
#   add_count(year) %>% view()

# -------------------------------------------------------------------------


# sicence -----------------------------------------------------------------


science %>% view()
science%>% separate(occupation_s,into = c("main_occupation","other_occupation"),sep=";",
                    remove = FALSE,convert = F,extra = "merge",fill='right') %>% 
  view()


science %>% mutate(death=replace_na(death,replace =2020 )) %>% 
  mutate(age=death-birth) %>% view()

science %>% count(occupation_s,sort = T) %>% 
  view()


science %>%
  mutate(occupation_s=str_to_lower(occupation_s)) %>% 
  mutate(science_field=case_when(str_detect(occupation_s,"computer")~"computing",
                                 str_detect(occupation_s,"engineer|elec|robo|acous")~"engineering",
                                 str_detect(occupation_s,"math|stat|prob")~"mathematics",
                                 str_detect(occupation_s,"surgeon|med|nurse|dent|ophtha|psycho|surg|ortho")~"health_science",
                                 str_detect(occupation_s,"astr|atmos")~"space_and_geography",
                                 str_detect(occupation_s,"chemis")~"chemistry",
                                 str_detect(occupation_s,"econ|finan")~"economics",
                                 str_detect(occupation_s,"physic")~"physics",
                                 str_detect(occupation_s,"bio|gene|botan|epidem|zoo")~"biology",
                                 str_detect(occupation_s,"resear")~"research",
                                 str_detect(occupation_s,"farmer")~"agriculture",
                                 str_detect(occupation_s,"art|draft|archi")~"design",
                                 str_detect(occupation_s,"ling|anthro")~"language_and_culture",
                                 str_detect(occupation_s,"inventor")~"inventor",
                                 TRUE~occupation_s,
                                 )) %>% add_count(science_field,name = "number")%>% 
  # na.omit(science_field) %>% 
  mutate(science_field= fct_reorder(science_field,number)) %>% 
  ggplot(aes(science_field,number,label=number,fill=science_field))+
  geom_col(na.rm = TRUE)+
  geom_text(color="black",hjust="inward")+
  theme(legend.position = "none",axis.text.x= element_blank())+
  coord_flip()+
  ggsci::scale_fill_simpsons()




# survival analysis  ------------------------------------------------------

# We want to answer the question is there any diffferences in survival between the groups 
# The Kaplan Meier plots are uselful in visualizing survival curves 
# Log-rank test to compare the survival curves of two or more groups - need to quanify the survival differences between two or more groups 
# Cox proportional hazards regression to describe the effect of variables on survival


# CENSORING 
# a patient has not (yet) experienced the event of interest- in this case death ie date of death 2020
# a patient is lost to follow-up during the study period-my interpretation -those with NA age 


library(survival)
library(survminer)


# we want to calculate survival by science field 


science %>%
  mutate(death=replace_na(death,replace =2020 )) %>% 
  mutate(age=death-birth) %>% 
  mutate(occupation_s=str_to_lower(occupation_s)) %>% 
  mutate(science_field=case_when(str_detect(occupation_s,"computer")~"computing",
                                 str_detect(occupation_s,"engineer|elec|robo|acous")~"engineering",
                                 str_detect(occupation_s,"math|stat|prob")~"mathematics",
                                 str_detect(occupation_s,"surgeon|med|nurse|dent|ophtha|psycho|surg|ortho")~"health_science",
                                 str_detect(occupation_s,"astr|atmos")~"space_and_geography",
                                 str_detect(occupation_s,"chemis")~"chemistry",
                                 str_detect(occupation_s,"econ|finan")~"economics",
                                 str_detect(occupation_s,"physic")~"physics",
                                 str_detect(occupation_s,"bio|gene|botan|epidem|zoo")~"biology",
                                 str_detect(occupation_s,"resear")~"research",
                                 str_detect(occupation_s,"farmer")~"agriculture",
                                 str_detect(occupation_s,"art|draft|archi")~"design",
                                 str_detect(occupation_s,"ling|anthro")~"language_and_culture",
                                 str_detect(occupation_s,"inventor")~"inventor",
                                 TRUE~occupation_s,
  )) %>% 
  select(-c(inventions_accomplishments,links,references)) %>% 
  mutate(censored=case_when(death==2020|is.na(birth)~"yes",
                            TRUE~"no")) ->profession
skimr::skim(profession)

profession %<>%
  mutate_if(is.character,factor) %>% 
  mutate(chemistry=case_when(science_field=="chemistry"~"chemistry",
                             TRUE~"non-chemistry"))
  



# model_fit ---------------------------------------------------------------

fit <- survfit(Surv(age)~chemistry,data = profession)
fit %>% broom::tidy() %>% 
  ggplot(aes(time,estimate,color=strata))+
  geom_line(size=3)+
  labs(
    y="survival_probability",
    x="age",
    title = "SURVIVAL PROBABILITY OF SCIENTISTS IN CHEMISTRY DEPARTMENT VS IN OTHER FIELDS",
    caption = "data from tidytuesday week  24"
  )+
  ggthemes::theme_economist()


fit
# there appears to be a survival advantage for those in the chemistry field as the median age is ~76 compared to the ones in the non chemistry field

fit_with_censoring <- survfit(Surv(age,censored)~chemistry,data = profession)
fit_with_censoring 




# scientists webscrapping  ------------------------------------------------



library(rvest)
read_html('https://en.wikipedia.org/wiki/David_Blackwell') %>% 
  html_node(".vcard") %>% 
  html_nodes("th+td") %>% 
  html_text()

read_html('https://en.wikipedia.org/wiki/David_Blackwell') %>% 
  html_node(".vcard") %>% 
  html_table() %>% 
  setNames(c('key','value')) %>% 
  as_tibble()

# now this can be done for multiple links using the map function and then use the possibly feature for those links that fail 




# # text annotation  --------------------------------------------------------
# 
# 
# 
# data("mtcars")
# df <- mtcars
# df$cyl <- as.factor(df$cyl)
# df$name <- rownames(df)
# head(df[, c("wt", "mpg", "cyl")], 3)
# library(ggtext)
# # Textual annotation
# # +++++++++++++++++
# 
# ggtext(df, x = "wt", y = "mpg",
#        color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#        label = "name", repel = TRUE)
# 
# # Add rectangle around label
# ggtext(df, x = "wt", y = "mpg",
#        color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#        label = "name", repel = TRUE,  label.rectangle = TRUE)
# 
# 
# # }
# 
# 
# 
# 
