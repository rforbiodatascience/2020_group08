#Shewa has stage,age, months of followup and weight_index.
installed.packages()
#load libraries
install.packages("broom")
install.packages("purrr")
library(purrr)
library(tidyr)
library(dplyr)
library(broom)
library(modelr)
library(ggplot2)
library(tibble)


is_tibble(prostate_data_clean)
class(prostate_data_clean)

#way of checking the categories 
factorvariables %>% group_by(ekg) %>% summarise(n())



#nested datasets
by_stage<-prostate_data_clean %>% group_by(stage) %>% nest()  

#by_stage %>%mutate(model= map(stage, diastolic_bp))

by_age<-prostate_data_clean %>% group_by(age) %>% nest()

by_weightindex<-prostate_data_clean %>% group_by(weight_index) %>% nest()

by_monthsfollow<-prostate_data_clean %>% group_by(months_of_follow_up) %>% nest()




##building models

#stage

model_stage<-function(df){
  
  lm(serum_hemoglobin~stage, df=prostate_data_clean)
  
}

#age

model_age<-function(df){
  
  return(lm(names(prostate_data_clean)[4] ~ age, data=df))
  
}
by_age %>%mutate(mdls=map(data, model_age))

model_age()

#weightindex

model_weightindex<-function(df){
  
  lm(serum_hemoglobin~age, df)
  
}

by_weightindex<-by_weightindex %>%mutate(mdls=map(data, model_weightindex)) %>% 
  mutate(glance=map(mdls,broom::glance)) %>% unnest(cols=glance)


by_weightindex %>% 
  arrange(desc(r.squared)) %>% 
  filter(r.squared >= 0.8)  

#know that those 10 datasets are good fit





