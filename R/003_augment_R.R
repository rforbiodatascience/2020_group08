#Shewa has stage,age, months of followup and weight_index.
installed.packages()
#load libraries
install.packages("broom")
install.packages("purrr")
install.packages("ggplot2")
library(purrr)
library(tidyr)
library(dplyr)
library(broom)
library(modelr)
library(ggplot2)
library(tibble)
library(rlang)


#preparing data-nesting
#grouping my categorical variable
#interested in death cause and status

by_deathcause<-prostate_data_clean %>% 
  group_by(cause_of_death, status_) %>%
  nest() 

##building models


model1<-function(df){
  
  lm(serum_hemoglobin~sdate, df)
  
}



#nesting and modelling
by_deathcause<-by_deathcause %>% 
  mutate(mdls=map(data,model1)) %>% 
  mutate(resids= map2(data, mdls, add_residuals), 
       pred=map2(data, mdls, add_predictions))


#unnest prediction
pred<-unnest(by_deathcause, pred)
resids<-unnest(by_deathcause, resids)
resids

#plot the prediction
pred %>% 
  ggplot(aes(sdate, pred, group=status_))+
  geom_line()+
  geom_smooth()


#want clearer view on each cause of death!

pred %>% 
  ggplot(aes(sdate, pred, group=status_))+
  geom_line()+
  facet_wrap(~status_)

pred %>% 
  ggplot(aes(sdate, pred, group=cause_of_death))+
  geom_line()+
  facet_wrap(~cause_of_death)

#
dim(pred)
dim(pred$sdate)
dim(pred$pred)

View(pred)

#plot the residual  !!!!!!!!!!!!
#weight_index
#bad
resids %>% 
  ggplot(aes(sdate, resids$weight_index, group=cause_of_death))+
  geom_line()+
  geom_smooth(se=FALSE)

#better
residual_weight<-resids %>% 
  ggplot(aes(sdate, resids$weight_index, group=cause_of_death))+
  geom_line()+
  facet_wrap(~cause_of_death)

#residuals on 
#age

residual_age<-resids %>% 
  ggplot(aes(sdate, resids$age, group=cause_of_death))+
  geom_line()+
  facet_wrap(~cause_of_death)

#months of follow up
residual_months<-resids %>% 
  ggplot(aes(sdate, resids$months_of_follow_up, group=cause_of_death))+
  geom_line()+
  facet_wrap(~cause_of_death)



#model 2
model2<-function(df){
  
  lm(diastolic_bp~sdate, df)
  
}                

#nesting and modelling
by_deathcause_dia<-by_deathcause %>% 
  mutate(mdls=map(data,model2)) %>% 
  mutate(resids= map2(data, mdls, add_residuals), 
         pred=map2(data, mdls, add_predictions))


#unnest prediction
pred2<-unnest(by_deathcause_dia, pred)
resids2<-unnest(by_deathcause_dia, resids)
resids
is.na(pred2)

#plot the prediction

pred2 %>% 
  ggplot(aes(sdate, pred, group=status_))+
  geom_line()+
  facet_wrap(~status_)

pred2 %>% 
  ggplot(aes(sdate, pred, group=cause_of_death))+
  geom_line()+
  facet_wrap(~cause_of_death)


#plot the residual  !!!!!!!!!!!!
#weight_index
residual_weight2<-resids2 %>% 
  ggplot(aes(sdate, resids2$weight_index, group=cause_of_death))+
  geom_line()+
  facet_wrap(~cause_of_death)

#age
residual_age2<-resids2 %>% 
  ggplot(aes(sdate, resids2$age, group=cause_of_death))+
  geom_line()+
 facet_wrap(~cause_of_death)

#months
residual_months2<-resids2 %>% 
  ggplot(aes(sdate, resids2$months_of_follow_up, group=cause_of_death))+
  geom_line()+
  facet_wrap(~cause_of_death)


