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



#plot the residual  !!!!!!!!!!!!
resids %>% 
  ggplot(aes(sdate, resids, group=cause_of_death))+
  geom_line()+
  geom_smooth(se=FALSE)

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

  ggplot(data=pred2, mapping=aes(sdate, pred2))+
  geom_line(aes(group=status_))+
  geom_smooth(se=FALSE)


#want clearer view on each cause of death!

ggplot(data=pred2, mapping=aes(sdate, pred2, group=cause_of_death))+
  geom_line()+
  facet_wrap(~status_)

#plot the residual  !!!!!!!!!!!!
resids2 %>% 
  ggplot(aes(sdate, resids2, group=cause_of_death))+
  geom_line()+
  geom_smooth(se=FALSE)

