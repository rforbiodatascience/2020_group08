#Sehwa has stage,age, months of followup and weight_index.
installed.packages()
#load libraries
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





#####models 

bsmodel<-function(prostate_data_clean){
  lm(bone_metastases~serum_hemoglobin, data=prostate_data_clean)
  
}

#nesting 

cause_of_death_nest_bs<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, bsmodel))

#mapping glance function
cause_glance_bs<-
  cause_of_death_nest_bs %>% mutate(glance=map(model, broom::glance))


cause_glance_bs %>% unnest(glance)



##no r squared values


##model weight index+serum hemoglobin

model_weightserum<-function(prostate_data_clean){
  
  lm(weight_index~serum_hemoglobin, data=prostate_data_clean)
  
}


cause_of_death_nest_ws<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_weightserum))

cause_glance_ws<-
  cause_of_death_nest_ws %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)

cause_glance_ws

#r-squared=0.582

#systolic bp+serum hemo
model_sysserum<-function(prostate_data_clean){
  
  lm(systolic_bp~serum_hemoglobin, data=prostate_data_clean)
  
}

cause_of_death_nest_sysserum<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_sysserum))

cause_glance_sysserum<-
  cause_of_death_nest_sysserum %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)

cause_glance_sysserum

#rsquared value-0.239

#diastolic+serum hemo
model_diaserum<-function(prostate_data_clean){
  
  lm(diastolic_bp~serum_hemoglobin, data=prostate_data_clean)
  
}


cause_of_death_nest_diaserum<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_diaserum))

cause_glance_diaserum<-
  cause_of_death_nest_diaserum %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)

cause_glance_diaserum

#rsquareed= 0.218

#PA+serum hemo

model_PAserum<-function(prostate_data_clean){
  
  lm(PA_phosphatase~serum_hemoglobin, data=prostate_data_clean)
  
}

cause_of_death_nest_PAserum<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_PAserum))

cause_glance_PAserum<-
  cause_of_death_nest_PAserum %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)

cause_glance_PAserum

#r squared value= 0.113

#sys+weight
model_sysweight<-function(prostate_data_clean){
  
  lm(systolic_bp~ weight_index, data=prostate_data_clean)
  
}


cause_of_death_nest_sysweight<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_sysweight))

cause_glance_sysweight<-
  cause_of_death_nest_sysweight %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)

cause_glance_sysweight

#rsquared=0.537

#dia+weight

model_diaweight<-function(prostate_data_clean){
  
  lm(diastolic_bp~ weight_index, data=prostate_data_clean)
  
}


cause_of_death_nest_diaweight<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_diaweight))

cause_glance_diaweight<-
  cause_of_death_nest_diaweight %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)

cause_glance_diaweight

#rsquared=0.00552
#PA+weight
model_PAweight<-function(prostate_data_clean){
  
  lm(PA_phosphatase~ weight_index, data=prostate_data_clean)
  
}


cause_of_death_nest_PAweight<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_PAweight))

cause_glance_PAweight<-
  cause_of_death_nest_PAweight %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)

cause_glance_PAweight

#rsqured=0.222

#dia+sys

model_diasys<-function(prostate_data_clean){
  
  lm(systolic_bp~diastolic_bp, data=prostate_data_clean)
  
}


cause_of_death_nest_diasys<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_diasys))

cause_glance_diasys<-
  cause_of_death_nest_diasys %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)

cause_glance_diasys
#PA+sys
model_PAsys<-function(prostate_data_clean){
  
  lm(PA_phosphatase~systolic_bp, data=prostate_data_clean)
  
}

cause_of_death_nest_PAsys<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_PAsys))

cause_glance_PAsys<-
  cause_of_death_nest_PAsys %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)

cause_glance_PAsys

#rsquared-0.716

#PA+dia
model_PAdia<-function(prostate_data_clean){
  
  lm(PA_phosphatase~diastolic_bp, data=prostate_data_clean)
  
}


cause_of_death_nest_PAdia<-prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model=map(data, model_PAdia))

cause_glance_PAdia<-
  cause_of_death_nest_PAdia %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance)


#maximum values of r squared values are reuiqred to figure out the variables to analyse
maxrsq<-tibble(
  
  x=c(max(cause_glance_PAsys$r.squared),
      max(cause_glance_PAdia$r.squared),
      max(cause_glance_diaserum$r.squared),
      max(cause_glance_sysserum$r.squared),
      max(cause_glance_sysweight$r.squared),
      max(cause_glance_diasys$r.squared),
      max(cause_glance_PAweight$r.squared),
      max(cause_glance_tm$r.squared),
      max(cause_glance_diaweight$r.squared),
      max(cause_glance_PAserum$r.squared)
  )
)

max(maxrsq)
maxrsq

#PA+systolic shows the second highest correlation

