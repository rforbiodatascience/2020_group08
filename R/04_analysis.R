# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(forcats)
library(caret)
library(caretEnsemble)

# Load data
# ------------------------------------------------------------------------------
prostate_data_clean_aug <- read_tsv(file = "Data/03_prostate_data_clean_aug.tsv")

#Ensure that factorial variables are actually factors
factor_columns <- c("stage", "activity", "history_of_CD", "ekg", "bone_metastases",
                    "estrogen_mg", "status_", "cause_of_death", "dead_from_prostate_cancer",
                    "Age_group")

prostate_data_clean_aug[factor_columns] <- lapply(prostate_data_clean_aug[factor_columns], factor)

# Wrangle and analyse data
# ------------------------------------------------------------------------------

###### Plots #######

###1. PA phosphatase + cause of death

prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(x = fct_reorder(cause_of_death, PA_phosphatase, .fun = mean, .desc = T), y = PA_phosphatase, color = bone_metastases)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cause of death",
       y = "Serum Prostatic Acid Phosphatase (unit?)")+
  theme(legend.position = "bottom")

##observation
#The highest PA phosphatase is observed with the prostate cancer cases with bone metastasis.
#Generally, the level of PA phosphatase is higher for bone mestasis than non-bone metastasis.

### Bone metastases + status
# Calculate "dead from prostate cancer" percentage
metastases_cancer_dead_percentage <- (prostate_data_clean_aug %>% 
                                        filter(status_ == "dead", bone_metastases == "1", dead_from_prostate_cancer == 1) %>% 
                                        nrow())/
  ((prostate_data_clean_aug %>% 
      filter(status_ == "dead", bone_metastases == "1", dead_from_prostate_cancer == 1) %>% 
      nrow())+
     (prostate_data_clean_aug %>% 
        filter(status_ == "dead", bone_metastases == "1", dead_from_prostate_cancer == 0) %>% 
        nrow())) * 100

metastases_cancer_dead_percentage <- str_c(round(metastases_cancer_dead_percentage, 2), "%")


nonmetastases_cancer_dead_percentage <- (prostate_data_clean_aug %>% 
                                           filter(status_ == "dead", bone_metastases == "0", dead_from_prostate_cancer == 1) %>% 
                                           nrow())/
  ((prostate_data_clean_aug %>% 
      filter(status_ == "dead", bone_metastases == "0", dead_from_prostate_cancer == 1) %>% 
      nrow())+
     (prostate_data_clean_aug %>% 
        filter(status_ == "dead", bone_metastases == "0", dead_from_prostate_cancer == 0) %>% 
        nrow())) * 100

nonmetastases_cancer_dead_percentage <- str_c(round(nonmetastases_cancer_dead_percentage, 2), "%")

#plot
prostate_data_clean_aug %>% 
  ggplot(aes(x = bone_metastases, y = status_, color = dead_from_prostate_cancer)) +
  geom_jitter() + 
  annotate("text", x = 1:2, y = 2.5, label = c(nonmetastases_cancer_dead_percentage, metastases_cancer_dead_percentage)) +
  labs(x = "Bone metastases",
       y = "Status")


### Bone metastases + activity

# Transform count by group
activity_percentage <- prostate_data_clean_aug %>% 
  group_by(activity, bone_metastases)%>% 
  summarise(counts = n()) %>% 
  pivot_wider(
    names_from = bone_metastases,
    values_from = counts) %>% 
  rename(. ,no_meta = "0", meta = "1")

# replace NA's with 0
activity_percentage[is.na(activity_percentage)] <- 0  

# calculate percentage and plot 
activity_percentage %>% 
  mutate(
    no = no_meta/sum(activity_percentage$no_meta) * 100,
    yes = meta/sum(activity_percentage$meta) * 100
  ) %>% 
  pivot_longer(
    -c(activity, meta, no_meta),
    names_to = "bone_metastases",
    values_to = "percentage"
  ) %>% 
  ggplot(aes(x = activity, y = percentage, fill = bone_metastases)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Serum hemoglbin + bone metastases
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(y = serum_hemoglobin, x = bone_metastases, color = Age_group)) +
  geom_boxplot()+
  theme(legend.position = "bottom")

### Tumor size + bone metastases
prostate_data_clean_aug %>% 
  ggplot(aes(y = tumor_size, x = bone_metastases, color = dead_from_prostate_cancer)) +
  geom_boxplot()+
  theme(legend.position = "bottom")


###Bone metastases vs estrogen and status 
# Shows that no patients with the low doses and bone metastases are alive, 
# but at the same time, the higher dose seems to be bad if you have bone metastases

# Transform count by grouped by bone_metastases
# Pivot_wide to have percentage for each bone_metastases group and not in total
estrogen_metastases_percentage <- prostate_data_clean_aug %>% 
  group_by(estrogen_mg, bone_metastases, status_)%>% 
  summarise(counts = n()) %>% 
  pivot_wider(
    names_from = bone_metastases,
    values_from = counts) %>% 
  rename(. ,no_meta = "0", meta = "1")

# replace NA's with 0
estrogen_metastases_percentage[is.na(estrogen_metastases_percentage)] <- 0  

# calculate percentage and plot 
estrogen_metastases_percentage %>% 
  mutate(
    no = no_meta/sum(estrogen_metastases_percentage$no_meta) * 100,
    yes = meta/sum(estrogen_metastases_percentage$meta) * 100
  ) %>% 
  pivot_longer(
    -c(estrogen_mg, status_, meta, no_meta),
    names_to = "bone_metastases",
    values_to = "percentage"
  ) %>% 
  ggplot(aes(x = estrogen_mg, y = percentage, fill = bone_metastases)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  facet_wrap(~ status_)

### Age_group vs estrogen and status

# Transform count grouped by Age_group (row 42 is removed due to age = NA)
# Pivot_wide to have percentage for each Age_group group and not in total
estrogen_age_percentage <- prostate_data_clean_aug[-42,] %>% 
  group_by(estrogen_mg, status_, Age_group)%>% 
  summarise(counts = n()) %>% 
  pivot_wider(
    names_from = Age_group,
    values_from = counts) %>% 
  rename(. ,fortyfive = "45 - 59", sixty = "60 - 69", seventy = "70 - 79", eighty = "80 - 90")

# replace NA's with 0
estrogen_age_percentage[is.na(estrogen_age_percentage)] <- 0  

# calculate percentage and plot 
estrogen_age_percentage %>% 
  mutate( 
    "45 - 59" = fortyfive/sum(estrogen_age_percentage$fortyfive) * 100,
    "60 - 69" = sixty/sum(estrogen_age_percentage$sixty) * 100,
    "70 - 79" = seventy/sum(estrogen_age_percentage$seventy) * 100,
    "80 - 90" = eighty/sum(estrogen_age_percentage$eighty) * 100
  ) %>% 
  pivot_longer(
    -c(estrogen_mg, status_, fortyfive, sixty, seventy, eighty),
    names_to = "Age_group",
    values_to = "percentage"
  ) %>% 
  ggplot(aes(x = estrogen_mg, y = percentage, fill = Age_group)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  facet_wrap(~ status_)


### PA_phosphatase vs stage_grade_index
prostate_data_clean_aug %>% 
  ggplot(aes(y = PA_phosphatase, x = stage_grade_index, color = dead_from_prostate_cancer)) +
  geom_jitter()

### Note: change/look at the plots where na.omit() has been used, since this removes  all rows with a NA in any column ###


###=======================================================================================
###=======================================================================================
###=======================================================================================
#Analysis on tumor size for it was found out that it had impact on the cause of death


#build model with tumor size and age

tumormodel<-function(prostate_data_clean_aug){
  
  lm(age~tumor_size, data=prostate_data_clean_aug)
  
}
#nesting and adding model to analyse the cause of death on the tumor size

cause_of_death_nest_tm<-prostate_data_clean_aug %>%
  na.omit() %>%
  group_by(cause_of_death) %>%
  nest() %>%
  mutate(model=map(data, tumormodel))

#mapping to glance function

cause_glance_tm<-cause_of_death_nest_tm %>% 
  mutate(glance=map(model, broom::glance))%>% 
           unnest(glance)

         cause_glance_tm
#plotting grouped by the cause of death

prostate_data_clean_aug %>% 
  ggplot(mapping=aes(x=tumor_size, y= age))+
  geom_jitter()+
  geom_smooth(method=lm, na.rm = TRUE, se=TRUE)+
  facet_wrap(~cause_of_death)

#observation: for the prostate cancer, the tumor size increases around the age of 70s
##
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(x=))

###=======================================================================================
###=======================================================================================
###=======================================================================================
#building model with PA_phosphatase and systolic_bp
ps_model<-function(prostate_data_clean_aug){
  
  lm(PA_phosphatase~systolic_bp, data = prostate_data_clean_aug)
}

#nesting and adding model
nest_ps<-prostate_data_clean_aug %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model = map(data, ps_model))

#mapping glance function on model go get its stats

glance_ps<-nest_ps %>% 
  mutate(glance=map(model, broom::glance))

#unnest the glance to see
glance_ps %>% unnest(glance)

#pulmonary and unknown cause of death contributed when the PA phosphatase and systolic bp came into play\


#plotting
prostate_data_clean_aug %>% 
  ggplot(mapping = aes(PA_phosphatase, systolic_bp))+
  geom_jitter() +
  geom_smooth(method = lm)+
  facet_wrap(~cause_of_death)

#Bone metastasis+weight (cate+conti)
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(mapping = aes(bone_metastases, weight_index, color=dead_from_prostate_cancer))+
  geom_jitter()+
  facet_wrap(cause_of_death~dead_from_prostate_cancer)+
  geom_smooth(method=lm)+
  theme(legend.position = "bottom")+
  annotate("text", label=c())

prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(mapping = aes(bone_metastases, weight_index, color=serum_hemoglobin))+
  geom_jitter()+
  geom_smooth(method=lm)+
  facet_wrap(cause_of_death~dead_from_prostate_cancer)+
  theme(legend.position = "bottom")
  
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(mapping = aes(bone_metastases, weight_index, color=PA_phosphatase))+
  geom_jitter()+
  geom_smooth(method=lm)+
  facet_wrap(cause_of_death~dead_from_prostate_cancer)+
  theme(legend.position = "bottom")


#BONE METASDTASIS+STATUS (conti+disc)

prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(mmapping = aes(status_, bone_metastases))+
  geom_boxplot()

#BONE METASDTASIS+ACTIVITY

#transform count by group
activity_bone<-prostate_data_clean_aug %>% 
  group_by(activity, bone_metastases) %>% 
  summarise(count=n()) %>% 
  pivot_wider(
    names_from = bone_metastases, 
    values_from = count) %>% 
  rename(. ,no_meta="0", meta="1")

#replace NA with 0

activity_bone[is.na(activity_bone)]<-0

#calculate the percentage and plt

activity_bone %>% 
  mutate(no=no_meta/sum(activity_bone$no_meta)*100,
        yes=meta/sum(activity_bone$meta)*100) %>% 
  pivot_longer(-c(activity, meta, no_meta), names_to = "bone_metastases", values_to = "percent") %>% 
  ggplot(aes(activity, percent, fill=bone_metastases))+
  geom_bar(stat = "identity", position = position_dodge())


#BONE METASDTASIS+SERUM_HEMOGLOBIN
#dead from  prostate cancer
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(bone_metastases, serum_hemoglobin,color=dead_from_prostate_cancer))+
  geom_boxplot()+
  theme(legend.position = "bottom")

#stage
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(bone_metastases, serum_hemoglobin,color=stage))+
  geom_boxplot()+
  theme(legend.position = "bottom")

#for the people without bone metastasis, the serum hemo level is about the same regardless of stage, but its not for the bone metastasis. 
#For people with bone metastasis, the serum hemoglobin level increased with cancer stage

#ekg
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(bone_metastases, serum_hemoglobin,color=ekg))+
  geom_boxplot()+
  theme(legend.position = "bottom")

#people without bone metastasis showwed almost similar ekg, but people with bone metastasis werent. 

#age group
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(bone_metastases, serum_hemoglobin,color=Age_group))+
  geom_boxplot()+
  theme(legend.position = "bottom")

#estrogen
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(bone_metastases, serum_hemoglobin,color=estrogen_mg))+
  geom_boxplot()+
  theme(legend.position = "bottom")

#people without bone metastasis showed constant level of serum hemoglobin when treated with different level of estrogen. 
#but for people with bone metastasis showed different level of serume hemoglobin with different estrogen injected. this pattern is irreguar


#BONE METASDTASIS+TUMOR SIZE

prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(bone_metastases, serum_hemoglobin,color=tumor_size))+
  geom_jitter()+
  theme(legend.position = "bottom")

#BONE METASDTASIS+ESTROGEN
#mathias
prostate_data_clean_aug  %>% 
  ggplot(aes(bone_metastases, estrogen_mg, color= status_))+
  geom_jitter()

#TRANSOFORM COUNT BY GROUPED BY BONE METASTASIS

estrogen_metastases_percentage<-prostate_data_clean_aug %>% 
  group_by(bone_metastases, estrogen_mg, status_) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from = bone_metastases,
              values_from = count)  %>%
  rename(. ,no_meta = "0", meta = "1")

estrogen_metastases_percentage
#replace na with 0

estrogen_metastases_percentage[is.na(estrogen_metastases_percentage)] <- 0  

estrogen_metastases_percentage %>% 
  mutate(no=no_meta/sum(estrogen_metastases_percentage$no_meta)*100,
         yes=meta/sum(estrogen_metastases_percentage$meta)*100) %>% 
  pivot_longer(-c(estrogen_mg, status_, meta, no_meta), 
               names_to = "bone_metastases", 
               values_to = "percent") %>% 
  ggplot(aes(estrogen_mg, percent), fill=bone_metastases)+
  geom_bar(stat = "identity", position = position_dodge())+
  facet_wrap(~ status_)

  
#BONE METASDTASIS+EKG (both are categorical)

prostate_data_clean_aug %>% 
  ggplot(ae)
#BONE METASDTASIS+DIASTOLIC BP\
prostate_data_clean_aug %>% 
  ggplot(aes(bone_metastases, systolic_bp), color=status_)+
  geom_jitter()+
  theme(legend.position = "bottom")
#BONE METASDTASIS+AGE GROUP


#WEIGHT+STATUS
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(weight_index, status_), color=dead_from_prostate_cancer)+
  geom_jitter()



#WEIGHT+ACTIVITY

prostate_data_clean_aug %>% 
  ggplot(aes(activity, weight_index), color=status_)+
  geom_jitter()

prostate_data_clean_aug %>% 
  ggplot(aes(activity, weight_index), color=status_)+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=50, hjust=1))


#WEIGHT+SERUM_HEMO
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(serum_hemoglobin,weight_index), color=estrogen_mg)+
  geom_jitter()+
  theme(axis.text.x = element_text(angle=50, hjust=1))

#WEIGHT+ESTROGEN
#ekg
prostate_data_clean_aug %>% 
  ggplot(aes(estrogen_mg,weight_index,color=ekg))+
  geom_boxplot()+
  geom_smooth()+
  theme(legend.position = "bottom")

#activity
prostate_data_clean_aug %>% 
  ggplot(aes(estrogen_mg,weight_index,color=activity))+
  geom_boxplot()+
  geom_smooth()+
  theme(legend.position = "bottom")
#status
#cause of death

#agegroup
#estrogen
#WEIGHT+EKG
#WEIGHT+DIASTOLIC
#WEIGHT+AGEGROUP


#STATUS+ACTIVITY
#STATUS+SERUM_HEMO
#STATUS+TUMOR
#STATUS+ESTROGEN
#STATUS+EKG
#STATUS+DIASTOLIC BP
#STATUS+AGEGROUP


#ACTIVITY+SERUM_HEMO
#ACTIVITY+TUMORSIZE
#ACTIVITY+ESTROGEN
#ACTIVITY+EKG
#ACTIVITY+DIA
#ACTIVITY+AGEGROUP

#SERUM_HEMO+TUMOR
#SERUM_HEMO+ESTROGEN
#SERUM_HEMO+EKG
#SERUM_HEMO+DIASTOLIC
#SERUM_HEMO+AGE GROUP

#TUMOR+ESTROGEN
#TUMOR+EKG
#TUMOR+DIASTOLIC
#TUMOR+AGE GROUP


#EKG+DIASTOLIC
#EKG+AGEGROUP

#DIASTOLIC+AGEGROUP

#stage
#serum_hemo
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(x = fct_reorder(cause_of_death, serum_hemoglobin, .fun = mean, .desc = T), y = serum_hemoglobin, color = stage)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cause of death",
       y = "Serum hemoglobin")+
  theme(legend.position = "bottom")

#serum hemoglobin for stage 4 patients dead from prostate cancer were lower. 
#second lowert serum hemoglobin level for prostate cancer

#lower serum hemoglobin---> cancerous!

#tumor size
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(x = fct_reorder(cause_of_death, tumor_size, .fun = mean, .desc = T), y = tumor_size, color = stage)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cause of death",
       y = "tumor size
       ")+
  theme(legend.position = "bottom")

#prostate cancer has the second highest tumor size, but the stage doesnt really matter


#weight_index
prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(x = fct_reorder(cause_of_death, weight_index, .fun = mean, .desc = T), y = weight_index, color = stage)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cause of death",
       y = "weight index
       ")+
  theme(legend.position = "bottom")
#weight index for stage 4 patients dead from prostate cancer were less
#prostate cancer has the third lowest weight index in general

#what could have made the tumor size bigger for the dead prostate cancer patients?
#analysis on tumor size+...other variables

#TUMOR+ESTROGEN

prostate_data_clean_aug %>% 
  na.omit() %>% 
  group_by(tumor_size, estrogen_mg) %>% 
  ggplot(aes(estrogen_mg,tumor_size, color=stage))+
  geom_jitter()

#math required!!!

#TUMOR+EKG

prostate_data_clean_aug %>% 
  na.omit() %>% 
  group_by(tumor_size, ekg) %>% 
  ggplot(aes(ekg, tumor_size, color=stage))+
  geom_jitter()+
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle=20, hjust=1))
  
#TUMOR+DIASTOLIC
#TUMOR+AGE GROUP
#TUMOR+bone metastasis
#TUMOR+PA
#TUMOR+sySTOLIC
#TUMOR+weight






