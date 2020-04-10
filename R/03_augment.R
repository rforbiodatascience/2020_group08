
#Shewa has stage,age, months of followup and weight_index.
install.packages("ggpubr")
library(ggpubr)
#For simplifying codes, saved the ggplot part.
a<- ggplot(data = prostate_data_clean)
#-----------------------------------------------------------------------
#for
## Variable: stage
names(prostate_data_clean)
#1 
max(prostate_data_clean$stage)
min(prostate_data_clean$stage)

a+geom_jitter(mapping = aes(stage,age))+
  xlim(3, 4)



?labs
#2
a+geom_jitter(mapping = aes(stage, months_of_follow_up))

#3
a+geom_jitter(mapping = aes(stage, weight_index))

#4 good!

  a+geom_jitter(mapping = aes(activity, stage, color=months_of_follow_up))
  
  ?spread
  a+geom_jitter(mapping = aes(activity, stage, color=weight_index))
  

a142<-a+geom_col(mapping = aes(activity, stage))
stageactivity<-ggarrange(a141, a142)
stageactivity

#5
?geom_jitter
a+geom_jitter(mapping = aes(stage, history_of_CD, color=activity))
a+geom_jitter(mapping = aes(stage, history_of_CD, color=serum_hemoglobin))

a151<-a+geom_col(mapping = aes(stage, history_of_CD))

    #discrete x,y
a152<-a+geom_count(mapping = aes(stage, history_of_CD)) 

#6 good
 
#interesting patterns
a163<-a+geom_jitter(mapping = aes(stage, systolic_bp, color=diastolic_bp))
a164<-a+geom_jitter(mapping = aes(stage, systolic_bp, color=serum_hemoglobin))
a165<-a+geom_jitter(mapping = aes(stage, systolic_bp, color=stage_grade_index))
a166<-a+geom_jitter(mapping = aes(stage, systolic_bp, color=bone_metastases))
a167<-a+geom_jitter(mapping = aes(stage, systolic_bp, color=cause_of_death))

#trials-----------
a+geom_jitter(mapping = aes(stage, systolic_bp, color=months_of_follow_up))
a+geom_jitter(mapping = aes(stage, systolic_bp, color=age))
a+geom_jitter(mapping = aes(stage, systolic_bp, color=weight_index))
a+geom_jitter(mapping = aes(stage, systolic_bp, color=activity))

a+geom_jitter(mapping = aes(stage, systolic_bp, color=history_of_CD))
a+geom_jitter(mapping = aes(stage, systolic_bp, color=PA_phosphatase))
a+geom_jitter(mapping = aes(stage, systolic_bp, color=sdate))
a+geom_jitter(mapping = aes(stage, systolic_bp, color=estrogen_mg))

a+geom_jitter(mapping = aes(stage, systolic_bp, color=status_))
a+geom_jitter(mapping = aes(stage, systolic_bp, color=tumor_size))
a+geom_jitter(mapping = aes(stage, systolic_bp, color=ekg))
a+geom_jitter(mapping = aes(stage, systolic_bp, color=sdate))



a+geom_boxplot(mapping = aes(stage, systolic_bp))
a+geom_violin(mapping = aes(stage, systolic_bp))
a+geom_col(mapping = aes(stage, systolic_bp))

#7 good
sdate
a+geom_jitter(mapping = aes(stage, diastolic_bp))+
  geom_smooth(mapping = aes(stage, diastolic_bp))

#interestsing pattern

a171<-a+geom_jitter(mapping = aes(stage, diastolic_bp, color=bone_metastases))
a172<-a+geom_jitter(mapping = aes(stage, diastolic_bp, color=cause_of_death))
a173<-a+geom_jitter(mapping = aes(stage, diastolic_bp, color=history_of_CD))
a174<-a+geom_jitter(mapping = aes(stage, diastolic_bp, color=PA_phosphatase))
a175<-a+geom_jitter(mapping = aes(stage, diastolic_bp, color=activity))

#trials------

a+geom_jitter(mapping = aes(stage, diastolic_bp, color=sdate))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=estrogen_mg))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=status_))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=tumor_size))

a+geom_jitter(mapping = aes(stage, diastolic_bp, color=ekg))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=months_of_follow_up))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=age))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=weight_index))

a+geom_jitter(mapping = aes(stage, diastolic_bp, color=activity))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=systolic_bp))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=stage_grade_index))
a+geom_jitter(mapping = aes(stage, diastolic_bp, color=sdate))

#------------
a+geom_boxplot(mapping = aes(stage, diastolic_bp))
a+geom_violin(mapping = aes(stage, diastolic_bp))
a+geom_col(mapping = aes(stage, diastolic_bp))

#8
a+geom_jitter(mapping = aes(stage, ekg))

#9 abit...
a+geom_jitter(mapping = aes(stage, serum_hemoglobin))+
  geom_smooth(mapping = aes(stage, serum_hemoglobin))

a191<-a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=bone_metastases))
a192<-a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=cause_of_death))
a196<-a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=sdate))
a195<-a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=PA_phosphatase))
a194<-a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=activity))


a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=sdate))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=estrogen_mg))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=status_))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=tumor_size))

a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=ekg))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=months_of_follow_up))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=age))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=weight_index))

a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=activity))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=systolic_bp))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=diastolic_bp))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=stage_grade_index))
a+geom_jitter(mapping = aes(stage, serum_hemoglobin, color=history_of_CD))


#a+geom_col(mapping = aes(stage, serum_hemoglobin))
a+geom_violin(mapping = aes(stage, serum_hemoglobin))

a+geom_boxplot(mapping = aes(stage, serum_hemoglobin))

#10 pattern!
a+geom_jitter(mapping = aes(stage, tumor_size))+
  geom_smooth(mapping = aes(stage, tumor_size))

a1101<-a+geom_jitter(mapping = aes(stage, tumor_size, color=bone_metastases))
a1102<-a+geom_jitter(mapping = aes(stage, tumor_size, color=sdate))
a1103<-a+geom_jitter(mapping = aes(stage, tumor_size, color=history_of_CD))

a+geom_jitter(mapping = aes(stage, tumor_size, color=cause_of_death))
a+geom_jitter(mapping = aes(stage, tumor_size, color=PA_phosphatase))
a+geom_jitter(mapping = aes(stage, tumor_size, color=activity))
a+geom_jitter(mapping = aes(stage, tumor_size, color=sdate))
a+geom_jitter(mapping = aes(stage, tumor_size, color=estrogen_mg))
a+geom_jitter(mapping = aes(stage, tumor_size, color=status_))
a+geom_jitter(mapping = aes(stage, tumor_size, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(stage, tumor_size, color=ekg))
a+geom_jitter(mapping = aes(stage, tumor_size, color=months_of_follow_up))
a+geom_jitter(mapping = aes(stage, tumor_size, color=age))
a+geom_jitter(mapping = aes(stage, tumor_size, color=weight_index))
a+geom_jitter(mapping = aes(stage, tumor_size, color=activity))
a+geom_jitter(mapping = aes(stage, tumor_size, color=systolic_bp))
a+geom_jitter(mapping = aes(stage, tumor_size, color=diastolic_bp))
a+geom_jitter(mapping = aes(stage, tumor_size, color=stage_grade_index))


#11 pattern!
a+geom_jitter(mapping = aes(stage, stage_grade_index))+
  geom_smooth(mapping = aes(stage, stage_grade_index))

a1111<-a+geom_jitter(mapping = aes(stage, stage_grade_index, color=bone_metastases))
a1112<-a+geom_jitter(mapping = aes(stage, stage_grade_index, color=sdate))
a1113<-a+geom_jitter(mapping = aes(stage, stage_grade_index, color=history_of_CD))
a1114<-a+geom_jitter(mapping = aes(stage, stage_grade_index, color=cause_of_death))

a1115<-a+geom_jitter(mapping = aes(stage, stage_grade_index, color=PA_phosphatase))+
  geom_smooth(mapping = aes(stage, stage_grade_index))

a1116<-a+geom_jitter(mapping = aes(stage, stage_grade_index, color=activity))
a1117<-a+geom_jitter(mapping = aes(stage, stage_grade_index, color=sdate))

a1118<-a+geom_jitter(mapping = aes(stage, stage_grade_index, color=estrogen_mg))+
  geom_smooth(mapping = aes(stage, stage_grade_index))
a1109<-a+geom_jitter(mapping = aes(stage, stage_grade_index, color=status_))


#general increase in  stage grade index as stage increases. 
a+geom_jitter(mapping = aes(stage, stage_grade_index, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(stage, stage_grade_index, color=ekg))
a+geom_jitter(mapping = aes(stage, stage_grade_index, color=months_of_follow_up))
a+geom_jitter(mapping = aes(stage, stage_grade_index, color=age))
a+geom_jitter(mapping = aes(stage, stage_grade_index, color=weight_index))

a+geom_jitter(mapping = aes(stage, stage_grade_index, color=activity))
a+geom_jitter(mapping = aes(stage, stage_grade_index, color=systolic_bp))
a+geom_jitter(mapping = aes(stage, stage_grade_index, color=diastolic_bp))
a+geom_jitter(mapping = aes(stage, stage_grade_index, color=tumor_size))



#12 pattern! #more spread in stage 4
a+geom_jitter(mapping = aes(stage, PA_phosphatase))+
  ylim(0, 100)

a1121<-a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=bone_metastases))+
  ylim(0, 100)
a1122<-a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=stage_grade_index))+
  ylim(0, 100)
a1123<-a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=activity))+
  ylim(0, 100)


a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=sdate))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=history_of_CD))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=cause_of_death))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=tumor_size))+
  ylim(0, 100)


a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=sdate))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=estrogen_mg))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=status_))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=serum_hemoglobin))+
  ylim(0, 100)

a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=ekg))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=months_of_follow_up))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=age))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=weight_index))+
  ylim(0, 100)


a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=activity))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=systolic_bp))+
  ylim(0, 100)
a+geom_jitter(mapping = aes(stage, PA_phosphatase, color=diastolic_bp))+
  ylim(0, 100)

#13 pattern!
# more bone metastasis in  stage 4
a+geom_jitter(mapping = aes(stage, bone_metastases))

a1131<-a+geom_jitter(mapping = aes(stage, bone_metastases, color=tumor_size))
a1132<-a+geom_jitter(mapping = aes(stage, bone_metastases, color=history_of_CD))
a1133<-a+geom_jitter(mapping = aes(stage, bone_metastases, color=serum_hemoglobin))
a1134<-a+geom_jitter(mapping = aes(stage, bone_metastases, color=stage_grade_index))

a+geom_jitter(mapping = aes(stage, bone_metastases, color=sdate))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=cause_of_death))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=PA_phosphatase))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=activity))

a+geom_jitter(mapping = aes(stage, bone_metastases, color=sdate))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=estrogen_mg))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=status_))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=ekg))

a+geom_jitter(mapping = aes(stage, bone_metastases, color=months_of_follow_up))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=age))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=weight_index))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=systolic_bp))
a+geom_jitter(mapping = aes(stage, bone_metastases, color=diastolic_bp))



#14
a+geom_jitter(mapping = aes(stage, sdate))

#15 pattern!
a+geom_jitter(mapping = aes(stage, estrogen_mg ))
a+geom_(mapping = aes(stage, estrogen_mg ))

a1151<-a+geom_jitter(mapping = aes(stage, estrogen_mg, color=tumor_size))
a1152<-a+geom_jitter(mapping = aes(stage, estrogen_mg, color=stage_grade_index))
a1153<-a+geom_jitter(mapping = aes(stage, estrogen_mg, color=activity))
a1154<-a+geom_jitter(mapping = aes(stage, estrogen_mg, color=status_))

a+geom_jitter(mapping = aes(stage, estrogen_mg, color=history_of_CD))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=sdate))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=cause_of_death))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=PA_phosphatase))


a+geom_jitter(mapping = aes(stage, estrogen_mg, color=sdate))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=bone_metastases))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=ekg))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=months_of_follow_up))

a+geom_jitter(mapping = aes(stage, estrogen_mg, color=age))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=weight_index))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=systolic_bp))
a+geom_jitter(mapping = aes(stage, estrogen_mg, color=diastolic_bp))

#16
a+geom_jitter(mapping = aes(stage, status_))

#17 pattern! ????
a+geom_jitter(mapping = aes(stage, cause_of_death))
a+geom_col(mapping = aes(stage, cause_of_death))
a+geom_line(mapping = aes(stage, cause_of_death))

#---------------------------------------------------------------
## Variable2: 	Months of Follow-up

#for
names(prostate_data_clean)

#1 
a+geom_jitter(mapping = aes(months_of_follow_up, stage))


#2
a+geom_jitter(mapping = aes(months_of_follow_up, age))

#3
a+geom_jitter(mapping = aes(months_of_follow_up, weight_index))

#4 good!
#mostly concentrated in the normal activity
a+geom_jitter(mapping = aes(months_of_follow_up, activity))

a241<-a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=status_))
a242<-a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=cause_of_death))

a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=stage))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=serum_hemoglobin))


a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=sdate))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=bone_metastases))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=ekg))


a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=systolic_bp))
a+geom_jitter(mapping = aes(months_of_follow_up, activity, color=diastolic_bp))


#5
a+geom_jitter(mapping = aes(months_of_follow_up, history_of_CD))

#6 good
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp))

a261<-a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=status_))
a262<-a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=cause_of_death))
a263<-a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=age))

a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=stage))
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=serum_hemoglobin))


a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=sdate))
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=bone_metastases))
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=ekg))



a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, systolic_bp, color=diastolic_bp))

#7 good
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp))


a271<-a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=status_))
a272<-a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=cause_of_death))

a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=stage))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=serum_hemoglobin))


a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=sdate))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=bone_metastases))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=ekg))


a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, diastolic_bp, color=systolic_bp))

#8 good
a+geom_jitter(mapping = aes(months_of_follow_up, ekg))

a281<-a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=status_))
a282<-a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=cause_of_death))

a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=stage))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=serum_hemoglobin))


a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=sdate))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=bone_metastases))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=diastolic_bp))


a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, ekg, color=systolic_bp))


#9 good
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin))


a291<-a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=status_))
a292<-a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=cause_of_death))

a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=stage))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=ekg))


a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=sdate))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=bone_metastases))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=diastolic_bp))


a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, serum_hemoglobin, color=systolic_bp))


#10 pattern!
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size))


a2101<-a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=status_))
a2102<-a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=cause_of_death))

a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=stage))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=ekg))


a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=sdate))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=bone_metastases))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=diastolic_bp))


a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, tumor_size, color=systolic_bp))

#11 pattern!
a+geom_jitter(mapping = aes(months_of_follow_up,stage_grade_index))

a2111<-a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=status_))
a2112<-a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=cause_of_death))

a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=serum_hemoglobin))
a2113<-a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=stage))

a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=ekg))


a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=sdate))
a2114<-a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=bone_metastases))
a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=diastolic_bp))


a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, stage_grade_index, color=systolic_bp))



#12 pattern!have to extend
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase))

a2121<-a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=status_))
a2122<-a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=cause_of_death))

a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=stage))

a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=ekg))


a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=sdate))
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=bone_metastases))+
  ylim(0,10)
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=diastolic_bp))


a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, PA_phosphatase, color=systolic_bp))




#13 pattern! 
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases))

a2131<-a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=status_))
a2132<-a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=cause_of_death))
a2133<-a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=stage))

a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=serum_hemoglobin))


a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=ekg))
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=sdate))

a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=diastolic_bp))
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, bone_metastases, color=systolic_bp))



#14 interesting pattern
a+geom_jitter(mapping = aes(months_of_follow_up, sdate))

a2141<-a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=status_))
a2142<-a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=cause_of_death))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=stage))

a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=serum_hemoglobin))


a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=ekg))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=bone_metastases))

a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=diastolic_bp))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, sdate, color=systolic_bp))

#15 good
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg ))


a2151<-a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=status_))
a2152<-a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=cause_of_death))+
  theme(legend.position = "bottom")
  
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=stage))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=serum_hemoglobin))


a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=ekg))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=bone_metastases))

a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=diastolic_bp))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color=systolic_bp))
a+geom_jitter(mapping = aes(months_of_follow_up, estrogen_mg, color= sdate ))

#16 good
a+geom_jitter(mapping = aes(months_of_follow_up, status_))

a2161<-a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=cause_of_death))+
  theme(legend.position = "bottom")
a2162<-a+geom_jitter(mapping = aes(months_of_follow_up, status_, color= sdate ))

a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=estrogen_mg))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=stage))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=serum_hemoglobin))


a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=history_of_CD))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=ekg))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=stage_grade_index))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=bone_metastases))

a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=diastolic_bp))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, status_, color=systolic_bp))

#17 pattern!
a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death))

a2171<-a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=status_))+
  theme(legend.position = "bottom")
a2172<-a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color= sdate ))

a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=estrogen_mg))
a2173<-a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=stage))

a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=age))
a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=serum_hemoglobin))


a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=tumor_size))
a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=history_of_CD))
a2174<-a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=ekg))
a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=stage_grade_index))
a2175<-a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=bone_metastases))

a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=PA_phosphatase))
a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=diastolic_bp))
a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=weight_index))
a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=activity))
a+geom_jitter(mapping = aes(months_of_follow_up, cause_of_death, color=systolic_bp))





a+geom_col(mapping = aes(months_of_follow_up, cause_of_death))

#---------------------------------------------------------------
## variable3: age
#for
names(prostate_data_clean)

#1 some
a+geom_jitter(mapping = aes(age, stage))



a311<-a+geom_jitter(mapping = aes(age, stage, color=estrogen_mg))
a312<-a+geom_jitter(mapping = aes(age, stage, color=stage_grade_index))
a313<-a+geom_jitter(mapping = aes(age, stage, color=bone_metastases))



a+geom_jitter(mapping = aes(age, stage, color=months_of_follow_up))
a+geom_jitter(mapping = aes(age, stage, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(age, stage, color=status_))
a+geom_jitter(mapping = aes(age, stage, color= sdate ))

a+geom_jitter(mapping = aes(age, stage, color=tumor_size))
a+geom_jitter(mapping = aes(age, stage, color=history_of_CD))
a+geom_jitter(mapping = aes(age, stage, color=ekg))
a+geom_jitter(mapping = aes(age, stage, color=cause_of_death))+
  theme(legend.position = "bottom")

a+geom_jitter(mapping = aes(age, stage, color=PA_phosphatase))
a+geom_jitter(mapping = aes(age, stage, color=diastolic_bp))
a+geom_jitter(mapping = aes(age, stage, color=weight_index))
a+geom_jitter(mapping = aes(age, stage, color=activity))
a+geom_jitter(mapping = aes(age, stage, color=systolic_bp))


#2 lots of 70s and 80s patients
a+geom_jitter(mapping = aes(age, months_of_follow_up))

a+geom_jitter(mapping = aes(age, months_of_follow_up, color=status_))+
  facet_grid(~status_)

a321<-a+geom_jitter(mapping = aes(age, months_of_follow_up, color=status_))
a322<-a+geom_jitter(mapping = aes(age, months_of_follow_up, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color= sdate ))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=estrogen_mg))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=cause_of_death))



a+geom_jitter(mapping = aes(age, months_of_follow_up, color=tumor_size))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=history_of_CD))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=ekg))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=stage_grade_index))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=bone_metastases))

a+geom_jitter(mapping = aes(age, months_of_follow_up, color=PA_phosphatase))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=diastolic_bp))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=weight_index))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=activity))
a+geom_jitter(mapping = aes(age, months_of_follow_up, color=systolic_bp))


#3some
a+geom_jitter(mapping = aes(age, weight_index))

a+geom_jitter(mapping = aes(age,weight_index, color=status_))
a+geom_jitter(mapping = aes(age,weight_index, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(age,weight_index, color= sdate ))
a+geom_jitter(mapping = aes(age,weight_index, color=estrogen_mg))
a+geom_jitter(mapping = aes(age,weight_index, color=cause_of_death))



a+geom_jitter(mapping = aes(age,weight_index, color=tumor_size))
a+geom_jitter(mapping = aes(age,weight_index, color=history_of_CD))
a+geom_jitter(mapping = aes(age,weight_index, color=ekg))
a+geom_jitter(mapping = aes(age,weight_index, color=stage_grade_index))
a+geom_jitter(mapping = aes(age,weight_index, color=bone_metastases))

a+geom_jitter(mapping = aes(age,weight_index, color=PA_phosphatase))+
  facet_grid(~PA_phosphatase)
a+geom_jitter(mapping = aes(age,weight_index, color=diastolic_bp))
a+geom_jitter(mapping = aes(age,weight_index, color=months_of_follow_up))
a+geom_jitter(mapping = aes(age,weight_index, color=activity))
a+geom_jitter(mapping = aes(age,weight_index, color=systolic_bp))


#4 good!
a+geom_jitter(mapping = aes(age, activity))

a341<-a+geom_jitter(mapping = aes(age,activity, color=status_))
a342<-a+geom_jitter(mapping = aes(age,activity, color=bone_metastases))

a+geom_jitter(mapping = aes(age,activity, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(age,activity, color= sdate ))
a+geom_jitter(mapping = aes(age,activity, color=estrogen_mg))
a+geom_jitter(mapping = aes(age,activity, color=cause_of_death))



a+geom_jitter(mapping = aes(age,activity, color=tumor_size))
a+geom_jitter(mapping = aes(age,activity, color=history_of_CD))
a+geom_jitter(mapping = aes(age,activity, color=ekg))
a+geom_jitter(mapping = aes(age,activity, color=stage_grade_index))


a+geom_jitter(mapping = aes(age,activity, color=PA_phosphatase))
a+geom_jitter(mapping = aes(age,activity, color=diastolic_bp))
a+geom_jitter(mapping = aes(age,activity, color=months_of_follow_up))
a+geom_jitter(mapping = aes(age,activity, color=weight_index))
a+geom_jitter(mapping = aes(age,activity, color=systolic_bp))


#5
a+geom_jitter(mapping = aes(age, history_of_CD))

#6 good
a+geom_jitter(mapping = aes(age, systolic_bp))

a+geom_jitter(mapping = aes(age,systolic_bp, color=status_))
a+geom_jitter(mapping = aes(age,systolic_bp, color=bone_metastases))

a+geom_jitter(mapping = aes(age,systolic_bp, color=serum_hemoglobin))
a+geom_jitter(mapping = aes(age,systolic_bp, color= sdate ))
a+geom_jitter(mapping = aes(age,systolic_bp, color=estrogen_mg))
a+geom_jitter(mapping = aes(age,systolic_bp, color=cause_of_death))


a+geom_jitter(mapping = aes(age,systolic_bp, color=tumor_size))
a+geom_jitter(mapping = aes(age,systolic_bp, color=history_of_CD))
a+geom_jitter(mapping = aes(age,systolic_bp, color=ekg))
a+geom_jitter(mapping = aes(age,systolic_bp, color=stage_grade_index))


a+geom_jitter(mapping = aes(age,systolic_bp, color=PA_phosphatase))
a+geom_jitter(mapping = aes(age,systolic_bp, color=diastolic_bp))
a+geom_jitter(mapping = aes(age,systolic_bp, color=months_of_follow_up))
a+geom_jitter(mapping = aes(age,systolic_bp, color=weight_index))
a+geom_jitter(mapping = aes(age,systolic_bp, color=activity))

#7 good
a+geom_jitter(mapping = aes(age, ekg))

#8 good
a+geom_jitter(mapping = aes(age, diastolic_bp))

#9 some
a+geom_jitter(mapping = aes(age, serum_hemoglobin))

#10 pattern!
a+geom_jitter(mapping = aes(age, tumor_size))

#11 pattern!
a+geom_jitter(mapping = aes(age, stage_grade_index))

#12 pattern! has to extend
a+geom_jitter(mapping = aes(age, PA_phosphatase))

#13 pattern! 
a+geom_jitter(mapping = aes(age, bone_metastases))

#14 
a+geom_jitter(mapping = aes(age, sdate))

#15 good
a+geom_jitter(mapping = aes(age, status_ ))

#16 some?
a+geom_jitter(mapping = aes(age, status_))

#17 pattern!
a+geom_jitter(mapping = aes(age, cause_of_death))


#---------------------------------------------------------------
## variable: weight_index
#for
names(prostate_data_clean)

#1 some
a+geom_jitter(mapping = aes(weight_index, stage))




#2
a+geom_jitter(mapping = aes(weight_index, months_of_follow_up))

#3 some
a+geom_jitter(mapping = aes(weight_index, age))

#4 good!
a+geom_jitter(mapping = aes(weight_index, systolic_bp))

#5some
a+geom_jitter(mapping = aes(weight_index, history_of_CD))

#6 good
a+geom_jitter(mapping = aes(weight_index, systolic_bp))

#7 good
a+geom_jitter(mapping = aes(weight_index, diastolic_bp))

#8 good
a+geom_jitter(mapping = aes(weight_index, ekg))

#9 good
a+geom_jitter(mapping = aes(weight_index, serum_hemoglobin))

#10 pattern!
a+geom_jitter(mapping = aes(weight_index, tumor_size))

#11 pattern!
a+geom_jitter(mapping = aes(weight_index, stage_grade_index))

#12 pattern!
a+geom_jitter(mapping = aes(weight_index, PA_phosphatase))

#13 pattern! have to extend
a+geom_jitter(mapping = aes(weight_index, bone_metastases))

#14 interesting pattern
a+geom_jitter(mapping = aes(weight_index, sdate))

#15
a+geom_jitter(mapping = aes(weight_index, estrogen_mg ))

#16 good
a+geom_jitter(mapping = aes(weight_index, status_))

#17 pattern!
a+geom_jitter(mapping = aes(weight_index, cause_of_death))


