## variable: weight_index
#for
names(prostate_data_clean)

#1 
a+geom_jitter(mapping = aes(weight_index, stweight_index))

#2
a+geom_jitter(mapping = aes(weight_index, months_of_follow_up))

#3
a+geom_jitter(mapping = aes(weight_index, weight_index))

#4 good!
a+geom_jitter(mapping = aes(weight_index, activity))

#5
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
a+geom_jitter(mapping = aes(weight_index, stweight_index_grade_index))

#12 pattern!
a+geom_jitter(mapping = aes(weight_index, PA_phosphatase))

#13 pattern! have to extend
a+geom_jitter(mapping = aes(weight_index, bone_metastases))

#14 interesting pattern
a+geom_jitter(mapping = aes(weight_index, sdate))

#15 good
a+geom_jitter(mapping = aes(weight_index, estrogen_mg ))

#16 good
a+geom_jitter(mapping = aes(weight_index, status_))

#17 pattern!
a+geom_jitter(mapping = aes(weight_index, cause_of_death))
