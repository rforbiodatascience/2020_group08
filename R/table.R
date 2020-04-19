rm(list = ls()) 
library(tibble)
variablenames<- prostate_data_clean %>% 
  names() 
variablenames

rowname<- tibble(text=variablenames) 
rowname
checktable<- prostate_data_clean %>% rownames_to_column() %>% 
  slice(1:19) %>% remove
checktable



prostate_data_clean %>% is

?add_row
?filter


table<- tibble("patno" =1:19)  %>%  
  cbind("stage"  =1:19, "dtime"  =1:19, "age" =1:19, 
        "wt" =1:19,"pf"=1:19, "hx"=1:19, "sbp"=1:19, 
        "dbp"=1:19,"ekg"=1:19, "hg" =1:19,  "sz"=1:19,
        "sg" =1:19, "ap"=1:19,"bm"=1:19, "sdate"=1:19,
        "estrogen_mg" =1:19, "status_" =1:19, "cause_of_death"=1:19) %>% 
  select(rownames)


rownames(table) <- variablenames


table
colnames(table) %in% rowname

??replace_if
rownames(table)
a <- rownames(table)
b<- names(table)
b



#-------------------------------
#part 1
#rx

