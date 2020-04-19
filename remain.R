library(caret)
library(hrbrthemes)
?featurePlot
?par
iris
iris %>% featurePlot(iris[, 1:4],)
#feature plot
featurePlot(iris[, 1:4], iris$Species, "ellipse")



#heatmap

ggplot(numericdata, aes(age, weight_index, fill=bone_metastases))+
  geom_tile()+
  scale_fill_gradient(low="white", high="blue") 

#correlogram



heatmappro <- as.matrix(prostate_data_clean)

# Default Heatmap
irismt<-as.matrix(iris)
heatmap(irismt, scale = "column")

mtcars


#-----------------
head(iris)
is_tibble(prostate_data_clean)

class(prostate_data_clean)
