library(tidyverse)
data<-read.csv(file="Levure_test.csv",sep=";")
data_2<-pivot_longer(data,cols=c(3:20))
data_2$name<-str_split(string = data_2$name,pattern = "X",simplify=TRUE)[,2]
data_2$name<-as.numeric(data_2$name)
data_2$value<-as.numeric(data_2$value)

ggplot(data=data_2,aes(name,value,color=Concentration))+
  geom_point()


data_subset_0.01<-data_2[data_2$Concentration==0.01&data_2$name<500,]

ggplot(data=data_subset_0.01,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()


mod<-lm(value~name,data=data_subset_0.01)
anova(mod)

data_subset_0.1<-data_2[data_2$Concentration==0.1&data_2$name<500,]

ggplot(data=data_subset_0.1,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

##METHODE HAMPEL
k= 3 
# calcule la borne inf de l'intervalle binf
binf <- median(data_subset_0.1$value) - k * mad (data_subset_0.1$value) 
binf 
# calcule la borne sup de l'intervalle bsup 
bsup <- median(data_subset_0.1$value) + k * mad (data_subset_0.1$value) 
bsup
outlier_idx <- which(data_subset_0.1$value < binf | data_subset_0.1$value > bsup)
outlier_idx

  outlier_val <- suv[outlier_idx,"value"]
outlier_val

##TEST STAT DE GRUBBS
library(outliers) 
grubbs.test(data_subset_0.1$value, opposite = TRUE)


mod<-lm(value~name,data=data_subset_0.1)
anova(mod)


data_subset_0.4<-data_2[data_2$Concentration==0.4&data_2$name<500,]

ggplot(data=data_subset_0.4,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

colnames(data_2)[colnames(data_2) == "value"] <- "Absorbance"
colnames(data_2)[colnames(data_2) == "name"] <- "Temps"

courbe<-read.csv(file="courbe_etalon.csv",sep=";")
courbe$Absorbance<-as.numeric(courbe$Absorbance)
courbe$Concentration<-as.numeric(courbe$Concentration)

ggplot(data=courbe,aes(Concentration,Absorbance,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE,   # Ajuster une régression linéaire sans intervalle de confiance
              formula = y ~ x - 1)
