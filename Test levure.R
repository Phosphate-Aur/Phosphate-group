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


mod<-lm(value~name,data=data_subset_0.1)
anova(mod)


data_subset_0.4<-data_2[data_2$Concentration==0.4&data_2$name<500,]

ggplot(data=data_subset_0.4,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()
