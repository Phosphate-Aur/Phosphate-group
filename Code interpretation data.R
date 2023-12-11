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

#  outlier_val <- suv[outlier_idx,"value"]
#outlier_val

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
              formula = y ~ x - 1)+
  labs(title = "Calibration curve", x = "Concentration", y = "Absorbance")



# Fit a linear model to the calibration data with intercept set to 0
calibration_model <- lm(Absorbance ~ Concentration - 1, data = courbe)
summary(calibration_model)
#L'équation de notre courbe d'étalonnage est: Absorbance= 13.8327* Concentration

# Generate a sequence of concentrations for prediction
concentration_sequence <- seq(min(courbe$Concentration), max(courbe$Concentration), length.out = 100)

# Create a data frame for prediction
prediction_data <- data.frame(Concentration = concentration_sequence)

# Predict absorbance values using the calibration model
prediction_data$Predicted_Absorbance <- predict(calibration_model, newdata = prediction_data)

# Plot the calibration curve and the predicted trend
plot_calibration <- ggplot(data = courbe, aes(x = Concentration, y = Absorbance, color = Concentration)) +
  geom_point() +
  theme_bw() +
  geom_line(aes(x = Concentration, y = Absorbance), linetype = "dashed", color = "black") +
  labs(title = "Calibration Curve")

# Plot the predicted trend
plot_trend <- ggplot(data = prediction_data, aes(x = Concentration, y = Predicted_Absorbance)) +
  geom_line(color = "red") +
  labs(title = "Predicted Calibration Trend")

# Combine the two plots using patchwork
library(patchwork)
combined_plot <- plot_calibration + plot_trend

# Show the combined plot
print(combined_plot)

# Optional: Save the prediction data to a CSV file
write.csv(prediction_data, "predicted_calibration_trend.csv", row.names = FALSE)

r_squared_value <- summary(calibration_model)$r.squared
# Plot the calibration curve and the predicted trend
library(grid)
library(gridExtra)

plot_calibration <- ggplot(data = courbe, aes(x = Concentration, y = Absorbance, color = Concentration)) +
  geom_point() +
  theme_bw() +
  labs(x = "Concentration (mMol/l)", y = "Absorbance") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x - 1) +
  annotate("text", x = 0.05, y = 1.2, label = sprintf("R-squared: %.4f", r_squared_value), color = "red")

# Create a title grob using textGrob
title_grob <- textGrob("Calibration Curve", gp = gpar(fontsize = 12, fontface = "bold"))

# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_calibration,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)
# Show the plot
print(plot_calibration)


###CONCENTRATION 0.01
colnames(data_subset_0.01)[colnames(data_subset_0.01) == "value"] <- "Absorbance"
colnames(data_subset_0.01)[colnames(data_subset_0.01) == "name"] <- "Temps"

data_subset_0.01$Concentration_2 <- data_subset_0.01$Absorbance / 13.8327

# Print or use the updated data_subset_0.01
print(data_subset_0.01)

plot_0.01 <- ggplot(data=data_subset_0.01,aes(Temps,Concentration_2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium (mMol/l)")

title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.01", gp = gpar(fontsize = 12, fontface = "bold"))

# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.01,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

reg<-lm(Concentration_2~ Temps ,data_subset_0.01 )
summary(reg)

mod<-lm(Concentration_2~Temps,data=data_subset_0.01)
anova(mod)

res <- reg$residuals
bartlett.test(res)
shapiro.test(res)

###CONCENTRATION 0.1
data_subset_0.1<-data_subset_0.1[-nrow(data_subset_0.1), ]
colnames(data_subset_0.1)[colnames(data_subset_0.1) == "value"] <- "Absorbance"
colnames(data_subset_0.1)[colnames(data_subset_0.1) == "name"] <- "Temps"

data_subset_0.1$Concentration_2 <- data_subset_0.1$Absorbance / 13.8327

# Print or use the updated data_subset_0.1
print(data_subset_0.1)

ggplot(data=data_subset_0.1,aes(Temps,Concentration_2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(title = "Phosphate concentration in the medium for an initial concentration of 0.1", x = "Time", y = "Phosphate concentration in the medium")

plot_0.1<-ggplot(data=data_subset_0.1,aes(Temps,Concentration_2/0.2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium (mMol/l)")

title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.1", gp = gpar(fontsize = 12, fontface = "bold"))

# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.1,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

reg<-lm(Concentration_2~ Temps ,data_subset_0.1 )
summary(reg)

mod<-lm(Concentration_2~Temps,data=data_subset_0.1)
anova(mod)

res <- reg$residuals
bartlett.test(res)
shapiro.test(res)

###CONCENTRATION 0.4
data_subset_0.4<-data_subset_0.4[-nrow(data_subset_0.4), ]
data_subset_0.4<-data_subset_0.4[-nrow(data_subset_0.4), ]
data_subset_0.4<-data_subset_0.4[-nrow(data_subset_0.4), ]

colnames(data_subset_0.4)[colnames(data_subset_0.4) == "value"] <- "Absorbance"
colnames(data_subset_0.4)[colnames(data_subset_0.4) == "name"] <- "Temps"

data_subset_0.4$Concentration_2 <- data_subset_0.4$Absorbance / 13.8327

# Print or use the updated data_subset_0.1
print(data_subset_0.4)

plot_0.4<-ggplot(data=data_subset_0.4,aes(Temps,Concentration_2/0.2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium (mMol/l)")

title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.4", gp = gpar(fontsize = 12, fontface = "bold"))

# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.4,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

reg<-lm(Concentration_2~ Temps ,data_subset_0.4 )
summary(reg)

mod<-lm(Concentration_2~Temps,data=data_subset_0.4)
anova(mod)

ggplot(data=data_subset_0.4,aes(Temps,Concentration_2/0.2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(title = "Phosphate concentration in the medium for an initial concentration of 0.1", x = "Time", y = "Phosphate concentration in the medium")

res <- reg$residuals
bartlett.test(res)
shapiro.test(res)

### IMPORTATION HUMICOLA
levure_h<-read.csv(file="Levure_humicola.csv",sep=";")
levure_h<-pivot_longer(levure_h,cols=c(3:25))
levure_h$name<-str_split(string = levure_h$name,pattern = "X",simplify=TRUE)[,2]
levure_h$name<-as.numeric(levure_h$name)
levure_h$value<-as.numeric(levure_h$value)

ggplot(data=levure_h,aes(name,value,color=Concentration))+
  geom_point()


data_h_0.01<-levure_h[levure_h$Concentration==0.01,]

ggplot(data=data_h_0.01,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()


mod<-lm(value~name,data=data_subset_0.01)
anova(mod)

data_h_0.1<-levure_h[levure_h$Concentration==0.1,]

ggplot(data=data_h_0.1,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

data_h_0.4<-levure_h[levure_h$Concentration==0.4,]

ggplot(data=data_h_0.4,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

###CONCENTRATION 0.01
colnames(data_h_0.01)[colnames(data_h_0.01) == "value"] <- "Absorbance"
colnames(data_h_0.01)[colnames(data_h_0.01) == "name"] <- "Temps"

data_h_0.01$Concentration_2 <- data_h_0.01$Absorbance / 13.8327
data_h_0.01<-data_h_0.01[data_h_0.01$Concentration_2<0.08,]
# Print or use the updated data_subset_0.01
print(data_subset_0.01)

plot_0.01 <- ggplot(data=data_h_0.01,aes(Temps,Concentration_2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium (mMol/l)")

title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.01", gp = gpar(fontsize = 12, fontface = "bold"))

# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.01,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

reg<-lm(Concentration_2~ Temps ,data_h_0.01 )
summary(reg)

mod<-lm(Concentration_2~Temps,data=data_h_0.01)
anova(mod)

res <- reg$residuals
bartlett.test(res)
shapiro.test(res)


###CONCENTRATION 0.1
colnames(data_h_0.1)[colnames(data_h_0.1) == "value"] <- "Absorbance"
colnames(data_h_0.1)[colnames(data_h_0.1) == "name"] <- "Temps"

data_h_0.1$Concentration_2 <- data_h_0.1$Absorbance / 13.8327
data_h_0.1<-data_h_0.1[data_h_0.1$Concentration_2<0.05,]

# Print or use the updated data_subset_0.1
print(data_h_0.1)

ggplot(data=data_h_0.1,aes(Temps,Concentration_2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(title = "Phosphate concentration in the medium for an initial concentration of 0.1", x = "Time", y = "Phosphate concentration in the medium")

plot_0.1<-ggplot(data=data_h_0.1,aes(Temps,Concentration_2/0.2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium (mMol/l)")

title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.1", gp = gpar(fontsize = 12, fontface = "bold"))

# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.1,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

reg<-lm(Concentration_2~ Temps ,data_h_0.1 )
summary(reg)

mod<-lm(Concentration_2~Temps,data=data_h_0.1)
anova(mod)

res <- reg$residuals
bartlett.test(res)
shapiro.test(res)

###CONCENTRATION 0.4
colnames(data_h_0.4)[colnames(data_h_0.4) == "value"] <- "Absorbance"
colnames(data_h_0.4)[colnames(data_h_0.4) == "name"] <- "Temps"

data_h_0.4$Concentration_2 <- data_h_0.4$Absorbance / 13.8327

# Print or use the updated data_subset_0.1
print(data_h_0.4)

plot_0.4<-ggplot(data=data_h_0.4,aes(Temps,Concentration_2/0.2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium (mMol/l)")

title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.4", gp = gpar(fontsize = 12, fontface = "bold"))

# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.4,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

reg<-lm(Concentration_2~ Temps ,data_h_0.4 )
summary(reg)

mod<-lm(Concentration_2~Temps,data=data_h_0.4)
anova(mod)

ggplot(data=data_h_0.4,aes(Temps,Concentration_2/0.2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(title = "Phosphate concentration in the medium for an initial concentration of 0.1", x = "Time", y = "Phosphate concentration in the medium")

res <- reg$residuals
bartlett.test(res)
shapiro.test(res)
