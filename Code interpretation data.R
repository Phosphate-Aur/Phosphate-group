library(tidyverse)

## CALIBRATION CURVE ##
#Importation and changment of the data 
courbe<-read.csv(file="courbe_etalon.csv",sep=";")
courbe$Absorbance<-as.numeric(courbe$Absorbance)
courbe$Concentration<-as.numeric(courbe$Concentration)


# Fit a linear model to the calibration data with intercept set to 0
calibration_model <- lm(Absorbance ~ Concentration - 1, data = courbe)
summary(calibration_model)
#**The equation for our calibration curve is:* Absorbance= 13.8327* Concentration

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



## FOR CEREVISAE ##
# Data importation and transformation
data<-read.csv(file="Levure_test.csv",sep=";")
data_2<-pivot_longer(data,cols=c(3:20))
data_2$name<-str_split(string = data_2$name,pattern = "X",simplify=TRUE)[,2]
data_2$name<-as.numeric(data_2$name)
data_2$value<-as.numeric(data_2$value)

#plot of all the data
ggplot(data=data_2,aes(name,value,color=Concentration))+
  geom_point()

#plot of the data for the concentration 0.01
data_subset_0.01<-data_2[data_2$Concentration==0.01&data_2$name<500,]
ggplot(data=data_subset_0.01,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

#plot of the data for the concentration 0.1
data_subset_0.1<-data_2[data_2$Concentration==0.1&data_2$name<500,]
ggplot(data=data_subset_0.1,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

#plot of the data for the concentration 0.4
data_subset_0.4<-data_2[data_2$Concentration==0.4&data_2$name<500,]
ggplot(data=data_subset_0.4,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

#Changement of the names of the data set
colnames(data_2)[colnames(data_2) == "value"] <- "Absorbance"
colnames(data_2)[colnames(data_2) == "name"] <- "Temps"


##**CONCENTRATION 0.01*##
#Modification of the names
colnames(data_subset_0.01)[colnames(data_subset_0.01) == "value"] <- "Absorbance"
colnames(data_subset_0.01)[colnames(data_subset_0.01) == "name"] <- "Temps"

#Find the concentration corresponding to the absorbance thanks to the equation of the calibration curve
data_subset_0.01$Concentration_2 <- data_subset_0.01$Absorbance / 13.8327

# Print or use the updated data_subset_0.01
print(data_subset_0.01)

#Print the Phosphate concentration in the medium for an initial concentration of 0.01 as in a fonction of time
plot_0.01 <- ggplot(data=data_subset_0.01, aes(Temps, Concentration_2)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method="lm", se=FALSE) +  # Utilization of the method="lm" to fit a straight line
  labs(x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")
title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.01", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.01,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_subset_0.01)
# Extract the slope coefficient from the linear model summary
slope <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope)
r_squared_value <- summary(lm_model)$r.squared
#Print the Phosphate concentration in the medium for an initial concentration of 0.01 as in a fonction of time with the function ln
plot_0.01 <- ggplot(data=data_subset_0.01, aes(Temps, Concentration_2)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(limits=c(0.001,0.4), trans="log10") +
  geom_smooth(method="lm", se=FALSE) +  # Utilization of the method="lm" to fit a straight line
  labs(x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)") +
  annotate("text", x = 10, y = 0.001, label = sprintf("Slope: %s", slope), color = "blue", hjust=0)+
  annotate("text", x = 100, y = 0.002, label = sprintf("R-squared: %.4f", r_squared_value), color = "black")


title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.01", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.01,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

#Statistic for the initial concentration of 0.01
reg<-lm(log(Concentration_2)~ Temps ,data_subset_0.01 )
summary(reg)
mod<-lm(log(Concentration_2)~Temps,data=data_subset_0.01)
anova(mod)
#Study of the residuals
res <- reg$residuals
#bartlett.test(res)
shapiro.test(res)


### WE TEST THE SECOND HYPOTHESIS 
data_subset_0.01_bis <- data_subset_0.01[-1,]
# Create two subsets of data for times less than and greater than or equal to 200 minutes
data_subset_0.01_before_100 <- subset(data_subset_0.01_bis, Temps <= 150)
data_subset_0.01_after_100 <- subset(data_subset_0.01_bis, Temps >= 100)

# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_subset_0.01_before_100)
# Extract the slope coefficient from the linear model summary
slope1 <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope1)
r_squared_value1 <- summary(lm_model)$r.squared

# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_subset_0.01_after_100)
# Extract the slope coefficient from the linear model summary
slope2 <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope2)
r_squared_value2 <- summary(lm_model)$r.squared


# Create a ggplot with both subsets
plot_0.01_combined <- ggplot(data = data_subset_0.01_bis, aes(Temps, Concentration_2)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(limits=c(0.001,0.4), trans="log10") +
  geom_smooth(data = data_subset_0.01_before_100, method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = data_subset_0.01_after_100, method = "lm", se = FALSE, color = "red") +
  labs(x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")+
  annotate("text", x = 10, y = 0.001, label = sprintf("Slope: %s", slope1), color = "blue", hjust=0)+
  annotate("text", x = 200, y = 0.001, label = sprintf("Slope: %s", slope2), color = "red", hjust=0)+
  annotate("text", x = 100, y = 0.002, label = sprintf("R-squared: %.4f", r_squared_value1), color = "black")+
  annotate("text", x = 300, y = 0.002, label = sprintf("R-squared: %.4f", r_squared_value2), color = "black")


# Title for the combined plot
title_grob <- textGrob("Phosphate concentration in the medium for an initial concentration of 0.1", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.01_combined,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plot and title
)



##**CONCENTRATION 0.1*##
#Remove outlayers and Modification of the names
colnames(data_subset_0.1)[colnames(data_subset_0.1) == "value"] <- "Absorbance"
colnames(data_subset_0.1)[colnames(data_subset_0.1) == "name"] <- "Temps"

#Find the concentration corresponding to the absorbance thanks to the equation of the calibration curve
data_subset_0.1$Concentration_2 <- data_subset_0.1$Absorbance / 13.8327


outlayer_S_0.1 <- data_subset_0.1 %>%
  filter(Concentration_2 < 0.039, Temps < 1000, !(Temps == 328.33 & Concentration_2 > 0.018))
data_subset_0.1 <- subset(data_subset_0.1, Concentration_2 < 0.039 & Temps < 1000 & !(Temps == 328.33 & Concentration_2 > 0.018))



# Print or use the updated data_subset_0.1
print(data_subset_0.1)

#Print the Phosphate concentration in the medium for an initial concentration of 0.1 as in a fonction of time
ggplot(data=data_subset_0.1,aes(Temps,Concentration_2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(title = "Phosphate concentration in the medium for an initial concentration of 0.1", x = "Time", y = "Phosphate concentration in the medium")

#Print the same graph taking into account the dilution
plot_0.1<-ggplot(data=data_subset_0.1,aes(Temps,Concentration_2/0.2))+
  geom_point()+
  theme_bw()+
  geom_smooth() +  
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium (mMol/l)")
title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.1", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.1,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

# Create two subsets of data for times less than and greater than or equal to 200 minutes
data_subset_0.1_before_200 <- subset(data_subset_0.1, Temps <= 200)
data_subset_0.1_after_200 <- subset(data_subset_0.1, Temps >= 190)

#data_subset_0.1$Concentration_2 <- log(data_subset_0.1$Concentration_2)
# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_subset_0.1_before_200)
# Extract the slope coefficient from the linear model summary
slope1 <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope1)
r_squared_value1 <- summary(lm_model)$r.squared

# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_subset_0.1_after_200)
# Extract the slope coefficient from the linear model summary
slope2 <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope2)
r_squared_value2 <- summary(lm_model)$r.squared


# Create a ggplot with both subsets
plot_0.1_combined <- ggplot(data = data_subset_0.1, aes(Temps, Concentration_2/0.2)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(limits=c(0.001,0.4), trans="log10") +
  geom_smooth(data = data_subset_0.1_before_200, method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = data_subset_0.1_after_200, method = "lm", se = FALSE, color = "red") +
  labs(x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")+
  annotate("text", x = 10, y = 0.001, label = sprintf("Slope: %s", slope1), color = "blue", hjust=0)+
  annotate("text", x = 200, y = 0.001, label = sprintf("Slope: %s", slope2), color = "red", hjust=0)+
  annotate("text", x = 100, y = 0.002, label = sprintf("R-squared: %.4f", r_squared_value1), color = "black")+
  annotate("text", x = 300, y = 0.002, label = sprintf("R-squared: %.4f", r_squared_value2), color = "black")


# Title for the combined plot
title_grob <- textGrob("Phosphate concentration in the medium for an initial concentration of 0.1", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.1_combined,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plot and title
)


#Statistic for the initial concentration of 0.1
reg<-lm(log(Concentration_2)~ Temps ,data_subset_0.1_before_200 )
summary(reg)
mod<-lm(log(Concentration_2)~Temps,data=data_subset_0.1_before_200)
anova(mod)
#Residuals study
res <- reg$residuals
#bartlett.test(res)
shapiro.test(res)

#Statistic for the initial concentration of 0.1
reg<-lm(log(Concentration_2)~ Temps ,data_subset_0.1_after_200 )
summary(reg)
mod<-lm(log(Concentration_2)~Temps,data=data_subset_0.1_after_200)
anova(mod)
#Residuals study
res <- reg$residuals
#bartlett.test(res)
shapiro.test(res)


##**CONCENTRATION 0.4*##
#Remove outlayers
data_subset_0.4<-data_subset_0.4[-nrow(data_subset_0.4), ]
data_subset_0.4<-data_subset_0.4[-nrow(data_subset_0.4), ]
data_subset_0.4<-data_subset_0.4[-nrow(data_subset_0.4), ]

#Modification of the names
colnames(data_subset_0.4)[colnames(data_subset_0.4) == "value"] <- "Absorbance"
colnames(data_subset_0.4)[colnames(data_subset_0.4) == "name"] <- "Temps"

#Find the concentration corresponding to the absorbance thanks to the equation of the calibration curve
data_subset_0.4$Concentration_2 <- data_subset_0.4$Absorbance / 13.8327

# Print or use the updated data_subset_0.1
print(data_subset_0.4)

# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_subset_0.4)
# Extract the slope coefficient from the linear model summary
slope <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope)
r_squared_value <- summary(lm_model)$r.squared

#Print the Phosphate concentration in the medium for an initial concentration of 0.1 as in a fonction of time taking into account dilution
plot_0.4<-ggplot(data=data_subset_0.4,aes(Temps,Concentration_2/0.2))+
  geom_point()+
  theme_bw()+
  scale_y_continuous(limits=c(0.01,0.4), trans="log10") +
  geom_smooth(method="lm", se=FALSE) +  # Utilization of the method="lm" to fit a straight line
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")+
  annotate("text", x = 10, y = 0.05, label = sprintf("Slope: %s", slope), color = "blue", hjust=0)+
  annotate("text", x = 50, y = 0.02, label = sprintf("R-squared: %.4f", r_squared_value), color = "black")


title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.4", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.4,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

# Create two subsets of data for times less than and greater than or equal to 100 minutes
data_subset_0.4_before_200 <- subset(data_subset_0.4, Temps <= 100)
data_subset_0.4_after_200 <- subset(data_subset_0.4, Temps >= 100)
# Create a ggplot with both subsets
plot_0.4_combined <- ggplot(data = data_subset_0.4, aes(Temps, Concentration_2/0.2)) +
  geom_point() +
  theme_bw() +
  geom_smooth(data = data_subset_0.4_before_200, method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = data_subset_0.4_after_200, method = "lm", se = FALSE, color = "red") +
  labs(x = "Time (minutes)", y = "Phosphate concentration in the medium (mMol/l)")
# Title for the combined plot
title_grob <- textGrob("Phosphate concentration in the medium for an initial concentration of 0.4", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.4_combined,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plot and title
)



#Statistic for the initial concentration of 0.4
reg<-lm(log(Concentration_2)~ Temps ,data_subset_0.4 )
summary(reg)
mod<-lm(log(Concentration_2)~Temps,data=data_subset_0.4)
anova(mod)
#Residuals study
res <- reg$residuals
#bartlett.test(res)
shapiro.test(res)


## IMPORTATION HUMICOLA ##
#Importation and modification of the data
levure_h<-read.csv(file="Levure_humicola.csv",sep=";")
levure_h<-pivot_longer(levure_h,cols=c(3:25))
levure_h$name<-str_split(string = levure_h$name,pattern = "X",simplify=TRUE)[,2]
levure_h$name<-as.numeric(levure_h$name)
levure_h$value<-as.numeric(levure_h$value)

#Plot of all the data
ggplot(data=levure_h,aes(name,value,color=Concentration))+
  geom_point()

#plot of the data for the concentration 0.01
data_h_0.01<-levure_h[levure_h$Concentration==0.01,]
ggplot(data=data_h_0.01,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

#plot of the data for the concentration 0.1
data_h_0.1<-levure_h[levure_h$Concentration==0.1,]
ggplot(data=data_h_0.1,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

#plot of the data for the concentration 0.4
data_h_0.4<-levure_h[levure_h$Concentration==0.4,]
ggplot(data=data_h_0.4,aes(name,value,color=Concentration))+
  geom_point()+
  theme_bw()+
  geom_smooth()

##**CONCENTRATION 0.01*##
#Modification of the names
colnames(data_h_0.01)[colnames(data_h_0.01) == "value"] <- "Absorbance"
colnames(data_h_0.01)[colnames(data_h_0.01) == "name"] <- "Temps"

#Find the concentration corresponding to the absorbance thanks to the equation of the calibration curve
data_h_0.01$Concentration_2 <- data_h_0.01$Absorbance / 13.8327

#Remove outlayers
data_h_0.01<-data_h_0.01[data_h_0.01$Concentration_2<0.029&data_h_0.01$Temps<1000,]
data_h_0.01$Concentration_2 <- head(data_h_0.01$Concentration_2, -1)
# Filter the data by excluding values at time 790 with concentration > 0.015
# and values at time 493 with concentration > 0.015
data_h_0.01_filtered <- subset(data_h_0.01, !(Temps == 790 & Concentration_2 > 0.015) & !(Temps == 493 & Concentration_2 > 0.015))
# Print or use the updated data_subset_0.01_filtered
print(data_h_0.01_filtered)

# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_h_0.01_filtered)
# Extract the slope coefficient from the linear model summary
slope <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope)
r_squared_value <- summary(lm_model)$r.squared


#Print the Phosphate concentration in the medium for an initial concentration of 0.01 as in a fonction of time
plot_0.01_h <- ggplot(data=data_h_0.01_filtered,aes(Temps,Concentration_2))+
  geom_point()+
  theme_bw()+
  scale_y_continuous(limits=c(0.001,0.4), trans="log10") +
  geom_smooth(method="lm", se=FALSE) +  # Utilization of the method="lm" to fit a straight line
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")+
  annotate("text", x = 10, y = 0.05, label = sprintf("Slope: %s", slope), color = "blue", hjust=0)+
  annotate("text", x = 200, y = 0.02, label = sprintf("R-squared: %.4f", r_squared_value), color = "black")

title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.01", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.01_h,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)


#Statistic analysis for the initial concentration of 0.01g/l
reg<-lm(log(Concentration_2)~ Temps ,data_h_0.01 )
summary(reg)
mod<-lm(log(Concentration_2)~Temps,data=data_h_0.01)
anova(mod)
#Residuals study
res <- reg$residuals
#bartlett.test(res)
shapiro.test(res)


##**CONCENTRATION 0.1*##
#Modification of the names
colnames(data_h_0.1)[colnames(data_h_0.1) == "value"] <- "Absorbance"
colnames(data_h_0.1)[colnames(data_h_0.1) == "name"] <- "Temps"

#Find the concentration corresponding to the absorbance thanks to the equation of the calibration curve
data_h_0.1$Concentration_2 <- data_h_0.1$Absorbance / 13.8327

#Remove outlayers
data_h_0.1 <- subset(data_h_0.1, Concentration_2 < 0.039 & Temps < 1000 & !(Temps == 328.33 & Concentration_2 > 0.01))

# Print or use the updated data_subset_0.1
print(data_h_0.1)

#Print the Phosphate concentration in the medium for an initial concentration of 0.1 as in a fonction of time
ggplot(data=data_h_0.1,aes(Temps,Concentration_2))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(title = "Phosphate concentration in the medium for an initial concentration of 0.1", x = "Time", y = "Phosphate concentration in the medium")

# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_h_0.1)
# Extract the slope coefficient from the linear model summary
slope <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope)
r_squared_value <- summary(lm_model)$r.squared

#Print the same plot taking into account dilution
plot_0.1<-ggplot(data=data_h_0.1,aes(Temps,Concentration_2/0.1))+
  geom_point()+
  theme_bw()+
  scale_y_continuous(limits=c(0.001,0.4), trans="log10") +
  geom_smooth(data = data_h_0.1, method = "lm", se = FALSE, color = "blue") +
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")+
  annotate("text", x = 0.001, y = 0.001, label = sprintf("Slope: %s", slope), color = "blue", hjust=0)+
  annotate("text", x = 200, y = 0.002, label = sprintf("R-squared: %.4f", r_squared_value), color = "black")

title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.1", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.1,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)



#Statistical analysis for the initial concentration of 0.1g/l
reg<-lm(Concentration_2~ Temps ,data_h_0.1 )
summary(reg)
mod<-lm(Concentration_2~Temps,data=data_h_0.1)
anova(mod)
#Residuals study
res <- reg$residuals
bartlett.test(res)
shapiro.test(res)


##**CONCENTRATION 0.4*##
#Modification of the names
colnames(data_h_0.4)[colnames(data_h_0.4) == "value"] <- "Absorbance"
colnames(data_h_0.4)[colnames(data_h_0.4) == "name"] <- "Temps"

#Find the concentration corresponding to the absorbance thanks to the equation of the calibration curve
data_h_0.4$Concentration_2 <- data_h_0.4$Absorbance / 13.8327

#Remove outlayers
data_h_0.4<-data_h_0.4[data_h_0.4$Concentration_2<0.039&data_h_0.4$Temps<1000,]

# Print or use the updated data_subset_0.1
print(data_h_0.4)

#Print the Phosphate concentration in the medium for an initial concentration of 0.4 as in a fonction of time taking into account dilution
plot_0.4<-ggplot(data=data_h_0.4,aes(Temps,Concentration_2/0.04))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE) +  # Utilization of the method="lm" to fit a straight line
  labs( x = "Time (minutes)", y = "Phosphate concentration in the medium (mMol/l)")
title_grob <- textGrob(" Phosphate concentration in the medium for an initial concentration of 0.4", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.4,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

# Filter the data by excluding values at time 493 with concentration > 0.02
data_h_0.4_filtered <- subset(data_h_0.4, !(Temps == 493 & Concentration_2 > 0.02))


# Create two subsets of data for times less than and greater than or equal to 174.33 minutes
data_h_0.4_before_174.33 <- subset(data_h_0.4_filtered,Temps <= 174.33)
data_h_0.4_after_174.33 <- subset(data_h_0.4_filtered, Temps >= 174.33)


# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_h_0.4_before_174.33)
# Extract the slope coefficient from the linear model summary
slope <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope1)
r_squared_value1 <- summary(lm_model)$r.squared

# Extract the linear model from the plot
lm_model <- lm(Concentration_2 ~ Temps, data = data_h_0.4_after_174.33)
# Extract the slope coefficient from the linear model summary
slope <- summary(lm_model)$coefficients[2, "Estimate"]
print(slope2)
r_squared_value2 <- summary(lm_model)$r.squared



# Create a ggplot with both subsets
plot_0.4_combined <- ggplot(data = data_h_0.4_filtered, aes(Temps, Concentration_2/0.2)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(limits=c(0.001,0.4), trans="log10") +
  geom_smooth(data = data_h_0.4_before_174.33, method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = data_h_0.4_after_174.33, method = "lm", se = FALSE, color = "red") +
  labs(x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")+
  annotate("text", x = 10, y = 0.001, label = sprintf("Slope: %s", slope1), color = "blue", hjust=0)+
  annotate("text", x = 400, y = 0.001, label = sprintf("Slope: %s", slope2), color = "red", hjust=0)+
  annotate("text", x = 100, y = 0.002, label = sprintf("R-squared: %.4f", r_squared_value1), color = "black")+
  annotate("text", x = 500, y = 0.002, label = sprintf("R-squared: %.4f", r_squared_value2), color = "black")

# Title for the combined plot
title_grob <- textGrob("Phosphate concentration in the medium for an initial concentration of 0.4", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_0.4_combined,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plot and title
)


#Statistical analysis for the initial concentration of 0.4g/l
reg<-lm(Concentration_2~ Temps ,data_h_0.4 )
ummary(reg)
mod<-lm(Concentration_2~Temps,data=data_h_0.4)
anova(mod)
#Residuals study
res <- reg$residuals
bartlett.test(res)
shapiro.test(res)


## SUPERPOSITION OF V.H AND C.S ##
##**CONCENTRATION 0.01* ##
# Combine the datasets
combined_data <- bind_rows(
  transform(data_subset_0.01, Source = "S.cerevisae"),
  transform(data_h_0.01_filtered, Source = "V.humicola"))
# Plot with both datasets
plot_combined <- ggplot(data = combined_data, aes(Temps, Concentration_2, color = Source)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(limits=c(0.001,0.4), trans="log10") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")
# Add a legend
plot_combined <- plot_combined + scale_color_manual(name= "Caption", values = c("S.cerevisae" = "blue", "V.humicola" = "red"))
# Add title
title_grob <- textGrob("Comparison of phosphate concentration in the medium for an initial concentration of 0.01\n between the yeasts S.cerevisae and V.humicola", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title
grid.arrange(
  plot_combined,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

##** CONCENTRATION 0.1* ##
# Combine the datasets
combined_data <- bind_rows(
  transform(data_subset_0.1, Source = "S.cerevisae"),
  transform(data_h_0.1, Source = "V.humicola"))
# Plot with both datasets
plot_combined <- ggplot(data = combined_data, aes(Temps, Concentration_2/0.2, color = Source)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(limits=c(0.001,0.4), trans="log10") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")
# Add a legend
plot_combined <- plot_combined + scale_color_manual(name= "Caption", values = c("S.cerevisae" = "blue", "V.humicola" = "red"))
# Add title
title_grob <- textGrob("Comparison of phosphate concentration in the medium for an initial concentration of 0.1\n between the yeasts S.cerevisae and V.humicola", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title
grid.arrange(
  plot_combined,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

##** CONCENTRATION 0.4* ##
# Combine the datasets
combined_data <- bind_rows(
  transform(data_subset_0.4, Source = "S.cerevisae"),
  transform(data_h_0.4_filtered, Source = "V.humicola"))
# Plot with both datasets
plot_combined <- ggplot(data = combined_data, aes(Temps, Concentration_2/0.2, color = Source)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(limits=c(0.001,0.4), trans="log10") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Time (minutes)", y = "Phosphate concentration in the medium\n (mMol/l)")
# Add a legend
plot_combined <- plot_combined + scale_color_manual(name= "Caption", values = c("S.cerevisae" = "blue", "V.humicola" = "red"))
# Add title
title_grob <- textGrob("Comparison of phosphate concentration in the medium for an initial concentration of 0.4\n between the yeasts S.cerevisae and V.humicola", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title
grid.arrange(
  plot_combined,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

## METHOD TO IDENTIFY OUTLIER ##
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

## CACUL OF THE OPTICAL DENSITY OF THE YEAST
#Importation and changment of the data 
OD_yeast<-read.csv(file="optical density.csv",sep=";", dec=",")
OD_yeast$Time<-as.numeric(OD_yeast$Time)
OD_yeast$OD<-as.numeric(OD_yeast$OD)
OD_yeast$yeast<- as.factor(OD_yeast$yeast)
OD_yeast$Phosphate<- as.factor(OD_yeast$Phosphate)

#For cerevisae
OD_s <-OD_yeast[OD_yeast$yeast=="cerevisae",]

plot_s<-ggplot(data=OD_s,aes(Time,OD, color= Phosphate))+
  ylim(0,0.4)+
  geom_point()+
  theme_bw()+ 
  geom_smooth() +  
  labs( x = "Time (minutes)", y = "Optical density ")
title_grob <- textGrob(" Suspension of the yeast S.cerevisae with and without phosphate", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_s,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)

#For humicola
OD_h <-OD_yeast[OD_yeast$yeast=="humicola",]

plot_h<-ggplot(data=OD_h,aes(Time,OD, color= Phosphate))+
  ylim(0,0.3)+
  geom_point()+
  theme_bw()+
  geom_smooth() +  
  labs( x = "Time (minutes)", y = "Optical density ")
title_grob <- textGrob(" Optical density of the yeast V.humicola with and without phosphate", gp = gpar(fontsize = 12, fontface = "bold"))
# Arrange the plot and title using grid.arrange
grid.arrange(
  plot_h,
  title_grob,
  ncol = 1, heights = c(9, 1)  # Adjust the heights of the plots
)
