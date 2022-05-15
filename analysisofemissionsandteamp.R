library(ggplot2)
library(dplyr)



shapiro.test(Emissions_data$Average_Yearly_Temperature)
shapiro.test(Emissions_data$Annual_Emission_Tonnes)


globalfuel_data <- read.csv("D:\\COLLEGE\\soft proj\\globalfuelemissions.csv", head=TRUE, sep=",")

head(globalfuel_data)

globalfuel_data %>% 
  ggplot(aes(x = Year, y = Gas.Flaring + Cement + Solid.Fuel + Liquid.Fuel + Gas.Fuel)) +
  geom_line(col = "red")


var.test(Emissions_data$Annual_Emission_Tonnes, Emissions_data$Average_Yearly_Temperature)


Emissions_data %>% 
  ggplot(aes(x = Year, y = Annual_Emission_Tonnes, color = Code)) +
  geom_line()

Emissions_data %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature, color = Code)) +
  geom_line()


hist(Emissions_data$Average_Yearly_Temperature,
     main = "Average Yearly Teamputer - Overall",
     xlab = "Temperature ( Celsius )",
     col = "Red",
     20)

hist(Emissions_data$Annual_Emission_Tonnes,
     main = " Annual Emissions - Overall",
     xlab = "Co2 Emissions ( tonnes )",
     col = "blue",
     20)

plot(Emissions_data$Average_Yearly_Temperature, Emissions_data$Annual_Emission_Tonnes,
     main = " Annual Emissions VS Average Yearly Temperature",
     xlab = " Average Yearly Temperature ",
     ylab = " Annual Emissions ", 
     col = "blue",
     )

boxplot(Emissions_data$Annual_Emission_Tonnes)
boxplot(Emissions_data$Average_Yearly_Temperature)



cor(Emissions_data$Average_Yearly_Temperature, Emissions_data$Annual_Emission_Tonnes)


itl.dat <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\Italy_data.csv", head=TRUE, sep=",")

itl.dat %>% 
ggplot(aes(x = Year, y = Annual_Emission_Tonnes)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm")

itl.dat %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm")

plot(itl.dat)
itlModel <- lm(Year ~ Annual_Emission_Tonnes + Average_Yearly_Temperature, data = itl.dat)
summary(itlModel)
predict(itlModel)
predict(itlModel, data.frame(Annual_Emission_Tonnes = 10467461756, Average_Yearly_Temperature = 15.2))

itlModel2 <- lm(Average_Yearly_Temperature ~ Annual_Emission_Tonnes + Year, data = itl.dat)
summary(itlModel2)
predict(itlModel2)
predict(itlModel2, data.frame(Annual_Emission_Tonnes = 1314069572, Year = 2013))

plot(itlModel2)


usa.dat <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\USA_data.csv", head=TRUE, sep=",")

usa.dat %>% 
  ggplot(aes(x = Year, y = Annual_Emission_Tonnes)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm")

usa.dat %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm")

plot(usa.dat)
