library(ggplot2)
library(dplyr)


#checking the datasets for information 
testEmissions_data <- read.csv("D:\\COLLEGE\\soft proj\\co2_emission.csv", head=TRUE, sep=",")
head(testEmissions_data)
str(testEmissions_data)
summary(testEmissions_data)
print(testEmissions_data)

testCity_temp <- read.csv("D:\\COLLEGE\\soft proj\\city_temperature.csv", head=TRUE, sep=",")
head(testCity_temp)
str(testCity_temp)
summary(testCity_temp)
print(testCity_temp)

testGlobalTemperatures <- read.csv("D:\\COLLEGE\\soft proj\\GlobalTemperatures.csv", head=TRUE, sep=",")
head(testGlobalTemperatures)
str(testGlobalTemperatures)
summary(testGlobalTemperatures)
print(testGlobalTemperatures)

testCity_temp <- read.csv("D:\\COLLEGE\\soft proj\\GlobalLandTemperaturesByCity.csv", head=TRUE, sep=",")
head(testCity_temp)
str(testCity_temp)
summary(testCity_temp)
print(testCity_temp)

testGlobalLandTemperaturesByMajorCity <- read.csv("D:\\COLLEGE\\soft proj\\GlobalLandTemperaturesByMajorCity.csv", head=TRUE, sep=",")
head(testGlobalLandTemperaturesByMajorCity)
str(testGlobalLandTemperaturesByMajorCity)
summary(testGlobalLandTemperaturesByMajorCity)
print(testGlobalLandTemperaturesByMajorCity)

#loading in a new dataset created from the other sets
Emissions_data <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\newDataset.csv", head=TRUE, sep=",")
head(Emissions_data)
summary(Emissions_data)


boxplot(nra.dat$Annual_Emission_Tonnes)
hist(nra.dat$Annual_Emission_Tonnes)
hist(nra.dat$Average_Yearly_Temperature)

x_out_rm_ <-nra.dat$Annual_Emission_Tonnes[!Emissions_data$Annual_Emission_Tonnes %in% boxplot.stats(nra.dat$Annual_Emission_Tonnes)$out]
length(Emissions_data$Annual_Emission_Tonnes) - length(x_out_rm_)
boxplot(x_out_rm_)

boxplot(nra.dat$Average_Yearly_Temperature)
x_out_rm_2 <-Emissions_data$Average_Yearly_Temperature[!Emissions_data$Average_Yearly_Temperature %in% boxplot.stats(Emissions_data$Average_Yearly_Temperature)$out]
length(Emissions_data$Average_Yearly_Temperature) - length(x_out_rm_2)
boxplot(x_out_rm_2)

#Checking for correlation between the variables
plot(Emissions_data)

#annual emissions in tonnes by region graph
Emissions_data %>% 
  ggplot(aes(x = Year, y = Annual_Emission_Tonnes, color = Code)) +
  geom_line() +
  labs(title = "Annual Emissions in tonnes by region",
       x = "Year",
       y = "Annual Emission in tonnes",
       color = "Region")

#annual temp average by region graph
Emissions_data %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature, color = Code)) +
  geom_line() +
  labs(title = "Annual average temperature 1949 - 2013",
       x = "Year",
       y = "Temperature average",
       color = "Region")

Emissions_data %>%
  ggplot(aes(x = Year , y = Average_Yearly_Temperature)) +
  geom_line(col = "brown") + geom_smooth(formula = y ~ x, method = "lm")+
  labs(title = "Yearly Emissions vs Average Yearly Temperature",
       x = "Year",
       y = "Yearly temperature average")


hist(globalfuel_emissiondata$Gas.Flaring,
     main = "Average Yearly Teamputer - Overall",
     xlab = "Temperature ( Celsius )",
     col = "Red",
     20)


#graph with the total emissions per year
globalfuel_emissiondata <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\globalfuelemissions.csv", head=TRUE, sep=",")

summary(globalfuel_emissiondata)

hist(globalfuel_emissiondata$Total,
     main = " Annual Emissions 1950 to 2010 - Total Overall",
     xlab = "Co2 Emissions ( tonnes )",
     col = "blue",
     20)


globalfuel_emissiondata %>%
  ggplot(aes(x = Year, y = Gas.Flaring + Cement + Solid.Fuel + Liquid.Fuel + Gas.Fuel)) +
  geom_line(col = "red") +
  labs(title = "Fossil fuel emissions from 1950 - 2010",
     x = "Year",
     y = "Annual Emission from fossil fuels - million metric tons of Carbon")



globalfuel_emissiondata %>%
  ggplot(aes(x = Year, y = Gas.Flaring + Cement + Solid.Fuel + Liquid.Fuel + Gas.Fuel)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm")+
  labs(title = "Fossil fuel emissions from 1950 - 2010",
     x = "Year",
     y = "Annual Emission from fossil fuels - million metric tons of Carbon")


plot(globalfuel_emissiondata$Gas.Fuel,
    main = "Gas fuel emissions from 1950 - 2010",
     xlab = "Frequency",
     ylab = "Annual Emission from Gas fuels")

plot(globalfuel_emissiondata$Liquid.Fuel,
main = "Liquid fuel emissions from 1950 - 2010",
xlab = "Frequency",
ylab = "Annual Emission from Liquid fuels")

plot(globalfuel_emissiondata$Solid.Fuel,
main = "Solid fuel emissions from 1950 - 2010",
xlab = "Frequency",
ylab = "Annual Emission from Solid fuels")

plot(globalfuel_emissiondata$Cement,
main = "Cement fuel emissions from 1950 - 2010",
xlab = "Frequency",
ylab = "Annual Emission from Cement fuels")

plot(globalfuel_emissiondata$Gas.Flaring,
     main = "Gas Flaring fuel emissions from 1950 - 2010",
     xlab = "Frequency",
     ylab = "Annual Emission from Gas flaring fuels")


Emissions_data %>% 
  ggplot(aes(x = Average_Yearly_Temperature, y = Annual_Emission_Tonnes, color = Code)) +
  geom_line()

#loading in separate datasets for each region
itl.dat <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\Italy_data.csv", head=TRUE, sep=",")
france.dat <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\France_data.csv", head=TRUE, sep=",")
ger.dat <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\Germany_data.csv", head=TRUE, sep=",")
jap.dat <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\Japan_data.csv", head=TRUE, sep=",")
nra.dat <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\NRA_data.csv", head=TRUE, sep=",")
afgn.dat <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\Afganistan_data.csv", head=TRUE, sep=",")


itl.dat %>% 
ggplot(aes(x = Year, y = Annual_Emission_Tonnes)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Emissions recorded for Italy 1949 - 2013",
       x = "Year",
       y = "Annual Emission recorded in Italy",
       color = "Region")

ger.dat %>% 
  ggplot(aes(x = Year, y = Annual_Emission_Tonnes)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Emissions recorded for Germany 1949 - 2013",
       x = "Year",
       y = "Annual Emission recorded in Germany",
       color = "Region")

france.dat %>% 
  ggplot(aes(x = Year, y = Annual_Emission_Tonnes)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Emissions recorded for France 1949 - 2013",
       x = "Year",
       y = "Annual Emission recorded in France",
       color = "Region")

jap.dat %>% 
  ggplot(aes(x = Year, y = Annual_Emission_Tonnes)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Emissions recorded for Japan 1949 - 2013",
       x = "Year",
       y = "Annual Emission recorded in Japan",
       color = "Region")

nra.dat %>% 
  ggplot(aes(x = Year, y = Annual_Emission_Tonnes)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Emissions recorded for North America 1949 - 2013",
       x = "Year",
       y = "Annual Emission recorded in North America",
       color = "Region")

afgn.dat %>% 
  ggplot(aes(x = Year, y = Annual_Emission_Tonnes)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Emissions recorded for Afghanistan 1949 - 2013",
       x = "Year",
       y = "Annual Emission recorded in Afghanistan",
       color = "Region")
################################################################

itl.dat %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Average Temp. for Italy 1949 - 2013",
       x = "Year",
       y = "Average Temp. for Italy",
       color = "Region")

ger.dat %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Average Temp. for Germany 1949 - 2013",
       x = "Year",
       y = "Average Temp. for Germany",
       color = "Region")

france.dat %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Average Temp. for France 1949 - 2013",
       x = "Year",
       y = "Average Temp. for France",
       color = "Region")

jap.dat %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Average Temp. for Japan 1949 - 2013",
       x = "Year",
       y = "Average Temp. for Japan",
       color = "Region")

nra.dat %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Average Temp. for North America 1949 - 2013",
       x = "Year",
       y = "Average Temp. for North America",
       color = "Region")

afgn.dat %>% 
  ggplot(aes(x = Year, y = Average_Yearly_Temperature)) + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Average Temp. for Afghanistan 1949 - 2013",
       x = "Year",
       y = "Average Temp. for Afghanistan",
       color = "Region")


#checking frequency distribution for solid fuels emissions
hist(globalfuel_emissiondata$Gas.Fuel)
hist(globalfuel_emissiondata$Liquid.Fuel)
hist(globalfuel_emissiondata$Solid.Fuel)
hist(globalfuel_emissiondata$Cement)

#building the temperature and emissions model

Emissions_data2 <- read.csv("D:\\COLLEGE\\soft proj\\datasets\\newDataset2.csv", head=TRUE, sep=",")

plot(nra.dat)
NraPredictModel <- lm(Year ~ Annual_Emission_Tonnes + Average_Yearly_Temperature, data = nra.dat)
summary(NraPredictModel)
predict(NraPredictModel)

summary(nra.dat)

NraPredictModel2 <- lm(Average_Yearly_Temperature ~ Annual_Emission_Tonnes + Year, data = nra.dat)

summary(NraPredictModel2)
predict(NraPredictModel2)
predict(NraPredictModel2, data.frame(Annual_Emission_Tonnes = 1333140572, Year = 2122))

plot(NraPredictModel2)

predict(NraPredictModel, data.frame(Annual_Emission_Tonnes = 11111111111, Average_Yearly_Temperature = 20))
