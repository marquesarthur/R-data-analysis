if (!require("ggplot2")) install.packages("ggplot2")
library("ggplot2")

if (!require("ggpubr")) install.packages("ggpubr")
library("ggpubr")

if (!require("dplyr")) install.packages("dplyr")
library("dplyr")

if (!require("janitor")) install.packages("janitor")
library("janitor")

if (!require("stringr")) install.packages("stringr")
library("stringr")

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")




scale2Decimals <- function(x) sprintf("%.2f", x)


dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data
globalTemperatures <- read_csv(paste(dir, "GlobalTemperatures.csv", sep="/"))
str(globalTemperatures)
head(globalTemperatures, n=5)
tail(globalTemperatures, n=5)



select(filter(globalTemperatures, dt > as.Date("1800-01-01")), dt, LandAverageTemperature)

dt_temperature <- globalTemperatures %>%
  filter(dt > as.Date("1800-01-01")) %>%
  select(dt, LandAverageTemperature)


head(dt_temperature, n=5)

dt_temperature <- dt_temperature %>%
  mutate(year = as.numeric(format(dt, "%Y")))


avg_temp_by_year <- dt_temperature %>%
  group_by(year) %>%
  summarise(avg_temp = mean(LandAverageTemperature))



p <- ggplot(data=avg_temp_by_year, aes(x=year, y=avg_temp)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(avg_temp_by_year %>% select(year)), max(avg_temp_by_year %>%select(year)), by = 25)) +
  scale_y_continuous(
    labels=scale2Decimals,
    breaks = seq(min(avg_temp_by_year %>% select(avg_temp)), max(avg_temp_by_year %>%select(avg_temp)), by = .2)
  ) +
  geom_hline(yintercept=9.0, linetype="dashed", color = "red")
p



globalTemperaturesByCountry <- read_csv(paste(dir, "GlobalLandTemperaturesByCountry.csv", sep="/"))
str(globalTemperaturesByCountry)
head(globalTemperaturesByCountry, n=5)
tail(globalTemperaturesByCountry, n=5)


unique(globalTemperaturesByCountry %>% select(Country))


dt_temperature <- globalTemperaturesByCountry %>%
  na.omit() %>%
  filter(dt > as.Date("1800-01-01")) %>%
  filter(Country == 'Canada' | Country == 'China' | Country == 'United States') %>%
  select(dt, AverageTemperature, Country) %>%
  mutate(year = as.numeric(format(dt, "%Y")))

str(dt_temperature)

head(dt_temperature, n=5)
tail(dt_temperature, n=5)



p <- ggplot(data=dt_temperature, aes(x=Country, y=AverageTemperature, colour=Country)) +
  geom_boxplot() +
  scale_y_continuous(
    labels=scale2Decimals,
    breaks = seq(min(dt_temperature %>% select(AverageTemperature)), max(dt_temperature %>%select(AverageTemperature)), by = 5)
  )
p


ggdensity(dt_temperature$AverageTemperature, 
          main = "Density plot of Average Temperature",
          xlab = "Average Temperature")


us_temp <- dt_temperature %>%
  filter(Country == 'United States') %>%
  select(AverageTemperature)

cn_temp <- dt_temperature %>%
  filter(Country == 'China') %>%
  select(AverageTemperature)

ca_temp <- dt_temperature %>%
  filter(Country == 'Canada') %>%
  select(AverageTemperature)

length(us_temp$AverageTemperature)
length(cn_temp$AverageTemperature)
length(ca_temp$AverageTemperature)

random_samples <- min(length(us_temp$AverageTemperature),
    length(cn_temp$AverageTemperature),
    length(ca_temp$AverageTemperature))


temperature <- c()
country <- c()

us_temp <- sample(us_temp$AverageTemperature, random_samples)
temperature <- c(temperature, us_temp)
country <- c(country, rep("United States", random_samples))

cn_temp <- sample(cn_temp$AverageTemperature, random_samples)
temperature <- c(temperature, cn_temp)
country <- c(country, rep("China", random_samples))

ca_temp <- sample(ca_temp$AverageTemperature, random_samples)
temperature <- c(temperature, ca_temp)
country <- c(country, rep("Canada", random_samples))



df <- data.frame(AverageTemperature=temperature, Country=country)

kruskal.test(Country ~ AverageTemperature, data = df)

kruskal.test(Country ~ AverageTemperature, data = df %>% filter(Country == 'Canada' | Country == 'China'))
kruskal.test(Country ~ AverageTemperature, data = df %>% filter(Country == 'Canada' | Country == 'United States'))
kruskal.test(Country ~ AverageTemperature, data = df %>% filter(Country == 'China' | Country == 'United States'))




