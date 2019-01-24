## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(ggplot2)
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line()

## ------------------------------------------------------------------------
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line()

## ------------------------------------------------------------------------
ggplot(BOD, aes(x = Time, y = demand)) +
  geom_line() +
  geom_point()

## ------------------------------------------------------------------------
library(dplyr)
tg <- ToothGrowth %>% 
  group_by(supp, dose) %>% 
  summarise(length = mean(len))

ggplot(tg, aes(x = dose, y = length, colour = supp)) +
  geom_line()

## ------------------------------------------------------------------------
ggplot(tg, aes(x = dose, y = length, linetype = supp)) +
  geom_line()

## ------------------------------------------------------------------------
ggplot(tg, aes(x = dose, y = length, fill = supp)) +
  geom_line() +
  geom_point(size = 4, shape = 21)

## ------------------------------------------------------------------------
ggplot(BOD, aes(x = Time, y = demand)) +
  geom_line(linetype = "dashed", size = 1, colour = "blue")

## ------------------------------------------------------------------------
sunspotyear <- data.frame(Year = as.numeric(time(sunspot.year)), 
                          Sunspots = as.numeric(sunspot.year))
ggplot(sunspotyear, aes(x = Year, y = Sunspots)) +
  geom_area()

## ------------------------------------------------------------------------
ggplot(sunspotyear, aes(x = Year, y = Sunspots)) +
  geom_area(colour = "black", fill = "blue", alpha = .2)

## ------------------------------------------------------------------------
ggplot(sunspotyear, aes(x = Year, y = Sunspots)) +
  geom_area(fill = "blue", alpha = .2) +
  geom_line()

## ------------------------------------------------------------------------
library(gcookbook)
ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
  geom_area()

## ------------------------------------------------------------------------
ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
  geom_area(colour = "black", size = .2, alpha = .4) +
  scale_fill_brewer(palette = "Blues", breaks = rev(levels(uspopage$AgeGroup)))

