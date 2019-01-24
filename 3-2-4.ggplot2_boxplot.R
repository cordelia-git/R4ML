## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot()

## ------------------------------------------------------------------------
ggplot(mpg, aes(x = class, y = cty)) +
  geom_boxplot()

## ------------------------------------------------------------------------
library(dplyr)
mpg_class <- mpg %>% 
  filter(class %in% c("compact", "subcompact", "suv"))

ggplot(mpg_class, aes(x = class, y = cty)) +
  geom_boxplot()  

## ------------------------------------------------------------------------
library(MASS)
ggplot(birthwt, aes(x = factor(race), y = bwt)) +
  geom_boxplot()

## ------------------------------------------------------------------------
ggplot(birthwt, aes(x = factor(race), y = bwt)) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21)

## ------------------------------------------------------------------------
ggplot(birthwt, aes(x = factor(race), y = bwt)) +
  geom_boxplot(notch = TRUE)

## ------------------------------------------------------------------------
ggplot(birthwt, aes(x = factor(race), y = bwt)) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "red")

## ------------------------------------------------------------------------
library(gcookbook)
p <- ggplot(heightweight, aes(x = sex, y = heightIn)) 
p +  geom_violin()

## ------------------------------------------------------------------------
p <- ggplot(heightweight, aes(x = sex, y = heightIn)) 
p +  geom_violin() +
  geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

## ------------------------------------------------------------------------
p + geom_violin(trim = FALSE)

## ------------------------------------------------------------------------
p + geom_violin(scale = "count")

## ------------------------------------------------------------------------
ggplot(heightweight, aes(x = sex, y = heightIn)) +
  geom_boxplot(aes(x = as.numeric(sex) + .2, group = sex), width = .25) +
  geom_dotplot(aes(x = as.numeric(sex) - .2, group = sex), binaxis = "y",
               banwidth = .5, stackdir = "center") +
  scale_x_continuous(breaks = 1:nlevels(heightweight$sex),
                     labels = levels(heightweight$sex))

## ------------------------------------------------------------------------
p <- ggplot(faithful, aes(x = eruptions, y = waiting))
p + geom_point() + stat_density2d()

## ------------------------------------------------------------------------
p + stat_density2d(aes(colour = ..level..))

## ------------------------------------------------------------------------
p + stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE)

## ------------------------------------------------------------------------
p + geom_point() +
  stat_density2d(aes(alpha = ..density..), geom = "tile", contour = FALSE)

## ------------------------------------------------------------------------
p + stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE, h = c(.5, 5))

