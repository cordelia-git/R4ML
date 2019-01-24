## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
ggplot(faithful, aes(x = waiting)) + geom_histogram()

## ------------------------------------------------------------------------
ggplot(faithful, aes(x = waiting)) + 
  geom_histogram(binwidth = 5, fill = "white", colour = "black")

## ------------------------------------------------------------------------
binsize <- diff(range(faithful$waiting))/15
ggplot(faithful, aes(x = waiting)) + 
  geom_histogram(binwidth = binsize, fill = "white", colour = "black")

## ------------------------------------------------------------------------
h <- ggplot(faithful, aes(x = waiting))
h + geom_histogram(binwidth = 8, fill = "white", colour = "black", boundary = 31)

## ------------------------------------------------------------------------
h + geom_histogram(binwidth = 8, fill = "white", colour = "black", boundary = 35)

## ------------------------------------------------------------------------
library(MASS)
#str(birthwt)
ggplot(birthwt, aes(x = bwt)) + 
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(smoke ~ .)

## ------------------------------------------------------------------------
birthwt1 <- birthwt
birthwt1$smoke <- factor(birthwt1$smoke)
levels(birthwt1$smoke)

## ------------------------------------------------------------------------
library(plyr)
birthwt1$smoke <- revalue(birthwt1$smoke, c("0" = "No smoke", "1" = "Smoke"))

## ------------------------------------------------------------------------
ggplot(birthwt1, aes(x = bwt)) + 
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(smoke ~ .)

## ------------------------------------------------------------------------
ggplot(birthwt1, aes(x = bwt)) + 
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(race ~ ., scale = "free")

## ------------------------------------------------------------------------
ggplot(birthwt1, aes(x = bwt, fill = smoke)) +
  geom_histogram(position = "identity", alpha = 0.4)

## ------------------------------------------------------------------------
ggplot(faithful, aes(x = waiting)) +
  geom_density()

## ------------------------------------------------------------------------
ggplot(faithful, aes(x = waiting)) +
  geom_line(stat = "density") +
  expand_limits(y = 0)

## ------------------------------------------------------------------------
ggplot(faithful, aes(x = waiting, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density() +
  xlim(35, 105)

## ------------------------------------------------------------------------
ggplot(birthwt1, aes(x = bwt, colour = smoke))  +
  geom_density()

## ------------------------------------------------------------------------
ggplot(birthwt1, aes(x = bwt, fill = smoke))  +
  geom_density(alpha = .3)

## ------------------------------------------------------------------------
ggplot(birthwt1, aes(x = bwt)) +
  geom_density() +
  facet_grid(smoke ~ .)

