## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

df_mpg

## ------------------------------------------------------------------------
ggplot(df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

## ------------------------------------------------------------------------
ggplot(df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

## ------------------------------------------------------------------------
ggplot(mpg, aes(x = drv)) + geom_bar()

## ------------------------------------------------------------------------
ggplot(mpg, aes(x = hwy)) + geom_bar()

## ------------------------------------------------------------------------
mpg <- as.data.frame((ggplot2::mpg))

df <- mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean.cty = mean(hwy)) %>% 
  arrange(desc(mean.cty)) %>% 
  head(5)
df

## ------------------------------------------------------------------------
ggplot(df, aes(x = reorder(manufacturer, -mean.cty), y = mean.cty)) + geom_col()

## ------------------------------------------------------------------------
ggplot(mpg, aes(x = class)) + geom_bar()

## ------------------------------------------------------------------------
ggplot(mpg, aes(x = class)) + geom_histogram(stat="count")

## ------------------------------------------------------------------------
library(gcookbook)
ggplot(pg_mean, aes(x = group, y = weight)) + geom_col()

## ------------------------------------------------------------------------
library(gcookbook)
ggplot(pg_mean, aes(x = group, y = weight)) + geom_bar(stat = "identity")

## ------------------------------------------------------------------------
ggplot(pg_mean, aes(x = group, y = weight)) +
  geom_col(fill = "lightblue", colour = "black")

## ------------------------------------------------------------------------
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(position = "dodge")

## ------------------------------------------------------------------------
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(position = "dodge", colour = "black") +
  scale_fill_brewer(palette="Pastel1")

## ------------------------------------------------------------------------
library(gcookbook)
upc <- subset(uspopchange, rank(Change) > 40)
upc

## ------------------------------------------------------------------------
ggplot(upc, aes(x = Abb, y = Change, fill = Region)) +
  geom_col()

## ------------------------------------------------------------------------
ggplot(upc, aes(x = reorder(Abb, Change), y = Change, fill = Region)) +
  geom_col(colour = "black") +
  scale_fill_manual(values = c("#669933", "#FFCC66")) +
  xlab("State")

## ------------------------------------------------------------------------
library(gcookbook)
csub <- subset(climate, Source == "Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0

## ------------------------------------------------------------------------
ggplot(csub, aes(x = Year, y = Anomaly10y, fill = pos)) +
  geom_col()

## ------------------------------------------------------------------------
ggplot(csub, aes(x = Year, y = Anomaly10y, fill = pos)) +
  geom_col(colour = "black", size = .25) +
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE)
  

## ------------------------------------------------------------------------
library(gcookbook)
ggplot(pg_mean, aes(x = group, y = weight)) +
  geom_col(width = .5)

## ------------------------------------------------------------------------
library(gcookbook)
ggplot(pg_mean, aes(x = group, y = weight)) +
  geom_col(width = 1)

## ------------------------------------------------------------------------
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col()

## ------------------------------------------------------------------------
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar, order = desc(Cultivar))) +
  geom_col() +
  guides(fill = guide_legend(reverse = TRUE))

## ------------------------------------------------------------------------
library(dplyr)
ce <- cabbage_exp %>% 
  group_by(Date) %>% 
  mutate(percent_weight = Weight / sum(Weight) * 100)

ce

## ------------------------------------------------------------------------
ggplot(ce, aes(x = Date, y = percent_weight, fill = Cultivar)) +
  geom_col()

## ------------------------------------------------------------------------
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
  geom_col() +
  geom_text(aes(label = Weight), vjust = 1.5, color = "white")

## ------------------------------------------------------------------------
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
  geom_col() +
  geom_text(aes(label = Weight), vjust = -0.2)

## ------------------------------------------------------------------------
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Weight), vjust = 1.5, color = "white", 
            position = position_dodge(.9), size = 3)

## ------------------------------------------------------------------------

ce <- cabbage_exp %>% 
  group_by(Date) %>% 
  mutate(percent_weight = Weight / sum(Weight) * 100)

ce <- ce %>% 
  arrange(Date, Cultivar) %>% 
  group_by(Date) %>% 
  mutate(label_y = cumsum(Weight) - 0.5*Weight)

ggplot(ce, aes(x = Date, y = percent_weight, fill = Cultivar)) +
  geom_col() +
  geom_text(aes(y = label_y, label = Weight), vjust = 1.5, color = "white")

