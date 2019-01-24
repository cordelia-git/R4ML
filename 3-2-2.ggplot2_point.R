## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(ggplot2)
ggplot(data = mpg, aes(x = displ, y = hwy)) + # x축과 y축 배경생성
  geom_point() + # 배경에 산점도 추가
  xlim(3, 6) + # x축의 범위 지정
  ylim(10, 30) # y축의 범위 지정

## ------------------------------------------------------------------------
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(.~class)

## ------------------------------------------------------------------------
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(alpha = .3) +
  facet_grid(cyl~class, scales = "free")

## ------------------------------------------------------------------------
mpg <- as.data.frame(ggplot2::mpg)
ggplot(data = mpg, aes(x = mpg$cty, y = mpg$hwy)) +
  geom_point()

## ------------------------------------------------------------------------
midwest <- as.data.frame(ggplot2::midwest)
ggplot(data = midwest, aes(x = midwest$poptotal, y = midwest$popasian)) +
  geom_point() +
  xlim(0, 500000) +
  ylim(0, 10000)
  
  

## ------------------------------------------------------------------------
#install.packages("gcookbook")
library(gcookbook)
library()

## ------------------------------------------------------------------------
heightweight1 <- heightweight[ , c("ageYear", "heightIn")]
head(heightweight1, 5)

## ------------------------------------------------------------------------
ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point()

## ------------------------------------------------------------------------
ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point(shape = 21)

## ------------------------------------------------------------------------
ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point(shape = 19, size = 1)

## ------------------------------------------------------------------------
ggplot(heightweight, aes(x = ageYear, y = heightIn, shape = sex, colour = sex)) +
  geom_point()

## ------------------------------------------------------------------------
ggplot(heightweight, aes(x = ageYear, y = heightIn, shape = sex, colour = sex)) +
  geom_point() +
  scale_shape_manual(values = c(1,2)) +
  scale_colour_brewer(palette = "Set1")

## ------------------------------------------------------------------------
ggplot2::ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
  geom_point()

## ------------------------------------------------------------------------
ggplot2::ggplot(heightweight, aes(x = ageYear, y = heightIn, size = weightLb)) +
  geom_point()

## ------------------------------------------------------------------------
ggplot2::ggplot(heightweight, aes(x = ageYear, y = heightIn, fill = weightLb)) +
  geom_point(shape = 21, size = 2.5) +
  scale_fill_gradient(low = "black", high = "white")

## ------------------------------------------------------------------------
ggplot2::ggplot(heightweight, aes(x = ageYear, y = heightIn, fill = weightLb)) +
  geom_point(shape = 21, size = 2.5) +
  scale_fill_gradient(low = "black", high = "white", breaks = seq(70, 170, by = 20), 
                      guide = guide_legend())

## ------------------------------------------------------------------------
ggplot2::ggplot(heightweight, aes(x = ageYear, y = heightIn, size = weightLb, colour = sex)) +
  geom_point(alpha = .5) +
  scale_size_area() + # 점의 면적을 수치형 값에 비례하도록 만듦
  scale_colour_brewer(palette = "Set1")

## ------------------------------------------------------------------------
sp <- ggplot(diamonds, aes(x = carat, y = price))
sp + geom_point()

## ------------------------------------------------------------------------
sp + geom_point(alpha = .1)

## ------------------------------------------------------------------------
sp + geom_point(alpha = .01)

## ------------------------------------------------------------------------
sp + stat_bin2d()

## ------------------------------------------------------------------------
sp + stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 6000))

## ------------------------------------------------------------------------
sp1 <- ggplot(ChickWeight, aes(x = Time, y = weight))
sp1 + geom_point()

## ------------------------------------------------------------------------
sp1 <- ggplot(ChickWeight, aes(x = Time, y = weight))
sp1 + geom_point(position = "jitter")

## ------------------------------------------------------------------------
sp1 <- ggplot(ChickWeight, aes(x = Time, y = weight))
sp1 + geom_point(position = position_jitter(width = .5, height = 0))

## ------------------------------------------------------------------------
sp1 + geom_boxplot(aes(group = Time))

## ------------------------------------------------------------------------
sp <- ggplot(heightweight, aes(x = ageYear, y = heightIn))
sp + geom_point() + stat_smooth(method = lm)

## ------------------------------------------------------------------------
sp + geom_point() + stat_smooth(method = lm, level = 0.99)

## ------------------------------------------------------------------------
sp + geom_point() + stat_smooth(method = lm, se=FALSE)

## ------------------------------------------------------------------------
sp + geom_point(colour = "grey60") + stat_smooth(method = lm, se=FALSE, colour = "black")

## ------------------------------------------------------------------------
ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  geom_rug()

## ------------------------------------------------------------------------
ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  geom_rug(position = "jitter", size = .2)

## ------------------------------------------------------------------------
library(gcookbook)
subset(countries, Year == 2009 & healthexp > 2000)

## ------------------------------------------------------------------------
sp <- ggplot(subset(countries, Year == 2009 & healthexp > 2000), 
             aes(x = healthexp, y = infmortality)) +
  geom_point()
sp + annotate("text", x = 4350, y = 5.4, label = "Canada") +
     annotate("text", x = 7400, y = 6.8, label = "USA")

## ------------------------------------------------------------------------
sp + geom_text(aes(label = Name), size = 3)

## ------------------------------------------------------------------------
sp + geom_text(aes(label = Name), size = 3, vjust = 0)

## ------------------------------------------------------------------------
sp + geom_text(aes(y = infmortality+.1, label = Name), size = 3, vjust = 0)

## ------------------------------------------------------------------------
cdat <- subset(countries, Year == 2009 & 
  Name %in% c("Canada", "Ireland", "United Kingdom", "United States", "New Zealand", "Iceland", 
              "Japan", "Luxembourg", "Netherlands", "Switzerland"))
cdat

## ------------------------------------------------------------------------
p <- ggplot(cdat, aes(x = healthexp, y = infmortality, size = GDP)) +
  geom_point(shape = 21, colour = "black", fill = "cornsilk")
p

## ------------------------------------------------------------------------
p + scale_size_area(max_size = 15)

## ------------------------------------------------------------------------
tophit <- tophitters2001[1:25, ]
ggplot(tophit, aes(x = avg, y = name)) +
  geom_point()

## ------------------------------------------------------------------------
tophit[ , c("name", "lg", "avg")]

## ------------------------------------------------------------------------
tophit <- tophitters2001[1:25, ]
ggplot(tophit, aes(x = avg, y = reorder(name, avg))) +
  geom_point(size = 3) +
  theme_bw(base_family = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))

## ------------------------------------------------------------------------
tophit <- tophitters2001[1:25, ]
ggplot(tophit, aes(x = reorder(name, avg), y = avg)) +
  geom_point(size = 3) +
  theme_bw(base_family = "") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))

## ------------------------------------------------------------------------
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
tophit$name <- factor(tophit$name, levels = nameorder)

ggplot(tophit, aes(x = avg, y = name)) +
  geom_segment(aes(yend=name), xend=0, colour = "grey50") +
  geom_point(size = 3, aes(colour = lg)) +
  scale_colour_brewer(palette = "Set1", limits = c("NL", "AL")) +
  theme_bw(base_family = "") +
  theme(panel.grid.major.y = element_blank(), #수평 격자선을 없앰
        legend.position = c(1, 0.55), #범례를 그래프 안쪽으로 옮김
        legend.justification = c(1, 0.5))

## ------------------------------------------------------------------------
ggplot(tophit, aes(x = avg, y = name)) +
  geom_segment(aes(yend=name), xend=0, colour = "grey50") +
  geom_point(size = 3, aes(colour = lg)) +
  scale_colour_brewer(palette = "Set1", limits = c("NL", "AL"), guide = FALSE) +
  theme_bw(base_family = "") +
  theme(panel.grid.major.y = element_blank()) + #수평 격자선을 없앰
  facet_grid(lg ~ ., scales = "free_y", space = "free_y")

