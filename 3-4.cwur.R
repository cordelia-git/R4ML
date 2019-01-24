## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
data <- read.csv("data/cwur_data.csv", stringsAsFactors = F)
str(data)
sapply(data, function(x){sum(is.na(x))})
#Hmisc::describe(data)

## ------------------------------------------------------------------------
# 범주형 변수인 2열과 3열을 제외함
data1 <- data[, -c(2,3)]
# 결측치가 존재하는 2018년 데이터를 제외함
data1 <-data1[data1$year != 2018, ]
# 결측치 갯수 확인 
sum(is.na(data1))
str(data1)

# dplyr 로 동일하게 표현하면.. 
data1 <- data %>% 
    select(-c(2,3)) %>% 
    filter(year != 2018)

# 상관계수 출력
cor(data1)
# 상관계수를 소수점 두자리로 출력
round(cor(data1), 2)


## ------------------------------------------------------------------------
library(corrplot)
corr <- cor(data1)
corrplot(corr, method="circle")

## ------------------------------------------------------------------------
corrplot(corr, method="pie")
corrplot(corr, method="color") 
corrplot(corr, method="number") # Display the correlation coefficient

## ------------------------------------------------------------------------
corrplot(corr, type="upper")
corrplot(corr, type="lower")

## ------------------------------------------------------------------------
# correlogram with hclust reordering
corrplot(corr, type="upper", order="hclust")

## ------------------------------------------------------------------------
library(RColorBrewer)
corrplot(corr, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"))

## ------------------------------------------------------------------------
corrplot(corr, type="upper", tl.col="black", tl.srt=45)

## ------------------------------------------------------------------------
data <- tbl_df(data)
data

## ------------------------------------------------------------------------
filter <- dplyr::filter
select <- dplyr::select
filter(data, world_rank == 1)

## ------------------------------------------------------------------------
filter(data, country == "South Korea", world_rank < 100)

## ------------------------------------------------------------------------
# 결측치 합계 확인
sum(is.na(data))
# 변수별 결측치 갯수(합계) 확인
sapply(data, function(x){sum(is.na(x))})

## ------------------------------------------------------------------------
gdata <- group_by(data, year)

## ------------------------------------------------------------------------
clean_data <- data %>% filter(year != 2018)
sapply(clean_data, function(x){sum(is.na(x))})  

## ------------------------------------------------------------------------
data %>% select(country)
data %>% select(quality_of_education:citations)

## ------------------------------------------------------------------------
data %>% 
    group_by(year) %>% 
    select(year, institution, world_rank) %>% 
    top_n(-5, wt = world_rank) %>%
    ggplot(aes(x = year, y = world_rank, group = institution)) +
    geom_line(aes(color=institution)) +
    geom_point(aes(shape=institution, color=institution)) + 
    theme_bw() +
    labs(x="Year", y="World Rank", 
         title="World Ranks (2012-2018)",
         subtitle="Best World ranked Universities by CWUR") 

## ------------------------------------------------------------------------
# data %>%
#     select(world_rank, institution, year) %>%
#     filter(institution == "California Institute of Technology")

## ------------------------------------------------------------------------
data %>% group_by(country,year) %>% 
    summarise(nr = length(world_rank), minw=min(world_rank), maxw=max(world_rank), avgw=round(mean(world_rank),0)) %>%
    select(country, year, nr, minw, maxw, avgw) %>% 
    ungroup() -> ccwur

# light grey boundaries
#l <- list(color = toRGB("grey"), width = 0.5)
ccwur$hover <- with(ccwur, 
        paste("Country: ", country, '<br>', 
              "Year: ",year, "<br>",
              "Universities in top: ", nr, "<br>",
              "Min rank in top: ", minw, "<br>",
              "Max rank in top: ", maxw, "<br>",
              "Mean rank in top: ", avgw,"<br>"
              ))
# specify map projection/options
g <- list(
  showframe = TRUE,
  showcoastlines = TRUE,
  projection = list(type = 'orthogonal')
)

library(plotly)
plot_geo(ccwur, locationmode = 'country names') %>%
  add_trace(
    z = ~nr, color = ~nr, colors = 'Spectral', frame = ~year,
    text = ~hover, locations=~country) %>%
  colorbar(title = 'Number of\nuniversities in top', tickprefix = '') %>%
  layout(
    title = with(ccwur, paste('Number of universities in top')),
    geo = g
  )

