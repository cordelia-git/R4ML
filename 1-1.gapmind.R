## ------------------------------------------------------------------------
#install.packages("gapminder")
library(gapminder)
head(gapminder)

## ------------------------------------------------------------------------
str(gapminder)

## ------------------------------------------------------------------------
summary(gapminder)  # 요약통계량
mean(gapminder$lifeExp)  # 평균값
median(gapminder$lifeExp)  # 중앙값
sd(gapminder$lifeExp)  # 표준편차 standard deviation
var(gapminder$lifeExp)  # 분산 variance
min(gapminder$lifeExp)  # 최소값
max(gapminder$lifeExp)  # 최대값
quantile(gapminder$lifeExp)  # 사분위수
fivenum(gapminder$lifeExp) # 최소값, 1사분위, 중위수, 3사분위, 최대값
round(fivenum(gapminder$lifeExp), 1) # 소수점 자리수 지정 (반올림)
ceiling(fivenum(gapminder$lifeExp)) # 올림
abs(fivenum(gapminder$lifeExp)) # 절대값

## ------------------------------------------------------------------------
library(Hmisc)
Hmisc::describe(gapminder)  # 변수별로 다양한 기술통계량을 한번에 구해줌

## ------------------------------------------------------------------------
gapminder[gapminder$country == "Korea, Rep.", "pop"]
gapminder[gapminder$country == "Korea, Rep.", c("year", "pop")]

## ------------------------------------------------------------------------
# gapminder$country  # $ 연산자로 열(column) 인덱스
gapminder[, c("year", "pop", "gdpPercap")]  # 열 이름을 벡터화하여 선택
gapminder[, 1:3]  # 숫자 인덱스

## ------------------------------------------------------------------------
gapminder[gapminder$country == "Korea, Rep.", ] 
gapminder[gapminder$lifeExp == 23.599, ] 
gapminder[gapminder$year == 2007, ]  
gapminder[gapminder$country == "Korea, Rep." & gapminder$year == 2007, ]  # Boolean AND operator
gapminder[1:10, ]  # 숫자 인덱스

## ------------------------------------------------------------------------
gapminder[order(gapminder$year, gapminder$country), ]  # 연도순, 국가순 정렬

## ------------------------------------------------------------------------
names(gapminder) # 변수명 조회
names(gapminder)[6] <- 'gdpPerCap' # 6번째 변수명 변경

## ------------------------------------------------------------------------
#  우리나라 표기방식 확인 : 유일한 값 확인
unique(gapminder$country)  # 열 중복제거
kor <- gapminder[gapminder$country == "Korea, Rep.", ]

## ------------------------------------------------------------------------
kor <- kor[, c(3, 4, 6)] # 3, 4, 6 행만 추출
names(kor) <- c("year", "life_exp", "gdp_cap") # 칼럼명 변경
str(kor)

## ------------------------------------------------------------------------
plot(kor$gdp_cap, kor$life_exp, type = "p")
plot(kor$gdp_cap, kor$life_exp, 
     main = "우리나라의 인당 국민소득과 기대수명",
     sub = "1952년~2007년",
     xlab = "인당 국민소득(US$) ",
     ylab = "기대수명",
     panel.first = grid(8, 8),
     pch = 1, cex = 0.8, col = "blue")

## ------------------------------------------------------------------------
#install.packages("latticeExtra")
library(latticeExtra)

## ------------------------------------------------------------------------
xyplot(life_exp + gdp_cap ~ year, kor, xlim = c(1955, 2005), type = "l")

## ------------------------------------------------------------------------
obj1 <- xyplot(life_exp ~ year, kor, xlim = c(1955, 2005), type = "l", lwd=2)
obj2 <- xyplot(gdp_cap ~ year, kor, xlim = c(1955, 2005), type = "l", lwd=2)
doubleYScale(obj1, obj2, add.ylab2 = TRUE)

## ------------------------------------------------------------------------
update(doubleYScale(obj1, obj2, 
    text = c("Life Expectation (year)", "GDP per Capita (US$)")),
    par.settings = simpleTheme(col = c('red','blue'), lty = 1:2))

## ------------------------------------------------------------------------
## multi-panel
data(SeatacWeather)
temp <- xyplot(min.temp + max.temp ~ day | month,
               data = SeatacWeather, type = "l", layout = c(3, 1))
rain <- xyplot(precip ~ day | month, data = SeatacWeather, type = "h")

doubleYScale(temp, rain, style1 = 0, style2 = 3, add.ylab2 = TRUE,
   text = c("min. T", "max. T", "rain"), columns = 3)

## ------------------------------------------------------------------------
# library(dplyr)
# library(ggplot2)
# gapminder %>% filter(year == 2007) %>%
#     ggplot(aes(gdpPercap, lifeExp)) +
#     geom_point() +
#     scale_x_log10() +
#     ggtitle("Gapminder data for 2007")

## ------------------------------------------------------------------------
# library(ggplot2)
# gapminder %>% filter(year == 2007) %>%
#     ggplot(aes(gdpPercap, lifeExp)) +
#     geom_point(aes(size=pop, col=continent)) +
#     scale_x_log10() +
#     ggtitle("Gapminder data for 2007")

## ------------------------------------------------------------------------
cor(kor$life_exp, kor$gdp_cap)

## ------------------------------------------------------------------------
cor.test(kor$life_exp, kor$gdp_cap)

## ------------------------------------------------------------------------
lm1 <- lm(life_exp ~ gdp_cap, kor)
summary(lm1) 

## ------------------------------------------------------------------------
boxplot(lifeExp~continent, gapminder, main="대륙별 기대수명")

## ------------------------------------------------------------------------
summary(aov(lifeExp ~ continent, gapminder))

## ------------------------------------------------------------------------
anova(lm(lifeExp ~ continent, gapminder))

## ------------------------------------------------------------------------
europe <- gapminder[gapminder$continent == "Europe", ]
boxplot.stats(europe$lifeExp)$out

## ------------------------------------------------------------------------
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

which(is_outlier(europe$lifeExp))

## ------------------------------------------------------------------------
europe[c(1, 2, 37, 38, 49, 193, 241, 265, 337, 338, 339, 340, 341, 342), ] 

## ------------------------------------------------------------------------
index_vector <- is_outlier(europe$lifeExp)
europe[index_vector, ]

## ------------------------------------------------------------------------
europe_lifeExp_outlier <- europe[c(1, 2, 37, 38, 49, 193, 241, 265, 337, 338, 339, 340, 341, 342), ] 
table(europe_lifeExp_outlier$year)
hist(europe_lifeExp_outlier$year)

## ------------------------------------------------------------------------
#stem and leaf 
stem(europe_lifeExp_outlier$year)

## ------------------------------------------------------------------------
aggregate(lifeExp ~ continent, gapminder, mean)
aggregate(lifeExp ~ continent, gapminder, median)

## ----message=FALSE, warning=FALSE, paged.print=FALSE---------------------
library(knitr)
purl("gapmind.Rmd", encoding = "UTF-8")

