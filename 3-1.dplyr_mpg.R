## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message=FALSE, warning=FALSE, paged.print=FALSE---------------------
#install.packages("dplyr")
library(dplyr)
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg) 
str(mpg)
#View(mpg)

## ------------------------------------------------------------------------
filter(mpg, manufacturer == 'hyundai')    # 테이블의 행을 조건문으로 선택, filter(df, 필터링조건)
select(mpg, class, cty)    # 필요한 열만 선택 select(df, 열이름)
arrange(mpg, cty)   # 행을 변수(들)의 오름차순으로 정렬, 내림차순으로 정렬하려면 desc()
mutate(mpg, displ_l = displ / 61.0237)    # 기존의 변수들을 변환한 결과를 기존변수나 새 변수에 할당변수추가
summarize(mpg, mean(displ)) # 데이터프레임을 요약. 요약통계량 함수 min()m max(), mean(), sum(), sd(), median(), IQR() 사용 
summarize(group_by(mpg, cyl), mean(displ))  # 집단별로 나누기.
# left_join() 데이터합치기(열)
# bind_row()  데이터합치기(행)

## ------------------------------------------------------------------------
mpg %>% filter(manufacturer == 'hyundai')    #행선택
mpg %>% select(class, cty)    #열추출
mpg %>% arrange(cty)   #정렬
mpg %>% mutate(displ_l = displ / 61.0237)    #변수추가 (Litres to Cubic Inche)
mpg %>% summarize(mean(displ)) #통계치 산출
mpg %>% group_by(cyl) %>% summarize(mean(displ))

## ------------------------------------------------------------------------
mpg[mpg$manufacturer =='hyundai', ]
mpg[, c("class", "cty")]
mpg[order(mpg$cty), ]

## ------------------------------------------------------------------------
mpg %>% 
    select(manufacturer) %>% 
    distinct()

## ------------------------------------------------------------------------
mpg %>% 
    filter(manufacturer == 'audi') %>%
    select(model, cty, hwy)

## ------------------------------------------------------------------------
mpg %>% 
    group_by(class) %>% 
    summarise(mean(cty), mean(hwy))

## ------------------------------------------------------------------------
mpg %>% 
    group_by(manufacturer) %>% 
    summarise(m_cty=mean(cty), m_hwy=mean(hwy)) %>%
    arrange(desc(m_cty))

mpg %>% 
    group_by(manufacturer) %>% 
    summarise(m_cty=mean(cty), m_hwy=mean(hwy)) %>%
    arrange(desc(m_hwy))

## ------------------------------------------------------------------------
mpg %>% 
    group_by(manufacturer) %>% 
    summarise(m_cty=mean(cty), m_hwy=mean(hwy)) %>%
    mutate(diff=m_hwy-m_cty) %>% 
    arrange(desc(diff))

## ------------------------------------------------------------------------
range(mpg$cty)  # 9 35
cut(mpg$cty, breaks=3)  # Levels: (8.97,17.7] (17.7,26.3] (26.3,35]
range(mpg$hwy) 
cut(mpg$hwy, breaks=3)  # Levels: (12,22.7] (22.7,33.3] (33.3,44]

## ------------------------------------------------------------------------
mpg %>% filter(cty > 26.3 & hwy > 33.3) 

## ------------------------------------------------------------------------
mpg %>% 
     filter(manufacturer %in% c('honda', 'volkswagen', 'hyundai')) %>%
     summarise(mean(hwy))

## ------------------------------------------------------------------------
mpg %>% mutate(level = ifelse(hwy >= 33.3, "상",
                              ifelse(hwy >= 22.7, "중", "하"))) 

