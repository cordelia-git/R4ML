## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE----------
library(dplyr)
library(ggplot2)
library(reshape)
library(stringr)
library(knitr)

## ------------------------------------------------------------------------
subway <- read.csv('data/subway.csv', header=TRUE, stringsAsFactors=FALSE)
subway <- subway %>% 
    select(stat_name, income_date, on_tot) # 역명, 일자, 총 탑승인원수 3개 변수만 선택
glimpse(subway)

## ------------------------------------------------------------------------
subway$year <- factor(str_sub(subway$income_date, 1, 4)) # 연도 변수
subway$month <- factor(str_sub(subway$income_date, 5, 6)) # 월 변수
distinct(subway, year) # 기록된 자료의 연도를 중복없이 출력

## ------------------------------------------------------------------------
subway %>% 
  distinct(year, month) %>% 
  tail(10) # 맨뒤 10개 자료 출력
subway2 <- subway %>% filter(year != "2014") # 2014년 제외

## ------------------------------------------------------------------------
subway2 %>% 
  distinct(stat_name) %>% # 역이름 중복없이 출력
  arrange(stat_name) %>%  # 가나다순 정렬
  head(5) # 앞에서 5개

## ------------------------------------------------------------------------
subway2 <- subway2 %>% 
    mutate(stat_name = 
           ifelse(grepl("\\(", subway2$stat_name), # T/F 반환
                  sapply(strsplit(subway2$stat_name, "\\("), # ( 기준 단어분리
                         function(x) x[1]), stat_name))
# 열이름에 ( 가 있으면, ( 를 기준으로 나눈 앞부분의 문자들을 취하고, ( 가 없으면 원래의 역이름을 그대로. 

## ------------------------------------------------------------------------
subway2 %>% 
  distinct(stat_name) %>% # 역명을 중복없이 출력
  arrange(stat_name) %>%  # 역명 가나다순 정렬
  head(5)

## ------------------------------------------------------------------------
subname <- read.csv('data/subway_latlong.csv', header = TRUE, stringsAsFactors = FALSE, skip = 1) # skip=1 파일의 첫행을 읽지 않음.

subname <- subname %>%
  select(STATION_NM, LINE_NUM)

## ------------------------------------------------------------------------
subway3 <- left_join(subway2, subname, by = c("stat_name" = "STATION_NM"), copy=FALSE) 
# copy=FALSE 로 역명 컬럼이 중복되지 않도록 함.

glimpse(subway3) 

## ----message=FALSE, warning=FALSE----------------------------------------
df1 <- subway3 %>% 
  filter(LINE_NUM %in% c(1:8)) %>% # 1~8호선 자료만 추출
  select(LINE_NUM, stat_name, year, on_tot) %>% 
  group_by(LINE_NUM, stat_name, year) %>% 
  summarize(y.total = sum(on_tot)) 

df1$LINE_NUM <- factor(paste0(df1$LINE_NUM, "호선")) # "~호선" 방식표기
head(cast(df1, LINE_NUM + stat_name ~ year)) # long form 을 wide formd 으로

## ------------------------------------------------------------------------
df2 <- df1 %>%  
  select(LINE_NUM, stat_name, y.total) %>% 
  group_by(LINE_NUM, stat_name) %>% 
  summarize(total = sum(y.total))

df2$stat_name <- factor(df2$stat_name, levels = df2$stat_name)
head(df2)

## ------------------------------------------------------------------------
col <- c("#00498B", "#009246", "#F36630", "#00A2D1", "#5940FF", "#CC660D", "#4D8000", "#FF33A6")

label_ko_num = function(num) {
  ko_num = function(x) {
    new_num = x %/% 10000
    return(paste(new_num, '만명', sep = ''))}
  return(sapply(num, ko_num))} # ~만명 단위으로 변경

ggplot(df2, aes(x=stat_name, y=total, fill=LINE_NUM)) +
  geom_bar(stat="identity", colour = "white") +
  scale_fill_manual(name="노선", values=col) +
  scale_y_continuous(labels = label_ko_num) +
  theme_bw(base_size = 10, base_family = "NanumGothic") +
  labs(x="지하철역", y="탑승객수",
        title="노선별 지하철역 누적 탑승객수",
        subtitle="2010~2013 4년 누적 승객수 기준",
        caption="NOTE: 색상출처 - 위키피디아") +
  theme(axis.title=element_text(colour="blue", size = 10)) +
  theme(axis.text.x=element_blank()) + # 지하철역명 안 나타나게
  theme(axis.text.y=element_text(size = 8)) +
  theme(legend.position="right") # 범례위치 오른쪽

## ------------------------------------------------------------------------
df3 <- df2 %>% 
  arrange(desc(total)) %>% 
  head(10)
df3

## ----message=FALSE, warning=FALSE----------------------------------------
ggplot(df3, aes(x = reorder(stat_name, -total), y = total, fill = LINE_NUM)) +
  geom_bar(stat="identity", colour = "white") +
  scale_fill_manual(name="노선", values=c("#00498B", "#5940FF", "#4D8000")) +
  scale_y_continuous(labels = label_ko_num, limits = c(0, max(df2$total))) +
  labs(x="지하철역", y="탑승객수",
      title="탑승객 수 기준 상위 10개 역",
      subtitle="2010~2013 4년 누적 승객수 기준") +
  theme_bw(base_size = 10, base_family = "NanumGothic") +
  theme(axis.title=element_text(colour="blue", size = 10)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(legend.position="right")

## ------------------------------------------------------------------------
df4 <- df2 %>% 
  arrange((total)) %>% 
  head(10)
df4

## ----message=FALSE, warning=FALSE----------------------------------------
ggplot(df4, aes(x = reorder(stat_name, total), y = total, fill = LINE_NUM)) +
  geom_col() +
  scale_fill_manual(name="노선", values=c("#00498B", "#5940FF", "#CC660D", "#4D8000")) +
  scale_y_continuous(labels = label_ko_num, limits = c(0, max(df2$total))) +
  labs(x="지하철역", y="탑승객수",
      title="탑승객 수 기준 하위 10개 역",
      subtitle="2013~2013 4년 누적 승객수 기준") +
  theme_bw(base_size = 10, base_family = "NanumGothic") +
  theme(axis.title=element_text(colour="blue", size = 10)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(legend.position="right")

## ----message=FALSE, warning=FALSE----------------------------------------
top10_stat_num <- df3$stat_name # 상위 10개역명 변수저장
df6 <- subway3 %>% 
  select(stat_name, year, month, on_tot) %>% 
  filter(stat_name %in% top10_stat_num & year == "2013") %>% 
  group_by(stat_name, month) %>% 
  summarize(m.total = sum(on_tot))

df6$stat_name <- factor(df6$stat_name)
df6$month <- paste0(df6$month, "월") # month 표현을 '~월'로 변환
cast(df6, stat_name ~ month) #long form 을 wide formd 으로 재구조화

## ----message=FALSE, warning=FALSE----------------------------------------
ggplot(df6, aes(x=month, y=m.total, colour=stat_name, group=stat_name)) +
  geom_line() + 
  theme_bw(base_size = 10, base_family = "NanumGothic") +
  geom_point(size=4, shape=19, alpha=0.5) + 
  scale_x_discrete("2013년") + 
  scale_y_continuous(labels = label_ko_num) +
  ylab("월별 탑승객수") + 
  scale_colour_discrete(name=c("지하철역")) + 
  ggtitle("2013년 상위 10개역의 월별 탑승객 추이")

## ------------------------------------------------------------------------
df7 <- df2 %>% 
  group_by(LINE_NUM, stat_name) %>% 
  summarize(sum.tot = sum(total))

df8 <- df7 %>% 
  group_by(LINE_NUM) %>% 
  summarize(mean.tot = mean(sum.tot)) %>% 
  mutate(pct = round(mean.tot/sum(mean.tot), 4)*100)

## ------------------------------------------------------------------------
col2 <- c("#00498B", "#009246", "#F36630", "#00A2D1", "#5940FF", "#CC660D", "#4D8000", "#FF33A6", "#BB1833", "#6E98BB")

label <- paste(df8$LINE_NUM, "(", df8$pct, "%", ")", sep = "")
#label <- paste0(df8$LINE_NUM, "(", df8$pct, "%", ")")

label <- paste(df8$LINE_NUM, "(", df8$pct, "%", ")", sep = "")
pie(df8$mean.tot, labels=label, col=col2, border="lightgray", main="노선별 평균 지하철 탑승객 수")


## ------------------------------------------------------------------------
subway3$yearmonth <- as.Date(paste(subway3$year, subway3$month, "01", sep = "-")) 
df9 <-  subway3 %>%
  select(LINE_NUM, yearmonth, on_tot) %>% 
  group_by(LINE_NUM, yearmonth) %>% 
  summarize(ym.tot = sum(on_tot))  
head(df9, 3)

## ------------------------------------------------------------------------
col2 <- c("#00498B", "#009246", "#F36630", "#00A2D1", "#5940FF", "#CC660D", "#4D8000", "#FF33A6", "#BB1833", "#6E98BB")
  
ggplot(df9, aes(x=yearmonth, y=ym.tot, fill=LINE_NUM)) +
  geom_area(colour="white") +
  scale_fill_manual(name="노선", values = col2) +
  scale_y_continuous(labels = label_ko_num) +
  theme_bw(base_size = 10, base_family = "NanumGothic") +
  labs(x="Year", y="누적승객수",
        title="노선별 누적 승객수 상대비교",
        subtitle="각 노선의 2010~2013년 월별 누적 탑승객수") +
  theme(axis.title=element_text(colour="red", size = 10)) +
  theme(axis.text.x=element_text(hjust=c(0, 0.5, 0.5, 0.5, 1))) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(legend.position="right")

## ------------------------------------------------------------------------
table(is.na(subway3$LINE_NUM))

## ------------------------------------------------------------------------
no_LINE_NUM <- subway3 %>% filter(is.na(LINE_NUM))
unique(no_LINE_NUM$stat_name)

## ------------------------------------------------------------------------
# 실행하지 않음. 
# subway2 %>% 
#   mutate(stat_name = ifelse(subway2$stat_name == "동대문역사문화공원5", "동대문역사문화공원", subway2$stat_name)) 
# 
# subname$STATION_NM[28] <- "이수"

