## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
# Read tabular data into R as a dataframe 
commute <- read.table("data/commute.txt", header = TRUE, sep = "\t", stringsAsFactors = F)
# 첫 6행 읽기
head(commute)  

## ------------------------------------------------------------------------
# commute data frame 구조 확인
str(commute)  

## ------------------------------------------------------------------------
# Date 와 Depart, Date 와 Arrive 컬럼을 각각 합쳐서 POSIXct 형으로 변환
Dep <- as.POSIXct(paste(commute$Date, commute$Depart))  # paste() : 문자열 합치기
Arr <- as.POSIXct(paste(commute$Date, commute$Arrive))
# POSIXct 형으로 변환한 이후에는 기본 연산으로 시간차이를 계산할 수 있음
Arr - Dep

## ------------------------------------------------------------------------
# 시간차이 계산한 것을 Diff 변수에 할당
Diff <- Arr - Dep
# commute 데이터프레임에 Diff 변수 합치기
new_commute <- cbind(commute, Diff)  # cbind : column 기준으로 합치기
head(new_commute)

## ------------------------------------------------------------------------
# new_commute$Diff 의 데이터타입 확인 : difftime class 는 그래프를 그릴 수 없음
class(new_commute$Diff)
# Diff 벡터를 숫자형으로 변환하여 new_commute 데이터프레임에 열 추가
Diff <- as.numeric(Diff)
new_commute <- cbind(commute, Diff) 
str(new_commute)
# 상자그림 그리기
boxplot(Diff~Day, new_commute,
        main = "요일별 출근 소요시간",
        ylab = "min")

## ------------------------------------------------------------------------
# Day 컬럼의 자료형을 요인으로 바꾸고 레벨의 순서를 지정
new_commute$Day <- factor(new_commute$Day, levels = c("M", "T", "W", "H", "F"))
str(new_commute)
# 상자그림 그리기
boxplot(Diff~Day, new_commute,
        main = "요일별 출근 소요시간 (요일순)",
        ylab = "min")

## ------------------------------------------------------------------------
levels(new_commute$Day)[levels(new_commute$Day)=="M"] <- "Monday"
levels(new_commute$Day)[levels(new_commute$Day)=="T"] <- "Tuesday"
levels(new_commute$Day)[levels(new_commute$Day)=="W"] <- "Wednesday"
levels(new_commute$Day)[levels(new_commute$Day)=="H"] <- "Thursday"
levels(new_commute$Day)[levels(new_commute$Day)=="F"] <- "Friday"

boxplot(Diff~Day, new_commute,
        main = "요일별 출근 소요시간 (요일순)",
        ylab = "min")

## ------------------------------------------------------------------------
class(new_commute$Date)
new_commute$Date <- as.Date(new_commute$Date)
str(new_commute)

library(googleVis)
t1 <- gvisMotionChart(new_commute, idvar="Date", timevar="Diff",
                      option=list(width=1000,height=500))
plot(t1)

## ------------------------------------------------------------------------
as.Date("2018-09-01")
as.Date("2018/09/01")
as.Date("09/01/2018") # "0009-01-20" 으로 변환됨
as.Date("09/01/2018", format = "%m/%d/%Y")
as.Date("09/01/18", format = "%m/%d/%y" ) 

## ------------------------------------------------------------------------
# ISOdate 함수로 POSIXct 객체로 변환
ISOdate(2018, 9, 1)
# Date 객체로 변환
as.Date(ISOdate(2018, 9, 1))

## ------------------------------------------------------------------------
Sys.Date()  # 현재 날짜를 반환하는 함수
class(Sys.Date())  # Sys.Date() 함수는 Date 객체를 반환 
as.POSIXlt(Sys.Date())$wday

## ------------------------------------------------------------------------
Sys.time()  # 현재 날짜시간을 반환하는 함수
class(Sys.time())  # Sys.time() 함수는 POSIXct 객체를 반환
names(unclass(as.POSIXlt(Sys.time())))  # unclass로 리스트의 이름을 

as.POSIXlt(Sys.time())$mon  # 월 (0-11, 0 = 1월)
as.POSIXlt(Sys.time())$mday  # 해당 달의 몇번째 날 (1-31)
as.POSIXlt(Sys.time())$wday  # 해당 주의 몇번째 날 (0=6, 0= 일요일)
as.POSIXlt(Sys.time())$yday  # 해당 해의 몇번째 날 (0-365, 0 = 1월1일)

## ------------------------------------------------------------------------
when <- as.Date(ISOdate(2018, 9, 1))
as.POSIXlt(when)$mon  # 월 (0-11, 0 = 1월)
as.POSIXlt(when)$mday  # 해당 달의 몇번째 날 (1-31)
as.POSIXlt(when)$wday  # 해당 주의 몇번째 날 (0=6, 0= 일요일)
as.POSIXlt(when)$yday  # 해당 해의 몇번째 날 (0-365, 0 = 1월1일)

## ------------------------------------------------------------------------
Sys.timezone() 
as.POSIXlt(Sys.time(), "GMT")
as.POSIXlt(Sys.time(), "EST5EDT")  # the current time in New York
as.POSIXlt(Sys.time(), "EST" )     # ditto, ignoring DST
as.POSIXlt(Sys.time(), "HST")      # the current time in Hawaii
as.POSIXlt(Sys.time(), "Australia/Darwin")

## ------------------------------------------------------------------------
library(readxl)
stat <- read_xlsx("pop.xlsx", 1, col_names = TRUE)
str(stat)
head(stat)

## ------------------------------------------------------------------------
names(stat)[1] <- "month"
names(stat)[2] <- "pop"
names(stat)[3] <- "unemp"
head(stat)

## ------------------------------------------------------------------------
library(latticeExtra)
obj1 <- xyplot(pop ~ month, stat, type = "l", lwd=2)
obj2 <- xyplot(unemp ~ month, stat, type = "l", lwd=2)
doubleYScale(obj1, obj2, add.ylab2 = TRUE)


## ------------------------------------------------------------------------
library(googleVis)
Line <- gvisLineChart(stat)
plot(Line)

## ------------------------------------------------------------------------
Line2 <- gvisLineChart(stat, "month", c("pop","unemp"),
                       options=list(
                         series="[{targetAxisIndex: 0},
                                 {targetAxisIndex:1}]",
                         vAxes="[{title:'경제활동인구 (천명)'}, {title:'실업률 (%)'}]"
                       ))
plot(Line2)

## ------------------------------------------------------------------------
library(knitr)
purl("1-1a.Timedata.RMD", encoding = "UTF-8")

