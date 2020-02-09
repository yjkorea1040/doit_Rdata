# ch15 R 내장함수, 변수타입과 데이터구조
# 데이터 -> 데이터분석
# 연속변수(number,) -> 사칙연산 가능
# 범주변수 (명목형: 남성, 여성,... 경기도, 서울,제주..., 지역번호) -> 사칙연산 불가
##따라서 범주변수는 전처리, 인식할 수 있는 이름으로 변수명 변경

exam <- read.csv("csv_exam.csv")

View(exam)

exam[]
#행번호
exam[1,]
exam[2,]
exam[,2:4] 선택(추출)

exam[exam$class==1,] #class가 1인 행 추출

exam[exam$math>=80,] #math 점수가 80이상인 행 추출

exam[exam$class==1 & exam$math>=50,]
exam[exam$english<90 | exam$science<50,]
#열번호 선택(추출)
exam[,1]

#변수(속성)이름으로 - 선택(추출)
exam[, "class"]
exam[, "math"]
exam[, "science"]
exam[, "english"]

# C 사용하기
exam[,c("class", "math", "english")]

exam[1,3]
exam[1,"math"]

exam[1,c("class","math","english")]

#-------------
#dplyr함수, 내장함수
#dplyr함수가 내장함수보다 코드 읽기가 더 쉽다(즉, 가독성이 높고 이해하기가 쉽다)

#1)내장함수 - 통계집계 요약 aggregate()
exam$tot<-(exam$math+exam$english+exam$science)/3
aggregate(data=exam[exam$math>=50&exam$english>=80,],tot~class, mean)
#2)dplyr함수

library(dplyr)

exam %>% 
  filter(math>=50&english>=80) %>% 
  mutate(tot=(math+english+science)/3) %>% 
  group_by(class) %>% 
  summarise(mean=mean(tot))

#mpg 데이터 사용해서 분석계산하기

library(ggplot2)

mpg<-as.data.frame(ggplot::mpg)

mpg %>% 
  mutate(tot=(cty+hwy)/2) %>% 
  filter(class=="compact"|class=="suv") %>% 
  group_by(class) %>% 
  summarise(mean_tot=mean(tot))

#15-2 변수타입

var1<-c(1,2,3,1,2)
var2<-factor(c(1,2,3,1,2))

var1
var2

str(var1) # -> 숫자 연속형 변수로 계산 가능
str(var2) # -> 범주형 변수 -> 계산 불가

class(var1)#numeric
class(var2) #factor

levels(var1)
levels(var2)

var1+2
var2+2


var3<-c("a","b","c","b","e")
var4<-factor(c("a","b","c","b","e"))

var3
var4

var2<-as.numeric(var2)
mean(Var2)
mean(var2)

class(var2)

levels(var2)

as.Date(var1)

as.numeric(var2)

#15-3 데이터구조


#벡터: c(,,,)->1차원: 같은 데이터타입으로 모아놓은 변수타입
#매트릭스 -> 2차원: 1차원에서 2차원으로 확장해놓은 같은 타입의 데이터를 모아놓은 변수타입
#어레이 -> 다차원: 2차원 이상의 매트릭스, 같은 타입의 데이터를 모아놓은 변수타입
#리스트 -> 다차원: 서로다른 테이터를 포함하는 데이터구조
#데이터프레임->2차원: 다른 데이터타입들을 모아놓은 데이터 구조


# 벡터만들기

a<-1
a

b<-"hello"
b

class(a)
class(b)
levels(a)
levels(b)
c<-c(1,2,3,4)
d<-c('a','b','c','d')
c
d
levels(c)
class(c)
levels(d)
class(d)

