install.packages("dplyr")
library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

# dplyr의 패키지 filter()를 이용해 특정 부류의 데이터의 필터값만 추출
# class 1인 행만 출력
exam %>% filter(class == 1)
table(exam)

exam %>% filter(class==2)
View(exam)
# 1반이고 2반인 and 값은 키보드 enter위에 | 이용
exam %>% filter(class==1 | class==2)
exam %>% filter(class>=3)
exam %>% filter(class!=4)
exam %>% filter(math>70 & english >=80)
exam %>% filter(math>70 & english >=80 & class==2)
exam %>% filter(math>80 & english>80 & science >80)
exam %>% filter(math>=80 & english>=80)
exam %>% filter(class==1 | class==2 | math>=50)

exam %>% filter(class %in% c(1,3,5))
exam %>% filter(class==1 | class==3 | class==5)

class1 <-exam %>% filter(class==1)
class2 <-exam %>% filter(class==2)

mean(class1$math)
mean(class2$math)
mean(class1$math,class2$english)

# ==================================================
# mpg 데이터를 이용한 분석 연습
install.packages("ggplot2")

mpg <- as.data.frame(ggplot2::mpg)

# displ 4 이하

mpg_a <- mpg %>% filter(displ <= 4)

mpg <- as.data.frame(ggplot2::mpg)
View(mpg)
mpg_a <- mpg %>% filter(displ <=4)
mpg_b <- mpg %>% filter(displ>=5)

max(mpg_a)

mean(mpg_a$hwy)
mean(mpg_b$hwy)

mpg_audi <- mpg %>% filter(manufacturer == "audi")
mpg_toyota <- mpg %>%  filter(manufacturer == "toyota")

mean(mpg_toyota$cty)
mean(mpg_audi$cty)

mpg_new <-mpg %>%  filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpg_new$hwy)

exam %>% select(math)
exam %>%select(english)

exam %>% select(class, math, english)
# -"" 값은 변수 제외하기
exam %>% select(-math)
exam %>% select(-english)

head(exam)

exam %>% select(-math, -english)

exam %>% filter(class == 1) %>% select(english)

exam %>% 
  filter(class==2) %>% 
  select(math) %>% 
  head(3)

exam %>% 
  filter(class==1) %>% 
  select(english) %>% 
  tail(2)  

exam %>% arrange(math)
exam %>% arrange(id)

exam %>% arrange(math)

exam %>% arrange(desc(english))

View(exam)

exam %>% 
  arrange(class, math) %>% 
  head(3)

exam %>% arrange(desc(class, english)) %>% 
  head(3)

exam %>% arrange(desc(math), desc(english))

exam %>% 
  arrange(desc(math), desc(english), class)

mpg_audi <- mpg %>% arrange(desc(hwy))
View(mpg_audi)

mpg %>% select(manufacturer)
View(mpg)

mpg <-as.data.frame(ggplot2::mpg)
View(mpg)

# == 다음에 숫자는 관계없으나 문자가 나올 때는 "" 사이에 문자입력 필요

mpg %>%
  filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

mpg %>% 
  filter(manufacturer == "honda") %>% 
  arrange(desc(cty)) %>% 
  head(3)

exam %>% 
  mutate(total=math+english+science) %>% 
  head

exam %>% arrange(desc(total))

exam %>% 
  mutate(total=math+english+science,
         mean=(math+english+science)/3) %>% 
  head

exam_mut <- exam %>% 
  mutate(total=math+english+science,
         mean=(math+english+science)/3)

exam_mut %>% arrange(desc(total))

exam %>% 
  mutate(test=ifelse(science>=60, "pass", "fail")) %>% 
  head

exam %>% 
  mutate(total=math+english+science) %>% 
  arrange(total) %>% 
  head

exam %>% 
  mutate(total=math+english+science) %>% 
  arrange(desc(total)) %>% 
  head

View(mpg)

mpg_data <- mpg %>% mutate(total=hwy+cty)
View(mpg_data)
mpg_data %>% 
  mutate(total=hwy+cty, 
         mean=(hwy+cty)/2) %>% 
  head

mpg_data %>% 
  arrange(desc(mean))

View(mpg_data)

mpg_data <- mpg %>% mutate(total=hwy+cty,
                           mean=(hwy+cty)/2)

mpg_data %>% 
  arrange(desc(mean)) %>% 
  head(3)

exam %>% summarise(mean_math=mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math=mean(math))

# 06-6 집단별로 요약하기:group_by(), summarise()

exam %>% summarise(mean_math=mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math=mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=median(math),
            sd_math=sd(math),
            n=n())

mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  head(10)


mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate(tot=(cty+hwy)/2) %>% 
  summarise(mean_tot=mean(tot)) %>% 
  arrange(desc(mean_tot)) %>%
  head(5)


mpg <-as.data.frame(ggplot2::mpg)  
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty=mean(cty))

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty))

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy=mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

mpg %>% 
  filter(class=="compact") %>% 
  group_by(manufacturer) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

# 06-7 데이터합치기: left_join(), bind_row()
test1<-data.frame(id=c(1,2,3,4,5),
                  midterm=c(60,80,70,90,85))
test1

test2<-data.frame(id=c(1,2,3,4,5),
                  final=c(70,83,65,95,80))
test2
# dplyr 패키지의 left(join()을 사용해서 데이터 합치기
library(dplyr)
total <- left_join(test1, test2, by="id")
total

total2 <- right_join(test1, test2, by="id")
total2

name <-data.frame(class=c(1,2,3,4,5),
                  teacher=c("kim", "lee", "park", "choi", "jung"))
name
exam
exam_teacher<-left_join(exam, name, by="class")
exam_teacher

# 세로합치기

group_a<-data.frame(id=c(1,2,3,4,5),
                    test=c(60,80,70,90,85))
group_a
group_b<-data.frame(id=c(6,7,8,9,10),
                    test=c(70,83,65,95,80))
group_b

group_all<-bind_rows(group_a, group_b)
group_all
group_all %>% 
  head(3)

group_ab<-bind_cols(group_a,group_b)
group_ab
group_ab<-bind_cols(test1, test2)
group_ab
group_ab<-bind_cols(test1, test2) %>% 
  select(-id1)
group_ab

fuel<-data.frame(fl=c("c","d","e","p","r"),
                 price_fl=c(2.35,2.38,2.11,2.76,2.22),
                 stringsAsFactors=F)
fuel
mpg

mpg<-as.data.frame(ggplot2::mpg)

head(mpg)
mpg<-left_join(mpg, fuel, by="fl")
mpg
mpg %>% 
  head(3)

mpg %>% 
  select(model, fl, price_fl) %>% 
  head(5)

?midwest

midwest<-as.data.frame(ggplot2::midwest)
midwest

midwest %>% 
  head(2)
View(midwest)

#  p.160 quiz
midwest <- midwest %>% 
  mutate(ratio_child=(poptotal-popadults)/poptotal*100)

midwest %>% 
  arrange(desc(ratio_child)) %>% 
  select(county, ratio_child) %>% 
  head(5)

midwest <- midwest %>% 
  mutate(grade=ifelse(ratio_child>=40, "large",
                      ifelse(ratio_child>=30, "middle", "small")))

table(midwest$grade)

midwest %>% 
  mutate(ratio_asian=(popasian/poptotal)*100) %>% 
  arrange(ratio_asian) %>% 
  select(state, county, ratio_asian) %>% 
  head(10)



# 07-1

df <-data.frame(sex=c("M","F", NA, "M", "F"),
                score=c(5,4,3,4,NA))
df

is.na(df)

table(is.na(df))

table(is.na(df$sex))
table(is.na(df$score))

mean(df$score)
sum(df$score)

library(dplyr)

df %>% 
  filter(is.na(score))

df %>% 
  filter(!is.na(score))

df %>% 
  filter(!is.na(sex),!is.na(score))

df %>% 
  filter(!is.na(sex) & !is.na(score))

na.omit(df)

mean(df$score, na.rm=T)

exam <- read.csv("csv_exam.csv")
exam[c(3,8,15),"math"]<-NA
exam


exam %>% summarise(mean_math=mean(math))

exam %>% summarise(mean_math=mean(math,na.rm=T))

exam %>% 
  summarise(mean_math=mean(math,na.rm=T),
            sum_math=sum(math,na.rm=T),
            median_math=median(math,na.rm=T))

mean(exam$math, na.rm=T)

exam$math<-ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))

exam
mean(exam$math)

mpg<-as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212), "hwy"]<-NA
mpg

table(is.na(mpg$hwy))
table(is.na(mpg$drv))

mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))

table(is.na(mpg$hwy))
table(is.na(mpg$drv))

mpg %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T))

outlier<-data.frame(sex=c(1,2,1,3,2,1),
                    score=c(5,4,3,4,2,6))
outlier

table(outlier$sex)
table(outlier$score)

outlier$sex<-ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

outlier$score<-ifelse(outlier$score == 6, NA, outlier$score)
outlier

outlier %>% 
  filter(!is.na(sex)&!is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score=mean(score))

boxplot(mpg$hwy)

boxplot(mpg$hwy)$stats

mpg$hwy<-ifelse(mpg$hwy<12 | mpg$hwy>37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T))

# p.178 quiz
mpg<-as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93),"drv"]<-"k"
mpg[c(29,43,129,203),"cty"]<-c(3,4,39,42)
# myself
table(is.na(mpg$drv))
table(outlier(mpg$drv))
# answer
table(mpg$drv)

mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"), mpg$drv, NA)

# %in% 기호와 c()함수를 이용해 조건 목록을 입력하면, %in% 기호는 변수의 값이 지정한 조건 목록에 해당하는지 확인하는 기능을 함.예를 들어 class%in% c(1,3,5)를 입력할 경우, 1,3,5반에 해당하는 값을 추출

table(mpg$drv)

boxplot(mpg$cty)

boxplot(mpg$cty)$stats

mpg$cty <- ifelse(mpg$cty<9 | mpg$cty>26, NA, mpg$cty)
table(is.na(mpg$cty))

boxplot(mpg$cty)

# myself-Q3: fail
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_cty=mean(cty,na.rm=T))

#  answer-Q3
mpg %>% 
  filter(!is.na(drv)&!is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty=mean(cty))

# ch08
library(ggplot2)
ggplot(data=economics, aes(x=date, y=unemploy)) +geom_line()
?economics

head(economics)
ggplot(data=economics, aes(x=date, y=unemploy)) +geom_line() +xlim(1990,2020)

?ggplot

ggplot(data=mpg, aes(x=drv)) + geom_bar()

