#######################################
#project ch03 ch03_1_variable.r
#
a=1
b=2
c="hello world"
a=12
c<-a+b;
c
# --------------
var1<-seq(1, 10, by=2);
var1
var2<-seq(1,10,by=4);
var2

a=10;
if(a>1000){
  1000;
} else if(a>100){
  100;
  
} else if(a<100){
  10;
} else if(a<=5){
  1;
} else{
  0;
}

str1="hello world"
a
c<-20
c

abc_123=20;
abc123=20;
#변수생성이름-숫자시작,-<-시작은 오류
#알파벳으로시작해야합니다
#123abc=20;
#_abc=20;
abc-av=20;
-23=10;
-a=11;
-aa=11;

aa
# -------------------------------
var1<-seq(1, 10, by=2);
var2<-c(1,2,4,7,8)

var3<-c(1:6)
var3
view(Var1)
var4<-c(1:10,by=3)
var4

var1+var2
var1+var4

str1<-"a"
str2<-"text"
str3<-"hello world"

#str1+str2 ##오류발생

str4<-c("a","b","C")

e=1.55


x<-c(1,2,3)
x
mean(x)
max(X)
min(x)
max(x)
str5<-c("hello","world","is","good")
num(str5)
str5
paste(str5)
paste(str5,collapse=",")
max(var3)

var3_max=max(var3)
var3_min=min(var3)
var3_mean=mean(var3)

var3_max
var3_min
var3_mean

paste(str5,collapse="*")
paste(str5,collapse="")
paste(str5)

# ----------------------------------------
#package 설치하고 사용하기


install.packages("ggplot2");

library(ggplot2);

qplot(str4)
qplot(var1)

x<- c("a","a","b","c","d")
qplot("x")

paste(str5, str1,collapse = ",")

qplot(data=mpg,x=cty)
qplot(data=mpg,x=drv,y=hwy)
?mpg        
??mpg
qplot(data=mpg,x=drv,y=hwy,geom="line")
qplot(data=mpg,x=drv,y=hwy,geom="boxplot")        
qplot(data=mpg,x=drv,y=hwy,geom="boxplot",colour=drv)        
?mpg        

head(mpg)
raw(1)     
column(model)
?qplot
# ------------------------------------

var5=("80","60","70","50","90")
mean(var5)

score<-c(80,60,70,50,90)
score

mean_score<-mean(score)
mean_score
score_mean

english<-c(90,80,6,70)
english

# -----------------
english<-c(90,80,60,70)
math<-c(50,60,100,20)
"english:"

english
math

class<-c(1,1,2,2)


df_midterm<-data.frame(english, math)
df_midterm<-data.frame(english, math, class)
df_midterm

mean(df_midterm$english)
mean(df_midterm$math)

class
head(class)

mean(df_midterm$class)

df_midterm<-data.frame(english=c(90,80,60,70),math=c(50,60,100,20),class=c(1,1,2,2))
df_midterm
# 데이터프레임생성오류
df_midterm<-data.frame(english<-c(90,80,60,70),math<-c(50,60,100,20),class<-c(1,1,2,2))
df_midterm

f

df_products<-data.frame(fruit=c("apple","strawberry","watermelon"),price=c("1800","1500","3000"),sales=c("24","38","13"))
df_products

mean(df_products$price)
mean(df_products$sales)

mean(df_products$price)


sales<-data.frame(fruit=c("apple","strawberry","watermelon"),price=c(1800,1500,3000),sales=c(24,38,13))
sales

mean(sales$price)
mean(sales$sales)


# --------------------------------------------------
# 04-03 89p

# xlsx
install.packages("readxl");
library(readxl);

df_excel<-read_excel("excel_exam.xlsx")
df_excel<-read_excel("C:/works/ch04/excel_exam.xlsx")

View(df_excel);

df_excel_mean_math=mean(df_excel$math)
df_excel_mean_english<-mean(df_excel$english)
df_excel_mean_science<-mean(df_excel$science)
df_excel_mean_math
df_excel_mean_english
df_excel_mean_science

# excel_exam_novar.xlsx
df_excel_novar<-read_excel("excel_exam_novar.xlsx")
df_excel_novar<-read_excel("C:/works/ch04/excel_exam_novar.xlsx", col_names=F)
df_excel_novar

View(df_excel_novar)


# excel_exam_sheet.xlsx
df_excel_sheet<-read_excel("C:/works/ch04/excel_exam_sheet.xlsx", col_names=T,sheet=3)
df_excel_sheet

View(df_excel_sheet)

# csv 읽기
# csv_exam.csv
# 숫자가 아닌 문자가 들어 있는 파일을 읽어올 때 옵션이 필요함.
df_csv_exam<-read.csv("csv_exam.csv")
df_csv_exam<-read.csv("C:/works/ch04/csv_exam.csv", stringsAsFactors = F)
df_csv_exam

View(df_csv_exam);

df_midterm
write.csv(df_midterm,file="df_midterm.csv")
df_midterm_csv<-read.csv("df_midterm.csv");
df_midterm_csv


# RData 저장하기

save(df_midterm,file="df_midterm.rda")

# RData 불러오기
rm(df_midterm)
df_midterm #오류발생

load("df_midterm.rda")
df_midterm


# xlsx 불러오기


english<-c(90,80,60,70)
math<-c(50,60,100,20)
english
math
data.frame(english,math)

install.packages("readxl");
library(readxl)
df_exam<-read_excel("excel_exam.xlsx")


df_csv_exam<-read.csv("csv_exam.csv")
write.csv(df_midterm,file="df_midterm.csv")
df_midterm_csv

load("df_midterm.rda")
df_midterm.rda
save(df_midterm,file="df_midterm.rda")
df_midterm.rda

head(df_midterm,2)
head(df_midterm.csv)
head(df_exam)
tail(df_midterm,2)
View(df_exam)

# 데이터 대략 파악하기
# p100

df_csv_exam

head(df_csv_exam)
dim(df_csv_exam)
summary(df_csv_exam)

tail(df_csv_exam)
tail(df_csv_exam,10)
str(df_csv_exam)

install.packages("ggplot2")
library("ggplot2")
library(ggplot2)
ggplot2

mpg<-as.data.frame(ggplot2::mpg)
mpg

head(mpg)
tail(mpg)
str(mpg)
Veiw(mpg)
View(mpg)
dim(mpg)
summarty(mpg)
summary(mpg)
summary(mpg$hwy)
summary(mpg$manufacturer)
summary(mpg$audi)

?dplyr

#변수명바꾸기
rvar1=c(1,2,1)
rvar2=c(2,3,2)
df_raw<-data.frame(rvar1,rvar2)
df_raw

#rename df's column name
install.packages("dplyr")
library(dplyr)
df_new<-df_raw
df_new<-rename(df_new,v2=rvar2)
df_new
df_raw
df_new

library(ggplot2);
mpg_new<-mpg
mpg_new<-rename(mpg_new,highway=hwy)
head(mpg_new)
str(mpg_new)

#파생변수만들기
#p113

df<-data.frame(var1=c(4,3,8),car2=c(2,6,1))
df

df$var_sum <- df$var1 + df$var2
df

df$var_sum<-df$var1+df$car2
df

df$var_mean<-(df$var1+df$car2)/2
df

str(mpg_new)
mpg_new$total<-(mpg_new$cty+mpg_new$highway)/2
mpg_new
head(mpg_new)
str(mpg_new)

#조건문을 이용한 파생변수 만들기

mean(mpg_new$total)
summary(mpg_new$total)
max(mpg_new$total)

hist(mpg_new$total)

ifelse(mpg_new$total>=20,"pass","fail")

mpg_new$test<-ifelse(mpg_new$total>=20,"pass","fail")
mpg_new$test

head(mpg_new, 20)

hist(mpg_new$total)

head(mpg_new)
str(mpg_new)

View(mpg_new)

#빈도표보기
table(mpg_new$test)
#막대그래프로 빈도 표현
library(ggplot2)
qplot(mpg_new$test)

?qplot

#중첩조건문

mpg_new$grade<-ifelse(mpg_new$total>=30,"A",ifelse(mpg_new$total>=20,"B","C"))
mpg_new$grade                      

str(mpg_new$grade)
summary(mpg_new$grade)
head(mpg_new$grade)
tail(mpg_new$grade)
mean(mpg_new$grade)

View(mpg_new$grade)
table(mpg_new$grade)
qplot(mpg_new$grade)

?midwest

library(ggplot2)
install.packages("ggplot2")
library(ggplot2)
library(midwest)

midwest<-as.data.frame(ggplot2::midwest)
dim(midwest)
View(midwest)
midwest_new<-midwest
midwest_new<-rename(midwest_new, total=poptotal, asain=popasian)




midwest
midwest<-rename(midwest,poptotal=total,popasian=asian)

midwest_new<-midwest
midwest_new
midwest_new<-rename(midwest_new,poptotal=total)
midwest_new<-rename(midwest_new,popasian=asian)


midwest
midwest<-as.data.frame(ggplot2::midwest)
midwest
head(midwest)
str(midwest)
dim(midwest)

library(dplyr)

midwest_new<-rename(midwest,total=poptotal)
midwest_new<-rename(midwest,asain=popasian)
View(midwest)
str(midwest)
summary(midwest)
dim(midwest)

midwest$ratio<-(midwest$asian/midwest$total)*100

qplot(midwest$ratio)
hist(midwest$ratio)
mean(midwest$ratio)
