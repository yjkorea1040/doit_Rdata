# ch09

#---------------------------------------------------
# 데이터 분석 프로젝트

#09-1 데이터: 한국복지패널데이터 from 한국 복지 패널 사이트
#09-2 성별에 따른 월급 차이  - "성별에 따라 월급이 다를까?"
#09-3 나이와 월급의 관계  - "몇 살 때 월급을 가장 많이 받을까?"
#09-4 연령대에 따른 월급 차이  - "어떤 연령대의 우러급이 가장 많을까?"
#09-5 연령대 및 성별 월급 차이  - "성별 월급 차이는 연령대별로 다를까?"
#09-6 직업별 월급 차이  - "어떤 직업이 월급을 가장 많이 받을까?"
#09-7 성별 직업 빈도  - "성별로 어떤 직업이 가장 많을까?"
#09-8 종교 유무에 따른 이혼율  - "종교가 있는 사람들이 이혼을 덜 할까?"
#09-9 지역별 연령대 비율  - "노년층이 많은 지역은 어디일까?"

# 분석 절차 : 
#-------------------------------------------
# 1. 변수 검토하기기
#-------------------------------------------
#-------------------------------------------
# 2. 전처리 - 나이(태어난 연도)
#-------------------------------------------
#-------------------------------------------
# 3. '연령대' 파생변수 만들기
#-------------------------------------------
#-------------------------------------------
# 4. 연령대, 월급의 관계 분석하기
#-------------------------------------------
#-------------------------------------------
# 5. 분석: 연령대에 대한 평균표 만들기
#-------------------------------------------
#-------------------------------------------
# 6. 분석: 그래프 만들기
#-------------------------------------------


#===========================================
#09-1 데이터: 한국복지패널데이터, p209
#===========================================

#-------------------------------------------
# 1. 데이터준비하기 p209
#-------------------------------------------
# Koweps_hpc10_2015_beta1.sav 을 ../datas/ 경로에 옮겨놓겠습니다.
# 한국복지패널 사이트에서 무료로 받을 수 있습니다. 교재에서는 2015년 자료를 사용했습니다.
# 깃허브 bit.ly/doit_rb 에서 다운로드받아도됩니다.
# 2016년 발간된 복지패널데이터에는 6914가구, 16664명에 대한 정보가 실렸습니다.
#-------------------------------------------
# 2. 패키지 설치 및 로드하기 p210
#-------------------------------------------
install.packages("foreign")
library(foreign) # spss 파일불러오기
library(dplyr) # 전처리
library(ggplot2) # 시각화
install.packages("readxl")
library(readxl) # 엑셀파일읽어오기

#-------------------------------------------
# 3. #데이터불러오기 p210
#-------------------------------------------
#데이터불러오기
raw_welfare<-read.spss(file="C:/works/works_r/datas/Koweps_hpc10_2015_beta1.sav", 
                       to.data.frame = T)

#복사본만들기
welfare<-raw_welfare;

#-------------------------------------------
# 4. #데이터 검토하기 p210
#-------------------------------------------

#데이터검토
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

?(welfare)

#-------------------------------------------
# 5. #변수명 바꾸기 p211
#-------------------------------------------
# 코드북: Koweps_Codebook.xlsx
# 배포되고 있는 코드북을 참조해서 변수명을 사용할 수 있습니다.
# 이해하기 쉬운 변수명으로 바꿔사용할 수 있습니다.
welfare <-rename(welfare, 
                 sex=h10_g3, #성별
                 birth=h10_g4, # 태어난 연도
                 marriage=h10_g10, # 혼인 상태
                 religion=h10_g11, # 종교
                 income=p1002_8aq1, # 월급
                 code_job=h10_eco9, # 직업코드
                 code_region=h10_reg7 # 지역코드
)

welfare <-rename(welfare, 
                 sex=h10_g3, 
                 birth=h10_g4, 
                 marriage=h10_g10, 
                 religion=h10_g11,
                 income=p1002_8aq1,
                 code_job=h10_eco9,
                 code_region=h10_reg7
                 )


# 데이터분석절차
# 1단계. 변수검토 및 전처리
# 2단계. 변수 간 관계분석

#===========================================
#09-2 성별에 따른 월급 차이  - "성별에 따라 월급이 다를까?" p213
#===========================================
#-------------------------------------------
# 1단계. 변수검토 및 전처리
#-> 성별, 월급에 대한 전처리
#-> 이상치 정체, 파생변수 생성
# 2단계. 변수 간 관계분석
#-> 성별 월급의 평균표 
#-> 그래프 작성
#-------------------------------------------

class(welfare$sex)
head(welfare$sex)
table(welfare$sex)
#-------------------------------------------
## 전처리 - 성별
#-------------------------------------------
# 이상치 확인

table(welfare$sex)
# 이상치 결측 처리

welfare$sex<-ifelse(welfare$sex==9, NA, welfare$sex)
# 결측치확인

table(is.na(welfare$sex))
# 명목형 변수 전처리
# 성별 항목 이름부여

welfare$sex <-ifelse(welfare$sex==1, "male", "female")
table(welfare$sex)
str(welfare$sex)

install.packages("ggplot2")
library(ggplot2)
qplot(welfare$sex)
?qplot


??farver
install.packages('farver')
library(farver)

#-------------------------------------------
# 월급변수 검토 및 전처리
#-------------------------------------------

#-------------------------------------------
# 1. 변수 검토하기
#-------------------------------------------
# 월급(수입)-> 연속변수이므로 summary()를 사용해서 통계량을 확인해본다, 
# 급여의 빈도가 높은 월급대역 분포를 알수있다.

class(welfare$income)
summary(welfare$income)
# 통계량의 대략 특징을 확인하기 위해 그래프 출력을 해보겠습니다. 

qplot(welfare$income)
# 출력된 대역을 더 잘 표현해 보기 위해 대역을 제한해보겠습니다. 

qplot(welfare$income) +
  xlim(0, 1000)
# 평균급여는 241.6이지만 다수의 빈도는 250 이하에 편중되어 있습니다. 

# 코드북에서 급여의 값 코딩값을 확인해보면...
# N(1~9998), 무응답 9999 코딩을 해놓았으므로...
# 이상치를 확인해서 전처리를 합니다.


#-------------------------------------------
# 2. 전처리 - 월급(급여)
#-------------------------------------------
#성별과 월급의 관계를 분석할 때는 결측치를 제외하고 분석해야 합니다. 
# 코드북의 코딩값을 기준으로 데이터를 분석해보고 결측치와 이상치를 전처리합니다.
summary(welfare$income)
# 1) NA's 12030으로 결측치가 확인됩니다...
# ...무응답의 경우와 같이 급여가 없어서 대답을 하지 않는 경우가 해당됩니다.
# 2) min. 0.0 이면 이상치에 해당됩니다. 값의 범위는  N(1~9998), 무응답 9999 입니다. 
# > summary(welfare$income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0   122.0   192.5   241.6   316.6  2400.0   12030 

# 이상치 결측 처리

welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

# 결측치 확인

table(is.na(welfare$income)) 

#12044건의 결측치가 확인된다
#-------------------------------------------
# 성별 월급 차이 분석해보기
#-------------------------------------------
dim(welfare)

#-------------------------------------------
# 1. 성별 월급 평균표 만들기
#-------------------------------------------

sex_income <-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))

sex_income <-welfare %>% 
  filter(!is.na(income))   %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

dim(sex_income)
sex_income

ggplot(data=sex_income, aes(x=sex,y=mean_income))+
  geom_col()

# > sex_income
# # A tibble: 2 x 2
# sex    mean_income
# <chr>        <dbl>
#   1 female        163.
# 2 male          312
# 계산된 통계결과를 보면 남성은 312, 여성은 163으로 
# 평균적으로 여성보다 남성이 150 더 많다


#-------------------------------------------
# 2. 그래프 만들기
#-------------------------------------------
ggplot(data=sex_income, aes(x=sex, y=mean_income)) + geom_col()
ggplot(data=sex_income, aes(x=sex, y=mean_income)) + geom_count()

#===========================================
#09-3 나이에 따른 월급 차이  - "몇 살 때 월급을 가장 많이 받을까?" p220
#===========================================

# storyline
# 비정규직이 많아지면서 안정된 직장에 취업하는 것도 어려워졌다. 
# 취직을 해도 소득 격차가 크다는 것을 발견하고 사회가 불평등하게 느껴진다.
# 나이에 따라 언제 월급을 많이 받는 것인지 궁금해진다. 
# 나는 언제 월급을 많이 받을 수 있을까?
#-------------------------------------------
# 관심변수 : 나이, 월급
# 분석방법 : 나이에 따른 월급 평균표와 그래프
#-------------------------------------------
#-------------------------------------------
# 1. 변수 검토하기기
#-------------------------------------------
# 태어난 연도 변수에서 나이 변수를 생성합니다.
class(welfare$birth)
#대략적인 통계치를 확인해보고
summary(welfare$birth)
# > summary(welfare$birth)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1907    1946    1966    1968    1988    2014 

# 대략적인 그래프를 확인해보겠습니다. 

qplot(welfare$birth)

#-------------------------------------------
# 2. 전처리 - 나이(태어난 연도)
#-------------------------------------------
# 코드북으로부터 코딩값의 범위를 확인해보고 전처리가 필요한지 결정합니다.
# h10_g4	태어난 연도	년
# 값의 범위: N(1900~2014)	모름/무응답=9999

# 이상치를 확인해보겠습니다.
summary(welfare$birth)
#-> 코딩값에서 벗어난 이상치는 없습니다. 
# 결측치를 확인해보겠습니다.
table(is.na(welfare
            $birth))
# FALSE 
# 16664 
#-> 결측치에 해당되는 값은 확인되지 않습니다. 

# 이상치, 결측치의 처리
welfare$birth<-ifelse(welfare$birth==9999, NA, welfare$birth)
table(is.na(welfare$birth))

#-------------------------------------------
# 3. '나이' 파생변수 만들기
#-------------------------------------------
# birth 변수로부터 나이변수 만들기
# 조사된 기준년도는 2015년 이므로 
# 계산식:  나이 = 2015 -  태어난연도 + 1
welfare$age <-2015 - welfare$birth + 1


summary(welfare$age)
qplot(welfare$age)
# 나이 그래프가 대략 출생연도 그래프와 비슷합니다.

#-------------------------------------------
# 4. 나이, 월급의 관계 분석하기
#-------------------------------------------
# 나이 변수에 대한 전처리 -> 완료
# 월급 변수에 대한 전치리 -> 
summary(welfare$income)
# 결측치 
# 1) NA's 12030으로 결측치가 확인됩니다...
# ...무응답의 경우와 같이 급여가 없어서 대답을 하지 않는 경우가 해당됩니다.
# 이상치, 코딩값과 비교합니다.
# 2) min. 0.0 이면 이상치에 해당됩니다. 값의 범위는  N(1~9998), 무응답 9999 입니다. 
# > summary(welfare$income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0   122.0   192.5   241.6   316.6  2400.0   12030 

# 이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
# 결측치 확인
table(is.na(welfare$income)) #12044건의 결측치가 확인된다

#-------------------------------------------
# 5. 분석: 나이에 대한 평균표 만들기
#-------------------------------------------
age_income <-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

# 소수자릿수 표현
options(scipen = 4)
head(age_income)

# cf. 소수점 자리 표현
# pi
# signif(pi, 4)
# options()
# # 소수자릿수 표현
# options(scipen = 4)
# options(digits = 4)
# r에서 최대 소수점 자리수는 22자리까지 표현한다.
#-------------------------------------------
# 6. 분석: 그래프 만들기
#-------------------------------------------

ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_col()
ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_count()
ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_line()

# story: 
# 20대 초반에서 100만원을 받고 이후 나이에 따라 증가합니다.
# 50 대에서는 300만원내외로 가장 많은 월급을 받고 
# 50이후에는 급여 감소가 되고 
# 70이후에는 20대의 월급여보다 더 낮은 급여를 받게됩니다.


#===========================================
#09-4 나이에 따른 월급 차이  - "어느 연령대에서 월급이 가장 높을까?" p225
#===========================================
# 나이를 연령대로 분류해서 분석하겠습니다. 
#
#-------------------------------------------
# 관심변수 : 연령대, 월급
# 분석방법 : 연령대별 월급 평균표와 그래프
#-------------------------------------------

#-------------------------------------------
# 1. 변수 검토하기
#-------------------------------------------
#-------------------------------------------
# 2. 전처리 - 나이(태어난 연도)
#-------------------------------------------
#-------------------------------------------
# 3. '연령대' 파생변수 만들기
#-------------------------------------------
#-------------------------------------------
# 4. 연령대, 월급의 관계 분석하기
#-------------------------------------------
#-------------------------------------------
# 5. 분석: 연령대에 대한 평균표 만들기
#-------------------------------------------
#-------------------------------------------
# 6. 분석: 그래프 만들기
#-------------------------------------------

# 나이 변수에 대한 전처리 -> 완료
# 월급 변수에 대한 전치리 -> 완료

#-------------------------------------------
# 1. '연령대' 파생변수 만들기
#-------------------------------------------
# 초년: 30세미만, 중년 30~59세, 노년 60세이상

welfare <-welfare %>% 
  mutate(ageg = ifelse(age<30, "young", 
                       ifelse(age<=59, "middle", "old")))

# 대략적인 값을 확인해보겠습니다. 
table(welfare$ageg)
# > table(welfare$ageg)
# 
# middle    old  young 
# 6049   6281   4334 

# 대략적인 그래프를 확인합니다.
qplot(welfare$ageg)

#-------------------------------------------
# 연령대에 따른 월급 차이를 분석해보겠습니다.
#-------------------------------------------

#-------------------------------------------
# 2. 전처리 - 월급, 연령대
#-------------------------------------------
# 연령대(나이) 변수에 대한 전처리 -> 완료
# 월급 변수에 대한 전치리 -> 완료
#-------------------------------------------
# 3. 분석: 연령대에 대한 평균표 만들기
#-------------------------------------------
ageg_income <-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
dim(ageg_income)
ageg_income
# > dim(ageg_income)
# [1] 3 2
# > ageg_income
# # A tibble: 3 x 2
# ageg   mean_income
# <chr>        <dbl>
#   1 middle        282.
# 2 old           125.
# 3 young         164.

#-------------------------------------------
# 4. 분석: 그래프 만들기
#-------------------------------------------
ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + geom_col()
# 그래프의 순서
# 막대그래프는 변수의 알파벳 순으로 정렬 (기본값)
ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + 
  geom_col()+
  scale_x_discrete(limits=c("young", "middle", "old"))

# storytelling
# 중년이 280만원 정도로 가장 많은 월급을 수령하는 나이대가 된다.
# 노년은 125만원정도이고 초년 163만원보다 적은 월급을 받게 된다. 
# 따라서 노년에 대한 급여 정책이 수립되어야 한다. 


#===========================================
#09-5 연령대 및 성별에 따른 월급 차이  
#     - "성별 월급차이는 연령대별로 차이가 있을까?" p228
#===========================================

#-------------------------------------------
# 관심변수 : 연령대, 성별, 월급
# 분석방법 : 연령대 및 성별 따른 월급 평균표와 그래프
#-------------------------------------------

#-------------------------------------------
# 1. 변수 검토하기기
#-------------------------------------------
#------------
# 월급(수입)
#------------
# 월급(수입)-> 연속변수이므로 summary()를 사용해서 통계량을 확인해본다, 
# 급여의 빈도가 높은 월급대역 분포를 알수있다.
class(welfare$income)
summary(welfare$income)
# 통계량의 대략 특징을 확인하기 위해 그래프 출력을 해보겠습니다. 
qplot(welfare$income)
# 출력된 대역을 더 잘 표현해 보기 위해 대역을 제한해보겠습니다. 
qplot(welfare$income) + xlim(0, 1000)
# 평균급여는 241.6이지만 다수의 빈도는 250 이하에 편중되어 있습니다. 

#------------
# 이상치, 결측치 처리
#------------
# 코드북에서 급여의 값 코딩값을 확인해보면...
# N(1~9998), 무응답 9999 코딩을 해놓았으므로...
# 이상치를 확인해서 전처리를 합니다.

summary(welfare$income)
# 결측치 
# 1) NA's 12030으로 결측치가 확인됩니다...
# ...무응답의 경우와 같이 급여가 없어서 대답을 하지 않는 경우가 해당됩니다.
# 이상치, 코딩값과 비교합니다.
# 2) min. 0.0 이면 이상치에 해당됩니다. 값의 범위는  N(1~9998), 무응답 9999 입니다. 
# > summary(welfare$income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0   122.0   192.5   241.6   316.6  2400.0   12030 

# 이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

# 결측치 확인
table(is.na(welfare$income)) #12044건의 결측치가 확인된다


welfare<-welfare %>% 
  mutate(ageg=ifelse(age<30, "young",
                     ifelse(age<59, "middle", "old")))



table(welfare$ageg)
qplot(welfare$ageg)

#------------
# 성별
#------------
class(welfare$sex)
# summary(welfare$sex) # 명목형 변수
table(welfare$sex)
qplot(welfare$sex)
#------------
# 이상치, 결측치 처리
#------------
# 이상치 확인
table(welfare$sex)
# 이상치 결측 처리
welfare$sex<-ifelse(welfare$sex==9, NA, welfare$sex)
# 결측치확인
table(is.na(welfare$sex))
#------------
# 명목형 변수 전처리
#------------
# 성별 항목 이름부여
welfare$sex <-ifelse(welfare$sex==1, "male", "female")
table(welfare$sex)
# 이상치 확인
table(welfare$sex)
# 이상치 결측 처리
welfare$sex<-ifelse(welfare$sex==9, NA, welfare$sex)
# 결측치확인
table(is.na(welfare$sex))
# 명목형 변수 전처리
# 성별 항목 이름부여
welfare$sex <-ifelse(welfare$sex==1, "male", "female")
table(welfare$sex)
# install.packages("ggplot2")
# library(ggplot2)
qplot(welfare$sex)

#------------
# 태어난연도 -> 나이 -> 연령대 파행변수 생성
#------------
# 태어난 연도 변수에서 나이 변수를 생성합니다.
class(welfare$birth)
#대략적인 통계치를 확인해보고
summary(welfare$birth)
# > summary(welfare$birth)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1907    1946    1966    1968    1988    2014 
# 대략적인 그래프를 확인해보겠습니다. 
qplot(welfare$birth)

#------------
# 이상치, 결측치 처리
#------------
# 코드북으로부터 코딩값의 범위를 확인해보고 전처리가 필요한지 결정합니다.
# h10_g4	태어난 연도	년
# 값의 범위: N(1900~2014)	모름/무응답=9999

# 이상치를 확인해보겠습니다.
summary(welfare$birth)
#-> 코딩값에서 벗어난 이상치는 없습니다. 
# 결측치를 확인해보겠습니다.
table(is.na(welfare
            $birth))
# FALSE 
# 16664 
#-> 결측치에 해당되는 값은 확인되지 않습니다. 

# 이상치, 결측치의 처리
welfare$birth<-ifelse(welfare$birth==9999, NA, welfare$birth)
table(is.na(welfare$birth))


# birth 변수로부터 나이변수 만들기
# 조사된 기준년도는 2015년 이므로 
# 계산식:  나이 = 2015 -  태어난연도 + 1

welfare$age <-2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

# 나이 그래프가 대략 출생연도 그래프와 비슷합니다.

# '연령대' 파생변수 만들기 
# 초년: 30세미만, 중년 30~59세, 노년 60세이상

welfare <-welfare %>% 
  mutate(ageg = ifelse(age<30, "young", 
                       ifelse(age<=59, "middle", "old")))

# 대략적인 값을 확인해보겠습니다. 
table(welfare$ageg)
# > table(welfare$ageg)
# 
# middle    old  young 
# 6049   6281   4334 

# 대략적인 그래프를 확인합니다.
qplot(welfare$ageg)

#-------------------------------------------
# 2. 전처리 - 나이(태어난 연도)
#-------------------------------------------
#-------------------------------------------
# 3. '연령대' 파생변수 만들기
#-------------------------------------------
#-------------------------------------------
# 4. 연령대, 성별, 월급의 관계 분석하기
#-------------------------------------------

# 연령대, 월급의 관계 분석하기

ageg_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income=mean(income))

dim(ageg_income)
ageg_income

# 그래프만들기

ggplot(data=ageg_income, aes(x=ageg, y=mean_income))+
  geom_col()


# 순서정렬
ggplot(data=ageg_income, aes(x=ageg, y=mean_income))+
  geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))




#-------------------------------------------
# 5. 분석: 연령대에 대한 평균표 만들기
#-------------------------------------------
# 연령대에서 성별에 따른 차이가 있지 분석하겠습니다. 

# 연령대: 초년, 중년, 장년, 노년
# 초년:30미만, 중년:30~45, 장년:46~59, 노년:60이상
# young,middle,adult,old

welfare <-welfare %>% 
  mutate(ageg = ifelse(age<30, "young", 
                       ifelse(age<=45, "middle",
                       ifelse(age<=59, "adult", "old"))))

table(welfare$ageg)
dim(welfare$ageg)
qplot(welfare$ageg)
dim(ageg_income)
ageg_income


ggplot(data=ageg_income, aes(x=ageg, y=mean_income))+
  geom_col()+
  scale_x_discrete(limits=c("young", "middle", "adult", "old"))

# 연령대: 초년(30미만), 30대(30~39), 40대(40~49), 50대(50~59), 60중(60~65), 60대(66이상)
# young, 30age, 40age, 50age, 60age, old

welfare<-welfare %>% 
  mutate(ageg = ifelse(age<30, "young",
                         ifelse(age<40, "30age",
                                ifelse(age<50, "40age",
                                       ifelse(age<60, "50age",
                                              ifelse(age<66, "60age","old"))))))
# 별도

table(welfare$ageg)
dim(welfare$ageg)
qplot(welfare$ageg)

ageg_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income=mean(income))

dim(ageg_income)
ageg_income
        
ggplot(data=ageg_income, aes(x=ageg, y=mean_income))+
  geom_col()+
  scale_x_discrete(limits=c("young", "30age", "40age", "50age", "60age", "old"))

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income)+ xlim(0, 1000)

welfare$income<-ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

table(is.na(welfare$income))

# 성별
class(welfare$sex)
table(welfare$sex)



sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))

dim(sex_income)
sex_income

# 그래프로 그려보기

ggplot(data=sex_income, aes(x=ageg, y=mean_income))+
  geom_col()+
  scale_x_discrete(limits=c("young", "30age", "40age", "50age", "60age", "old"))

# sex 남여 성별에 따른 그래프 구분을 위해 색상으로 채움
ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex))+
  geom_col()+
  scale_x_discrete(limits=c("young", "30age", "40age", "50age", "60age", "old"))

ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex))+
  geom_col(position="dodge")+
  scale_x_discrete(limits=c("young", "30age", "40age", "50age", "60age", "old"))



# > dim(sex_income)
# [1] 6 3
# > sex_income
# # A tibble: 6 x 3
# # Groups:   ageg [3]
# ageg   sex    mean_income
# <chr>  <chr>        <dbl>
#   1 middle female       188. 
# 2 middle male         353. 
# 3 old    female        81.5
# 4 old    male         174. 
# 5 young  female       160. 
# 6 young  male         171. 


#-------------------------------------------
# 6. 분석: 그래프 만들기
#-------------------------------------------
# 축 순서를 지정해서 연령대 순으로 표시하겠습니다.
# fill =sex 는 막대 그래프가 성별에 따라 다른 색으로 표시되도록 합니다.
ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col()+
  scale_x_discrete(limits=c("young", "middle", "old"))
# 연령대 그래프에서 성별로 함께 표시되어 구분은 가능하지만 비교는 쉽지 않습니다.
# 막대그래프를 분리해서 표시하겠습니다. 
# position="dodge" 를 사용합니다. dodge는 피하다라는 듯입니다.
ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col(position = "dodge")+
  scale_x_discrete(limits=c("young", "middle", "old"))

# storytelling:
# 연령대별로 막대 그래프에서 성별 월급의 차이가 나타나는 것이 확인된다. 
# 사회 초년에는 차이가 별 차이가 없다고 볼 수 있다.
# 중년에서는 차이가 크게 벌어지고 남성이 166만원 더 많이 받는다.
# 노년에서는 다시 남성이나, 여성이나 중년에 비해 급여는 급락하고
# 노년에서 사회초년보다 급여가 같거나 줄어들지만 
# 여성에서는 노년에 사회초년이하의 급여 평균을 보이므로 
# 노년에 대한 사회정책에 있어 보다 여성에 대한 대책이 필요하다. 
# 중년에서의 급여 수준에 있어서 여성의 급여 수준이 남성과 차이가 크다는 점이 큰 특징이다. 


#-------------------------------------------
# 7. 나이 및 성별 월급 차이 분석하기
#-------------------------------------------
# 연령대가 아닌 나이에 대한 성별 월급 평균표, 그래프를 작성하겠습니다. 
# 성별, 연령별 월급 평균표 만들기

sex_age <-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

dim(sex_age)
head(sex_age)
# > dim(sex_age)
# [1] 134   3
# > head(sex_age)
# # A tibble: 6 x 3
# # Groups:   age [3]
# age sex    mean_income
# <dbl> <chr>        <dbl>
#   1    20 female        147.
# 2    20 male           69 
# 3    21 female        107.
# 4    21 male          102.
# 5    22 female        140.
# 6    22 male          118.

sex_age

# 그래프 만들기
ggplot(data=sex_age, aes(x=age, y=mean_income))+
  geom_col()

ggplot(data=sex_age, aes(x=age, y=mean_income))+
  geom_count()

ggplot(data=sex_age, aes(x=age, y=mean_income))+
  geom_line()

ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex)) + geom_col()
ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex)) + geom_count()
ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex)) + geom_line()

# storytelling: 
# 남성의 월급은 50세 전후까지 지속적으로 증가하다가 급격하게 감소한다. 
# 여성은 30세 전후까지 상승하다가 이후로는 지속, 완만한 감소를 보인다. 
# 남성과 여성은 모두 50세 이후 급격히 월급이 감소하고 70세 이후에는 비슷한 수준이된다. 
# 성별 급여는 30세이후 지속적으로 차이가 벌어진다.
# 남성의 최고 급여는 50세, 여성의 최고 급여는 30세를 나타낸다.
# 여성은 30세 기점으로 결혼하게 되면서 결혼과 함께 직장을 그만두는 경향이 있으므로 
# 사회적인 급여 수준에서 차이점을 분석해야 한다. 
# 남성은 50세 이후 퇴직이 증가하는 점을 고려해서 급여에 대한 안정책이 필요하다. 



#===========================================
#09-6 직업에 따른 월급 차이  
#     - "어떤 직업이 월급을 가장 많이 받을까?" p233
#===========================================



#-------------------------------------------
# 관심변수 : 직업, 월급
# 분석방법 : 직업별 월급 평균표와 그래프
#-------------------------------------------

#-------------------------------------------
# 1. 변수 검토하기
#-------------------------------------------

class(welfare$code_job)
summary(welfare$code_job)# 명목형 변수 -> '직업별 코드' 
table(welfare$code_job)
str(welfare$code_job)
#---------------
# 한국복지패널 사이트에서 제공하는 코드북을 참조, '직종코드'
#---------------

install.packages("readxl")
library(readxl)
library(read.excel)
# Koweps_Codebook.xlsx
list_job<-read_excel("C:/works/works_r/datas/Koweps_Codebook.xlsx", col_names=T, sheet=2)

dim(list_job)
list_job


#-------------------------------------------
# 2. 전처리 - 직업
#-------------------------------------------
#---------------
# 직종코드를 사용해서 직업의 명칭으로 된 변수를 만들어 놓겠습니다.
#---------------
# 코드가 많으므로 excel을 읽어들여서 가져오는 방법을 사용합니다. 
install.packages("readxl")
library(readxl)

list_job <-read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
dim(list_job)
# > dim(list_job)
# [1] 149   2
head(list_job)
# > head(list_job)
# # A tibble: 6 x 2
# code_job job                                
# <dbl> <chr>                              
#   1      111 의회의원 고위공무원 및 공공단체임원
# 2      112 기업고위임원                       
# 3      120 행정 및 경영지원 관리자            
# 4      131 연구 교육 및 법률 관련 관리자      
# 5      132 보험 및 금융 관리자                
# 6      133 보건 및 사회복지 관련 관리자 

#-------------------------------------------
# 3. '직업' 변수 만들기
#-------------------------------------------
#-----------------
# left_join() 을 사용해서 job 변수를 welfare 에 결합합니다.
#-----------------
# 공통으로 들어 있는 code_job을 기준으로 결합합니다. 

head(welfare$code_job)

?left_join
library(left_join)
library(dplyr)

welfare <-left_join(welfare, list_job, id="code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)


#-------------------------------------------
# 4. 직업별 월급의 관계 분석하기
#  어떤 영령대의 월급이 가장 부득 많을까? p225
#-------------------------------------------
job_income <-welfare %>% 
  filter(!is.na(job)&!is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income=mean(income))

dim(job_income)
job_income

bottom10 <-job_income %>% 
  arrange(mean_income) %>% 
  head(10)

bottom10

top10 <-job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

# 그래프그리기
ggplot(data=top10, aes(x=job, y=mean_income))+
  geom_col()
# 순서
ggplot(data=top10, aes(x=reorder(job, mean_income), y=mean_income))+
  geom_col()+
  coord_flip()

ggplot(data=bottom10, aes(x=reorder(job, -mean_income), y=mean_income))+
  geom_col()+
  coord_flip()


sex_income <-welfare %>% 
  filter(!is.na(sex)&!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))

View(sex_income)
dim(sex_income)

sex_income

#-------------------------------------------
# 5. 분석: 직업별 월급 평균표 만들기
#-------------------------------------------
# 직업이 없거나 월급이 없는 사람은 분석에서 제외합니다. 

job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

dim(job_income)
head(job_income)
# > dim(job_income)
# [1] 142   2
# > head(job_income)
# # A tibble: 6 x 2
# job                           mean_income
# <chr>                               <dbl>
#   1 가사 및 육아 도우미                  80.2
# 2 간호사                              241. 
# 3 건설 및 광업 단순 종사원            190. 
# 4 건설 및 채굴 기계운전원             358. 
# 5 건설 전기 및 생산 관련 관리자       536. 
# 6 건설관련 기능 종사자                247. 

# 월급 기준으로 내림차순 정렬
# 상위 10개를 추출해서 보겠습니다. 

top10 <-job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10
# > top10
# # A tibble: 10 x 2
# job                                  mean_income
# <chr>                                      <dbl>
# 1 금속 재료 공학 기술자 및 시험원             845.
# 2 의료진료 전문가                             844.
# 3 의회의원 고위공무원 및 공공단체임원         750 
# 4 보험 및 금융 관리자                         726.
# 5 제관원 및 판금원                            572.
# 6 행정 및 경영지원 관리자                     564.
# 7 문화 예술 디자인 및 영상 관련 관리자        557.
# 8 연구 교육 및 법률 관련 관리자               550.
# 9 건설 전기 및 생산 관련 관리자               536.
# 10 석유 및 화학물 가공장치 조작원              532.


#-------------------------------------------
# 6. 분석: 그래프 만들기
#-------------------------------------------
# coord_flip() 를 추가하면 막대그래프를 오른쪽으로 90도 눕혀서 표시합니다.
# 직업이름이 길어서 겹치지 않도록 표시방향을 가로로 했습니다.
ggplot(data=top10, aes(x=reorder(job, mean_income), y=mean_income))+
  geom_col()+
  coord_flip()


# 금속 재료 공학 기술자 및 시험원의 평균 급여가 845만원으로 가장 많은 월급을 받고
# 의료진료 전문가, 의회의원 고위공무원 및 공공단체임원, 보험및금융관리자 순입니다

#===========================================
#09-6 직업에 따른 월급 차이  
#     - "어떤 직업이 월급을 가장 적게 받을까?" p238
#===========================================
# 하위 10위를 뽑아보겠습니다. 
bottom10 <-job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

# > bottom10
# # A tibble: 10 x 2
# job                          mean_income
# <chr>                              <dbl>
#   1 가사 및 육아 도우미                 80.2
# 2 임업관련 종사자                     83.3
# 3 기타 서비스관련 단순 종사원         88.2
# 4 청소원 및 환경 미화원               88.8
# 5 약사 및 한약사                      89  
# 6 작물재배 종사자                     92  
# 7 농립어업관련 단순 종사원           102. 
# 8 의료 복지 관련 서비스 종사자       104. 
# 9 음식관련 단순 종사원               108. 
# 10 판매관련 단순 종사원               117. 

# 그래프 만들기
ggplot(data=bottom10, aes(x=reorder(job, -mean_income), y=mean_income))+
  geom_col()+
  coord_flip()+
  ylim(0, 200)

# story:
# '가사 및 육아 도우미' 월급이 평균 80만원으로 가장 적고, 
#  임업관련종사자, 기타 서비스관련 단순 종사원, 청소원 및 환경미화원 순입니다. 
# 최상위, 최하위의 격차는 열배가 넘습니다. 


#===========================================
#09-7 성별 직업 빈도  
#     - "성별에 따라 어떤 직업이 가장 많을까?" p240
#===========================================
# story: 
# 남여평등이라고 하지만 여전히 성별에 따라 다른 직업을 갖는 경향이 있습니다. 
# 성별에 따라 어떤 직업이 많은지 분석하게씃ㅂ니다. 

#-------------------------------------------
# 관심변수 : 성별, 직업
# 분석방법 : 성별 직업 빈도표와 그래프
#-------------------------------------------

#-------------------------------------------
# 1. 변수 검토하기
#-------------------------------------------
#-------------------------------------------
# 2. 전처리 - 성별, 직업
#-------------------------------------------
#-------------------------------------------
# 3. '' 파생변수 만들기
#-------------------------------------------
#-------------------------------------------
# 4. 성별, 직업의 관계 분석하기
#-------------------------------------------
#-------------------------------------------
# 5. 분석: 성별 직업 빈도표 만들기
#-------------------------------------------
# 남성 기준, 직업빈도가 가장 많은 순서로 상위 10직업
job_male <- welfare %>% 
  filter(!is.na(job) & sex=="male") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male
# > job_male
# # A tibble: 10 x 2
# job                          n
# <chr>                    <int>
#   1 작물재배 종사자            640
# 2 자동차 운전원              251
# 3 경영관련 사무원            213
# 4 영업 종사자                141
# 5 매장 판매 종사자           132
# 6 제조관련 단순 종사원       104
# 7 청소원 및 환경 미화원       97
# 8 건설 및 광업 단순 종사원    95
# 9 경비원 및 검표원            95
# 10 행정 사무원                 92

# 여성 기준, 직업빈도가 가장 많은 순서로 상위 10직업
job_female <- welfare %>% 
  filter(!is.na(job) & sex=="female") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female
# > job_female
# # A tibble: 10 x 2
# job                              n
# <chr>                        <int>
#   1 작물재배 종사자                680
# 2 청소원 및 환경 미화원          228
# 3 매장 판매 종사자               221
# 4 제조관련 단순 종사원           185
# 5 회계 및 경리 사무원            176
# 6 음식서비스 종사자              149
# 7 주방장 및 조리사               126
# 8 가사 및 육아 도우미            125
# 9 의료 복지 관련 서비스 종사자   121
# 10 음식관련 단순 종사원           104
#-------------------------------------------
# 6. 분석: 그래프 만들기
#-------------------------------------------

# 남성 기준, 직업빈도가 가장 많은 순서로 상위 10직업
ggplot(data=job_male, aes(x=reorder(job, n), y=n))+
  geom_col()+
  coord_flip()+
  ylim(0, 800)

# 여성 기준, 직업빈도가 가장 많은 순서로 상위 10직업
ggplot(data=job_female, aes(x=reorder(job, n), y=n))+
  geom_col()+
  coord_flip()+
  ylim(0, 800)

# story: 
# 남성은 작물재배종사자, 자동차운전원, 경영관련 사무원, 영업종사자 직업 순서
# 여성은 작물재배종사자, 청소원 및 환경미화원, 매장 판매 종사자, 제조관련 단순 종사원 직업순서이다. 
#==============================================
# geom_col() 과 geom_bar()
#==============================================
# 원자료를 이용해서 막대 그래프를 만들때는 geom_bar()를 사용하고
# 요약표를 이용해서 막대 그래프를 만들때는 geom_col()을 사용합니다. 
# 직업별 빈도표를 이용해서 막대 그래프를 만들 대는 geom_col()을 사용합니다. 



#===========================================
#09-8 종교 유무에 따른 이혼율
#     - "종교가 있는 사람들이 이혼을 덜 할까?" p244
#===========================================
# story:
# 뉴스에 연예인들, 종교인들의 결혼소식과 이혼소식이 종종 보도된다.
# 종교가 있는 사람들은 종교가 없는 사람들보다 이혼을 덜 하고 사는지 
# welfare 데이터에서 분석해보겠습니다. 
#-------------------------------------------
# 관심변수 : 종교, 혼인상태
# 분석방법 : 종교 유무에 따른 이혼율에 대한 표 그래프
#-------------------------------------------

#-------------------------------------------
# 1. 변수 검토하기 -  종교유무
#-------------------------------------------
class(welfare$religion)
table(welfare$religion)
# > class(welfare$religion)
# [1] "numeric"
# > table(welfare$religion)
# 1    2 
# 8047 8617 

# 이상치는 존재하지 않습니다.
# 코드북을 참조하여 명목변수에 대한 코드 값을 의미 있는 이름을 적용하겠습니다. 


#--------------------
# 코딩: 
#--------------------

# h10_g10	혼인상태	0.비해당(18세 미만)
# 1.유배우 2.사별 3.이혼 4.별거 5.미혼(18세이상, 미혼모 포함) 6.기타(사망 등)	N(0~6) 모름/무응답=9

# h10_g11	종교
# 1.있음 2.없음	N(1~2) 모름/무응답=9	
# 출처: 10차 머지데이터_변수명.xlsx

# h10_g10	혼인상태	"0.비해당(18세 미만)
# 1.유배우         2.사별         3.이혼          4.별거          
# 5.미혼(18세이상, 미혼모 포함)   6.기타(사망 등)"	N(0~6)	모름/무응답=9	
# 출처: 10차 머지데이터_변수명.xlsx
#--------------------

# 종교유무 전처리
welfare$religion <-ifelse(welfare$religion==1, "yes", "no")

table(welfare$religion)
qplot(welfare$religion)

# 혼인상태
class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <-ifelse(welfare$marriage==1, "marriage",
                                ifelse(welfare$marriage==3,"divorce", NA))

table(welfare$group_marriage)

# 종교 그룹에 대해 분석, 빈도표 작성
religion_marriage<-welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group)*100,1))

religion_marriage

divorce<-religion_marriage %>% 
  filter(group_marriage=="divorce") %>% 
  select(religion, pct)

divorce

#-------------------------------------------
# 2. 전처리 - 종교유무
#-------------------------------------------
# 종교 유무에 대해 코드 값에 대해 이름을 부여하겠습니다. 
welfare$religion <-ifelse(welfare$religion==1, "yes", "no")
# 대략적인 도표를 출력해보겠습니다. 
table(welfare$religion)
# > table(welfare$religion)
# no  yes 
# 8617 8047
# 대략적인 그래프를 그려보겠습니다. 
qplot(welfare$religion)

#-------------------------------------------
# 1. 변수 검토하기 -  혼인상태
#-------------------------------------------
class(welfare$marriage)
table(welfare$marriage)
# 숫자 타입이지만 명목변수
# > class(welfare$marriage)
# [1] "numeric"
# > table(welfare$marriage)
# 
# 0    1    2    3    4    5    6 
# 2861 8431 2117  712   84 2433   26 

#-------------------------------------------
# 2. 전처리 - 혼인상태
#-------------------------------------------

# 코딩 값으로부터 이혼여부를 알수 있는 파생 변수를 생성합니다. 
# h10_g10	혼인상태	"0.비해당(18세 미만)
# 1.유배우         2.사별         3.이혼          4.별거          
# 5.미혼(18세이상, 미혼모 포함)   6.기타(사망 등)"	N(0~6)	모름/무응답=9	
# 출처: 10차 머지데이터_변수명.xlsx

# 배우자가 있을 경우: 1
# 이혼했을 경우 : 3

#-------------------------------------------
# 3. '이혼여부' 파생변수 만들기
#-------------------------------------------
# 이혼여부에 대한 변수 만들기
welfare$group_marriage <- ifelse(welfare$marriage ==1, "marriage", 
                                 ifelse(welfare$marriage==3, "divorce", NA))
table(welfare$group_marriage)
# > table(welfare$group_marriage)
# 
# divorce marriage 
# 712     8431 

table(is.na(welfare$group_marriage))
# > table(is.na(welfare$group_marriage))
# 
# FALSE  TRUE 
# 9143  7521 

# 결혼상태인 사람은 8431명, 이혼상태인 사람은 712명입니다.
# 결혼상태와 이혼상태인 사람은 9143명이고 
# 그외 상태(미혼, 사별, 별거, 사망 등)인 사람은 7521명으로 분류됩니다.
# 그외 상태는 이후 분석에서 제외합니다. 

#-------------------------------------------
# 4. 종교, 이혼율의 관계 분석하기
#-------------------------------------------
#-------------------------------------------
# 5. 분석: 종교 유무에 따른 이혼율 표 만들기
#-------------------------------------------
# 종교 유무 및 결혼 상태별로 빈도표를 만들고
# 종교 유무 집단의 전체 빈도로 나눠서 비율을 구하겠습니다. 
# 비율은 round()이용해서 소수점 첫째 자리까지 표현하겠습니다. 
religion_marriage <-welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100, 1))

religion_marriage

ggplot(data=divorce, aes(x=religion, y=pct))+
  geom_col()

# > religion_marriage
# # A tibble: 4 x 5
# # Groups:   religion [2]
# religion group_marriage     n tot_group   pct
# <chr>    <chr>          <int>     <int> <dbl>
#   1 no       divorce          384      4602   8.3
# 2 no       marriage        4218      4602  91.7
# 3 yes      divorce          328      4541   7.2
# 4 yes      marriage        4213      4541  92.8


# 연령대별 이혼율 표
ageg_marriage<-welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(pct=round(n/sum(n)*100,1))

ageg_marriage

# 그래프그리기

ageg

ageg_divorce<-ageg_marriage %>% 
  filter(ageg!="young"&group_marriage=="divorce") %>% 
  select(ageg, pct)

ageg_divorce

ggplot(data=ageg_divorce, aes(x=ageg, y=pct))+
  geom_col()

# 다른 방법:
# dplyr의 count()는 집단별 빈도를 구하는 함수이므로
# count()를 이용하고 mutate를 하나로 합쳐서 비율을 구할 수 있습니다.
religion_marriage_2 <-welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct=round(n/sum(n)*100, 1))
religion_marriage_2

# 이혼에 대한 지표만 모아서 
# 이혼율 표를 만들겠습니다.
divorce <- religion_marriage %>% 
  filter(group_marriage=="divorce") %>% 
  select(religion, pct)

divorce
# > divorce
# # A tibble: 2 x 2
# # Groups:   religion [2]
# religion   pct
# <chr>    <dbl>
#   1 no         8.3
# 2 yes        7.2

#-------------------------------------------
# 6. 분석: 그래프 만들기
#-------------------------------------------
ggplot(data=divorce, aes(x=religion, y=pct)) +
  geom_col()
#story:
# 이혼율은 종교가 있는 경우 7.2% 종교가 없는 경우 8.3%로 나타납니다. 
# 종교가 있는 사람들이 근소하게 이혼율이 작게 나타납니다. 



#===========================================
#09-8 종교 유무에 따른 이혼율
#     - "연령대별 종교가 있는 사람들의 이혼율을 분석" p250
#===========================================

# 연령대에 대한 분석을 추가해보겠습니다. 

#-------------------------------------------
# 관심변수 : 연령대, 종교, 혼인상태
# 분석방법 : 연령대별 종교 유무에 따른 이혼율에 대한 표, 그래프
#-------------------------------------------


#-------------------------------------------
# 1. 변수 검토하기 -  종교유무
#-------------------------------------------
#-------------------------------------------
# 2. 전처리 - 혼인상태
#-------------------------------------------
#-------------------------------------------
# 3. '연령대' 파생변수 만들기
#-------------------------------------------
# 초년: 30세미만, 중년 30~59세, 노년 60세이상

welfare <-welfare %>% 
  mutate(ageg = ifelse(age<30, "young", 
                       ifelse(age<=59, "middle", "old")))

# 대략적인 값을 확인해보겠습니다. 
table(welfare$ageg)
# > table(welfare$ageg)
# 
# middle    old  young 
# 6049   6281   4334 

# 대략적인 그래프를 확인합니다.
qplot(welfare$ageg)
#-------------------------------------------
# 4. 종교, 이혼율의 관계 분석하기
#-------------------------------------------
#-------------------------------------------
# 5. 분석: 종교 유무에 따른 이혼율 표 만들기
#-------------------------------------------
##-----------
##1) 연령대별 이혼율 표 만들기
##-----------
ageg_marriage<-welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n=n()) %>% 
  #mutate(tot_group=sum(n)) %>% 
  #mutate(pct=round(n/tot_group*100, 1))
  mutate(pct=round(n/sum(n)*100, 1))
ageg_marriage

# > ageg_marriage
# # A tibble: 6 x 5
# # Groups:   ageg [3]
# ageg   group_marriage     n tot_group   pct
# <chr>  <chr>          <int>     <int> <dbl>
# 1 middle divorce          437      4918   8.9
# 2 middle marriage        4481      4918  91.1
# 3 old    divorce          273      4165   6.6
# 4 old    marriage        3892      4165  93.4
# 5 young  divorce            2        60   3.3
# 6 young  marriage          58        60  96.7

ageg_marriage<-welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(pct=round(n/sum(n)*100, 1))
ageg_marriage

# > ageg_marriage
# # A tibble: 6 x 4
# # Groups:   ageg [3]
# ageg   group_marriage     n   pct
# <chr>  <chr>          <int> <dbl>
# 1 middle divorce          437   8.9
# 2 middle marriage        4481  91.1
# 3 old    divorce          273   6.6
# 4 old    marriage        3892  93.4
# 5 young  divorce            2   3.3
# 6 young  marriage          58  96.7


# count()를 이용하는 계산 비율표
ageg_marriage_2 <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg, group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct=round(n/sum(n)*100, 1))
ageg_marriage_2


##-----------
##2) 연령대별 이혼율 그래프 만들기
##-----------

# 초년(young)의 경우에는 사례가 부족하여 이후 분석 작업에서 제외합니다. 

agege_divorce<-ageg_marriage %>% 
  filter(ageg!="young" & group_marriage=="divorce") %>% 
  select(ageg, pct)

agege_divorce
# > agege_divorce
# # A tibble: 2 x 2
# # Groups:   ageg [2]
# ageg     pct
# <chr>  <dbl>
#   1 middle   8.9
# 2 old      6.6
#-------------------------------------------
# 6. 분석: 그래프 만들기
#-------------------------------------------

ggplot(data=agege_divorce, aes(x=ageg, y=pct)) + 
  geom_col()
# story: 
# 초년(young) 을 제외하고 이혼율에 대해 비교하면
# 노년보다 중년에서 이혼율이 더 높게 확인됩니다. 


##-----------
##3) 연령대 및 종교 유무에 따른 이혼율 표 만들기
##-----------

# 연령대, 종교유무, 결혼 상태별 집단을 나눠 빈도를 구하고
# 각 집단 전체 빈도를 구해서 비율을 계산하겠습니다. 
# 이혼에 해당하는 값만 추출해서 
# 연령대, 종교 유무, 이혼율에 대한 표를 생성하겠습니다.

# 연령대, 종교유무, 결혼 상태별 비율표 만들기
ageg_religion_marriage<- welfare %>% 
  filter(!is.na(group_marriage)&ageg!="young") %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct=round(n/tot_group*100, 1))

ageg_religion_marriage
# > ageg_religion_marriage
# # A tibble: 8 x 6
# # Groups:   ageg, religion [4]
# ageg   religion group_marriage     n tot_group   pct
# <chr>  <chr>    <chr>          <int>     <int> <dbl>
# 1 middle no       divorce          260      2681   9.7
# 2 middle no       marriage        2421      2681  90.3
# 3 middle yes      divorce          177      2237   7.9
# 4 middle yes      marriage        2060      2237  92.1
# 5 old    no       divorce          123      1884   6.5
# 6 old    no       marriage        1761      1884  93.5
# 7 old    yes      divorce          150      2281   6.6
# 8 old    yes      marriage        2131      2281  93.4

# count()를 이용해서 구할때
ageg_religion_marriage_2<- welfare %>% 
  filter(!is.na(group_marriage)&ageg!="young") %>% 
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct=round(n/sum(n)*100, 1))
ageg_religion_marriage_2

##-----------
#연령대 및 종교 유무별 이혼율 표 만들기 
##-----------
df_divorce<-ageg_religion_marriage %>% 
  filter(group_marriage=="divorce") %>% 
  select(ageg, religion, pct)

df_divorce

# > df_divorce
# # A tibble: 4 x 3
# # Groups:   ageg, religion [4]
# ageg   religion   pct
# <chr>  <chr>    <dbl>
# 1 middle no         9.7
# 2 middle yes        7.9
# 3 old    no         6.5
# 4 old    yes        6.6

##-----------
##4) 연령대 및 종교 유무에 따른 이혼율 그래프 만들기
##-----------
# 종교유무에 따라 막대 색을 다르게 표현하기 위해 fill=religion 을 지정
# 그래프 막대를 분리해서 표시하기 위해 position="dodge" 설정 

ggplot(data=df_divorce,aes(x=ageg, y=pct, fill=religion))+
  geom_col(position="dodge")

ggplot(data=df_divorce, aes(x=ageg, y=pct, fill=religion))+
  geom_col(position="dodge")

#story:
# 노년에서는 종교 유무에 따른 이혼율 차이가 0.1%로 작다.
# 중년에서는 종교가 없는 사람들의 이혼율이 1.8% 더 높다.


#===========================================
#09-9 지역별 연령대 비율
#     - "노년층이 많은 지역은 어디일까" p254
#===========================================
# story:
# 고령 사회가 되면서 노인들을 위한 시설을 마련하는 일이 점점 더 중요해지고 있습니다. 
# 노인들을 위한 시설을 마련하려면 우선 어느 지역에 노인들이 많이 살고 있는지 알아야 합니다. 
# 지역별 연령대 비율을 분석해 노년층이 많은 지역이 어디인지 알아보겠습니다. 

#-------------------------------------------
# 관심변수 : 지역, 연령대
# 분석방법 : 지역별 연령대 비율표와 그래프
#-------------------------------------------

#-------------------------------------------
# 1. 변수 검토하기
#-------------------------------------------
# 연령대 변수
# 지역 변수 
class(welfare$code_region)
table(welfare$code_region)
#-------------------------------------------
# 2. 전처리 - 성별, 직업
#-------------------------------------------

# 코딩북에서 지역 코드를 참조해서 지역코드목목을 생성하겠습니다. 
# h10_reg7	7개 권역별 지역구분	
# "1. 서울      2. 수도권(인천/경기)    3. 부산/경남/울산   
# 4.대구/경북   5. 대전/충남   6. 강원/충북   7.광주/전남/전북/제주도"	
# N(1~7)		(2015년 10차 한국복지패널조사) 조사설계서-가구용(beta1).xlsx

# 
# code_region 변수를 이용해서 
# 지역명 변수를 추가하겠습니다. 
# 
# 지역코드목록 만들기
list_region<-data.frame(code_region=c(1:7), 
                        region=c("서울", 
                                 "수도권(인천/경기)", 
                                 "부산/경남/울산", 
                                 "대구/경북", 
                                 "대전/충남", 
                                 "강원/충북",
                                 "광주/전남/전북/제주도"))
list_region

# > list_region
# code_region                region
# 1           1                  서울
# 2           2     수도권(인천/경기)
# 3           3        부산/경남/울산
# 4           4             대구/경북
# 5           5             대전/충남
# 6           6             강원/충북
# 7           7 광주/전남/전북/제주도

# 지역명 변수 추가
welfare<- left_join(welfare, list_region, id="code_region")

welfare %>% 
  select(code_region, region) %>% 
  head


#-------------------------------------------
# 3. '' 파생변수 만들기
#-------------------------------------------
#-------------------------------------------
# 4. 지역, 연령대 비율의 관계 분석하기
#-------------------------------------------

##------------
# 지역별 연령대 비율 분석하기 
##------------

##------------
# 1. 지역별 연령대 비율표 만들기 p256
##------------
# 지역별 연령대 비율표를 만들겠습니다. 
# 지역 및 연령대별로 나눠서 빈도를 구하고 
# 각 지역의 전체 빈도로 나눠 비율을 구합니다. 
region_ageg<-welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 2))

head(region_ageg)
# > head(region_ageg)
# # A tibble: 6 x 5
# # Groups:   region [2]
# region                ageg       n tot_group   pct
# <fct>                 <chr>  <int>     <int> <dbl>
#   1 강원/충북             middle   417      1257  33.2
# 2 강원/충북             old      555      1257  44.2
# 3 강원/충북             young    285      1257  22.7
# 4 광주/전남/전북/제주도 middle   947      2922  32.4
# 5 광주/전남/전북/제주도 old     1233      2922  42.2
# 6 광주/전남/전북/제주도 young    742      2922  25.4
region_ageg

# count() 이용해서 계산하기 
region_ageg<-welfare %>% 
  count(region, ageg) %>% 
  group_by(region) %>% 
  mutate(pct = round(n/sum(n)*100, 2))
region_ageg
#-------------------------------------------
# 5. 분석: 지역, 연령대 비율 표 만들기
#-------------------------------------------
##------------
# 2. 그래프 만들기 
##------------

ggplot(data=region_ageg, aes(x=region, y=pct))+
  geom_col()+
  coord_flip()


ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg))+
  geom_col()+
  coord_flip()

##------------
# 3. 노년층 비율 높은 순으로 막대 정렬하기 
##------------
# 노년층 비율이 높은 순으로 정렬하겠습니다. 
# 노년층 비율 순으로 정렬한 후 지역명만 추출해서 변수를 만들겠습니다. 

# 노년층 비율 내림차순 정렬
list_order_old <-region_ageg %>% 
  filter(ageg=="old" ) %>%
  arrange(pct)

list_order_old
# > list_order_old
# # A tibble: 7 x 5
# # Groups:   region [7]
# region                ageg      n tot_group   pct
# <fct>                 <chr> <int>     <int> <dbl>
# 1 수도권(인천/경기)     old    1109      3711  29.9
# 2 서울                  old     805      2486  32.4
# 3 대전/충남             old     527      1467  35.9
# 4 부산/경남/울산        old    1124      2785  40.4
# 5 광주/전남/전북/제주도 old    1233      2922  42.2
# 6 강원/충북             old     555      1257  44.2
# 7 대구/경북             old     928      2036  45.6

# 지역명 순서 변수 만들기 
order <-list_order_old$region
order
# > order
# [1] 수도권(인천/경기)     서울                  대전/충남            
# [4] 부산/경남/울산        광주/전남/전북/제주도 강원/충북            
# [7] 대구/경북            
# 7 Levels: 강원/충북 광주/전남/전북/제주도 대구/경북 ... 수도권(인천/경기)

##----------
# 노년층 비율이 높은 순서로 그래프 출력하기 
##----------
ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits=order)


##------------
# 4. 연령대 순으로 막대 색깔 나열하기 
##------------
# 초년-노년-중년 에서 초년-중년-노년 순서로 설정하겠습니다. 
# fill 파라미터에 지정할 변수의 범주(levels)순서를 지정합니다. 
# 현재 ageg변수는 character 타입으로 levels가 없습니다. 
class(region_ageg$ageg)
levels(region_ageg$ageg)
# factor()를 이용해서 ageg변수를 factor 타입으로 변환하고 
# level 파라미터를 이용해 순서를 지정합니다. 



region_ageg$ageg<-factor(region_ageg$ageg, 
                         level = c("old", "middle", "young"))

region_ageg$ageg<-factor(region_ageg$ageg, 
                         level = c("young", "30age", "40age", "50age", "60age", "old"))

class(region_ageg$ageg)
levels(region_ageg$ageg)

#그래프 
ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits=order)

##########################################################
# 직접 자신만의 데이터 분석 프로젝트를 진행해보세요
##########################################################
# 한국복지패널데이터
# 신체건강, 정신건강, 가족간의 관계, 주거환경, 교육수준 등 천여개의 변수가 있습니다.  
# 조사설계서(코드북)을 참조해서 
# 주제를 설정하고 자신만의 데이터 분석 프로젝트를 설계해볼 수 있습니다. 