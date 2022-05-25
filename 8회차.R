
# 기술 통계와 추론 통계
# • 기술 통계(Descriptive statistics)
# – 데이터를 요약해 설명하는 통계 기법
# • 추론 통계(Inferential statistics)
# – 단순히 숫자를 요약하는 것을 넘어 어떤 값이 발생할 확률을 계산하는 통계 기법

# t 검정(t-test) - 두 집단의 평균 비교
# 두 집단의 평균에 통계적으로 유의한 차이가 있는지 알아볼 때 사용하는 통계 분석 기법

# compact 자동차와 suv 자동차의 도시 연비 t 검정
mpg <- as.data.frame(ggplot2::mpg)
library(dplyr)

# class,cty 변수만 남긴뒤 class 변수가 compact인 자동차와 suv인 자동차를 추출
mpg_diff <- mpg %>%
  select(class, cty) %>%
  filter(class %in% c("compact", "suv"))
head(mpg_diff)
table(mpg_diff$class)

# t-test를 이용한 t 검정
# 집단간 분산(값이 퍼져있는 정도)이 같다고 가정하고 var.equal에 T를 지정
# t.test(data = 데이터 세트, 변수2 ~ 변수1, var.equal = T)
# 변수1(명목변수)가 와야한다. 
# ex남녀간의 키 비교(변수2-키, 변수1-남,여)
# var.equal = T: 두집단 간의 분산이 동일하다는 전제

t.test(data = mpg_diff, cty ~ class, var.equal = T) 
# p-value < 2.2e-16 : 2.2x10의 -16승

# 결과 : T검정 결과에서 p-value가 유의확률을 의미 -> 유의확률 5%를 판단 기준으로 삼고 p-value가 0.05미만이면 집단간 차이가 통계적으로 유의하다고 해석 따라서 cipact와 suv간 평균 도시 연비 차이가 통계적으로 유의하다
# compact 20.12766  > suv 13.50000 suv보다 compact의 도시 연비가 더높다고 할수있다
# (자유도,df): 총 데이터 수 -1

# 일반 휘발유와 고급 휘발유의 도시 연비 t 검정
mpg_diff2 <- mpg %>%
  select(fl, cty) %>%
  filter(fl %in% c("r", "p")) # r:regular, p:premium
table(mpg_diff2$fl)

t.test(data = mpg_diff2, cty ~ fl, var.equql =T)
# p-value = 0.2283 > 0.05 H제로 채택 H1 기각
# p-value 가 0.05보다 큰 0.2875로 실제로는 차이가 없는데 우연에 의해 이런 차이가 관찰될 확률이 28.75%라는 의미
# 결론 : 일반 휘발류와 고급휘발유를 사용하는 자동차 간 도시 연비 차이가 통계적으로 유의하지 않다. 고급휘발유와 자동차의 도시 연비 평균이 0.6정도 높다. 이런 정도의 차이는 우연히 발생했을 가능성이 크다고 해석

# 실업자 수와 개인 소비 지출의 상관관계
economics <- as.data.frame(ggplot2::economics)
head(economics,10)
# 상관분석
cor.test(economics$unemploy, economics$pce)
# p-value < 2.2e-16 -> p-value가 0.05미만이므로 실업자 수와 개인 소비 지출의 상관이 통계적으로 유의하다.
# 상관계수가 cor가 양수 0.61 -> 실버자 수와 개인 소비지출은 한변수가 증가하가하면 다른 변수가 증가하는 정비례 관계임을 알 수 있다.

# 상관행렬 히트맵 만들기 - 상관행렬(Correlation Matrix)
head(mtcars)
# 0.8≤r 일 때, 강한 상관이 있다.
# 0.6≤r<0.8 일 때, 상관이 있다.
# 0.4≤r<0.6 일 때, 약한 상관이 있다.
# r≤0.4 거의 상관이 없다.

# 상관행렬 만들기
car_cor <- cor(mtcars) # 상관행렬 생성
round(car_cor, 2) # 소수점 셋째 자리에서 반올림해서 출력
# Mpg(연비)행과 cyl(실린더 수 )열이 교차되는 부분을 보면 연비가 높을수록 실린더 수가 적은 경향이 있다는 것을 안다.
# cyl(실린더수)와 wt(무게)의 상관계수가 0.78이므로, 실린더 수가 많을수록 자동차는 무거운 경향이 있다.

# 상관행렬 히트맵  - 히트맵(heat map) : 값의 크기를 색깔로 표현한 그래프
install.packages("corrplot")
library(corrplot)

corrplot(car_cor)
# 상관관계가 클수록 원의 크기가 크고 색깔이 진하다. 상관계수가 양수이면 파란색, 음수면 빨간색, 표현 원의 크기와 색깔을 보면 상관관계의 정도와 방향을 쉽게 파악

# 원 대신 상관계수 표시
corrplot(car_cor, method = "number")
corrplot(car_cor, method = "pie")

# 다양한 파라미터 지정하기
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(car_cor,
         method = "color", # 색깔로 표현
         col = col(200), # 색상 200 개 선정
         type = "lower", # 왼쪽 아래 행렬만 표시
         order = "hclust", # 유사한 상관계수끼리 군집화
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black", # 변수명 색깔
         tl.srt = 45, # 변수명 45 도 기울임
         diag = F) # 대각 행렬 제외

getwd()
setwd("C:/Work._R/data")
dustdata <- read_excel("dustdata.xlsx")
dustdata
library(dplyr)
# 성북구와 중구 데이터만 추출 및 확인
dustdata_anal <- dustdata %>% 
  filter(area %in% c("성북구","중구"))
View(dustdata_anal)

# dustdata_anal 데이터 세트에 yyyymmdd에 따른 데이터 수 파악
count(dustdata_anal,yyyymmdd) %>% arrange(desc(n))
# dustdata_anal 데이터 세트에 area에 따른 데이터 수 파악
count(dustdata_anal,area) %>% arrange(desc(n))

# area 값이 성북구인 데이터를 dust_anal_area_sb에 할당
dust_anal_area_sb <- subset(dustdata_anal, area == "성북구")
dust_anal_area_sb
# area 값이 중구인 데이터를 dust_anal_area_sb에 할당
dust_anal_area_jg <- subset(dustdata_anal, area == "중구")
dust_anal_area_jg

install.packages("psych")
library(psych)
# 성북구의 미세먼지량에 대한 기초 통계량 도출
describe(dust_anal_area_sb$finedust)
# 중구의 미세먼지량에 대한 기초 통계량 도출
describe(dust_anal_area_jg$finedust)
# 성북구와 중구의 미세먼지 농도에 대해 boxplot을 통한 분포 차이 확인
boxplot(dust_anal_area_sb$finedust, dust_anal_area_jg$finedust,
        main = "finedust_compare",xlab = "AREA", name = c("성북구","중구"),
        ylab = "FINEDUST_PM",col=c("blue","green"))
# dustdata_anal 데이터 세트에서 측정소명(area)에 따라 미세먼지 농도 평균에 대한 차이를 입증
t.test(data = dustdata_anal, finedust ~ area, var.equal = T)

# 치킨집 데이터 다운로드하기
library(readxl)
ck <- read_excel("치킨집_가공.xlsx")
head(ck)

# substr()함수를 이용하여 소재지 전체 주소에 있는 12 ~16번째 문자 추출
addr <- substr(ck$소재지전체주소,12,16)
head(addr)
# gsub(찾을것, 바꿀것,열지정): sub(substitute),g(global)을 의미
addr_num <- gsub("[0-9]","",addr)# 정규표현식 숫자제거
head(addr_num)

addr_trim <- gsub(" ","",addr_num)# 공백제거
head(addr_trim)
str(addr_trim)

#table 함수를 이용해서 숫자세기,변수가 한개일떄 도수분표표를 만들기
addr_count <- addr_trim %>% table() %>% data.frame()
head(addr_count)

# install.packages("treemap")
library(treemap) # 트리맵

# treemap(데이터, index=인덱스 표시 열 제목, vSize=크기를 이용할 열제목,vColor= 컬러, tite=제목)
treemap(addr_count, index = ".",vSize = "Freq",title = "서대문구 동별 치킨집 분표")
a <- arrange(addr_count,desc(Freq)) %>% head(10)
a
# 색 변경
library(RColorBrewer)
help(RColorBrewer)
treemap(addr_count, index = ".",palette = brewer.pal(n=8,"RdYlGn"),
        vSize = "Freq", title = "서대문구 동별 치킨집 분표")
treemap(addr_count, index = ".",palette = brewer.pal(n=8,"Purples"),
        vSize = "Freq", title = "서대문구 동별 치킨집 분표")

# 주소만으로 지도 표시하기
line5 <- read_excel("line5.xlsx")
line5 <- line5 %>% 
  select("역명","도로명주소")
line5_addr <- geocode(line5$도로명주소)
line5 <- cbind(line5, line5_addr)
head(line5)
