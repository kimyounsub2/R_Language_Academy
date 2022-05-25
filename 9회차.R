getwd()
# R 내장 함수로 데이터 추출하기
exam <- read.csv("data/csv_exam.csv")
exam

# 행 번호로 행 추출하기 대괄호안 쉼표 기준, 왼쪽에 행 번호(인덱스) 입력
exam[] # 조건 없이 전체 데이터 출력
exam[1,] # 1 행 추출
exam[2,] # 2 행 추출

# 조건을 충족하는 행 추출하기
exam[exam$class == 1,] # class 가 1 인 행 추출
exam[exam$math >= 80,] # 수학점수가 80 점 이상인 행 추출

# 대괄호 안에 &과|를 사용해 여러조건을 동시에 충족하거나 하나 이상 충족하는 행을 추출 변수명 앞에 데이터 프레임 이름을 반복해서 사용
exam[exam$class == 1 & exam$math >= 50,]# 1 반 이면서 수학점수가 50점 이상
exam[exam$english < 90 | exam$science < 50,]# 영어점수가 90점 미만이거나 과학점수가 50 점 미만

# 열 번호로 변수 추출하기 - 대괄호안 쉼표 오른쪽에 조건을 입력
exam[,1] # 첫 번째 열 추출
exam[,2] # 두 번째 열 추출
exam[,3] # 세 번째 열 추출

# 변수명으로 변수 추출하기, 쉼표 오른쪽에 ""와 함께 변수명 입력
exam[, "class"] # class 변수 추출
exam[, "math"] # math 변수 추출
exam[,c("class", "math", "english")] # class, math, english 변수 추출

# 행, 변수 동시 추출하기
exam[1,3] # 행, 변수 모두 인덱스
exam[5, "english"] # 행 인덱스, 열 변수명
exam[exam$math >= 50, "english"] # 행 부등호 조건, 열 변수명
exam[exam$math >= 50, c("english", "science")] # 행 부등호 조건, 열 변수명

# dplyr과 내장 함수의 차이
# 문제) 수학 점수 50 이상, 영어 점수 80 이상인 학생들을 대상으로 각 반의 전 과목 총평균을 구하라.
# 내장 함수 코드
exam$tot <- (exam$math + exam$english + exam$science)/3
aggregate(data=exam[exam$math >= 50 & exam$english >= 80,], tot~class, mean)
# aggregate함수는 R내장 함수로 데이터를 그룹별로 묶어서 연산
# 사용법 aggregate(계산 열 ~ 기준열, 데이터 세트, 연산함수)
# dplyr 코드
exam %>%
  filter(math >= 50 & english >= 80) %>%
  mutate(tot = (math + english + science)/3) %>%
  group_by(class) %>%
  summarise(mean = mean(tot))

# 변수에는 여러 가지 타입(Type, 속성)이 있음
# 1. 연속 변수(Continuous Variable) - Numeric 타입
# 2. 범주 변수(Categorical Variable) - Factor 타입

# 변수 타입 간 차이 알아보기
var1 <- c(1,2,3,1,2) # numeric 변수 생성
var2 <- factor(c(1,2,3,1,2)) # factor 변수 생성
var1 # numeric 변수 출력 : 값들의 출력
var2 # factor 변수 출력 : factor값이 어떤 범주로 구성되었는지 의미

# Factor 변수는 연산이 안된다. 데이터가 숫자로 되어 있지만 크기가 아니라 범주를 의미하기 때문에 연산이 안된다.
var1+2 # numeric 변수로 연산
var2+2 # factor 변수로 연산 X

# 변수 타입 확인하기
class(var1)
class(var2)

# factor 변수의 구성 범주 확인하기 factor변수의 값이 어떤 범주로 구성되는지 알수 있다.
levels(var1)
levels(var2)

# 문자로 구성된 factor 변수
var3 <- c("a", "b", "b", "c") # 문자 변수 생성
var4 <- factor(c("a", "b", "b", "c")) # 문자로 된 factor 변수 생성
var3
var4
class(var3)
class(var4)

# 함수마다 적용 가능한 변수 타입이 다르다
# mean에는 numeric 변수만 적용, factor 변수를 mean 적용하면 에러가 난다.
mean(var1)
mean(var2)

# 변수 타입 바꾸기
var2 <- as.numeric(var2) # 변수 타입을 numeric타입으로 변환
mean(var2) # 함수 재적용
class(var2) # 타입 확인
levels(var2) # 범주 확인

# 변환 함수(Coercion Function) :as로 시작하는 함수는 변수타입을 바꾼다

# 혼자서 해보기
mpg <- as.data.frame(ggplot2::mpg)
class(mpg$drv)
mpg$drv <- as.factor(mpg$drv)
class(mpg$drv)
levels(mpg$drv)

# 지하철역 주변 아파트 가격 알아보기
library(devtools)
library(ggmap)
library(dplyr)

station_datal <- read.csv("data/지하철역별_주소_전화번호.csv")
str(station_datal)
googleAPIkey <- "구글 API키"
register_google(googleAPIkey)
# as.character 함수로 문자형으로 변환한 후 staion_code에 할당
station_code <- as.character(station_datal$"구주소")
# geocode함수로 station_code 값을 위도와 경도로 변환
station_code <- geocode(station_code)
head(station_code)
# 문자형으로 변환하고 utf8로 변환한 후 위도와 경도로 변환
station_code <- as.character(station_datal$"구주소") %>% enc2utf8() %>% geocode()
head(station_code)

# station_datal와 station_code를 합친후 station_code_final에 할당
station_code_final <- cbind(station_datal, station_code)
head(station_code_final)

# 아파트 실거래가
apart_data <- read.csv("data/아파트_실거래가.csv")
head(apart_data)

# 전용면적의 값을 반올림하여 정수로 표현
apart_data$전용면적 = round(apart_data$전용면적)
head(apart_data)

# 전용면적을 기준으로 빈도를 구한 후 빈도에 따라 내림차순 정렬
count(apart_data, 전용면적) %>% 
  arrange(desc(n))

# 전용면적이 85인 데이터만 추출하여 apart_data_85에 할당
apart_data_85 <- subset(apart_data, 전용면적 == "85")
head(apart_data_85)

# 쉼표를 공백("")으로 대체하여 제거
apart_data_85$거래금액 <- gsub(",", "", apart_data_85$거래금액)
head(apart_data_85)

# 거래금액을 정수형으로 변환하여 단지명별 평균을 구한다
apart_data_85_cost <- aggregate(as.integer(거래금액) ~ 단지명,
                                apart_data_85, mean)
head(apart_data_85_cost)

# as.integer(거래금액)을 거래금액으로 변경
apart_data_85_cost <- rename(apart_data_85_cost, "거래금액" = "as.integer(거래금액)")
head(apart_data_85_cost)

# 당지명이 중복된 행을 제거
apart_data_85 <- apart_data_85[!duplicated(apart_data_85$단지명),]
head(apart_data_85)

# 단지명을 기준으로 apart_data_85와 apart_data_85_cost 합치기
apart_data_85 <- left_join(apart_data_85,apart_data_85_cost,by="단지명")
head(apart_data_85)

# "단지명","시군구","번지","전용면적","거래금액.y"만 추출하고 저장
apart_data_85 <- apart_data_85 %>% 
  select("단지명","시군구","번지","전용면적","거래금액.y")
head(apart_data_85)

# 거래금액.y 를 거래금액으로 변경
apart_data_85 <- rename(apart_data_85, "거래금액"="거래금액.y")
head(apart_data_85)

# 시군구와 번지를 하나로 합치기
apart_address <- paste(apart_data_85$"시군구", apart_data_85$"번지")
head(apart_address)
class(apart_address)

# 시군구와 번지 열을 합친 후 데이터 프레임 구조로 저장
apart_address <- paste(apart_data_85$"시군구", apart_data_85$"번지") %>% data.frame()
head(apart_address)

# .을 주소로 컬럼 명칭 변경
apart_address <- rename(apart_address, "주소" = ".")
head(apart_address)

# 아파트 주소를 위.경도로 변환하여 저장
apart_address_code <- as.character(apart_address$"주소") %>% 
  enc2utf8() %>%
  geocode()

# 데이터 세트를 합친 후 일부 열만 저장
apart_code_final <- cbind(apart_data_85, apart_address, apart_address_code) %>% 
  select("단지명","전용면적","거래금액","주소",lon,lat)
head(apart_code_final)

# 홍대입구역
hongdae_map <- get_googlemap("hongdae station", maptype = "roadmap",zoom = 15)

# 홍대입구역 지도에 지하철 정보 및 아파트 정보 일괄 표시
ggmap(hongdae_map)+
  geom_point(data = station_code_final, aes(x=lon,y=lat), colour = "red", size = 3)+
  geom_text(data = station_code_final,aes(label = 역명, vjust = -1))+
  geom_point(data = apart_code_final, aes(x = lon,y=lat))+
  geom_text(data = apart_code_final, aes(label = 단지명, vjust = -1)) +
  geom_text(data = apart_code_final,aes(label=거래금액,vjust =1))

library(leaflet)
leaflet(apart_code_final) %>% 
  setView(lng=126.9510,lat=37.54671,zoom = 15) %>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addMarkers(lng=~lon,lat=~lat,
             popup=paste( "전용면적:",apart_code_final$전용면적,"<br>",
                          "거래금액:",apart_code_final$거래금액,"<br>",
                          "주소",apart_code_final$주소,"<br>"),
             label=~단지명,
             clusterOptions = markerClusterOptions())

