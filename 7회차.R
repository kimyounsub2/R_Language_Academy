# 미국 주별 강력 범죄율 단계 구분도 만들기
# install.packages("ggiraphExtra")
library(ggiraphExtra)

str(USArrests)
head(USArrests)

library(tibble) # 행이름 변수로 바꾸기 위해

# 행 이름을 state 변수로 바꿔 데이터 프레임 생성
crime <- rownames_to_column(USArrests, var = "state")
# 지도 데이터와 동일하게 맞추기 위해 state 의 값을 소문자로 수정
crime$state <- tolower(crime$state)
str(crime)

# 미국 주 지도 데이터 준비하기
library(ggplot2)
states_map <- map_data("state")
str(states_map)

# 단계 구분도 패키지 설치
install.packages("mapproj")
library(mapproj)

# 단계 구분도 만들기
ggChoropleth(data = crime, # 지도에 표현할 데이터
             aes(fill = Murder, # 색깔로 표현할 변수
                 map_id = state), # 지역 기준 변수
             map = states_map, # 지도 데이터
             interactive = T) # 인터랙티브

# 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기
# 대한민국 시도별 인구 단계 구분도 만들기
install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

# 대한민국 시도별 인구 데이터 준비하기
str(changeCode(korpop1)) # 한글꺠지기 쉬어 changeCode사용

# 한글 출력시 문제가 발생할수 있어 컬럼명을 바꾸자
library(dplyr)
korpop1 <- rename(korpop1,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
str(changeCode(kormap1))

# 단계 구분도 만드기
ggChoropleth(data = korpop1, # 지도에 표현할 데이터
             aes(fill = pop, # 색깔로 표현할 변수
                 map_id = code, # 지역 기준 변수
                 tooltip = name), # 지도 위에 표시할 지역명
             map = kormap1, # 지도 데이터
             interactive = T) # 인터랙티브 
korpop1$name <- iconv(korpop1$name, "UTF-8","CP949") # 한글변환

# 대한민국 시도별 결핵 환자 수 단계 구분도 만들기
str(changeCode(tbc)) # tbc는 결핵환자수

ggChoropleth(data = tbc, # 지도에 표현할 데이터
             aes(fill = NewPts, # 색깔로 표현할 변수
                 map_id = code, # 지역 기준 변수
                 tooltip = name), # 지도 위에 표시할 지역명
             map = kormap1, # 지도 데이터
             interactive = T) # 인터랙티브
tbc$name <- iconv(tbc$name, "UTF-8","CP949")


# 구글지도 가져오기
install.packages("devtools")
library(devtools)

devtools::install_github("dkahle/ggmap")
install.packages("ggmap")
library(ggmap)

googleAPIkey = "메모장에 api" # 자신의 구글 api
register_google(googleAPIkey)

gg_seoul <- get_googlemap("seoul", zoom = 6, maptype = "roadmap")
ggmap(gg_seoul)

# seoul의 위치 정보를가져온 후 gg_seoul 변수에 할당
gg_seoul <- get_googlemap("seoul", maptype = "terrain")
ggmap(gg_seoul) # gg_seoul의 위치 값에 따른 구글 지도 호출

# 구글 지도 위에 산점도 그리기
library(dplyr)
library(ggplot2)

# 한글 검색을 위해 utf8로 변환한 후 위도와 경도 데이터를 geo_code 변수에 할당
geo_code <- enc2utf8("대전역") %>% geocode()
geo_code
geo_data <- as.numeric(geo_code) # 리스트를 숫자로 변환
geo_data

# 대전역의 위치 정보를 가져온 후 구글 지도 호출
get_googlemap(center = geo_data, maptype = "roadmap", zoom = 13) %>% 
  ggmap()+
  # geo_code에 있는 경도(lon)와 위도(lat) 값으로 산점도 그리기
  geom_point(data = geo_code, aes(x = geo_code$lon, y = geo_code$lat))

# 서울시 종로구 근방의 지도 보기
gc <- geocode(enc2utf8("종로구")) # 지점의 경도 위도
gc
cen <- as.numeric(gc) # 경도 위도를 숫자로
cen
map <- get_googlemap(center=cen) #지도 생성 종로구를 가운데로
ggmap(map)

# 설악산 지도보기
gc <- geocode(enc2utf8("설악산")) # 지점의 경도 위도
cen <- as.numeric(gc) # 경도 위도를 숫자로
map <- get_googlemap(center = cen, # 지도의 중심점 좌표
                     zoom = 9, # 지도 확대 정도
                     size = c(640,640),# 지도의 크기
                     maptype = "roadmap") # 지도의 유형
ggmap(map)

cen <- c(-118.233248,34.085015)
map <- get_googlemap(center = cen)
ggmap(map)

# 분석실전 예제
getwd()
library(stringr)
loc <- read.csv("data/서울_강동구_공영주차장_위경도.csv",header = T)
loc

kd <- get_map("Amsa_dong", zoom = 13, maptype = "roadmap")
kor.map <- ggmap(kd)+geom_point(data = loc, aes(x=LON, y=LAT),
                                size = 3, alpha = 0.7, color = "red")
kor.map + geom_text(data = loc, aes(x = LON, y = LAT +0.001, label = 주차장명),
                    size = 3)
kor.map
ggsave("C:/Work._R/data/kd.png",dpi=500) # dpi 해상도

# 서울시 지역별 장애인도서관 위치 표시하기
loc <- read.csv("data/지역별장애인도서관정보.csv",header = T)
loc
kor <- get_map("seoul", zoom = 11, maptype = "roadmap")
kor.map <- ggmap(kor)+geom_point(data = loc, aes(x=LON, y=LAT),size=5,alpha =0.7)
kor.map + geom_text(data = loc,aes(x = LON, y = LAT+0.01 , label=자치구명),size =3)
ggsave("C:/Work._R/lib.png",dpi=500)

# 서울 지하철 2호선 역위치를 지도에 표시하기
line <- read.csv("data/서울지하철2호선위경도정보.csv",header = T)
line
kor <- get_map("seoul", zoom = 11, maptype = "roadmap") 
kor.map <- ggmap(kor)+geom_point(data = line, aes(x=LON,y=LAT),size=3,alpha=0.7)
kor.map + geom_text(data = line, aes(x=LON,y=LAT+0.005, label = 역명),size = 3)

# 서울 지하철 2,3호선 역위치를 지도에 표시하기
line <- read.csv("data/서울지하철2호선위경도정보.csv",header = T)
line
line1 <- read.csv("data/서울지하철3호선역위경도정보.csv",header = T)
line1
lab_name <- c("2호선","3호선")
lab_color <- c("green","red")
center <- c(mean(line1$LON)-0.03, mean(line1$LAT))
kor <- get_map(center, zoom = 11,maptype = "roadmap")
kor.map <- ggmap(kor) + geom_point(data = line, aes(x=LON,
                                                    y=LAT), size=3,alpha =0.7,color = "green") +
  geom_point(data = line1, aes(x=LON,
                               y=LAT),size=3,alpha =0.7,color = "red")

kor.map + geom_text(data = line,aes(x=LON, y = LAT+0.005,label = 역명),size = 3) +geom_text(data = line1, aes(x=LON, y = LAT+0.005,label = 역명),size = 3)
ggsave("C:/Work._R/lineplus_2.png",dpi=500)

# R로 인터랙티브 지도 만들기(leaflet)
install.packages('leaflet')
library(leaflet)

m <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = 174.768, lat = -36.852,
             popup = "The birthplace of R")
m
# setView() 지도보기의 중심과 확대/축소 수준을 설정
leaflet() %>% 
  setView(lng = 126.9784,lat = 37.566, zoom = 11) %>% 
  addTiles()

# 인터랙티브로 커피 매장 나타내기
sb <- read.csv("data/starbucks.csv")
sb
leaflet(sb) %>% 
  setView(lng=126.9784, lat = 37.566,zoom=11) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addCircles(lng = ~long,lat = ~lat, color = "#006633")

leaflet(sb) %>% 
  setView(lng = 127.7669, lat = 35.90776, zoom = 6) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addMarkers(lng = ~long, lat = ~lat, label = ~address)

# 강원도 으뜸 음식점 만들기
kfood <- read.csv("data/강원도으뜸음식점.csv", header = T)
kfood
library(dplyr)
kfood <- rename(kfood,
                LON = 경도,
                LAT = 위도,
                NAME = 업소명)

leaflet(kfood) %>% s
setView(lng=mean(kfood$LON)-0.03, lat=mean(kfood$LAT), zoom = 6) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addCircleMarkers(lng = ~LON,lat = ~LAT, label = ~NAME, radius = 10)

