##### 2회차 #####
# 1. 2013년_프로야구선수_성적.csv을 data1 변수에 넣으세요.
setwd('c:/Work._R/Data')
getwd()
data1 <- read.csv("2013년_프로야구선수_성적.csv")
data1
# 2. data1에서 경기수가 120 경기 이상인 선수들만 data_a 변수에 넣으세요.
data_a <- data1 %>% filter(경기 >= 120) 
data_a
# 3. data1에서 경기수가 120 경기 이상이고 득점도 80점 이상인 선수들만 data_b 변수에 저장하세요.
data_b <- data1 %>% filter(경기 >=120 & 득점 >= 80)
data_b
# 4. 포지션이 1루수와 3루수인 선수만 data_c에 넣어 출력하세요.
data_c <- data1 %>% filter(포지션=="1루수" | 포지션=="3루수" )
data_c                             
# 5. 선수명, 포지션, 팀 컬럼 데이터만 조회하기
data1 %>% 
  select(선수명, 포지션, 팀)
# 6. 순위 ~ 타수 컬럼까지 출력하기
data1 %>% 
  select(순위, 선수명, 포지션, 팀, 경기, 타수)
# 7. 특정 컬럼만 제외하고 출력하기(홈런, 타점, 도루)
data1 %>% 
  select(-홈런,-타점,-도루)
# 8. 타수가 400초과한 선수명, 팀, 경기, 타수 칼럼을 출력하세요.
data1 %>% 
  filter(타수>400) %>% 
  select(선수명,팀,경기,타수)
# 9.위 8번을 타수 순으로 정렬하세요.
data1 %>% 
  filter(타수>400) %>% 
  select(선수명,팀,경기,타수) %>% 
  arrange(타수)

##### 3회차 ######
# 1. 회사별로 "suv" 자동차의 도시 및 고속도로 통합 연비 평균을 구해 내림차순으로 정렬하고, 1~5위까지 출력하기

mpg %>%
  group_by(manufacturer) %>% # 회사별로 분리
  filter(class == "suv") %>% # suv 추출
  mutate(tot = (cty+hwy)/2) %>% # 통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>% # 통합 연비 평균 산출
  arrange(desc(mean_tot)) %>% # 내림차순 정렬
  head(5) # 1~5 위까지 출력

# 2.mpg 데이터를 이용해서 분석 문제를 해결해 보세요.
# mpg 데이터의 fl 변수는 자동차에 사용하는 연료(fuel)를 의미합니다. 아래는 자동차 연료별 가격을 나타낸 표입니다.
# fl 연료 종류 가격(갤런당 USD)
# c CNG 2.35
# d diesel 2.38
# e ethanol E85 2.11
# p premium 2.76
# r regular 2.22
#우선 이 정보를 이용해서 연료와 가격으로 구성된 데이터 프레임을 만들어 보세요.
mpg <- as.data.frame(ggplot2::mpg)
View(mpg)
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel
mpg <- left_join(mpg, fuel, by = "fl")
head(mpg,20)
# 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 model, fl, price_fl 변수를 추출해 앞부분 5행을 출력해 보세요.
mpg %>% 
  select(model,fl,price_fl) %>% 
  head(5)

# excel_exam.xlsx에서 20명의 학생의 데이터를 불러와서
exam_excl <- read_excel("data/excel_exam.xlsx")
head(exam_excl,20)

# 1. math, english, science 3과목의 합계를 출력하세요.(s_total)
s_total <- exam_excl %>% mutate(total = math + english + science)
head(s_total,20)

# 2. math, english, science 3과목의 평균을 출력하세요.(s_mean)
s_mean <- exam_excl %>% mutate(total = math + english + science, 
                               mean = (math + english + science)/3)
head(s_mean,20)

# 3. 평균이 60점 이상이면 "pass" 그렇지 않으면 "fail"를 부여하는파생변수test1를 생성하세요.
test1 <- exam_excl %>% mutate(total = math + english + science,
                              mean = (math + english + science)/3) %>% 
  mutate (test=ifelse(mean >= 60, "pass", "fail"))
head(test1)

# 4. 3의 결과의 빈도수와 그래프로 나타내세요
table(test1$test)
qplot(test1$test)

# 5. 평균이 80점 이상이면 "A", 70이상이면 "B", 그외는 "C"를 부여하는 파생변수 test2를 생성하세요.
test1$test2 <- ifelse(test1$mean >= 80, "A",
                      ifelse(test1$mean >= 70, "B","c"))
View(test1)

# 6. 5의 결과를 상위 10개만 출력하세요.
test1$test2 <- ifelse(test1$mean >= 80, "A",
                      ifelse(test1$mean >= 70, "B","c"))
View(head(test1,10))

# 7. 5의 결과를 하위 7개만 출력하세요.
test1$test2 <- ifelse(test1$mean >= 80, "A",
                      ifelse(test1$mean >= 70, "B","c"))
View(tail(test1,7))

# 8. 5의 결과를 빈도수와 그래프로 나타내세요.
table(test1$test2)
qplot(test1$test2)

##### 4회차 #####
mpg <- as.data.frame(ggplot2::mpg) # mpg 데이터 불러오기
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA # NA 할당하기
View(mpg)

# Q1. drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고 합니다. 분석을 하기 전에 우선 두 변수에 결측치가 있는지 확인해야 합니다. drv 변수와 hwy 변수에 결측치가 몇 개 있는지 알아보세요.
table(is.na(mpg))
table(is.na(mpg$drv))
table(is.na(mpg$hwy))

# Q2. filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동방식의 hwy 평균이 높은지 알아보세요. 하나의 dplyr 구문으로 만들어야 합니다
mpg %>% filter(!is.na(hwy)) %>% # 결측치 제외
  group_by(drv) %>% # drv 별 분리
  summarise(mean_hwy = mean(hwy)) # hwy 평균 구하기

# Q1. mpg 데이터의 cty(도시 연비)와 hwy(고속도로 연비) 간에 어떤 관계가 있는지 알아보려고 합니다. x 축은 cty, y 축은 hwy 로 된 산점도를 만들어 보세요.
mpg <- as.data.frame(ggplot2::mpg)
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()

# Q2. 미국 지역별 인구통계 정보를 담은 ggplot2 패키지의 midwest 데이터를 이용해서 전체 인구와 아시아인 인구 간에 어떤 관계가 있는지 알아보려고 합니다. x축은 poptotal(전체 인구), y축은 popasian(아시아인 인구)으로 된 산점도를 만들어 보세요. 전체 인구는 50만 명 이하, 아시아인 인구는 1만 명 이하인 지역만 산점도에 표시되게 설정하세요.
midwest <- as.data.frame(ggplot2::midwest)
View(midwest)
ggplot(data = midwest, aes(x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0, 500000) +
  ylim(0, 10000)

options(scipen = 99)
options(scipen = 0)

# Q1. 어떤 회사에서 생산한 "suv" 차종의 도시 연비가 높은지 알아보려고 합니다. "suv" 차종을 대상으로 평균 cty(도시 연비)가 가장 높은 회사 다섯 곳을 막대 그래프로 표현해 보세요. 막대는 연비 가 높은 순으로 정렬하세요.
mpg <- as.data.frame(ggplot2::mpg)

mpg_suv <- mpg %>%
  filter(class == "suv") %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)
mpg_suv

ggplot(data = mpg_suv, aes(x = reorder(manufacturer, -mean_cty),
                           y = mean_cty)) + geom_col()

# Q2. 자동차 중에서 어떤 class(자동차 종류)가 가장 많은지 알아보려고 합니다. 자동차 종류별 빈도를 표현한 막대 그래프를 만들어 보세요.
ggplot(data = mpg, aes(x = class)) + geom_bar()

# Q1. psavert(개인 저축률)가 시간에 따라서 어떻게 변해왔는지 알아보려고 합니다. 시간에 따른 개인 저축률의 변화를 나타낸 시계열 그래프를 만들어 보세요.
ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()

# Q1. class(자동차 종류)가 "compact", "subcompact", "suv"인 자동차의 cty(도시 연비)가 어떻게 다른지 비교해보려고 합니다. 세 차종의 cty를 나타낸 상자 그림을 만들어보세요.
mpg <- as.data.frame(ggplot2::mpg)
new <- mpg %>% 
  filter(class %in% c("compact", "subcompact", "suv"))

ggplot(data = new, aes(x = class, y = cty)) + geom_boxplot()

#1.학생별국어성적_new.txt 데이터를 불러오세요.
library(readxl)
data <- read.table("data/학생별국어성적_new.txt", fileEncoding = "euc-kr",header = TRUE, sep=",")
View(data)

#2 x축은 이름, y축은 점수로 산점도에 표시되게 그래프를 그려 보세요.
ggplot(data = data, aes(x=이름, y=점수)) + geom_point(color="red", size=5)+geom_point(color="white", size=2)

#3 2을 막대그래프로 그려 보세요.
ggplot(data = data, aes(x = 이름, y = 점수)) + geom_col(color= 'black', fill='skyblue')

#4 학생별과목별성적_3기_3명.csv를 아래와 같은 그래프로 출력하세요.
data3 <- read.csv('data/학생별과목별성적_3기_3명.csv',fileEncoding = "euc-kr", header = T, sep = ',')
View(data3)
ggplot(data = data3, aes(x = 과목, y = 점수,group = 이름, color = 이름)) + geom_line() +geom_point(size=5,  pch = 15)

##### 5회차 #####
# 1. 결혼 유/무와 소득은 관계가 있을까요? (0:비해당, 1:유배우자, 5: 미혼 )
# 1-1 혼인상태 데이터 검토 및 전처리
class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage == 1, "유배우자",
                                 ifelse(welfare$marriage == 5, "미혼",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
# 1-2 qplot로 나타내기
qplot(welfare$group_marriage)

# 2. 결혼 유/무와 남녀 소득의 관계를 나타내세요.
# 2-1 결혼유무와 남녀 소득의 월급 평균표
sex_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(group_marriage, sex ) %>% 
  summarise(n = n())

sex_marriage
# 2-2 그래프로 나타내기
ggplot(data = sex_marriage, aes(x=group_marriage, y = n ,fill =sex )) +geom_col(position = "dodge") +
  scale_x_discrete(limits = c("유배우자", "미혼")) +
  scale_fill_brewer(palette = "Set1")

# 3. 교육 수준별 월급차이 - 교육 정도에 따른 급여 차이가 나는지 확인해 보세요.
# 3-1 교육 수준별 평균 소득
class(welfare$h10_g6)
table(welfare$h10_g6)

welfare$Education <- ifelse(welfare$h10_g6 == 1, "미취학(만 7세미만)",
                            ifelse(welfare$h10_g6 == 2, "무학(만 7세이상)",
                                   ifelse(welfare$h10_g6 == 3, "초등학교",
                                          ifelse(welfare$h10_g6 == 4, "중학교",
                                                 ifelse(welfare$h10_g6 == 5, "고등학교", 
                                                        ifelse(welfare$h10_g6 == 6, "전문대학",
                                                               ifelse(welfare$h10_g6 == 7, "대학교",
                                                                      ifelse(welfare$h10_g6 == 8, "대학원(석사)",
                                                                             ifelse(welfare$h10_g6 == 9, "대학원(박사)",NA)))))))))
table(welfare$Education)
table(is.na(welfare$Education))

Education_income <- welfare %>%
  filter(!is.na(Education) & !is.na(income)) %>%
  group_by(Education) %>%
  summarise(mean_income = mean(income))
Education_income

# 3-2 그래프로 나타내기
ggplot(data = Education_income, aes(x = reorder(Education, mean_income), y = mean_income)) +
  geom_col()


##### 6회차 #####
# wordcloud로 데이터 분석(jeju.txt 이용)

# 1. 최다 빈도 단어를 막대그래프로 나타내시고
# 파일 불러오기
txt <- readLines("data/jeju.txt")
head(txt)
# 특수문자 제거
txt <- str_replace_all(txt, "\\W", " ")
head(txt)
# 제주도여행이네 제주관련 단어 빈칸으로 변경
aa1 <- gsub("제주", "",txt)
# 명사들을 추출한다.
nouns <- extractNoun(aa1)
nouns
# 추출한 명사 list 를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
wordcount
# 데이터 프레임으로 변환 - 문자열,character로 가져와라
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
head(df_word)
# 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
head(df_word,20)
# 두글자 이상 단어 추출
df_word <- filter(df_word, nchar(word) >= 2)
View(df_word)
# 탑 20개만 조회되게 
top_20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top_20
# 빈도 순서 변수 생성
order <- arrange(top_20, freq)$word
# 탑20 막대그래프 
ggplot(data = top_20, aes(x = word, y = freq)) +
  geom_col(fill = "skyblue") +
  coord_flip() +scale_x_discrete(limit = order) + 
  geom_text(aes(label = freq), hjust = 1.5)

# 2.
pal <- brewer.pal(8,"Dark2") 
set.seed(1234) 
wordcloud(words = df_word$word, 
          freq = df_word$freq, 
          min.freq = 10, 
          max.words = 200, 
          random.order = F, 
          rot.per = .1, 
          scale = c(6, 0.2),
          colors = pal) 


##### 7회차 #####
# 강원도 음식점 정보를 지도상에 표현해 보세요,
# 강원도으뜸음식점.csv
kfood <- read.csv("data/강원도으뜸음식점.csv", header = T)
kfood
library(dplyr)
kfood <- rename(kfood,
                LON = 경도,
                LAT = 위도,
                NAME = 소재지지번주소)
leaflet(kfood) %>% 
  setView(lng=mean(kfood$LON)-0.03, lat=mean(kfood$LAT), zoom = 6) %>%
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addCircleMarkers(lng = ~LON,lat = ~LAT, label = ~NAME, radius = 10,
                   popup=paste( "주소:",kfood$소재지도로명주소
                                ,"<br>","주요메뉴:",kfood$주요메뉴
                                ,"<br>","업태:",kfood$업태
                                ,"<br>"))

# 전기차 충전소 위치를 지도상에 표현해 보세요.
loc <- read.csv("data/car.csv", header = T)
loc
loc <- rename(loc,
              LON = 경도,
              LAT = 위도,
              NAME = 충전소명)
leaflet(loc) %>% 
  setView(lng = 128.639976, lat = 35.918978, zoom = 6) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(lng = ~LON,lat = ~LAT, label = ~NAME, radius = 10,clusterOptions = markerClusterOptions())

##### 8회차 #####
# 주소만으로 지도 표시하기
line5 <- read_excel("line5.xlsx")
line5 <- line5 %>% 
  select("역명","도로명주소")
line5_addr <- geocode(line5$도로명주소)
line5 <- cbind(line5, line5_addr)
head(line5)


#강남구 커피숍 데이터 분석 트리맵으로 표현해 보세요.
# 1) 트리맵으로 표현,
library(readxl)
coffee <- read_excel("gangnam_coffee.xlsx")
head(coffee)

addr <- substr(coffee$소재지전체주소,10,14)
head(addr)
addr_trim <- gsub(" ","",addr)# 공백제거
head(addr_trim)
str(addr_trim)
addr_count <- addr_trim %>% table() %>% data.frame()
addr_count

addr_count$num <- paste(addr_count$.,addr_count$Freq, sep = "\n")
head(addr_count)

treemap(addr_count, index = "num" ,vSize = "Freq",palette = brewer.pal(n=8,"Purples"),title = "강남구 커피숍 데이터 분석")
a <- arrange(addr_count,desc(Freq)) %>% head(10)
a
# 2. 리플렛으로 지도상에 표현해보세요
coffee_addr <- geocode(coffee$소재지전체주소)
coffee <- cbind(coffee, coffee_addr)
head(coffee)

leaflet(coffee) %>% 
  setView(lng = 127.0505, lat = 37.50184, zoom = 6) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addMarkers(lng = ~lon,lat = ~lat,clusterOptions = markerClusterOptions(),popup=paste("사업장명:",coffee$사업장명,"<br>",
                                                                                       "주소:",coffee$소재지전체주소,"<br>"))

##### 9회차 #####
# 1.파일(line5.xlsx)을 로드하시고, 위도와 경도로 변환하세요.
line5 <- read_excel("data/line5.xlsx")
line5 <- line5 %>% 
  select("역명","도로명주소")
line5_addr <- geocode(line5$도로명주소)
line5 <- cbind(line5, line5_addr)
head(line5)

# 2. 강서구 아파트 데이터 (apartment.xlsx)에서 전용면적=85인 아파트만 추출
apartment <- read_excel("data/apartment.xlsx", skip = 15)
head(apartment)

apartment <- rename(apartment, "전용면적" = "전용면적(㎡)")
head(apartment)
class(apartment$전용면적)

apartment$전용면적 <- as.numeric(apartment$전용면적)

apartment$전용면적 = round(apartment$전용면적)
head(apartment)

apartment_85 <- subset(apartment, 전용면적 == "85")
head(apartment_85)

apartment_85 <- apartment_85[!duplicated(apartment_85$단지명),]
head(apart_data_85)

apartment_data_85 <- apartment_85 %>% 
  select("단지명", "시군구", "번지", "전용면적")
head(apartment_data_85)

apart_addr <- paste(apartment_data_85$"시군구", apartment_data_85$"번지")
head(apart_addr)

apart_addr <- paste(apartment_data_85$"시군구", apartment_data_85$"번지") %>% 
  data.frame()

apart_addr <- rename(apart_addr, "주소" = ".")

apart_addr_code <- as.character(apart_addr$"주소") %>% 
  geocode()

# 3. 산점도를 이용해 지하철역 위치 표시 및 역명을 표시하시고 25평 아파트를 지도상에 표현해 보세요.
final <- 
  cbind(apartment_85, apart_addr, apart_addr_code) %>% 
  select("단지명", "전용면적", "주소", lon, lat)

leaflet(final) %>% 
  setView(lng = 126.85, lat = 37.55, zoom = 14) %>% 
  addProviderTiles('Esri.WorldTopoMap') %>%
  addMarkers(lng = ~lon, lat = ~lat, label = ~단지명,clusterOptions = markerClusterOptions()) %>% 
  addMarkers(lng = ~line5$lon, lat = ~line5$lat, label = ~line5$역명,)
##### 10회차 #####
# 지역별 미세먼지 농도 비교하기
library(readxl)
library(dplyr)
# 미세먼지 데이터 - 영등포구와 용산구를 비교해서
dustdata <- read_excel("data/dustdata.xlsx")
View(dustdata)
str(dustdata)

dustdata_anal <- dustdata %>% filter(area %in% c("영등포구","용산구"))
View(dustdata_anal)

count(dustdata_anal, yyyymmdd) %>% arrange(desc(n))
count(dustdata_anal, area) %>% arrange(desc(n))

dust_anal_area_ydp <- subset(dustdata_anal, area == "영등포구")
dust_anal_area_ydp
dust_anal_area_yg <- subset(dustdata_anal, area == "용산구")
dust_anal_area_yg

library(psych)

describe(dust_anal_area_ydp$finedust)
describe(dust_anal_area_yg$finedust)
dust_anal_area_ydp
# 1.boxplot로 나타내기 
boxplot(dust_anal_area_ydp$finedust, dust_anal_area_yg$finedust,
        main = "finedust_compare", xlab = "AREA", name = c("영등포구","중구"),
        ylab = "FINEDUST_PM", col = c("blue","green"))
# 2. t.test로 가설을 검정해서 결론을 토출하기
t.test(data = dustdata_anal, finedust ~ area, var.equal = T)
View(dustdata_anal)

# 3. 시계열 그래프 그리기
dust_anal_area_ydp <- rename(dust_anal_area_ydp, "date" = "yyyymmdd")
dust_anal_area_yg <- rename(dust_anal_area_yg, "date" = "yyyymmdd")
dustdata_anal

library(xts)
ydp <- xts(dust_anal_area_ydp$finedust, order.by =as.Date(dust_anal_area_ydp$date))

head(ydp)
dygraph(ydp)

yg <- xts(dust_anal_area_yg$finedust, order.by =as.Date(dust_anal_area_yg$date))

final <- cbind(ydp,yg)

dygraph(final) %>% dyRangeSelector()

