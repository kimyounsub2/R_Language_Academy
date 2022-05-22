#정리하기

##### 1.조건에 맞는 데이터만 추출하기 #####
exam %>% filter(english >= 80)
# 여러 조건 동시 충족
exam %>% filter(class == 1 & math >= 50)
# 여러 조건 중 하나 이상 충족
exam %>% filter(math >= 90 | english >= 90)
exam %>% filter(class %in% c(1,3,5))

# 2.필요한 변수만 추출하기
exam %>% select(math)
exam %>% select(class, math, english)

# 3.함수 조합하기, 일부만 출력하기
exam %>%
  select(id, math) %>%
  head(10)

# 4.순서대로 정렬하기
exam %>% arrange(math) # 오름차순 정렬
exam %>% arrange(desc(math)) # 내림차순 정렬
exam %>% arrange(class, math) # 여러 변수 기준 오름차순 정렬

# 5.파생변수 추가하기
exam %>% mutate(total = math + english + science)
# 여러 파생변수 한 번에 추가하기
exam %>%
  mutate(total = math + english + science,
         mean = (math + english + science)/3)
# mutate()에 ifelse() 적용하기
exam %>% mutate(test = ifelse(science >= 60, "pass", "fail"))
# 추가한 변수를 dplyr 코드에 바로 활용하기
exam %>%
  mutate(total = math + english + science) %>%
  arrange(total)

# 6.집단별로 요약하기
exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math))
# 각 집단별로 다시 집단 나누기
mpg %>%
  group_by(manufacturer, drv) %>%
  summarise(mean_cty = mean(cty))

# 7.데이터 합치기
# 가로로 합치기
total <- left_join(test1, test2, by = "id")
# 세로로 합치기
group_all <- bind_rows(group_a, group_b)

##### 2.결측치 정제하기 #####
# 결측치 확인
table(is.na(df$score))
# 결측치 제거
df_nomiss <- df %>% filter(!is.na(score))
# 여러 변수 동시에 결측치 제거
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
# 함수의 결측치 제외 기능 이용하기
mean(df$score, na.rm = T)
exam %>% summarise(mean_math = mean(math, na.rm = T))

# 2.이상치 정제하기
# 이상치 확인
table(outlier$sex)
# 결측 처리
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
# boxplot 으로 극단치 기준 찾기
boxplot(mpg$hwy)$stats
# 극단치 결측 처리
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
##### 3.산점도 #####
# 1.산점도
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
# 축 설정 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)
# 2.평균 막대 그래프
# 1 단계.평균표 만들기
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))
# 2 단계.그래프 생성하기, 크기순 정렬하기
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()
# 3.빈도 막대 그래프
ggplot(data = mpg, aes(x = drv)) + geom_bar()# 4.선 그래프
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()
# 5.상자 그림
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

##### 4.주요함수 #####
readLines()
# - 파일을 행 단위로 읽는 기능을 합니다.

extractNoun()
# - 저장된 단어에서 명사만 분리할 때 사용한다.
# - 문장에서 추출한 명사를 list 구조로 출력
# 
as. #으로 시작하는 함수는 변수의 타임을 바꾸는 기능
# - (as.data.frame(), as.Date, as.factor() 등)
# - as.data.frame()은 데이터 속성을 data.frame 형태로 바꾸는 함수
# 
unlist()
# - 데이터를 벡터로 변환할 때 사용하는 함수
# - (단어 처리된 데이터를 더 편하게 처리하기 위해 단어 뭉치형태인 벡터로 변환)
# 
table()
# - 통계분석 함수로 사용 빈도에 따라 워드클라우드에서 크고 굵게 표시해야
# 하므로 단어의 사용 빈도를 파악함
# 
nchar()
# - 두 글자 이상인 단어만 변수에 할당
# 
# 8. wordcloud(데이터, 옵션) 형식으로 사용합니다.
# 
# - minSize(시각화할 최소 빈도 수 설정)
# - size(배수 기준 워드클라우드 크기 변경)
# - col(색상 설정)
# - rotateRation(회전율)
# - backgroundColor(배경색)
# - figPath(이미지)
# help 기능 이용
# 
# 
# 
# 
# 9.
# -stringsAsFactors()
# -문자열을 character로 가져온다.(factor로 인식하지 말라는 뜻)
# 
# 10.
# - str_replace_all(string, pattern, replace)
# - str_replace_all(): 패턴에 해당되는 부분을 replace로 모두 교체(치환)한다.
# - stringr 패키지
# 
# 11.
# - hjust은 수평 정렬을 제어하고 vjust은 수직 정렬을 제어합니다.
# - hjust와 vjust의 값은 0과 -1 사이에서만 정의됩니다.
# - 0은 왼쪽 맞춤, -1은 오른쪽 맞춤을 의미
# 
# 12. wordcloud에서
# - scale : 폰트크기 c(MAX, MIN)