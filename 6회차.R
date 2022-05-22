# 텍스트 마이닝(Text mining) 
# 문자로 된 데이터에서 가치 있는 정보를 얻어 내는 분석 기법
useSystemDic() # 시스템 사전 설정
useSejongDic() # 세종 사전설정

# KoNLP 한글 형태소 분석 패키지 설치하기
# 1. 자바와 rJava 패키지 설치하기
install.packages("multilinguer")
library(multilinguer)
install_jdk()

# 2. KoNLP 의존성 패키지 설치하기
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),type = "binary")

# 3. KoNLP 패키지 설치하기
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))

library(KoNLP)
getwd()
# 사전 설정하기 NIA 사전은 120 만여 개의 단어로 구성된 형태소 사전
# 형태소 분석을 위해 이 사전을 이용하도록 설정
useNIADic()
# 데이터 불러오기
txt <- readLines("data/hiphop1.txt")
head(txt)
# 특수문자 제거 ,Regexp. 한글을 문자별, 단어별 잘라내고 바꾸는일을 담당
# stringr 패키지의 str_replace_all()을 이용해서 문장에 들어있는 특수문자 제거
install.packages("stringr")
library(stringr)

# 특수문자 제거
txt <- str_replace_all(txt, "\\W", " ")
head(txt)

# 명사 추출하기
#install.packages("dplyr")
#install.packages("rJava")
#install.packages("memoise")
#install.packages("KoNLP")
library(KoNLP)
library(dplyr)
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")
extractNoun("너희 사진을 보고 있어도")

# 가사에서 명사추출
nouns <- extractNoun(txt)
nouns
# 추출한 명사 list 를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
wordcount
# 데이터 프레임으로 변환 - 문자열,character로 가져와라
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word
# 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
df_word
# 두글자 이상 단어 추출
df_word <- filter(df_word, nchar(word) >= 2)
top_20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top_20
# 워드 클라우드 만들기
# 패키지 설치
install.packages("wordcloud")
# 패키지 로드
library(wordcloud)
library(RColorBrewer)
#단어 색상 목록 만들기
pal <- brewer.pal(8,"Dark2") # Dark2 색상 목록에서 8 개 색상 추출
# 워드 클라우드 생성
set.seed(1234) # 난수 고정
wordcloud(words = df_word$word, # 단어
          freq = df_word$freq, # 빈도
          min.freq = 2, # 최소 단어 빈도
          max.words = 200, # 표현 단어 수
          random.order = F, # 고빈도 단어 중앙 배치
          rot.per = .1, # 회전 단어 비율
          scale = c(4, 0.3), # 단어 크기 범위
          colors = pal) # 색깔 목록

# 단어 색상 바꾸기
pal <- brewer.pal(9,"Blues")[5:9] # 색상 목록 생성
set.seed(1234) # 난수 고정
wordcloud(words = df_word$word, # 단어
          freq = df_word$freq, # 빈도
          min.freq = 2, # 최소 단어 빈도
          max.words = 200, # 표현 단어 수
          random.order = F, # 고빈도 단어 중앙 배치
          rot.per = .1, # 회전 단어 비율
          scale = c(4, 0.3), # 단어 크기 범위
          colors = pal) # 색상 목록

# 국정원 트윗 텍스트 마이닝
# 데이터 로드
twitter <- read.csv("data/twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")
# 변수명 수정 - 한글변수명을 다루기 쉽게 영어이름으로 변경하고 특수문자 제거
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)
# 특수문자 제거
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)

# 단어 빈도표 만들기
# 트윗에서 명사추출
nouns <- extractNoun(twitter$tw)
# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
# 데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
# 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
#두 글자 이상으로 된 단어 추출, 빈도 상위 20개 단어 추출
#두 글자 이상 단어만 추출
df_word <- filter(df_word, nchar(word) >= 2)
# 상위 20 개 추출
top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top20

# ggplot2를 이용하여 막대 그래프 만든다.
library(ggplot2)

order <- arrange(top20, freq)$word # 빈도 순서 변수 생성
ggplot(data = top20, aes(x = word, y = freq)) +
  ylim(0, 2500) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) + # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label = freq), hjust = 1.5) # 빈도 표시

# 워드 클라우드 만들기
pal <- brewer.pal(8,"Dark2") # 색상 목록 생성
set.seed(1234) # 난수 고정
wordcloud(words = df_word$word, # 단어
          freq = df_word$freq, # 빈도
          min.freq = 10, # 최소 단어 빈도
          max.words = 200, # 표현 단어 수
          random.order = F, # 고빈도 단어 중앙 배치
          rot.per = .1, # 회전 단어 비율
          scale = c(6, 0.2), # 단어 크기 범위
          colors = pal) # 색상 목록
# 색깔 바꾸기
pal <- brewer.pal(9,"Blues")[5:9] # 색상 목록 생성
set.seed(1234) # 난수 고정
wordcloud(words = df_word$word, # 단어
          freq = df_word$freq, # 빈도
          min.freq = 10, # 최소 단어 빈도
          max.words = 200, # 표현 단어 수
          random.order = F, # 고빈도 단어 중앙 배치
          rot.per = .1, # 회전 단어 비율
          scale = c(6, 0.2), # 단어 크기 범위
          colors = pal) # 색상 목록

install.packages("wordcloud2")
library(wordcloud2)
# 덱스트파일 데이터
word_data <- readLines("data/애국가(가사).txt")
word_data

# word_data 변수의 각 행에서 명사를 추출하여 word_date2 변수에 할당
word_date2 <- sapply(word_data, extractNoun, USE.NAMES = F) 
word_date2

# 추가할 add_words 변수에 할당
add_words <- c("백두산","남산","철갑","가을","하늘","달") 
add_words

# add_words 변수의 데이터를 사용자 정의단어(user_dic)로 추가
buildDictionary(user_dic = data.frame(add_words,
                                      rep("ncn",length(add_words))),
                replace_usr_dic = T )
# word_data 변수의 각 행에서 명사를 word_date2에 할당
word_date2 <- sapply(word_data, extractNoun,USE.NAMES = F)
word_date2

# 행렬을 벡터로 전환하고 데이터 정렬하기
# word_data2를 벡터로 변환한 후 undata변수에 할당
undata <- unlist(word_date2)
# undata의 빈도 수 확인 후 word_table에 할당
word_table2 <- table(undata2)
# undata에서 두글자 이상 필터링
undata2 <- Filter(function(x){nchar(x) >=2}, undata)
undata2
# word_table2를 내림차순 정렬, 색상, 배경색상
sort(word_table2, decreasing = T)
wordcloud2(word_table2, color = "random-light",backgroundColor = "black")

sort(word_table2, decreasing = T)
wordcloud2(word_table2,color = "random-light",backgroundColor = "black",
           fontFamily = "맑은 고딕", size = 1.2, shape = "star")

