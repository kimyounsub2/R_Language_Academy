# 변수에는 여러 개의 값을 넣을수 있다. c는 combine의 머리글자 
var1 <- c(1,2,5,7,8) # 숫자 다섯 개로 구성된 var 생성
var1

var2 <- c(1:5) # 1~5까지 연속값으로 var2 생성
var2

# 수열(sequence)은 seq()함수는 '연속'을 의미하는 sequence의 머리글자
var3 <- seq(1,5)
var3
var4 <- seq(1, 10, by =2)
var4
var5 <- seq(1, 10, by =3)
var5

# 문제. 1 ~ 100까지 5의 배수만 출력해 보세요
var55 <- seq(5, 100, by=5)
var55

var1+2
var1 + var2

str1 <- "a"
str1
str2 <- "text"
str2
str3 <- 'Hello World!'
str3
str4 <- c("a","b","c")
str4
str5 <- c("Hello!","World","is","good!")
str5

#문자로 된 변수로는 연산할 수 없다.
str1+2 #str1은 문자로 변수를 줬다

x <- c(1, 2, 3)
x
mean(x)
max(x)
min(x)

#문자를 다루는 함수 이용하기
str5
paste(str5, collapse =",")#쉽표를 구분자로 str4의 단어들 하나로 합치기
#함수의 옵션 설정하기 - 파라미터
paste(str5, collapse =" ")

str11 <- c("우리","나라는","대한민국","입니다.")
paste(str11, collapse=" ")
str123 <- c("hell","R","nice","to","meet","you")
paste(str123, collapse=" ")

#함수의 결과물로 새 변수 만들기
x_mean <- mean(x)
x_mean

str_paste <- paste(str5, collapse = " ")
str_paste

x <- c("a","a","b","c")
x
#x의 빈도그래프 그리기
qplot(x)

#ggplot2의 패키지에 mpg데이터로 그래프 만들기
# data에 mpg, x축에 hwy 변수 지정하여 그래프 생성
qplot(data=mpg,x=hwy)
View(mpg)
# x축 cty
qplot(data = mpg, x=cty)
# x축 drv, y축 hwy
qplot(data = mpg, x = drv, y = hwy)
# x축 drv, y축 hwy, 선 그래프 형태
qplot(data = mpg, x = drv, y = hwy, geom = "line")
# x축 drv, y축 hwy, 상자 그림형태
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot")
# x축 drv, y축 hwy, 상자 그림 형태, drv 별 색 표현
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv)

test <- c(80,60,70,50,90)
test
mean(test)
test1 <- mean(test)
test1