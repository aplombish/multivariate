#1장. 다변량 데이터 시각화
hald.data <- read.table("hald.txt", header=T)
head(hald.data)
hald.ls <- lm(Y~X1+X2+X3+X4, data=hald.data)
summary(hald.ls)
anova(hald.ls)

#2장. 주성분 분석
library(HSAUR)
data(heptathlon)
heptathlon$hurdles = max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m = max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m = max(heptathlon$run800m) - heptathlon$run800m

library(stats)
hep.data <- heptathlon[,-8]
heptathlon.pca <- princomp(hep.data, cor=T, scores=T)
names(heptathlon.pca)
heptathlon.pca
summary(heptathlon.pca)
screeplot(heptathlon.pca, type="lines", pch=19, main="Scree Plot")
heptathlon.pca$loadings[,1:2]
heptathlon.pca$scores[,1:2]
biplot(heptathlon.pca, col=c("red","blue"), main="Biplot")

beer <- read.csv("beer.csv", header=T,)
beer
round(cor(beer), 2)
library(stats)
beer.pca <- princomp(beer, cor=F, scores=T)
beer.pca
summary(beer.pca)
screeplot(beer.pca, type="lines", pch=19)
beer.pca$loadings[,1:3]

#3장. 인자분석
install.packages("psych")
med.data <- read.table("medFactor.txt", header=T)
library(psych)
install.packages("GPArotation")
library(GPArotation)
med.factor <- principal(med.data, rotate="none")
names(med.factor)
med.factor$values
plot(med.factor$values, type="b")

med.Varimax <- principal(med.data, nfactors=3, rotate="varimax")
med.Varimax
med.Varimax$scores

med.oblimin <- principal(med.data, nfactors=3, rotate="oblimin", scores=T, method="regression")
med.oblimin
med.oblimin$scores
biplot(med.Varimax)

state.x77
state <- state.x77
summary(state)
library(stats)
state.fact <- factanal(state, factor=3, rotation="none")
state.fact1 <- factanal(state, factors=3, rotation="varimax") #varimax 회전전
state.fact2 <- factanal(state, factors=3, rotation="promax")
names(state.fact)

#고유근 구하기
state.fact0 <- factanal(state, factors=4)
sosq <- function(v){sum(v^2)} #sum of squares
loadings <- as.matrix(state.fact0$loadings)
evalues <- apply(loadings, 2, sosq)
evalues

state.fact1
state.fact1.1 <- factanal(state[,-1], factors=3, rotation="varimax", scores="Bartlett")
state.fact1.1
state.fact1.1$scores

#plot(factor1, factor2)
fact1 <- state.fact1.1
namevar=colnames(state)
plot(fact1$loadings[,1], fact1$loadings[,2], xlab="factor1", ylab="factor2", pch=19)
text(x=fact1$loadings[,1], y=fact1$loadings[,2], labels=namevar, adj=-0.1, cex=0.8)
abline(h=0, v=0, lty=2)

#plot(factor1, factor3)
plot(fact1$loadings[,1], fact1$loadings[,3], xlab="factor1", ylab="factor3", pch=19)
text(x=fact1$loadings[,1], y=fact1$loadings[,3], labels=namevar, adj=-0.1, cex=0.8)
abline(h=0, v=0, lty=2)

#예제3.3.
fem <- read.csv("grntFem.csv", header=T)
uls <- fa(fem, 2, rotate="none", fm="minres")
names(uls)

uls$values #고유근
plot(uls$values, type="b", pch=19) #스크리그림
uls
uls$scores #인자점수
biplot(uls, cex=0.7) #행렬도

#4장. 군집분석 - 계층적 군집분석
beer.data <- read.csv("beerbrand.csv", header=T)
hc <- hclust(dist(beer.data), method="single")
hc
plot(hc)
plot(hc, hang=-1) #맥주이름 정렬

#자료표준화 후 계층적 군집분석
library(pls)
zbeer.data <- stdize(as.matrix(beer.data))
zhc <- hclust(dist(zbeer.data), method="centroid")
plot(zhc, hang=-1)
zhc.cent24 <- cutree(zhc, 2:4)
zhc.cent24

#k-평균 군집분석
kmc <- kmeans(zbeer.data, 2)
kmc
#소속 군집 산점도
plot(zbeer.data, col=kmc$cluster, pch=16)
pairs(zbeer.data, col=kmc$cluster, pch=16)

#군집분석 사례 분석
hc1 <- hclust(dist(USArrests), method="average")
plot(hc1, hang=-1)
#소속군집 알기
hcmember <- cutree(hc1, k=4)
hcmember
#각 군집별 중심점 찾기
cent <- NULL
for(k in 1:4) {
  cent <- rbind(cent, colMeans(USArrests[hcmember==k, , drop=FALSE]))
}
cent
#K평균 군집분석
library(pls)
zUSArrests <- stdize(as.matrix(USArrests))
kmc1 <- kmeans(zUSArrests, 4)
kmc1

#소속 군집 산점도
pairs(zUSArrests, col=kmc1$cluster, pch=16)

#5장. 다차원척도법(MDS)
#표준화 변수 만들기
auto.data <- read.csv("auto.csv", header=T)
X <- auto.data[,-1]
autoName = auto.data[,1]
zX <- scale(X, center=T, scale=T)
maxX <- apply(X, 2, max)
minX <- apply(X, 2, min)
z01X <- scale(X, center=minX, scale=maxX-minX)
#거리행렬 만들기
z01X.dist <- dist(z01X, method="euclidean")
z01X.dist <- as.matrix(z01X.dist)
colnames(z01X.dist) <- autoName
rownames(z01X.dist) <- autoName
z01X.dist
#cmdscale 실행
mds1 <- cmdscale(z01X.dist, k=2)
plot(mds1[,1], mds1[,2], type="n", xlab="", ylab="", main="cmdscale(Auto")
text(mds1[,1], mds1[,2], rownames(z01X.dist), cex=0.9)
abline(h=0, v=0, lty=3)

install.packages("smacof")
library(smacof)
mds2 <- smacofSym(z01X.dist, ndim=2)
plot(mds2$conf[,1], mds2$conf[,2], type="n", xlab="", ylab="", main="smacof(Auto)")
text(mds2$conf[,1], mds2$conf[,2], rownames(z01X.dist), cex=0.9)
abline(h=0, v=0, lty=3)

#스크리그림 그리기 p.160 안 되는데 
mds2.1 <- smacofSym(z01X.dist, ndim=1)
mds2.2 <- smacofSym(z01X.dist, ndim=2)
mds2.3 <- smacofSym(z01X.dist, ndim=3)
mds2.4 <- smacofSym(z01X.dist, ndim=4)
stress.value <- c(mds2.1$stress.m, mds2.2$stress.m, mds2.3$stress.m, mds2.4$stress.m)
plot(stress.value, type="1")
points(stress.value, cex=0.9)
#적합도 진단 산점도 그리기
plot(mds2$confdist, mds2$delta, xlab="Observed distance", ylab="configuration distance")

#예제. 5.2. 비메트릭 MDS 분석 예
#하삼각 행렬 읽기 위한 함수 (R로 저장해서 프로그램으로 쓴다 )
readMatrix <- function()
{
cat("\n-----------------------------")
cat("\n (1) Upper Triangular Matrix ")  
cat("\n (2) Lower Triangular Matrix ")
cat("\n-----------------------------")
cat("\n Type the number(Default=1 : ")
upperValue <- readline( )
cat("\n --- Number of rows : ")
n <- readline( )
n <- as.integer(n)
if( upperValue == "2")
{ DistanceArray <- array(0, n*(n-1)/2 )
for (i in 1:(n-1) )
  for (j in (i+1):n )
  {kk1 <- (j-1)*(j-2)/2 + i 
   kk2 <- n*(i-1)-i*(i-1)/2 + j-i
   DistanceArray[kk2] <- data.m[kk1]}
}
else
   DistanceArray <- data.m

#data Recode
DistanceArray = 10 - DistanceArray

MD <- matrix(0, nrow=n, ncol=n)
for (j in 1:(n-1))
  for (k in (j+1):n)
  { kk <- n*(j-1) 0 j*(j-1)/2 + k-j
    MD[j,k] <- MD[k,j] <- DistanceArray[kk]
  }
return(MD)
}

source("readMatrix.R")
data = readMatrix()
country1968.txt
2    
12    
country.name <- scan("countryname.txt", what="")  
colnames(data) <- country.name
rownames(data) <- country.name
data

#비메트릭 MDS 실행
library(MASS)
fit <- isoMDS(data, k=2)
fit
x <- fit$points[,1]
y <- fit$points[,2]
plot(x,y, xlab="", ylab="", main="Non-metric MDS(isoMDS)", type="n")
text(x,y, labels=row.names(data), cex=0.9)
abline(h=0, v=0, lty=3)

#비메트릭 MDS smacofSym 함수 실행 안 되는데
library(smacof)
fit2 <- smacofSym(data, ndim=2, metric=FALSE)
names(fit2)
fit2$conf
fit2$stress.nm
plot(fit2$conf[,1], fit2$conf[,2], type="n", xlab="", ylab="", main="smacof(non-metric)")
text(fit2$conf[,1], fit2$conf[,2], rownames(data), cex=0.9)
abline(h=0, v=0, lty=3)

#6장. 정준상관분석
sales <- read.table("sales.txt", header=T)
library(psych)
SD(sales[,c(2:8)])

#상관계수행렬 구하기
library(ggplot2)
library(GGally)
install.packages("GGally")
library(GGally)
library(CCA)
install.packages("CCA")
exam <- sales[,c(2:5)]
perform <- sales[,c(6:8)]
#scatterplot and correlation of exam
ggpairs(exam)
ggpairs(perform)
matcor(exam, perform)

#정준상관분석 실행하기
canonical.result <- cc(exam, perform)
names(canonical.result)
canonical.result$cor

canonical.result$names
canonical.result$xcoef
canonical.result$scores

#정준변수 산점도 그리기
w1 <- canonical.result$scores$xscores[,1]
v1 <- canonical.result$scores$yscores[,1]
cor(w1, v1)
plot(w1, v1, xlab="X(1) cano'l variates", ylab="Y(1) cano'l variates", pch=19)

#7장 판별분석
#사후확률 이용 방법
library(MASS)
data7 <- read.table("data7-1.txt", header=T)
data7.lda <- lda(group~., data=data7)
pred.lda <- predict(data7.lda, newdata=data7)
pred.lda$class
pred.lda$posterior

#판별모형 평가
confm.lda <- table(data7$group, pred.lda$class)
confm.lda

#예제
alcohol.data <- read.csv("alcohol.csv", header=T)
library(MASS)
alcohol.lda <- lda(TYPE ~ ., data=alcohol.data)
alcohol.lda
#분류하기
pred.lda <- predict(alcohol.lda, newdata=alcohol.data)
names(pred.lda)
head(pred.lda$class)
head(pred.lda$posterior)
#분류표 구하기
confm.lda <- table(alcohol.data$TYPE, pred.lda$class)
confm.lda
#error rate
error = 1-sum(diag(confm.lda))/sum(confm.lda)
error
#판별변수 선택
install.packages("klaR")
library(klaR)
alcohol.forward <- greedy.wilks(TYPE ~., data=alcohol.data, niveau=0.01)
alcohol.forward
#변수 선택 후 판별분석 실행하기
alcohol.fwd.lda <- lda(alcohol.forward$formula, data=alcohol.data)
alcohol.fwd.lda
pred.fwd.lda <- predict(alcohol.fwd.lda, newdata=alcohol.data)
confm.fwd <- table(alcohol.data$TYPE, pred.fwd.lda$class)
confm.fwd
#분류함수 구하기
source("clfunction.R")
library(MASS)       
X <- alcohol.data[,-1]
classfunc.result <- classfunc.lda(X, alcohol.data$TYPE)
names(classfunc.result)
classfunc.result$class.func

#로지스틱 회귀분석
drug.data <- read.csv("drug.csv", header=T)
attach(drug.data)
plot(age, purchase, pch=19)
#그룹화한 후 산점도 그리기
agr <- age
agr[agr >=20 & agr <= 29] =1 
agr[agr >=30 & agr <= 34] =2 
agr[agr >=35 & agr <= 39] =3 
agr[agr >=40 & agr <= 44] =4 
agr[agr >=45 & agr <= 49] =5 
agr[agr >=50 & agr <= 54] =6 
agr[agr >=55 & agr <= 59] =7 
agr[agr >=60 & agr <= 69] =8 
purchase.table <- table(agr, purchase)
purchase.table
percent.table <- prop.table(purchase.table, 1)
percent.table
perc.1 <- percent.table[,2]
agr.1 <- rownames(percent.table)
agr.1 <- as.numeric(agr.1)
plot(agr.1, perc.1, pch=19)

#로지스틱 예제 8.1
mower.data <- read.csv("mower.csv", 1)
str(mower.data)
mower.data$owner <- as.factor(mower.data$owner)
mower.logit <- glm(owner ~., family=binomial, data=mower.data)
summary(mower.logit)

#새로운 자료의 분류
mower.predict <- predict(mower.logit, newdata=mower.data, type="response")
pred <- ifelse(mower.predict < 0.5, "no", "yes")
pred <- factor(pred)
confusion.matrix <- table(mower.data$owner, pred)
error = 1 - (sum(diag(confusion.matrix)/sum(confusion.matrix)))
error
#RWeka 패키지 logistic 함수 이용
install.packages("RWeka")
library(RWeka)
mower.m <- Logistic(owner ~ ., data=mower.data)
names(mower.m)
summary(mower.m)
mower.m

#새로운 자료의 분류
mower.p <- predict(mower.m, newdata=mower.data, type="class")
mower.cm <- table(mower.data$owner, mower.p)
mower.cm
mower.error <- 1.0 - (sum(diag(mower.cm))/sum(mower.cm))
mower.error

#예제 8.2 
#두 변수간 그래프 보기
library(MASS)
data(menarche)
plot(Menarche/Total ~ Age, data=menarche, pch=19)
#로지스틱 회귀모형 적합
menar.out <- glm(cbind(Menarche, Total-Menarche)~Age, family=binomial, data=menarche)
summary(menar.out)
plot(Menarche/Total ~ Age, data=menarche, pch=19)
lines(menarche$Age, menar.out$fitted, type="l", col="blue")
title(main="Menarche Data with Fitted Logistic Regression Line")

#9장. 나무모형
#데이터 마이닝 예제 참고 