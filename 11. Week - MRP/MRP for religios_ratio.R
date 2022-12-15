setwd("C:/Users/user/Desktop/Dindarlik/MRP")
getwd()

install.packages("readxl")
install.packages("foreign")
install.packages("lme4")
install.packages("arm")
install.packages("extrafont")
install.packages("rio")

library(foreign)
library(lme4)
library(arm)
library(extrafont)
library(readxl)
library(magrittr)
library(rio)

data1 <- read_xlsx("politus_data_300K.xlsx")

model1 <- glmer(religious ~ 1 + (1|gender) + (1|age) + (1|nuts1),
                data= data1, family=binomial("probit"))
summary(model1)

re.female <- ranef(model1)$gender[[1]]
re.agegroup <- ranef(model1)$age[[1]]
re.nuts1 <- ranef(model1)$nuts1[[1]]

female.re <- rep(re.female,4)
age.re <- rep(kronecker(re.agegroup,c(1,1)), 1)
ind.re <- rowSums(cbind(female.re, age.re))
ind.re <- ind.re + fixef(model1)

y.lat1 <- rep(NA,12*8)
for (i in 1:12){
  a <- ((i-1)*8)+1
  b <- a + 7
  y.lat1[a:b] <- ind.re + re.nuts1[i]
}

predict1 <- pnorm(y.lat1)

popul.object <- read_excel("modified_tüik_matrix.xlsx")

dim(popul.object)
head(popul.object)
sum(predict1*popul.object)/sum(popul.object)

popul.object[,1]

mrp.relig1 <- rep(NA,12)
for (i in 1:12){
  a1 <- ((i-1)*8)+1
  a2 <- a1 + 7
  p1 <- pnorm(y.lat1[a1:a2])
  a <- popul.object[,i]
  mrp.relig1[i] <- sum(p1*a)/sum(a)
}

dis.relig <- c(0.451528, 0.426713, 0.416663, 0.473246, 0.471286, 0.459685, 0.529158, 0.493151, 0.519867, 0.560888, 0.588191, 0.556921)
#for religious ratio:
relig.TIDA.ratio <- c(0.359649, 0.235294, 0.333333, 0.410714, 0.5, 0.394366, 0.605263, 0.586207, 0.652174, 0.625, 0.473684, 0.622951)

mean(mrp.relig1)
mrp.relig1

plot(dis.relig, relig.TIDA.ratio, pch=18, cex=1,
     bty="n", col="black", ylab="TIDA Religios Ratio",
     xlab="Estimated Religious Ratio", ylim=c(0,1), xlim=c(0,1))
abline(c(0,1), lty=2, lwd=.5)
points(mrp.relig1,relig.TIDA.ratio, pch=18, col="gray", cex=1)
legend(0.7,0.2,legend = c("Disaggregation","MrP"), pch=18,
       col=c("black","gray"), bty="n", pt.cex=c(1,1,1,1),
       cex=0.8)
text(.9,.9,"'45 Degree Line", cex=0.8)

#for religiosity_score:
relig.TIDA.score <- c(0.602761, 0.553191, 0.537879, 0.619205, 0.636905, 0.605263, 0.671429, 0.665033, 0.695122, 0.682143, 0.645408, 0.664286)

plot(dis.relig, relig.TIDA.score, pch=18, cex=1,
     bty="n", col="black", ylab="TIDA Religiosity Score",
     xlab="Estimated Religious Ratio", ylim=c(0,1), xlim=c(0,1))
abline(c(0,1), lty=2, lwd=.5)
points(mrp.relig1,relig.TIDA.score, pch=18, col="gray", cex=1)
legend(0.7,0.2,legend = c("Disaggregation","MrP"), pch=18,
       col=c("black","gray"), bty="n", pt.cex=c(1,1,1,1),
       cex=0.8)
text(.9,.9,"'45 Degree Line", cex=0.8)