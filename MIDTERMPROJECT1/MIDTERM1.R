SF <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
dim(SF)
head(SF)
SF1<-subset(SF, select=-c(id))
head(SF1)
chisq.test(SF1$honors, SF1$prog)
READ<-anova(lm(read~prog, data=SF1))
READ
READ1<-anova(lm(write~prog, data=SF1))
READ1
READ2<-anova(lm(math~prog, data=SF1))
READ2
READ3<-anova(lm(science~prog, data=SF1))
READ3
READ4<-anova(lm(write~prog, data=SF1))
READ4
READ5<-anova(lm(socst~prog, data=SF1))
READ5
READ6<-anova(lm(awards~prog, data=SF1))
READ6
#fit a multinomial logistic regression model to SF
SF2<-vglm(prog~., data=SF1, family=multinomial)
summary(SF2)
levels(SF1$prog)[3]
pchisq(308.1006, 374, lower.tail=F)
resid(SF2)
soft<-round(predict(SF2, newdata=SF1, type="response"), 3)
head(soft)
Hard <- rep(0,200)
for (i in 1:200)
  (
    Hard[i] <- ifelse (soft[i,1] == max (soft[i, ]), "general", ifelse (soft[i,2] == max(soft[i, ]), "academic", "vocation"))
  )
Hard
#confusion matrix
confusion <- data.frame(observed = SF1$prog, predicted = Hard)
confusion1<-table(confusion$observed, confusion$predicted)
confusion1

#fit with ses and write
SF3<-vglm(prog~ ses+write, data=SF1, family=multinomial)
summary(SF3)
pchisq(359.9635, 392, lower.tail=F)
soft2<- round(predict(SF3, newdata=SF1, type="response"), 3)
head(soft2)
Hard2 <- rep(0,200)
for (i in 1:200)
  (
    Hard2[i] <- ifelse (soft2[i,1] == max (soft2[i, ]), "general", ifelse (soft2[i,2] == max(soft2[i, ]), "academic", "vocation"))
  )
Hard2
confusion2<-data.frame(observed=SF1$prog, predicted = Hard2)
confusion3<-table(confusion2$observed, confusion2$predicted)
confusion3

#curves write vs probability

SF4 <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
with(SF4, table(ses,prog))
with(SF4, do.call(rbind, tapply(write, prog, function(x) c(M=mean(x), SD = sd(x)))))
SF4$prog2<-relevel(SF4$prog, ref="academic")
test<-multinom(prog2 ~ ses+write, data=SF4)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
z
p<-(1-pnorm(abs(z),0, 1))*2
p
#models
#ln(P(prog=general)/P(prog=academic))=1.448e-2+(0.2294*ses=middle)+(.02374*ses=high)+6.819e-3*write
#ln(P(prog=voc)/(prog=acd))=7.299e-6+(0.5408*ses=middle)+(.09895*ses=high)+3.176e-7*write
exp(coef(test))
head(pp<-fitted(test))
dses <- data.frame(ses = c("low", "middle", "high"), write = mean(SF4$write))
predict(test, newdata=dses, "probs")
dwrite<- data.frame(ses=rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70),3))

pp.write<- cbind(dwrite, predict(test, new=dwrite, type="probs", se=TRUE))
by(pp.write[ , 3:5], pp.write$ses, colMeans)
lpp <- melt (pp.write, id.vars = c("ses", "write"), value.name = "probability")
head(lpp)
ggplot(lpp, aes(x=write, y=probability, colour=ses)) + geom_line() + facet_grid(variable ~., scales="free")


