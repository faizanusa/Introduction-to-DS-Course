data(kyphosis)
summary(kyphosis)
SF<-rpart(Kyphosis ~., data=kyphosis)
rpart.plot(SF, type=4, extra=1, digits=3, roundint=FALSE,  col="red")
#landmark of STart = 9 and 15
#landmark of Age 55 and 111
#min of Start 1 and max 18
#min of Age 1 and ma is 206
plot(kyphosis$Start, kyphosis$Age, type=n)
polygon(c(1, 9, 9, 1), c(1, 1, 206, 206), col="mistyrose")
polygon(c(15,18, 18, 15), c(1, 1, 206, 206), col="green")
polygon(c(9, 15, 15, 9), c(1, 1, 55, 55), col="green")
polygon(c(9, 15, 15, 9), c(111, 111, 206, 206), col="green")
polygon(c(9, 15, 15, 9), c(55,55,111,111), col="mistyrose")
axis(side=2, at=c(1,55, 111,206), labels=c(1,55,  111, 206), col="magenta")
axis(side=1, at=c(1,9,15, 18), labels=c(1, 9,15,18), col="magenta")
title(ylab="Age", xlab="Start", main="Regression Tree - Polygonal Graph", sub="Kyphosis Data")
text(4.5,103, "Present", col="magenta")
text(16.5, 103, "Absent", col="magenta")
text(12,27.5, "Absent", col="magenta")
text(12, 83, "Present", col="magenta")
text(12, 158.5, "Absent", col="magenta")
print(SF)

