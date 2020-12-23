data(biopsy)
dim(biopsy)
head(biopsy, 10)
df<-subset(biopsy, select=-c(ID))
class(df$V1)
summary(df)
?biopsy
df1<-na.omit(df)
SF1<-glm(class~.,data=df, family=binomial)
summary(SF1)
pchisq(102.89, 673, lower.tail=F)
#tighten model
SF2<-glm(class~ V1+V4+V6+V7, data=df, family=binomial)
summary(SF2)
pchisq(125.77, 678, lower.tail = F)
names(SF2)
SF2$na.action
confusion_matrix(SF2)
confusion_matrix
