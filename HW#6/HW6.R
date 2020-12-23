data(biopsy)
head(biopsy)
df=na.omit(biopsy)
df1=subset(df, select=-c(ID))
head(df1)
summary(df1)
#p-value approach
df2=ctree(class~., data=df1)
plot(df2, col ="red", main="Classification Tree - Statistical Approach")
df2
#GIni INdex approach
df3=rpart(class~., data=df1)
rpart.plot(df3, col="blue", type=4, extra=1, main="Classification Tree - Gini's Index")
df3
#Entropy Approach

SF<-rpart(class~., data=df1, parms=list(split="information"))
rpart.plot(SF, type=4, col="purple", digits=3, extra=2, main="Classification Tree - Entropy approach")
print(SF)
#printcp, pruning
printcp(df3)
pruneddf3<-prune(df3, cp=0.01)
rpart.plot(pruneddf3, type=4, col="red", extra=2, main="Pruned Classification tree")
#partitioning
V62left<-subset(df1, df1$V6<=2)
V62right<-subset(df1, df1$V6>2)
dim(V62left)
dim(V62right)
head(V62left)

#control
ctree_control
