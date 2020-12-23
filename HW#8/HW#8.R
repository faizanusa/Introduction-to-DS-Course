head(aneur, 500:550)
dim(aneur)
?aneur
aneur[450:500,]
df<-aneur

#add column with time in df
df$years <-c((df$age-60))
head(df, 100)
df1<-na.omit(df)
#progress of patients
ptnum1<-df%>%filter(ptnum==1)
ptnum1
ptnum101<-df%>%filter(ptnum==101)
ptnum101
ptnum827<-df%>%filter(ptnum==827)
ptnum827

transitioncount<-statetable.msm(state, ptnum, data=df)
transitioncount
sum(transitioncount)
trans<-transitioncount/sum(transitioncount)
trans
#q matrix
Q <- rbind(c(0.25, 0.25, 0.25, 0.25), c(0.166, 0, 0.166, 0.166), c(0.25, 0.25, 0, 0.25), 
           + c(0.1, 0.1, 0.1, 0))
Q

#crude matrix
crude<-crudeinits.msm(state~years,ptnum, data=df, qmatrix=Q)
crude
#infinitesimal generator
aneur.msm<-msm(state~years, subject=ptnum, data=df, qmatrix=Q)
aneur.msm
printold.msm(aneur.msm)
summary(aneur.msm)
aneur.msm$Qmatrices
names(aneur.msm)

pmatrix.msm(aneur.msm, t=1)
pmatrix.msm(aneur.msm, t=5)
pmatrix.msm(aneur.msm, t=10)
