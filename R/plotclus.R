plotclus<-function(nomlst,m,limx=c(0,100),limy=c(0,100),col1=225,rcex=0.68,pop,k=floor((m+1)/2))
{

bc<-nomlst$bc[[m]]

T<-nrow(nomlst$res)-1
bc<-c(0,bc,T)

ninc<-nomlst$res$ninc[1:T]
x<-nomlst$res$x
y<-nomlst$res$y

# repr�entation graphique des points dans le cluster

c<-rep(0,(m+1))

for (i in 1:(m+1))
{
c[i]<-mean(nomlst$res$distp[(bc[i]+1):bc[i+1]])
}

plot(x, y,pch="+",xlab="X-axis",ylab="Y-axis",xlim=limx,ylim=limy,cex.lab=1.5,cex.axis=1.5)
#plot(x, y,pch="+",xlab="X-axis",ylab="Y-axis",xlim=limx,ylim=limy,cex.lab=1,cex.axis=1)
r<-sqrt((limx[2]-limx[1])*(limy[2]-limy[1])/((T+1)*pi))



radj<-rep(0,T)
for (l in 1:T)
{
radj[l]<-dist2p(x[l],y[l],pop[,1],pop[,2])[order(dist2p(x[l],y[l],pop[,1],pop[,2]))][floor(nrow(pop)/(T+1))]
}

#toto<-densite(nomlst)
#tutu<-toto$nin[toto$nin[,6]<0.05,]
#k<-nrow(tutu)

for (i in k:1)
{
for (j in (bc[order(c)[i]]+1):bc[order(c)[i]+1])
#for (j in tutu[i,1]:tutu[i,2])
{
#points(x[j],y[j],pch=19,cex=radj[j]*rcex,col=(i+1))# gray(0.9-i/10)) #colors()[col1+(i-1)*100]
points(x[j],y[j],pch=19,cex=radj[j]*rcex,col=gray(0.9-i/10)) #colors()[col1+(i-1)*100]
}
#text(x=mean(x[(bc[order(c)[i]]+1):bc[order(c)[i]+1]]),y=mean(y[(bc[order(c)[i]]+1):bc[order(c)[i]+1]]),labels=i)
}
for (i in k:1)
{
for (j in (bc[order(c)[i]]+1):bc[order(c)[i]+1])
#for (j in tutu[i,1]:tutu[i,2])
{
points(x[j],y[j],pch=16) #colors()[col1+(i-1)*100]
}
}

points(x, y,pch="+")

}

# cercle(20,20,r,0.1)
# points(20,20,pch=19,cex=r*1.51)
