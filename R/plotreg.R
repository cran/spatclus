plotreg<-function(nomlst,m)
{
bc<-nomlst$bc[[m]]

T<-nrow(nomlst$res)-1
bc<-c(0,bc,T)

ninc<-nomlst$res$ninc[1:T]
distp<-nomlst$res$distp[1:T]

plot(ninc, distp, xlab="Order",ylab="Weighted distance",cex.lab=1.5,cex.axis=1.5)
for (i in 1:(m+1))
{
segments((bc[i]+1),mean(distp[(bc[i]+1):bc[i+1]]),bc[i+1],mean(distp[(bc[i]+1):bc[i+1]]))
}
for (i in 2:(m+1))
{
segments(bc[i],mean(distp[(bc[i-1]+1):bc[i]]),(bc[i]+1),mean(distp[(bc[i]+1):bc[i+1]]))
}

}

