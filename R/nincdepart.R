nincdepart<-function(data,ordre,limx,limy)
{
n<-nrow(data)
ninc<-rep(0,n)
data<-cbind(data,ninc)

# on attribue ninc=1 au ordre_ième point le plus proche du bord
data[order(apply(data,1,function(a) min(a[1]-limx[1],a[2]-limy[1],limx[2]-a[1],limy[2]-a[2]))),][ordre,]$ninc<-1

data
}

