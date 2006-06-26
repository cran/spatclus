nincdepart3d<-function(data,ordre,limx,limy,limz)
{
n<-nrow(data)
ninc<-rep(0,n)
data<-cbind(data,ninc)

# on attribue ninc=1 au ordre_i�e point le plus proche du bord
data[order(apply(data,1,function(a) min(a[1]-limx[1],a[2]-limy[1],a[3]-limz[1],limx[2]-a[1],limy[2]-a[2],limz[2]-a[3]))),][ordre,]$ninc<-1

data
}

