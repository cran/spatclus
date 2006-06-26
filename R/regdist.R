regdist<-function(data,pop,start,m,h,T,limx,limy)
{

data<-nincdepart(data,start,limx,limy)

esp<-espdist(data,pop)

bc<-list()
for (i in 1:m)
{
bc[[i]]<-multbreak(esp$res,m=i,h,T)
}
# rajout de derpoint à la fin de res
derpoint<-cbind(esp$derpoint,dist=NaN,espth=NaN,distp=NaN) #,espth2=NaN)
row.names(derpoint)<-"der"
res<-rbind(esp$res,derpoint)

# fin méthode de Dematteï et Molinari
#########################################################################################
#########################################################################################
list(res=res,bc=bc)
}
