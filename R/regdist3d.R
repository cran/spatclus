regdist3d<-function(data,pop,start,m,h,T,limx,limy,limz)
{

data<-nincdepart3d(data,start,limx,limy,limz)

esp<-espdist3d(data,pop)

bc<-list()
for (i in 1:m)
{
bc[[i]]<-multbreak(esp$res,m=i,h,T)
}
# rajout de derpoint �la fin de res
derpoint<-cbind(esp$derpoint,dist=NaN,espth=NaN,distp=NaN) #,espth2=NaN)
row.names(derpoint)<-"der"
res<-rbind(esp$res,derpoint)

# fin m�hode de Dematte�et Molinari
#########################################################################################
#########################################################################################
list(res=res,bc=bc)
}
