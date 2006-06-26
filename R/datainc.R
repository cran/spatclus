datainc<-function(data,pop)
{
row.names(pop)<-nrow(data)+(1:nrow(pop))

if (ncol(data)==2) pop<-rbind(cbind(c(data[,1]),c(data[,2]),rep(1,nrow(data))),pop)

if (ncol(data)==3) pop<-rbind(cbind(c(data[,1]),c(data[,2]),c(data[,3]),rep(1,nrow(data))),pop)

pop
}
