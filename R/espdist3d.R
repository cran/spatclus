airegrille3d<-function(pop,x,y,z,r)
{
poptemp<-pop
poptemp[dist2p3d(poptemp[,1],poptemp[,2],poptemp[,3],x,y,z)<r,3]<-0
airenorm<-sum(poptemp[,4])/sum(pop[,4]) # attention : l'aire unit�(de r��ence) n'est plus le rectangle a*b mais le rectangle moins le chemin parcouru
airenorm
}

######################################################

integre3d<-function(pop,x,y,z,rmax,n,k,pas)
{
# au dela de rmax, probout=0 et � change pas l'integrale
r<-seq(0,rmax,pas)
aire<-rep(0,length(r))
probout<-rep(0,length(r))
somme<-0
for (i in 1:length(r))
{
poptemp2<-pop
aire[i]<-airegrille3d(poptemp2,x,y,z,r[i])
probout[i]<-(aire[i])^(n-k)
somme<-somme+probout[i]
}
int<-pas*(aire[1]/2+somme-aire[1]) # probout(0)=1, probout(r[length(r)])=0
int
}

######################################################

espdist3d<-function(data,pop)
{
n<-nrow(data)

# attribution de l'ordre et calcul de la distance pond��
res<-data.frame(x=0,y=0,z=0,ninc=0,dist=0,espth=0,distp=0,row.names = "0")
popl<-list()
rowname<-character(n-1)
for (i in 1:(n-1)) rowname[i]<-i

d<-date()
# calcul de la distance au plus proche voisin (dist)
cat("Computation of the expected distances (3D) in progress ... \n")
for (i in 1:(n-1))
{
if (floor(i/10)==(i/10)) cat(i,"/",n-1,"\n")
# calcul de la distance entre le point ninc=i et tous les autres points
data<-cbind(data,dist=dist2p3d(data$x,data$y,data$z,data$x[data$ninc==i],data$y[data$ninc==i],data$z[data$ninc==i]))
# calcul de l'esp theorique de la dist du point i au point le plus proche parmi n-i
popl[[1]]<-pop
if (i>1)
{
popl[[i]]<-popl[[i-1]]
popl[[i]][dist2p3d(popl[[i]][,1],popl[[i]][,2],popl[[i]][,3],res$x[res$ninc==(i-1)],res$y[res$ninc==(i-1)],res$z[res$ninc==(i-1)])<=res$dist[res$ninc==(i-1)],4]<-0
}
# calcul de la distance maxi entre le point ninc=i et les points de la pop = 1
if (sum(popl[[i]][,4])>1)
{
rmax<-max(dist2p3d(data$x[data$ninc==i],data$y[data$ninc==i],data$z[data$ninc==i],popl[[i]][popl[[i]][,4]==1,][,1],popl[[i]][popl[[i]][,4]==1,][,2],popl[[i]][popl[[i]][,4]==1,][,3]))
}
if (sum(popl[[i]][,4])==1)
{
rmax<-dist2p3d(data$x[data$ninc==i],data$y[data$ninc==i],data$z[data$ninc==i],popl[[i]][popl[[i]][,4]==1,][1],popl[[i]][popl[[i]][,4]==1,][2],popl[[i]][popl[[i]][,4]==1,][3])
}
if (sum(popl[[i]][,4])==0)
{
cat("The computation of the expected distance (3D) is impossible: there is no more population point outside of the trajectory already done. A finest population have to be considered. \n")
}
espth<-integre3d(popl[[i]],data$x[data$ninc==i],data$y[data$ninc==i],data$z[data$ninc==i],rmax,n,i,0.2)
# on recherche le point le plus pr� de ninc=i et on attribue ninc=(i+1)
data[data$ninc!=i,]$ninc[data[data$ninc!=i,]$dist[]==min(data[data$ninc!=i,]$dist[])]<-i+1
if (nrow(data[data$ninc==(i+1),])==0)
{
cat("No point of order ",i+1,"\n")
}
# on supprime les �entuels doublons ninc=i+1 en ne gardant que celui dont l'abscisse est min
if (nrow(data[data$ninc==(i+1),])>1)
{
data[data$ninc==(i+1),]$ninc[data[data$ninc==(i+1),]$x[]!=min(data[data$ninc==(i+1),]$x[])]<-0
}
# on supprime les �entuels doublons restants (plusieurs abscisses min) ninc=i+1 en ne gardant que celui dont l'ordonn� est min
if (nrow(data[data$ninc==(i+1),])>1)
{
data[data$ninc==(i+1),]$ninc[data[data$ninc==(i+1),]$y[]!=min(data[data$ninc==(i+1),]$y[])]<-0
}
# on supprime les �entuels doublons restants (plusieurs abscisses min et plusieurs ordonn�s min) ninc=i+1 en ne gardant que celui dont le z est min
if (nrow(data[data$ninc==(i+1),])>1)
{
data[data$ninc==(i+1),]$ninc[data[data$ninc==(i+1),]$z[]!=min(data[data$ninc==(i+1),]$z[])]<-0
}
# on ajoute �res la ligne ninc=i
res<-rbind(res,data.frame(x=data[data$ninc==i,]$x,y=data[data$ninc==i,]$y,z=data[data$ninc==i,]$z,ninc=data[data$ninc==i,]$ninc,dist=data[data$ninc==(i+1),]$dist,espth=espth,distp=data[data$ninc==(i+1),]$dist/espth))
# suppression de data de la ligne ninc=i
data<-data[data$ninc!=i,]
#on supprime de data la colonne de distance (la derni�e)
data<-data[,-ncol(data)]
}

derpoint<-data[data$ninc==n,]
# on supprime la ligne du 1er point ninc=1
res<-res[-1,]
# changement des noms de ligne de res (sinon 11111111111111...)
row.names(res)<-rowname

cat("End of the computation of the expected distances (3D). Total time ellapsed: ")
delai(d,date())

list(res=res,derpoint=derpoint)
}

