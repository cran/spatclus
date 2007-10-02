###########################################################################################
###########################################################################################
# m�hode de Kulldorff : tous les points de la pop comme centres, programmation avec 1 boucle for au lieu de 2 pr��emment, donn�s pr�lablement tranform� pour �re au standard de SatScan

kulld<-function(pop,rndm,methk,export,repexport)
{
d<-date()

# transformation des donn�s une fois que pop contient les cas et la pop de fond

pop<-cbind(1:nrow(pop),pop)
geo<-pop[,-4]
popk<-cbind(1:nrow(pop),rep(2004,nrow(pop)),rep(1,nrow(pop))) # 2004 = ann� fictive utilis� sous SatScan
cas<-cbind(pop)
cas[,4]<-0
cas[,4][rndm]<-1
cas<-cas[,-c(2,3)]
ctl<-cas
ctl<-cbind(ctl,1-ctl[,2])
ctl<-ctl[,-2]
# initiallisation des valeurs

nk<-nrow(geo)
ck<-sum(cas[,2]==1)

#####################
# Bernouilli

if (methk==1 | methk==3)
{
d<-date()

l<-0
for (index in 1:nk)
{
idxord<-geo[order(dist2p(geo[index,2],geo[index,3],geo[,2],geo[,3])),][,1]
nzk<-cumsum(popk[idxord,][1:(nk*0.5),3])
czk<-cumsum(cas[idxord,][1:(nk*0.5),2])
denlog<-ck*log(ck)+(nk-ck)*log(nk-ck)-nk*log(nk)
toto<-cbind(idxord[1:(nk*0.5)],nzk,czk,(czk/nzk)-((ck-czk)/(nk-nzk))<=0,numlog=czk*log(czk)-nzk*log(nzk)+(nzk-czk)*log((nzk-czk)+(nzk==czk)*1)-(nk-nzk)*log(nk-nzk)+(ck-czk)*log(ck-czk)+(nk-nzk-ck+czk)*log(nk-nzk-ck+czk))
toto<-cbind(toto,L=toto[,5]-denlog)
toto[toto[,4]==1,6]<-0
idxmax<-toto[toto[,6]==max(toto[,6]),][1]

L<-toto[toto[,6]==max(toto[,6]),][6]
if (sum(toto[,6])==0) L<-0

if (L>l)
{
l<-L
r<-dist2p(geo[index,2],geo[index,3],geo[idxmax,2],geo[idxmax,3])
centre<-c(geo[index,2],geo[index,3])
}
rm(idxord,nzk,czk,denlog,toto,idxmax,L)
}

bern<-data.frame(lambda=exp(l),loglambda=l,cx=centre[1],cy=centre[2],rayon=r)
rm(l,centre,r)
cat("Computation of the Kulldorff zone (Bernouilli) achieved in ")
delai(d,date())
}



#####################
# Poisson

if (methk==2 | methk==3)
{
d<-date()

l<-0
for (index in 1:nk)
{
idxord<-geo[order(dist2p(geo[index,2],geo[index,3],geo[,2],geo[,3])),][,1]
nzk<-cumsum(popk[idxord,][1:(nk*0.5),3])
czk<-cumsum(cas[idxord,][1:(nk*0.5),2])
denlog<-ck*(log(ck)-log(nk))
toto<-cbind(idxord[1:(nk*0.5)],nzk,czk,(czk/nzk)-((ck-czk)/(nk-nzk))<=0,numlog=czk*(log(czk)-log(nzk))+(ck-czk)*(log(ck-czk)-log(nk-nzk)))
toto<-cbind(toto,L=toto[,5]-denlog)
toto[toto[,4]==1,6]<-0
idxmax<-toto[toto[,6]==max(toto[,6]),][1]

L<-toto[toto[,6]==max(toto[,6]),][6]
if (sum(toto[,6])==0) L<-0

if (L>l)
{
l<-L
r<-dist2p(geo[index,2],geo[index,3],geo[idxmax,2],geo[idxmax,3])
centre<-c(geo[index,2],geo[index,3])
}
rm(idxord,nzk,czk,denlog,toto,idxmax,L)
}

pois<-data.frame(lambda=exp(l),loglambda=l,cx=centre[1],cy=centre[2],rayon=r)
rm(l,centre,r)
cat("Computation of the Kulldorff zone (Poisson) achieved in ")
delai(d,date())
}

if (methk==1) pois<-data.frame(lambda=NaN,loglambda=NaN,cx=NaN,cy=NaN,rayon=NaN)
if (methk==2) bern<-data.frame(lambda=NaN,loglambda=NaN,cx=NaN,cy=NaN,rayon=NaN)

if (export=="y")
{
write.table(geo,file=paste(repexport,"geo.geo",sep=""),sep="\t",col.names=FALSE,row.names=FALSE)
write.table(cas,file=paste(repexport,"cas.cas",sep=""),sep="\t",col.names=FALSE,row.names=FALSE)
write.table(popk,file=paste(repexport,"popk.pop",sep=""),sep="\t",col.names=FALSE,row.names=FALSE)
write.table(ctl,file=paste(repexport,"ctl.ctl",sep=""),sep="\t",col.names=FALSE,row.names=FALSE)
}

list(pois=pois,bern=bern)
}

# fin m�hode de Kulldorff
#########################################################################################
#########################################################################################
