clus<-function

################################# paramètres par défaut ##################################
(
data,           #### les cas

pop,            #### pop de fond avec les cas (datainc="y") ou sans les cas (datainc="n")

dataincyn="n",  #### cas déjà inclus dans la pop de fond ("y" ou "n")

rndm=NaN,       #### lignes de pop correspondant à des cas
                ####             (si datainc="y", sinon on peut mettre n'importe quoi, NaN par exemple)

m=9,            #### Nombre de breaks maxi

eps=0.1,        #### longueur mini inter break en poucentage du nombre de points

limx,           #### 2 elements vector of the minimum and maximum X coordinate

limy,           #### 2 elements vector of the minimum and maximum Y coordinate

limz,           #### 2 elements vector of the minimum and maximum Z coordinate

method=1,      #### 1 = DEMATTEI : calcul de la distance pondérée et des bornes par régression
                #### 2 = KULLDORFF : calcul du centre et du rayon du cluster circulaire
                #### 3 = Application des 2 méthodes

methk=3,        #### Modèle utilisé dans la méthode de Kulldorff
                #### 1 = Modèle de Bernouilli (cas / Contrôle)
                #### 2 = Modèle de Poisson (Population de fond)

start=1,       #### ordre du point de départ. ex : si ordre=3, le point de départ est le 3eme point le plus proche du bord

export="n",     #### exportation des données au format SatScan ("y" ou "n")

repexport       #### répertoire de destination des 4 fichiers au format SatScan (noms prédéfinis geo,pop,cas,ctl)

)


############################## début de la fonction #######################################################
{
d<-date()

pop<-cbind(pop,rep(1,nrow(pop)))

if (dataincyn=="n")
{
pop<-datainc(data,pop)
rndm<-(1:nrow(data))
}

T<-nrow(data)-1
h<-floor(eps*T)

if ((m+1)>=1/eps)
{
cat("m = ",m," is greater than the value allowed by epsilon and replaced by m = ",floor(1/eps)-1,"\n")
}

m<-min(m,floor(1/eps)-1)
m<-m-1

########################################################################

###########################################################################################
###########################################################################################
# méthode régression sur distances

if (method!=1 & method!=3)
{
res<-NaN
bc<-NaN
rd<-list(res=res,bc=bc)
}

if (method==1 | method==3)
{
if (ncol(data)==2) rd<-regdist(data,pop,start,m,h,T,limx,limy)
if (ncol(data)==3) rd<-regdist3d(data,pop,start,m,h,T,limx,limy,limz)
rd$stat<-fstat(rd$res,rd$bc,m,T,eps)
}


#######################

###########################################################################################
###########################################################################################
# méthode de Kulldorff


# attribution de valeurs fictives pour output dans le cas ou Kulldorff non appliqué
if (method != 2 & method !=3)
{
pois<-NaN
bern<-NaN
kdf<-list(pois=pois,bern=bern)
}

if (method==2 | method==3)
{
kdf<-kulld(pop,rndm,methk,export,repexport)
}

# fin méthode de Kulldorff
#########################################################################################
#########################################################################################

delai(d,date())

list(res=rd$res,pop=pop[(T+2):nrow(pop),],bc=rd$bc,stat=rd$stat,kulld.p=kdf$pois,kulld.b=kdf$bern)
######################################################################################################
# fin de la function
}


