critval<-function(q=1,k=2,e=eps)
{
(8.238+1.756*q-0.043*q*q-0.659*k-15.436*e+0.025*q/e)*exp(0.389/k-0.013/(e*k))
}

critvalwdm<-function(q=1,e=eps)
{
(9.039+3.318*q-9.969*e)*exp(-0.03*q-0.327*e)
}

#################################################################

supf<-function(reslst,bc,k,T)
{
nomlst<-reslst
bc<-c(0,bc[[k]],T)

distp<-nomlst$distp[-(T+1)]
ninc<-nomlst$ninc[-(T+1)]
delta<-rep(0,k)

for (i in 1:(k+1)) delta[i]<-mean(distp[(bc[i]+1):bc[i+1]])

q<-1
p<-0

R<-matrix(0,nrow=k,ncol=(k+1))
diag(R)<-1
if (k>1) diag(R[,-1])<-(-1)
if (k==1) R[1,2]<-(-1)

Z<-matrix(0,nrow=T,ncol=(k+1))
for (i in 1:(k+1)) Z[(bc[i]+1):bc[i+1],i]<-1

sigdiff<-rep(0,k+1)
for (i in 1:(k+1)) sigdiff[i]<-sum((Z[,i]*distp-Z[,i]*delta[i])^2)/sum(Z[,i])

Vdiff<-diag(sigdiff*T*diag(solve(t(Z)%*%Z)))
Fdiff<-((T-(k+1)*q-p)/(k*q))*t(delta)%*%t(R)%*%solve(R%*%Vdiff%*%t(R))%*%R%*%delta
list(Fdiff=Fdiff)
}

#################################################################

fstat<-function(reslst,bc,m,T,eps)
{
f<-matrix(0,nrow=2,ncol=m)

for (i in 1:m) f[1,i]<-supf(reslst,bc,k=i,T)$Fdiff

for (j in 1:m) f[2,j]<-(critval(k=1,e=eps)/critval(k=j,e=eps))*f[1,j]

wdms<-critvalwdm(e=eps)
wdm<-max(f[2,])
signif<-(wdm>=wdms)

for (i in 1:m)
{
if (wdm==f[2,i]) kmax<-i
}

list(F=f,wdm=wdm,wdms=wdms,signif=signif,kmax=kmax)
}
