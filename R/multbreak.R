multbreak<-function(res,m,h,T)
{

######################################## matrice SSR

ssr<-matrix(0,T,T)

for (i in 1:T)
{
for (j in 1:T)
{
if ((j-i)>=(h-1))
{
ssr[i,j]<-sum((res$distp[i:j]-mean(res$distp[i:j]))^2)
}
}
}

######################################## minimisation du ssr


b<-rep(0,m)

######################################## 1 point de rupture
if (m==1)
{
a<-ssr[1,h]+ssr[h+1,T]
for (j in h:(T-h))
{
a<-min(a,ssr[1,j]+ssr[j+1,T])
if (a==(ssr[1,j]+ssr[j+1,T]))
{
b<-j
}
}
}


######################################## 2 points de rupture
if(m==2)
{
SSR1<-cbind(rep(0,(T-(m+1)*h+1)),rep(0,(T-(m+1)*h+1)),rep(0,(T-(m+1)*h+1)))

for (n in (2*h):(T-(m-1)*h))
{
SSR1[n-2*h+1,2]<-ssr[1,h]+ssr[h+1,n]
for (j in h:(n-h))
{
SSR1[n-2*h+1,1]<-n
SSR1[n-2*h+1,2]<-min(SSR1[n-2*h+1,2],ssr[1,j]+ssr[j+1,n])
if (SSR1[n-2*h+1,2]==(ssr[1,j]+ssr[j+1,n]))
{
SSR1[n-2*h+1,3]<-j
}
}
}

SSR2<-SSR1[SSR1[,1]==m*h,2]+ssr[m*h+1,T]

for (j in (m*h):(T-h))
{
SSR2<-min(SSR2,(SSR1[SSR1[,1]==j,2]+ssr[j+1,T]))
if (SSR2==(SSR1[SSR1[,1]==j,2]+ssr[j+1,T]))
{
b[2]<-j
}
}

b[1]<-SSR1[SSR1[,1]==b[2],3]
}


######################################## m points de rupture

if (m>2)
{
SSR<-list()

SSR[[1]]<-rep(0,(T-(m+1)*h+1))
SSR[[1]]<-cbind(SSR[[1]],SSR[[1]],SSR[[1]])

for (n in (2*h):(T-(m-1)*h))
{
SSR[[1]][n-2*h+1,2]<-ssr[1,h]+ssr[h+1,n]
for (j in h:(n-h))
{
SSR[[1]][n-2*h+1,1]<-n
SSR[[1]][n-2*h+1,2]<-min(SSR[[1]][n-2*h+1,2],ssr[1,j]+ssr[j+1,n])
if (SSR[[1]][n-2*h+1,2]==(ssr[1,j]+ssr[j+1,n]))
{
SSR[[1]][n-2*h+1,3]<-j
}
}
}



for (r in 2:(m-1))
{
SSR[[r]]<-rep(0,(T-(m+1)*h+1))
SSR[[r]]<-cbind(SSR[[r]],SSR[[r]],SSR[[r]])

for (n in ((r+1)*h):(T-(m-r)*h))
{
SSR[[r]][n-(r+1)*h+1,2]<-SSR[[r-1]][1,2]+ssr[h+1,n]
for (j in (r*h):(n-h))
{
SSR[[r]][n-(r+1)*h+1,1]<-n

SSR[[r]][n-(r+1)*h+1,2]<-min(SSR[[r]][n-(r+1)*h+1,2],SSR[[r-1]][SSR[[r-1]][,1]==j,2]+ssr[j+1,n])
if (SSR[[r]][n-(r+1)*h+1,2]==(SSR[[r-1]][SSR[[r-1]][,1]==j,2]+ssr[j+1,n]))
{
SSR[[r]][n-(r+1)*h+1,3]<-j
}
}
}
}


SSRm<-SSR[[m-1]][SSR[[m-1]][,1]==m*h,2]+ssr[m*h+1,T]

for (j in (m*h):(T-h))
{
SSRm<-min(SSRm,(SSR[[m-1]][SSR[[m-1]][,1]==j,2]+ssr[j+1,T]))
if (SSRm==(SSR[[m-1]][SSR[[m-1]][,1]==j,2]+ssr[j+1,T]))
{
b[m]<-j
}
}

for (i in (m-1):1)
{
b[i]<-SSR[[i]][SSR[[i]][,1]==b[i+1],3]
}
}
b

}


