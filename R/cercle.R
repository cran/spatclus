cercle<-function(cx,cy,r,pas)
{
for (t in seq(0,2*pi,pas))
{
lines(c(cx+r*cos(t),cx+r*cos(t+pas)),c(cy+r*sin(t),cy+r*sin(t+pas)))
}
}