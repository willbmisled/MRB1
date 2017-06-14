load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100923.rda')
attach(MRB1)
plot(log10(Pinput),log10(Ninput),xlim=c(-2.5,8),ylim=c(-2.5,8),main="Don't include P in the Random Forest")
abline(summary(lm(log10(Ninput)~log10(Pinput))),lwd=2,col='red')
abline(0,1,lwd=2,col="blue")   #one to one line
text(-1,7,'Rsq=.8884',cex=2)
text(6,-2,'PlotPxN.r',cex=1)
