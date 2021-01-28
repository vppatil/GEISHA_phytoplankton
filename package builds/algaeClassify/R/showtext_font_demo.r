
##demo script for specifying calibri fonts using showtext package.

install.packages('showtext')
library(showtext)

grep('calibri',dir(path='C:/Windows/Fonts/'),value=T)
 font_add("Calibri",regular="C:/Windows/Fonts/calibri.ttf",
					"bold=C:/Windows/Fonts/calibrib.ttf",
					italic="C:/Windows/Fonts/calibrib.ttf") #might have to search for path
 showtext_auto() #use showtext fonts
 
 #make a demo plot
 pdf('Calibri_histogram.pdf')
 
 hist(rnorm(1000),col="steelblue",border="white",main='',xlab='',ylab='')
 title('histogram title in Calibri',family='Calibri',cex.main=2)
 title(xlab='these axis labels are in calibri',family='Calibri',cex.main=2)
 title(ylab='these axis labels are in calibri',family='Calibri',cex.main=2)
 
 dev.off()