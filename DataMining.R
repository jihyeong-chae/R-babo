x <- data.frame( beer=c(0,1,1,1,0), bread=c(1,1,0,1,1), cola=c(0,0,1,0,1),
                 diapers=c(0,1,1,1,1), eggs=c(0,1,0,0,0), milk=c(1,0,1,1,1) )
x
install.packages("arules")
library(arules)
trans <- as.matrix(x,"Transaction")
trans
rules <- apriori(trans, parameter=list(supp=0.2, conf=0.6, target = "rules"))
rules
inspect(sort(rules))
install.packages("sna")
install.packages("rgl")
library(sna)
library(rgl)
#visualization
b2 <- t(as.matrix(trans)) %*% as.matrix(trans)
b2.w <- b2 - diag(diag(b2))
#rownames(b2.w)
#colnames(b2.w)
gplot(b2.w , displaylabel=T , vertex.cex=sqrt(diag(b2)) ,
      vertex.col = "Pink" , edge.col="grey" , boxed.labels=F ,
      arrowhead.cex = .3 , label.pos = 3 , edge.lwd = b2.w*2)