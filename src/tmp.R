thisMn <- which(outB[[k]][,"p"]==p&outB[[k]][,"t"]==t&outB[[k]][,"c"]=="nondt"&outB[[k]][,"d"]=="metaregression")

outB[[k]][thisMn,"p"]
outB[[k]][thisMn,"mean.estimates"]$mean.estimates$nondt

outB[[k]][thisMn,]