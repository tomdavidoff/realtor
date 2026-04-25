# concentration.R
# How concentrated are singles and multis, say in 2019?
# Tom Davidoff
# 04/24/26

library(data.table)
library(ggplot2)
YEAR <- 2018 # ends 2019, effectively starts 2014

fileDetached <- "~/DropboxExternal/Roomvu_Sauder\ projects/MLS\ BC\ data/detached.csv"
fileAttached <- "~/DropboxExternal/Roomvu_Sauder\ projects/MLS\ BC\ data/attached.csv"



for (ff in c(fileDetached,fileAttached)) {
	segment <- ifelse(grepl("detached",ff),"Detached","Attached")
	for (aa in c("North Vancouver","West Vancouver","Vancouver West")) {
		print(paste("NOW FOR THIS SEGMENT AND AREA:",segment,aa))
		dD <- fread(ff,select=c("List.Sales.Rep.1...CREA.ID","List.Price","List.Date","Region","Area"))
		dD <- dD[Region=="Greater Vancouver" & Area==aa]
		dD[,price:=as.numeric(gsub("[^0-9.]","",List.Price))/1000000]
		dD[,date:=as.Date(List.Date,format="%m/%d/%Y")]
		dD[,year:=year(date)]
		dD <- dD[year==YEAR]
		setnames(dD,"List.Sales.Rep.1...CREA.ID","id")
		dD <- dD[!is.na(id) & id!=""]
		# calculate herf
		dDR <- dD[,.(count=.N,sumPrice=sum(price)),by="id"]
		dDR[,share:=count/sum(count)]
		dDR[,shareP:=sumPrice/sum(sumPrice)]
		print(summary(dDR))
		herfN <- sum((dDR$share)^2)
		herfP <- sum((dDR$shareP)^2)
		print(c("HERF FOR THIS SEGMENT/AREA:",herfN,herfP))
		print(c("TOTAL TRANSACTIONS THIS SEGMENT/AREA:",sum(dDR$count)))
		# plot a histogram of shares
		ggplot(dDR,aes(x=shareP)) + geom_histogram() + ggtitle(paste("HERF for price shares in ",ff))
		ggsave(paste("text/herfPriceShares_",segment,aa,".png",sep=""))
		# print top 20 brokers by count
		print("TOP 20 BROKERS BY COUNT")
		print(dDR[order(-count)][1:20])
		# plot share vs price share
		ggplot(dDR,aes(x=share,y=shareP)) + geom_point() + ggtitle(paste("Share vs Price Share in ",ff))
		ggsave(paste0("text/shareVsPriceShare_",segment,aa,".png",sep=""))
		print("top 20% of brokers cumulative share")
		dDR <- dDR[order(-share)]
		print(sum(dDR[1:round(0.2*nrow(dDR)),share]))
	}
}
