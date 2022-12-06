#This script comes from: https://nicolawongwaiyee.wordpress.com/2017/01/24/extract-fst-mat-data-from-arlequin-xml-output/

pkg_list = c("XML","corrplot","magrittr","dplyr")

check_pkg <- function(pkg){
  if(!require(pkg)){
    install.packages(pkg)
  }
}

lapply(pkg_list,check_pkg)

#Load the packages
lapply(pkg_list,require,character.only=TRUE)

arqxml <- xmlParse("./ARLEQUIN/geographic.res/geographic.xml")
arq_fstmat <- xpathSApply(arqxml,"//PairFstMat",xmlValue) %>% as.vector()
arq_fstmat <-strsplit(arq_fstmat,"\n")

arq_fstmat_mt <- do.call(cbind,arq_fstmat[1])
arq_fstmat_mt %<>% subset(arq_fstmat_mt[,1] != "")
arq_fstmat_mt <- gsub(" + "," ",arq_fstmat_mt)
arq_fstmat_mt <- strsplit(arq_fstmat_mt," ")
try(arq_fstmat_mt <- do.call(rbind,arq_fstmat_mt))
arq_fstmat_mt <- arq_fstmat_mt[3:nrow(arq_fstmat_mt),3:ncol(arq_fstmat_mt)]

#Full matrix
arq_fstmat_mt[upper.tri((arq_fstmat_mt))] <- t(arq_fstmat_mt)[upper.tri(arq_fstmat_mt)]
diag(arq_fstmat_mt) <-0

arq_rowname <- xpathSApply(arqxml,"//pairDistPopLabels",xmlValue) %>% 
  strsplit("\n")
arq_rowname <- do.call(cbind,arq_rowname) 
arq_rowname <- gsub("+\\d+:\t","",arq_rowname)
arq_rowname <- arq_rowname[5:nrow(arq_rowname)] %>% as.vector()
arq_fstmat_mt %<>% apply(c(1,2),as.numeric)

rownames(arq_fstmat_mt) <- arq_rowname
colnames(arq_fstmat_mt) <- arq_rowname

#Now the matrix looks...
arq_fstmat_mt[1:5,1:5]

write.csv(arq_fstmat_mt, "arq_fstmat_mt.csv")

arq_fstmat_mt[arq_fstmat_mt<0] <- 0
fstmat_plot <- corrplot(arq_fstmat_mt,method="color",type="lower",na.label = " ",
                        tl.col="black",tl.srt=8,order="FPC",cl.lim = c(0,1))

#for p-values

arq_fstp <- xpathSApply(arqxml,"//PairFstPvalMat",xmlValue) %>% as.vector()
arq_fstp<-strsplit(arq_fstp,"\n")

arq_fstp <- do.call(cbind,arq_fstp[1])
arq_fstp %<>% subset(arq_fstp[,1] != "")
arq_fstp <- gsub(" + "," ",arq_fstp)
arq_fstp <- strsplit(arq_fstp," ")
try(arq_fstp <- do.call(rbind,arq_fstp))

arq_fstp <- arq_fstp[2:nrow(arq_fstp),3:ncol(arq_fstp)]
arq_fstp[upper.tri(arq_fstp)] <-NA
diag(arq_fstp) <- 0

#Remove the SD of pvalue
arq_fstp <- gsub(" ","",arq_fstp)
arq_fstp <- gsub("\\+.*","",arq_fstp)

write.csv(arq_fstp, "arq_fstp.csv")

#Full matrix
arq_fstp[upper.tri(arq_fstp)] <- t(arq_fstp)[upper.tri(arq_fstp)]
rownames(arq_fstp) <- arq_rowname
colnames(arq_fstp) <- arq_rowname
arq_fstp %<>% apply(c(1,2),as.numeric)

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))

#FstMat with insig p-value mark
corrplot(arq_fstmat_mt,method="color",type="lower",na.label = " ",
         tl.col="black",tl.srt=10,order="original",cl.lim = c(0,1),
         col = col1(100),
         addCoef.col = "black", # Add coefficient of correlation
         p.mat = arq_fstp,sig.level = 0.05,insig="blank",pch.cex = 1,pch.col = "#6b717a")
