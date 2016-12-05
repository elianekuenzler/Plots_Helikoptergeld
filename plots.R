#import library
library(gdata)
library(colorspace)

#import dataset
publicDebt_GR <- read.xls("data/Staatsverschuldung_Griechenland.xlsx", sheet="Daten")
publicDebt_DE <- read.xls("data/Staatsverschuldung_Deutschland.xlsx", sheet="Daten")
bip_DE <- read.xls("data/BIP_Deutschland.xlsx", sheet="Daten")
bip_GR <- read.xls("data/BIP_Griechenland.xlsx", sheet="Daten")
bip_GR_percent <- read.xls("data/BIP_Deutschland.xlsx", sheet="prozent")
bip_DE_percent <- read.xls("data/BIP_Griechenland.xlsx", sheet="prozent")
moneySupply_M1 <- read.xls("data/Daten_Geldmenge_M1.xlsx", sheet="Daten")
moneySupply_M2 <- read.xls("data/Daten_Geldmenge_M2.xlsx", sheet="Daten")
moneySupply_M3 <- read.xls("data/Daten_Geldmenge_M3.xlsx", sheet="Daten")
moneySupply_M3_percent <- read.xls("data/Daten_Geldmenge_M3.xlsx", sheet="prozent")

#merge datasets into moneySupply
moneySupply_all <- merge(moneySupply_M1,moneySupply_M2,by="year")
moneySupply_all <- merge(moneySupply_all,moneySupply_M3,by="year")
colnames(moneySupply_all)[2] <- "m1"
colnames(moneySupply_all)[3] <- "m2"
colnames(moneySupply_all)[4] <- "m3"

#colors
startColor <- 120
endColor <- 240
luminance <- 70
chroma <- 50

color3 <- rainbow_hcl(3, c = chroma, l = luminance, start = startColor, end = endColor)
color2 <- rainbow_hcl(2, c = chroma, l = luminance, start = startColor, end = endColor)

#plot bip
png("images/plot_bip.png", width = 800, height = 600)
plot(bip_DE, type="o", ylim=c(0,3500), main="Bruttoinlandsprodukt", xlab="Jahr", ylab="BIP (in Milliarden Dollar/Euro)", pch=15, col=color2[1])
lines(bip_GR,type="o", pch=16, col=color2[2])
legend("topleft", inset=0.05 ,1, c("Deutschland (Euro)","Griechenland (Dollar)"), bty = "n",  col=color2, pch=c(15,16), lty=(1))
dev.off()

#plot bip_with_m3
png("images/plot_bip_with_m3.png", width = 800, height = 600)
plot(moneySupply_M3_percent,xaxt="n" , ylim=c(90,300),type="o", main="Prozentualeveränderung des BIP Deutschland und BIP Griechenland im Vergleich mit der Geldmenge M3", xlab="Jahr", ylab="Delta zu 1997 (%)", pch=15, col=color3[1])
lines(bip_GR_percent,type="o", pch=16,  col=color3[2])
lines(bip_DE_percent,type="o", pch=17,  col=color3[3])
axis(1, at = seq(1997, 2015, by = 1))
legend("topleft", inset=0.05 ,1, c("Geldmenge M3","BIP Griechenland","BIP Deutschland"), bty = "n",col=color3, pch=15:17, lty=(1))
dev.off()

#plot money supply
png("images/barplot_geldmenge.png", width = 800, height = 600)
par(xpd=T, mar=par()$mar+c(0,0,0,4))
barplot(t(moneySupply_all[, 2:4]),xlab="Jahr", ylim=c(0,30000), names.arg = moneySupply_all[ ,1], main="Geldmengen pro Jahr", ylab="Geldmenge (in Milliarden Euro)", col=color3)
legend(24, 17000, names(moneySupply_all[, 2:4]), fill=color3);
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()

#plot government debt
png("images/plot_staatsschulden.png", width = 800, height = 600)
plot(publicDebt_GR, type="o",xaxt="n" ,ylim=c(0,200), main="Staatsschulden", xlab="Jahr", ylab="Staatsschuldenquote (in % vom BIP)", pch=15, col=color2[1])
lines(publicDebt_DE,type="o", pch=16, col=color2[2])
axis(1, at = seq(1997, 2015, by = 1))
legend("topleft", inset=0.05 ,1, c("Griechenland","Deutschland"), bty = "n",col=color2 , pch=c(15,16), lty=(1))
dev.off()



