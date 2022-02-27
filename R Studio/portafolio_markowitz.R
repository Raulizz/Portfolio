#Portfolio Markowits 3 actives

library(tseries)
library(fPortfolio)

Indice<- get.hist.quote(instrument = "^GSPC", 
                        start=as.Date("2020-09-09"), 
                        end=as.Date("2021-09-09"), quote = "AdjClose")

plot(Indice, col="darkgreen", xlab="Fecha", ylab="AdjClose"); title(main="Evolución S&P 5OO") 

summary(Indice)

AAPL<- get.hist.quote(instrument = "AAPL", 
                      start=as.Date("2020-09-09"), 
                      end=as.Date("2021-09-09"), quote = "AdjClose")

JPM <- get.hist.quote(instrument = "SPCE", 
                      start=as.Date("2020-09-09"), 
                      end=as.Date("2021-09-09"), quote = "AdjClose")

JNJ <- get.hist.quote(instrument = "TSLA", 
                      start=as.Date("2020-09-09"), 
                      end=as.Date("2021-09-09"), quote = "AdjClose")

Cartera <- merge(AAPL,JPM,JNJ, all = FALSE) 
names(Cartera)
names(Cartera)<-c("AAPL", "SPCE", "TSLA")

plot(Cartera, main=" ", col="darkgreen", xlab="Fecha")
title(main="Evolución de la Cartera")

RetornoIndice<-diff(log(Indice))
head(RetornoIndice,10)

plot(RetornoIndice, main=" ", col="darkgreen", xlab="Fecha", ylab="Rendimientos")
title(main="Rendimientos del Indice S&P 500")

Rendimientos<-diff(log(Cartera))
head(Rendimientos,10)

RendimientoPromedio = c(mean(RetornoIndice),mean(Rendimientos$AAPL),mean(Rendimientos$JPM),mean(Rendimientos$JNJ))

Volatilidad = c(sd(RetornoIndice),sd(Rendimientos$AAPL),sd(Rendimientos$JPM),sd(Rendimientos$JNJ) )

Tabla1 = data.frame (rbind(RendimientoPromedio,Volatilidad))
colnames(Tabla1)<- c("S&P500","AAPL", "JPM", "JNJ")

Tabla1*100 ##Expresado en %

## Calculo la varianza (riesgo) de los activos individuales 
var(RetornoIndice)*100
var(Rendimientos$AAPL)*100

## Matriz de Varianzas-Covarianzas en %
Cov <- cov(Rendimientos)*100
Cov

corr <- cor(Rendimientos) * 100
corr

## Heat Matrix
#require(gplots)

generate_heat_map <- function(correlationMatrix, title)
{
  
  heatmap.2(x = correlationMatrix,    
            cellnote = correlationMatrix,   
            main = title,           
            symm = TRUE,            
            dendrogram="none",      
            Rowv = FALSE,           
            trace="none",           
            density.info="none",        
            notecol="black")          
}

corr1 <- round(cor(Rendimientos) * 100, 2)
generate_heat_map(corr1,"Heatmap: Correlaciones")

espcartera<-portfolioSpec()

setRiskFreeRate(espcartera)<- -0.001 ##Rentabilidad Activo Libre de Riesgo
setNFrontierPoints(espcartera) <- 20

constraints="LongOnly"

Frontera <- portfolioFrontier(as.timeSeries(Rendimientos),spec=espcartera,constraints )
Frontera

frontierPlot(Frontera)
grid()
tangencyPoints(Frontera, pch = 19, col = "red", cex=2)
tangencyLines(Frontera, col="darkgreen", pch=19, cex=2)
minvariancePoints(Frontera, col="blue", pch=19, cex=2)
monteCarloPoints(Frontera, mCsteps=500, col="green", cex=0.001)

col <- qualiPalette(ncol(Rendimientos), "Dark2")
weightsPlot(Frontera, col=col)

efPortfolio <- efficientPortfolio(as.timeSeries(Rendimientos),espcartera,constraints)
efPortfolio 

tgPortfolio <- tangencyPortfolio(as.timeSeries(Rendimientos),espcartera,constraints)
tgPortfolio

weightsPie(efPortfolio, col=col )
mtext(text = "Portafolio eficiente", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)

weightsPie(tgPortfolio, col=col)
mtext(text = "Portafolio tangente", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)








