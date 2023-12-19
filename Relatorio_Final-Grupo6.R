### Data description

nome <- c("Nome do Estado", "Ano de Adoção", "Tempo de Falha", "Falha/Censura", "Gov_Dem", "Gov_Rep", "Dif_Dem-Rep (%)", "Ideologia do Cidadão", "Ideologia do Governo","Profissionalismo Legislativo", "População")

descricao <- c("Nome do Estado americano","Ano da adoção da política de autismo","Tempo até a adoção da política de autismo (Tempo inicial é quando o primeiro estado adota a política)","Se o estado adotou a política (1 - adotou/ 0 - não adotou)","Se o governo é democrata (1 - democrata/ 0 - não democrata)","Se o governo é republicano (1 - republicano / 0 - não republicano)","Diferença percentual entre os votos republicanos e democratas nas eleições para Governador do Estado","Medida da ideologia dos cidadãos (0 - Mais Conservador/ 100 - Mais liberal)","Medida da ideologia do Governo do Estado (0 - Mais Conservador/ 100 - Mais liberal)","Medida do profissionalismo legislativo (variável baseada na assembleia de cada estado - número de projetos aprovados, horas trabalhadas, salário dos funcionários, etc.)","População do Estado no ano de aprovação da lei ou no último ano")

table <- data.frame(nome,descricao)

knitr::kable(table, col.names = c("Nome","Descrição"), align = "l")



### Data and Packages Import

banco_autism <- read.csv2("C:/{DIRECTORY}autism.csv")


names(banco_autism)[1:11] <- c("Nome do Estado", "Ano de Adoção", "Tempo de Falha", "Falha/Censura", "Gov_Dem", "Gov_Rep", "Dif_Dem-Rep (%)", "Ideologia do Cidadão", "Ideologia do Governo","Profissionalismo Legislativo", "População")

sem_na <- banco_autism[complete.cases(banco_autism), ]
sem_na2 <- sem_na
banco_modelo <- banco_autism

require(pacman)
p_load("ggplot2", "usmap", "survival", "knitr", "AdequacyModel", "GGally")


### Exploratory Analysis

## USA Plots

a <- statepop
a <- a[-c(2, 9, 12),]

banco_autism <- banco_autism[order(banco_autism$`Nome do Estado`),]
banco_autism <- cbind(banco_autism, a$fips)
banco_autism$Gov_Dem <- as.character(banco_autism$Gov_Dem)
colnames(banco_autism)[12] <- "fips"

#GOV DEM/REP Plot
plot_usmap(data = banco_autism, values = "Gov_Dem", color = "black", labels = TRUE) +
  scale_fill_manual(values = c("#bd1313", "#1386bf"), name = "",
                    labels = c("Republicano", "Democrata", "Sem dados")) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))+
  ggtitle("Partido do Governador do Estado")

# Time until Bill Approval Plot and Table
plot_usmap(data = banco_autism, values = "Tempo de Falha", color = "black", labels = TRUE) + 
  scale_fill_continuous(name = "", label = scales::comma,
                        low = "#73b2ff", high = "#212962") + 
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))+
  ggtitle("Tempo até a Adoção da Medida")

x <- banco_autism
x$`Falha/Censura` <- ifelse(x$`Falha/Censura`==1,"Falha", "Censura")
a <- data.frame(table(x$`Falha/Censura`))
colnames(a) <- c("Tipo de Ocorrência", "Frequência")
kable(a, align = 'c')


## Variable plots and tables

# Vote discrepancy
ggplot(banco_autism, aes(x=factor(""), y=banco_autism$`Dif_Dem-Rep (%)`)) +
  geom_boxplot(fill=c("#7acedc"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Parcela de Votos (%)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

meanX1 = mean(banco_autism$`Dif_Dem-Rep (%)`)
medianX1= median(banco_autism$`Dif_Dem-Rep (%)`)
q1X1 = quantile(banco_autism$`Dif_Dem-Rep (%)`, names=FALSE)[2]
q2X1 = quantile(banco_autism$`Dif_Dem-Rep (%)`, names=FALSE)[3]
q3X1 = quantile(banco_autism$`Dif_Dem-Rep (%)`, names=FALSE)[4]
q4X1 = quantile(banco_autism$`Dif_Dem-Rep (%)`, names=FALSE)[5]
maxX1 = max(banco_autism$`Dif_Dem-Rep (%)`)
minX1 = min(banco_autism$`Dif_Dem-Rep (%)`)

varX1 = var(banco_autism$`Dif_Dem-Rep (%)`)
sdX1 = sd(banco_autism$`Dif_Dem-Rep (%)`)

d1X1 = quantile(banco_autism$`Dif_Dem-Rep (%)`, seq(0, 1, 0.1), names=FALSE)[2]
d9X1 = quantile(banco_autism$`Dif_Dem-Rep (%)`, seq(0, 1, 0.1), names=FALSE)[10]

skewX1 = 3*(meanX1 - medianX1)/sdX1
kurtX1 = (q3X1 - q1X1)/(2*(d9X1 - d1X1))
coef.varX1 = (sdX1/meanX1)*100

mX1 <- c(round(c(meanX1,minX1,q1X1,medianX1,q3X1,maxX1,varX1,sdX1,skewX1,kurtX1,coef.varX1),2))
nX1 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo","Variância","Desvio Padrão","Assimetria","Curtose","Coeficiente de Variação(%)")
variX1 <- data.frame(Medidas = nX1, Valores = mX1)
kable(variX1,caption = "Medidas de posição, variabilidade, assimetria e curtose")


# Citizen Ideology

x <- banco_autism[complete.cases(banco_autism), ]
ggplot(x, aes(x=factor(""), y=x$`Ideologia do Cidadão`)) +
  geom_boxplot(fill=c("#7acedc"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Ideologia do Cidadão")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

meanX1 = mean(x$`Ideologia do Cidadão`)
medianX1= median(x$`Ideologia do Cidadão`)
q1X1 = quantile(x$`Ideologia do Cidadão`, names=FALSE)[2]
q2X1 = quantile(x$`Ideologia do Cidadão`, names=FALSE)[3]
q3X1 = quantile(x$`Ideologia do Cidadão`, names=FALSE)[4]
q4X1 = quantile(x$`Ideologia do Cidadão`, names=FALSE)[5]
maxX1 = max(x$`Ideologia do Cidadão`)
minX1 = min(x$`Ideologia do Cidadão`)

varX1 = var(x$`Ideologia do Cidadão`)
sdX1 = sd(x$`Ideologia do Cidadão`)

d1X1 = quantile(x$`Ideologia do Cidadão`, seq(0, 1, 0.1), names=FALSE)[2]
d9X1 = quantile(x$`Ideologia do Cidadão`, seq(0, 1, 0.1), names=FALSE)[10]

skewX1 = 3*(meanX1 - medianX1)/sdX1
kurtX1 = (q3X1 - q1X1)/(2*(d9X1 - d1X1))
coef.varX1 = (sdX1/meanX1)*100

mX1 <- c(round(c(meanX1,minX1,q1X1,medianX1,q3X1,maxX1,varX1,sdX1,skewX1,kurtX1,coef.varX1),2))
nX1 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo","Variância","Desvio Padrão","Assimetria","Curtose","Coeficiente de Variação(%)")
variX1 <- data.frame(Medidas = nX1, Valores = mX1)
kable(variX1,caption = "Medidas de posição, variabilidade, assimetria e curtose")


# Government Ideology

ggplot(banco_autism, aes(x=factor(""), y=banco_autism$`Ideologia do Governo`)) +
  geom_boxplot(fill=c("#7acedc"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Ideologia do Governo")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

meanX1 = mean(banco_autism$`Ideologia do Governo`)
medianX1= median(banco_autism$`Ideologia do Governo`)
q1X1 = quantile(banco_autism$`Ideologia do Governo`, names=FALSE)[2]
q2X1 = quantile(banco_autism$`Ideologia do Governo`, names=FALSE)[3]
q3X1 = quantile(banco_autism$`Ideologia do Governo`, names=FALSE)[4]
q4X1 = quantile(banco_autism$`Ideologia do Governo`, names=FALSE)[5]
maxX1 = max(banco_autism$`Ideologia do Governo`)
minX1 = min(banco_autism$`Ideologia do Governo`)

varX1 = var(banco_autism$`Ideologia do Governo`)
sdX1 = sd(banco_autism$`Ideologia do Governo`)

d1X1 = quantile(banco_autism$`Ideologia do Governo`, seq(0, 1, 0.1), names=FALSE)[2]
d9X1 = quantile(banco_autism$`Ideologia do Governo`, seq(0, 1, 0.1), names=FALSE)[10]

skewX1 = 3*(meanX1 - medianX1)/sdX1
kurtX1 = (q3X1 - q1X1)/(2*(d9X1 - d1X1))
coef.varX1 = (sdX1/meanX1)*100

mX1 <- c(round(c(meanX1,minX1,q1X1,medianX1,q3X1,maxX1,varX1,sdX1,skewX1,kurtX1,coef.varX1),2))
nX1 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo","Variância","Desvio Padrão","Assimetria","Curtose","Coeficiente de Variação(%)")
variX1 <- data.frame(Medidas = nX1, Valores = mX1)
kable(variX1,caption = "Medidas de posição, variabilidade, assimetria e curtose")


# Professional Legislatism
ggplot(banco_autism, aes(x=banco_autism$`Profissionalismo Legislativo`)) + geom_histogram(colour="white", fill="#7acedc", binwidth = 0.1)+
  labs(x="Índice de Profissionalismo Legislativo", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

meanX1 = mean(banco_autism$`Profissionalismo Legislativo`)
medianX1= median(banco_autism$`Profissionalismo Legislativo`)
q1X1 = quantile(banco_autism$`Profissionalismo Legislativo`, names=FALSE)[2]
q2X1 = quantile(banco_autism$`Profissionalismo Legislativo`, names=FALSE)[3]
q3X1 = quantile(banco_autism$`Profissionalismo Legislativo`, names=FALSE)[4]
q4X1 = quantile(banco_autism$`Profissionalismo Legislativo`, names=FALSE)[5]
maxX1 = max(banco_autism$`Profissionalismo Legislativo`)
minX1 = min(banco_autism$`Profissionalismo Legislativo`)

varX1 = var(banco_autism$`Profissionalismo Legislativo`)
sdX1 = sd(banco_autism$`Profissionalismo Legislativo`)

d1X1 = quantile(banco_autism$`Profissionalismo Legislativo`, seq(0, 1, 0.1), names=FALSE)[2]
d9X1 = quantile(banco_autism$`Profissionalismo Legislativo`, seq(0, 1, 0.1), names=FALSE)[10]

skewX1 = 3*(meanX1 - medianX1)/sdX1
kurtX1 = (q3X1 - q1X1)/(2*(d9X1 - d1X1))
coef.varX1 = (sdX1/meanX1)*100

mX1 <- c(round(c(meanX1,minX1,q1X1,medianX1,q3X1,maxX1,varX1,sdX1,skewX1,kurtX1,coef.varX1),2))
nX1 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo","Variância","Desvio Padrão","Assimetria","Curtose","Coeficiente de Variação(%)")
variX1 <- data.frame(Medidas = nX1, Valores = mX1)
kable(variX1,caption = "Medidas de posição, variabilidade, assimetria e curtose")


# Population
ggplot(banco_autism, aes(x=log(banco_autism$População))) + geom_histogram(colour="white", fill="#7acedc", bins = 10)+
  labs(x="População", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

lp <- log(banco_autism$População)


meanX1 = mean(lp)
medianX1= median(lp)
q1X1 = quantile(lp, names=FALSE)[2]
q2X1 = quantile(lp, names=FALSE)[3]
q3X1 = quantile(lp, names=FALSE)[4]
q4X1 = quantile(lp, names=FALSE)[5]
maxX1 = max(lp)
minX1 = min(lp)

varX1 = var(lp)
sdX1 = sd(lp)

d1X1 = quantile(lp, seq(0, 1, 0.1), names=FALSE)[2]
d9X1 = quantile(lp, seq(0, 1, 0.1), names=FALSE)[10]

skewX1 = 3*(meanX1 - medianX1)/sdX1
kurtX1 = (q3X1 - q1X1)/(2*(d9X1 - d1X1))
coef.varX1 = (sdX1/meanX1)*100

mX1 <- c(round(c(meanX1,minX1,q1X1,medianX1,q3X1,maxX1,varX1,sdX1,skewX1,kurtX1,coef.varX1),2))
nX1 <- c("Média","Mínimo","1º Quartil","Mediana","3º Quartil","Máximo","Variância","Desvio Padrão","Assimetria","Curtose","Coeficiente de Variação(%)")
variX1 <- data.frame(Medidas = nX1, Valores = mX1)
kable(variX1,caption = "Medidas de posição, variabilidade, assimetria e curtose")


## Survival function plot and table

#kaplan meier estimator

tempo <- banco_autism$`Tempo de Falha`
censura <- banco_autism$`Falha/Censura`
KM<-survfit(Surv(tempo,censura)~1,conf.int=F)
plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)",mark.time = T, col = "#7acedc", main = "Gráfico da Função de Sobrevivência Estimada", cex.main = 1, lwd = 2.5)

a <- summary(KM)
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c')


# Cumulative hazard fuction

#KM

sobkm <- KM$surv
HHt <- -log(sobkm)
par(mfrow = c(1,2))
plot(stepfun(KM$time,c(0,HHt)),do.points = F,xlab="Tempo", ylab="H(t)",main = "Estimação por KM", col = "#7acedc", lwd=2.5, cex.main = 1)

#NA
plot(KM,conf.int=F, fun="cumhaz", xlab="Tempo", ylab="H(t)", col="#7acedc", main = "Estimação por NA",lwd = 2.5 , cex.main = 1)


# TTT
TTT(tempo, col = "#7acedc",lwd = 2.5, lty = 2)
title("Gráfico TTT", cex.main = 1)


## Time until failure and covariables

# Gov Ideology
idg <- banco_autism$`Ideologia do Governo`
for (i in 1:length(idg)) {
  if (idg[i]<50) {
    idg[i] <- 0
  }
  else {
    idg[i] <- 1
  }
  
}  

KM <- survfit(Surv(tempo,censura)~idg)


plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)", lty = c(1,2), col = c("#bd1313","#1386bf"), mark.time = T, lwd = 2.5, main = "Função de Sobrevivência por Ideologia do Governo", cex.main = 1)
legend(12,1.1, lty = c(1,2),col = c("#bd1313","#1386bf"), c("Conservador", "Liberal"))



a <- summary(KM[1])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Governo Conservador")

a <- summary(KM[2])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Governo Liberal")
idg2 <- idg


# TTT
dad2<-data.frame("Tempo"= banco_autism$`Tempo de Falha`,idg)
tempo1<-dad2[idg == "0",]
tempo2<-dad2[idg == "1",]
par(mfrow = c(1,2))
TTT(tempo1$Tempo, col= "#bd1313", lwd=2.5, grid=TRUE, lty=2)
title("TTT Governo Conservador", cex.main = 1)
TTT(tempo2$Tempo, col="#1386bf", lwd=2.5, grid=TRUE, lty=2)
title("TTT Governo Liberal", cex.main = 1)


survdiff(Surv(tempo,censura)~idg, rho = 1)


# Citizen Ideology
x <- banco_autism[complete.cases(banco_autism), ]
idc <- x$`Ideologia do Cidadão`
for (i in 1:length(idc)) {
  if (idc[i]<50) {
    idc[i] <- 0
  }
  else {
    idc[i] <- 1
  }
  
}  


KM <- survfit(Surv(x$`Tempo de Falha`,x$`Falha/Censura`)~idc)


plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)", lty = c(1,2), col = c("#bd1313","#1386bf"), mark.time = T, lwd = 2.5, main = "Função de Sobrevivência por Ideologia do Cidadão", cex.main = 1)
legend(1,0.3, lty = c(1,2),col = c("#bd1313","#1386bf"), c("Conservador", "Liberal"))



a <- summary(KM[1])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Cidadão Conservador")

a <- summary(KM[2])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Cidadão Liberal")


# TTT
dad2<-data.frame("Tempo"= x$`Tempo de Falha`,idc)
tempo1<-dad2[idc == "0",]
tempo2<-dad2[idc == "1",]
par(mfrow = c(1,2))
TTT(tempo1$Tempo, col= "#bd1313", lwd=2.5, grid=TRUE, lty=2)
title("TTT Cidadão Conservador", cex.main = 1)
TTT(tempo2$Tempo, col="#1386bf", lwd=2.5, grid=TRUE, lty=2)
title("TTT Cidadão Liberal", cex.main = 1)


survdiff(Surv(x$`Tempo de Falha`,x$`Falha/Censura`)~idc, rho = 1)


# Government Party
gov <- banco_autism$Gov_Dem
KM<-survfit(Surv(tempo,censura)~gov)
#summary(KM)

plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)", lty = c(1,2), col = c("#bd1313","#1386bf"), mark.time = T, lwd = 2.5, main = "Função de Sobrevivência por partido do governo", cex.main = 1)
legend(11,1.1, lty = c(1,2),col = c("#bd1313","#1386bf"), c("Republicano", "Democrata"))



a <- summary(KM[1])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Governo Republicano")

a <- summary(KM[2])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Governo Democrata")


# TTT
dad2<-data.frame("Tempo"= banco_autism$`Tempo de Falha`,gov)
tempo1<-dad2[idc == "0",]
tempo2<-dad2[idc == "1",]
par(mfrow = c(1,2))
TTT(tempo1$Tempo, col= "#bd1313", lwd=2.5, grid=TRUE, lty=2)
title("TTT Governo Republicano", cex.main = 1)
TTT(tempo2$Tempo, col="#1386bf", lwd=2.5, grid=TRUE, lty=2)
title("TTT Governo Democrata", cex.main = 1)

survdiff(Surv(tempo,censura)~gov, rho = 1)


# Vote discrepancy

idp <- banco_autism$`Dif_Dem-Rep (%)`
for (i in 1:length(idp)) {
  if (idp[i]< 26) {
    idp[i] <- 0
  }
  else {
    idp[i] <- 1
  }
  
}  

KM<-survfit(Surv(tempo,censura)~idp)
#summary(KM)

plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)", lty = c(1,2), col = c("#73b2ff","#212962"), mark.time = T, lwd = 2.5, main = "Função de Sobrevivência pela Diferença Percentual dos Votos")
legend(13,1,lty = c(1,2), col = c("#73b2ff","#212962"), c("< 26%", "> 26%"))

a <- summary(KM[1])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Diferença de votos menor do que 26%")

a <- summary(KM[2])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Diferença de votos maior do que 26%")
idp2 <- idp

# TTT
dad2<-data.frame("Tempo"= banco_autism$`Tempo de Falha`,idp)

tempo1<-dad2[idp == "0",]
tempo2<-dad2[idp == "1",]

par(mfrow = c(1,2))
TTT(tempo1$Tempo, col="#73b2ff", lwd=2.5, grid=TRUE, lty=2)
title("TTT Diferença menor do que 26%", cex.main = 1)
TTT(tempo2$Tempo, col="#212962", lwd=2.5, grid=TRUE, lty=2)
title("TTT Diferença maior do que 26%", cex.main = 1)


survdiff(Surv(tempo,censura)~idp, rho = 0)


# Legislative Professionalism
idpl <- banco_autism$`Profissionalismo Legislativo`
for (i in 1:length(idpl)) {
  if (idpl[i]< 0.303) {
    idpl[i] <- 0
  }
  else {
    idpl[i] <- 1
  }
  
}  

KM<-survfit(Surv(tempo,censura)~idpl)
#summary(KM)
plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)", lty = c(1,2), col = c("#73b2ff","#212962"), mark.time = T, lwd = 2.5, main = "Função de Sobrevivência pelo Profissionalismo Legislativo")
legend(13,1,lty = c(1,2), col = c("#73b2ff","#212962"), c("< 0.303", "> 0.303"))

a <- summary(KM[1])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Profissionalismo legislativo menor do que 0.303")

a <- summary(KM[2])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "Profissionalismo legislativo maior do que 0.303")
idpl2 <- idpl

# TTT
dad2<-data.frame("Tempo"= banco_autism$`Tempo de Falha`,idpl)

tempo1<-dad2[idpl == "0",]
tempo2<-dad2[idpl == "1",]

par(mfrow = c(1,2))
TTT(tempo1$Tempo, col="#73b2ff", lwd=2.5, grid=TRUE, lty=2)
title("TTT Prof. legis. < 0.303", cex.main = 1)
TTT(tempo2$Tempo, col="#212962", lwd=2.5, grid=TRUE, lty=2)
title("TTT Prof. legis. > 0.303", cex.main = 1)

survdiff(Surv(tempo,censura)~idpl, rho = 1)


# Population
idpop <- banco_autism$População
for (i in 1:length(idpop)) {
  if (idpop[i]< median(banco_autism$População)) {
    idpop[i] <- 0
  }
  else {
    idpop[i] <- 1
  }
  
}  

KM<-survfit(Surv(tempo,censura)~idpop)
#summary(KM)
plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)", lty = c(1,2), col = c("#73b2ff","#212962"), mark.time = T, lwd = 2.5, main = "Função de Sobrevivência pela População")
legend(12,1,lty = c(1,2), col = c(4,2), c("< 4.439.848", "> 4.439.848"))

a <- summary(KM[1])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "População menor do que 4.439.848")

a <- summary(KM[2])
b <- data.frame("Tempo de Falha" = a$time,
                "Indivíduos sob Risco" = a$n.risk,
                "Número de Falhas" = a$n.event,
                "Função de Sobrevivência Estimada" = a$surv,
                "Erro Padrão" = a$std.err)
names(b) <- c("Tempo de Falha",
              "Indivíduos sob Risco",
              "Número de Falhas",
              "Função de Sobrevivência Estimada",
              "Erro Padrão")
knitr::kable(b,align = 'c', caption = "População maior do que 4.439.848")
idpop2 <- idpop

# TTT
dad2<-data.frame("Tempo"= banco_autism$`Tempo de Falha`,idpop)

tempo1<-dad2[idpop == "0",]
tempo2<-dad2[idpop == "1",]

par(mfrow = c(1,2))
TTT(tempo1$Tempo, col="#73b2ff", lwd=2.5, grid=TRUE, lty=2)
title("TTT População menor que 4.439.848", cex.main = 1)
TTT(tempo2$Tempo, col="#212962", lwd=2.5, grid=TRUE, lty=2)
title("TTT População maior que 4.439.848", cex.main = 1)

survdiff(Surv(tempo,censura)~idpop, rho = 0)


### Models comparison

## Parameters
# Continuous Weibull
mwe <- survreg(Surv((tempo+0.01),censura)~1, data=banco_autism, dist = "weibull") # dá erro se não somar 0.01
#summary(mwe)
# estimativas estão com base no log(T)

# Estimativas da Weibull
alphaw <- exp(mwe$coefficients[1])
gammaw <- 1/mwe$scale

varalphaw <- ( exp(mwe$coefficients[1])^2 )*(mwe$var[1])
eppalphaw <- sqrt(varalphaw)

cw <- summary(mwe)$table[2,1]
vargammaw <- ((-exp(-cw))^2)*(mwe$var[4])
eppgammaw <- sqrt(vargammaw)

valores <- c(alphaw,gammaw)
b <- matrix(valores,nrow = 2,byrow = FALSE)
colnames(b) <- c("Estimativa")
rownames(b) <- c("$\\alpha$", "$\\beta$")
knitr::kable(b, align = 'c')

log_w <- mwe$loglik[1]


# Discrete Weibull
vero <- function(para){
  
  alfa <- para[1]
  gama <- para[2]
  
  q <- exp(-1/(alfa^gama))
  dens <- q^(tempo^gama)-q^((tempo+1)^gama)
  sobrev <- q^((tempo+1)^gama)
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1),vero,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

alphawd <- v1$par[1]
gammawd <- v1$par[2]

varalphawd <- variancia[1]
vargammawd <- variancia[2]

eppalphawd <- epp[1]
eppgammawd <- epp[2]

log_wd <- (-1)*v1$value

valores <- c(alphawd,gammawd)
b <- matrix(valores,nrow = 2,byrow = FALSE)
colnames(b) <- c("Estimativa")
rownames(b) <- c("$\\alpha$", "$\\beta$")
knitr::kable(b, align = 'c')

# Log-Logistic Continuous
mll<-survreg(Surv(tempo+0.01,censura)~1, dist='loglogistic')

alphall<-exp(mll$coefficients[1])
gammall<- 1/mll$scale


log_ll <- mll$loglik[1]


#cw <- summary(mll)$table[2,1]
#vargammall <- ((-exp(-cw))^2)*(mll$var[4])
#eppgammall <- sqrt(vargammall)

vargammall <- "-"
eppgammall <- "-"
varalphall <- "-"
eppalphall <- "-"

valores <- c(alphall,gammall)
b <- matrix(valores,nrow = 2,byrow = FALSE)
colnames(b) <- c("Estimativa")
rownames(b) <- c("$\\alpha$", "$\\beta$")
knitr::kable(b, align = 'c')


# Log-Logistic Discrete

vero_lld <- function(para){
  
  alfa <- para[1]
  gama <- para[2]
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1),vero_lld,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

alphalld <- v1$par[1]
gammalld <- v1$par[2]

varalphalld <- variancia[1]
vargammalld <- variancia[2]

eppalphalld <- epp[1]
eppgammalld <- epp[2]

log_lld <- (-1)*v1$value

valores <- c(alphalld,gammalld)
b <- matrix(valores,nrow = 2,byrow = FALSE)
colnames(b) <- c("Estimativa")
rownames(b) <- c("$\\alpha$", "$\\beta$")
knitr::kable(b, align = 'c')


## Results
KM<-survfit(Surv(tempo,censura)~1,conf.int=F)
time <- KM$time

sw <- exp(-(time/alphaw)^gammaw) # sobrevivencia log exponencial


q <- exp(-1/(alphawd^gammawd))
swd <- q^((time+1)^gammawd)


sll <- 1/(1+(time/alphall)^gammall) 


slld <- (1/(1+((time+1)/alphalld)^gammalld))


# Plots
plot(KM,conf.int = F,xlab = "Tempos", ylab = "S(t)",lwd=1)
lines(c(0,time),c(1,sw),lty=1,col="#00BFFF",lwd=2)
lines(c(0,time),c(1,swd),lty=2,col="#DC143C", lwd=2)
lines(c(0,time),c(1,sll),lty=3,col="#166928",lwd = 2)
lines(c(0,time),c(1,slld),lty=1,col="#eb9234",lwd = 2)
legend(0.7,0.7,lty = c(1,1,2,3,1),col=c(1,"#00BFFF","#DC143C", "#166928","#eb9234"),
       c("Kaplan-Meier","Weibull","Weibull Discreta", "Log-Logística", "Log-Logística Discreta"),
       bty="n",cex=0.8,lwd=2)
title("Ajuste da Função de Sobrevivência dos Modelos", cex.main = 1)

skm <- KM$surv
par(mfrow=c(2,2))
plot(skm,sw,xlab = "S(t): Kaplan-Meier",ylab = "S(t)", pch=16)
lines(c(0,1),c(0,1),type = "l",lty=1,col="#00BFFF")
title("Weibull")
plot(skm,swd,xlab = "S(t): Kaplan-Meier",ylab = "S(t)",pch=16)
lines(c(0,1),c(0,1),type = "l",lty=1,col="#DC143C")
title("Weibull Discreta")
plot(skm,sll,xlab = "S(t): Kaplan-Meier",ylab = "S(t)",pch=16)
lines(c(0,1),c(0,1),type = "l",lty=1,col="#166928")
title("Log-Logística")
plot(skm,slld,xlab = "S(t): Kaplan-Meier",ylab = "S(t)",pch=16)
lines(c(0,1),c(0,1),type = "l",lty=1,col="#eb9234")
title("Log-Logística Discreta")


# Selection criteria
## AIC, AICc e BIC - Weibull:
pw <- 2 # locação e escala
n <- 48

AICw <- -2*log_w + 2*pw
AICcw <- AICw + (2*pw*(pw+1))/(n-pw-1)
BICw <- -2*log_w + pw*log(n)

## AIC, AICc e BIC - Weibull D:
pwd <- 2 # locação e escala

AICwd <- -2*log_wd + 2*pwd
AICcwd <- AICwd + (2*pwd*(pwd+1))/(n-pwd-1)
BICwd <- -2*log_wd + pwd*log(n)

## AIC, AICc e BIC - LL:
pll <- 2 # locação e escala

AICll <- -2*log_ll + 2*pll
AICcll <- AICll + (2*pll*(pll+1))/(n-pll-1)
BICll <- -2*log_ll + pll*log(n)

## AIC, AICc e BIC - LLD
plld <- 2 # locação e escala

AIClld <- -2*log_lld + 2*plld
AICclld <- AIClld + (2*plld*(plld+1))/(n-plld-1)
BIClld <- -2*log_lld + plld*log(n)

d <- data.frame("Modelo" = c("Weibull", "Weibull Discreta", "Log-Logística", "Log-Logística Discreta"), AIC = c(AICw, AICwd, AICll, AIClld), "AICc" = c(AICcw,AICcwd,AICcll, AICclld),"BIC" = c(BICw,BICwd, BICll,BIClld))
knitr::kable(d,align = 'c',caption = "Critérios de Seleção dos Modelos")


# Correlation
names(banco_autism)[7:11] <- c("Dif_Dem-Rep (%)", "Id. do Cidadão", "Id. do Governo","Prof. Legislativo", "População")
a <- banco_autism[,6:11]
a$Gov_Rep <-ifelse(a$Gov_Rep==0,'Democratas','Republicanos')
ggpairs(a, 
        title="Correlograma das Variavéis Explicativas",
        lower=list(continuous = wrap("points"), 
                   discrete = "blank", combo="blank"),
        upper=list(combo = wrap("box_no_facet")))


## Cox Model
fit1 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  1)

fit2 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem)

fit3 <- coxph(Surv(sem_na$`Tempo de Falha`, sem_na$`Falha/Censura`) ~  sem_na$`Ideologia do Cidadão`)

fit4 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$`Dif_Dem-Rep (%)`)

fit5 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$`Ideologia do Governo`)

xx <- banco_modelo[complete.cases(banco_modelo), ]
fit5b <- coxph(Surv(xx$`Tempo de Falha`, xx$`Falha/Censura`) ~  xx$`Ideologia do Governo`)

fit6 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$`Profissionalismo Legislativo`)

fit7 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$População)

#Tables
df <- data.frame("Variáveis" = c("Tipo de Governo","Ideologia do Cidadão",
                                 "Diferença dos Votos (%)","Ideologia do Governo","Profissionalismo Legislativo", "População"),
                 "Estimativas"= c(0.6103, 0.0022,-0.0055,0.0295, 0.0676, "2.622e-08"),
                 "Erro Padrão"=c(0.3680,0.0115,0.0126,0.0114, 1.4501, "2.044e-08"),
                 "Estatística" = c(1.6580,0.1950,-0.4390,2.5870,
                                   0.0470,1.2830),
                 "P-valor" = c(0.0973,0.8450,0.6610,0.0097,0.9630,0.1990))
names(df) <- c("Variável","Estimativa","Erro Padrão",
               "Estatística","P-valor")
row.names(df) <- c("Modelo 1", "Modelo 2","Modelo 3", "Modelo 4","Modelo 5", "Modelo 6")
kable(df, caption = "Modelos com uma variável")


p <- data.frame("ll" = c(fit1$loglik,fit2$loglik[2], fit3$loglik[2], fit4$loglik[2], fit5$loglik[2], fit6$loglik[2], fit7$loglik[2]),
                "lll"= c(0,2*(fit2$loglik[2] - fit2$loglik[1]),2*(fit3$loglik[2] - fit3$loglik[1]), 2*(fit4$loglik[2] - fit4$loglik[1]),
                         2*(fit5$loglik[2] - fit5$loglik[1]), 2*(fit6$loglik[2] - fit6$loglik[1]),2*(fit7$loglik[2] - fit7$loglik[1])),
                "llll" = c(0,1-pchisq(2*(fit2$loglik[2] - fit2$loglik[1]),1),1-pchisq(2*(fit3$loglik[2] - fit3$loglik[1]),1), 1-pchisq(2*(fit4$loglik[2] - fit4$loglik[1]),1),
                           1-pchisq(2*(fit5$loglik[2] - fit5$loglik[1]),1), 1-pchisq(2*(fit6$loglik[2] - fit6$loglik[1]),1), 1-pchisq(2*(fit7$loglik[2] - fit7$loglik[1]),1)))

p <- round(p,4)

p[1,2] <- ""
p[1,3] <- ""

colnames(p) <- c("LogL-Verossimilhança","Estatística", "P-valor")
rownames(p) <- c("Modelo Simples", "Tipo de Governo", "Ideologia do Cidadão", "Diferença de Votos",
                 "Ideologia do Governo", "Profissionalismo Legislativo", "População")
kable(p,align = 'c', caption = "TRV dos modelos com uma variável")


# Categorized models
fit2d <- coxph(Surv(sem_na$`Tempo de Falha`, sem_na$`Falha/Censura`) ~  idc) 

fit3d <- coxph(Surv(banco_autism$`Tempo de Falha`, banco_autism$`Falha/Censura`) ~  idp2)

fit4d <- coxph(Surv(banco_autism$`Tempo de Falha`, banco_autism$`Falha/Censura`) ~  idg2) 

fit5d <- coxph(Surv(banco_autism$`Tempo de Falha`, banco_autism$`Falha/Censura`) ~  idpl2) 

fit6d <- coxph(Surv(banco_autism$`Tempo de Falha`, banco_autism$`Falha/Censura`) ~  idpop2)


# Tables
df <- data.frame("Variáveis Categorizadas" = c("Ideologia do Cidadão",
"Diferença dos Votos (%)","Ideologia do Governo","Profissionalismo Legislativo", "População"),
"Estimativas"= c(-0.2636, -0.2670, 0.7172,0.0407, 0.3934),
"Erro Padrão"=c(0.3801,0.3844,0.3768,0.5366, 0.3615),
"Estatística" = c(-0.6940,-0.6950,1.9030,0.0760,1.088),
"P-valor" = c(0.4880,0.4870,0.0570,0.9400,0.2760))
names(df) <- c("Variável Categorizada","Estimativa","Erro Padrão",
               "Estatística","P-valor")
row.names(df) <- c( "Modelo 2 discretizado","Modelo 3 discretizado", "Modelo 4 discretizado","Modelo 5 discretizado", "Modelo 6 discretizado")
kable(df, caption = "Modelos com uma variável")


g <- data.frame("ll" = c(fit1$loglik,fit2d$loglik[2], fit3d$loglik[2], fit4d$loglik[2], fit5d$loglik[2], fit6d$loglik[2]),
                "lll"= c(0,2*(fit2d$loglik[2] - fit2d$loglik[1]),2*(fit3d$loglik[2] - fit3d$loglik[1]), 2*(fit4d$loglik[2] - fit4d$loglik[1]),
                         2*(fit5d$loglik[2] - fit5d$loglik[1]), 2*(fit6d$loglik[2] - fit6d$loglik[1])),
                "llll" = c(0,1-pchisq(2*(fit2d$loglik[2] - fit2d$loglik[1]),1),1-pchisq(2*(fit3d$loglik[2] - fit3d$loglik[1]),1), 1-pchisq(2*(fit4d$loglik[2] - fit4d$loglik[1]),1),
                           1-pchisq(2*(fit5d$loglik[2] - fit5d$loglik[1]),1), 1-pchisq(2*(fit6d$loglik[2] - fit6d$loglik[1]),1)))

g <- round(g,4)

g[1,2] <- ""
g[1,3] <- ""

colnames(g) <- c("LogL-Verossimilhança","Estatística", "P-valor")
rownames(g) <- c("Modelo Simples", "Ideologia do Cidadão", "Diferença de Votos",
                 "Ideologia do Governo", "Profissionalismo Legislativo", "População")
kable(g,align = 'c' , caption = "TRV dos modelos com uma variável")


# Temporary final model

fit8 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem + banco_modelo$`Ideologia do Governo` + banco_modelo$População)

df <- data.frame("Variável" = c("Tipo de Governo",
                                "Ideologia do Governo","População"),"Estimativas"= c(-0.2000, 0.0319,"1.045e-08"),
                 "Erro Padrão"=c(0.4829,0.0150, "2.032e-08"),
                 "Estatística" = c(-0.4140,2.1320,0.5140),
                 "P-valor" = c(0.6790,0.0330,0.6070))
names(df) <- c("Variável","Estimativa","Erro Padrão",
               "Estatística","P-valor")
row.names(df) <- NULL
kable(df, caption = "Modelo Final Provisório")


# Tests
fitg1 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$População+banco_modelo$`Ideologia do Governo`)

fitg2 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$`Ideologia do Governo`+banco_modelo$Gov_Dem)

fitg3 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$População+banco_modelo$Gov_Dem)


# Tables

df <- data.frame("Variável" = c("População",
                                "Ideologia do Governo"),"Estimativas"= c("1.002e-08", 0.0279),
                 "Erro Padrão"=c("2.036e-08",0.0117),
                 "Estatística" = c(0.4920,2.3790),
                 "P-valor" = c(0.6228,0.01740))
names(df) <- c("Variável","Estimativa","Erro Padrão",
               "Estatística","P-valor")
row.names(df) <- NULL
kable(df, caption = "Modelo sem Tipo de Governo")

df <- data.frame("Variável" = c("Ideologia do Governo",
                                "Tipo de Governo"),"Estimativas"= c(0.0331, -0.1838),
                 "Erro Padrão"=c(0.0147,0.4737),
                 "Estatística" = c(2.2570,-0.3880),
                 "P-valor" = c(0.0240,0.6980))
names(df) <- c("Variável","Estimativa","Erro Padrão",
               "Estatística","P-valor")
row.names(df) <- NULL
kable(df, caption = "Modelo sem População")

df <- data.frame("Variável" = c("População",
                                "Tipo de Governo"),"Estimativas"= c("1.920e-08", 0.5534),
                 "Erro Padrão"=c("2.056e-08",0.3761),
                 "Estatística" = c(0.9340,1.4720),
                 "P-valor" = c(0.3500,0.1410))
names(df) <- c("Variável","Estimativa","Erro Padrão",
               "Estatística","P-valor")
row.names(df) <- NULL
kable(df, caption = "Modelo sem Ideologia do Governo")


g <- data.frame("ll" = c(fit8$loglik[2],fitg1$loglik[2],fitg2$loglik[2], fitg3$loglik[2]),
                "lll"= c(0,2*(fit8$loglik[2] - fitg1$loglik[2]),2*(fit8$loglik[2] - fitg2$loglik[2]), 2*(fit8$loglik[2] - fitg3$loglik[2])),
                "llll" = c(0,1-pchisq(2*(fit8$loglik[2] - fitg1$loglik[2]),1),1-pchisq(2*(fit8$loglik[2] - fitg2$loglik[2]),1), 1-pchisq(2*(fit8$loglik[2] - fitg3$loglik[2]),1)))

g <- round(g,4)

g[1,2] <- ""
g[1,3] <- ""

colnames(g) <- c("LogL-Verossimilhança","Estatística", "P-valor")
rownames(g) <- c("Modelo Final Provisório", "Modelo sem Tipo de Governo", "Modelo sem População", 
                 "Modelo sem Ideologia do Governo")
kable(g,align = 'c', caption ="TRV dos modelos com duas variáveis contra o modelo final provisório" )



g <- data.frame("ll" = c(fitg2$loglik[2],fit2$loglik[2],fit5$loglik[2]),
                "lll"= c(0,2*(fitg2$loglik[2] - fit2$loglik[2]),2*(fitg2$loglik[2] - fit5$loglik[2])),
                "llll" = c(0,1-pchisq(2*(fitg2$loglik[2] - fit2$loglik[2]),1),1-pchisq(2*(fitg2$loglik[2] - fit5$loglik[2]),1)))

g <- round(g,4)

g[1,2] <- ""
g[1,3] <- ""

colnames(g) <- c("LogL-Verossimilhança","Estatística", "P-valor")
rownames(g) <- c("Modelo sem População", "Tipo de Governo",  "Ideologia do Governo")
kable(g,align = 'c', caption ="TRV dos modelos com uma variável contra o modelo sem População" )


# Final model X Complete Model

mod_tot1 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem + banco_modelo$`Ideologia do Governo` + banco_modelo$`Dif_Dem-Rep (%)` + banco_modelo$`Ideologia do Cidadão` + banco_modelo$`Profissionalismo Legislativo`)

mod_tot2 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem + banco_modelo$`Ideologia do Governo` + banco_modelo$`Dif_Dem-Rep (%)` + banco_modelo$`Ideologia do Cidadão` + banco_modelo$População)


df <- data.frame("Variável" = c("Tipo de Governo",
                                "Ideologia do Governo","Diferença de Votos (%)","Ideologia do Cidadão","Profissionalismo Legislativo"),"Estimativas"= c(0.4008, -0.0127,-0.0033,0.0094,-1.4231),
                 "Erro Padrão"=c(0.5334,0.0208,0.0157,0.0142,1.2578),
                 "Estatística" = c(0.7510,-0.6120,-0.2090,0.6640,-1.1310),
                 "P-valor" = c(0.4520,0.5410,0.8350,0.5070,0.2580))
names(df) <- c("Variável","Estimativa","Erro Padrão",
               "Estatística","P-valor")
row.names(df) <- NULL
kable(df, caption = "Modelo Completo 1")

df <- data.frame("Variável" = c("Tipo de Governo",
                                "Ideologia do Governo","Diferença de Votos (%)","Ideologia do Cidadão","População"),"Estimativas"= c(0.3338, -0.0099,-0.0037,0.0067,"-7.125e-09"),
                 "Erro Padrão"=c(0.5448,0.0207,0.0164,0.0145,"2.181e-08"),
                 "Estatística" = c(0.6130,-0.4770,-0.2260,0.4610,-0.3270),
                 "P-valor" = c(0.5400,0.6330,0.8210,0.6450,0.7440))
names(df) <- c("Variável","Estimativa","Erro Padrão",
               "Estatística","P-valor")
row.names(df) <- NULL
kable(df, caption = "Modelo Completo 2")

g <- data.frame("ll" = c(fit5b$loglik[2],mod_tot1$loglik[2],mod_tot2$loglik[2]),
                "lll"= c(0,2*(mod_tot1$loglik[2] - fit5b$loglik[2]),2*(mod_tot2$loglik[2] - fit5b$loglik[2])),
                "llll" = c(0,1-pchisq(2*(mod_tot1$loglik[2] - fit5b$loglik[2]),1),1-pchisq(2*(mod_tot2$loglik[2] - fit5b$loglik[2]),1)))

g <- round(g,4)

g[1,2] <- ""
g[1,3] <- ""

colnames(g) <- c("LogL-Verossimilhança","Estatística", "P-valor")
rownames(g) <- c("Modelo 4", "Modelo Completo 1", "Modelo Completo 2")
kable(g,align = 'c', caption ="TRV do Modelo 4 com contra os modelos Completos" )


# Manual backwards modeling
mod_tot1a <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem + banco_modelo$`Ideologia do Governo` +  banco_modelo$`Ideologia do Cidadão` + banco_modelo$`Profissionalismo Legislativo`)

mod_tot1b <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem +   banco_modelo$`Ideologia do Cidadão` + banco_modelo$`Profissionalismo Legislativo`)

mod_tot1c <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem  + banco_modelo$`Profissionalismo Legislativo`)

mod_tot1d <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem)

mod_tot2a <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem + banco_modelo$`Ideologia do Governo` +  banco_modelo$`Ideologia do Cidadão` + banco_modelo$População)

mod_tot2b <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem +   banco_modelo$`Ideologia do Cidadão`+ banco_modelo$`Ideologia do Governo` )

mod_tot2c <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$Gov_Dem +    banco_modelo$`Ideologia do Governo` )

mod_tot2d <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$`Ideologia do Governo` )

## Parametric Model

vet_gamma <- c()
vet_beta1 <- c()

vet_epp_gamma <- c()
vet_epp_beta1 <- c()

vet_logv <- c()


vero_simples <- function(para){
  
  beta0 <- para[1]
  gama <- para[2]
  alfa <- exp(beta0)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1),vero_simples,NULL,hessian = TRUE)

vet_logv[1] <- (-1)*v1$value


############################################################################


vero_tipogov <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*banco_modelo$Gov_Dem)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1,1),vero_tipogov,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vet_beta1[1] <- v1$par[2]
vet_gamma[1] <- v1$par[3]

vet_epp_beta1[1] <- epp[2]
vet_epp_gamma[1] <- epp[3]

vet_logv[2] <- (-1)*v1$value


############################################################################


vero_difp <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*banco_autism$`Dif_Dem-Rep (%)`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(2.5,0,0.5),vero_difp,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vet_beta1[2] <- v1$par[2]
vet_gamma[2] <- v1$par[3]

vet_epp_beta1[2] <- epp[2]
vet_epp_gamma[2] <- epp[3]

vet_logv[3] <- (-1)*v1$value


############################################################################

sem_na2 <- banco_autism[complete.cases(banco_autism), ]

tempo2 <- sem_na2$`Tempo de Falha`
censura2 <- sem_na2$`Falha/Censura`

vero_idc <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*sem_na2$`Id. do Cidadão`)
  
  
  dens <- (1/(1+(tempo2/alfa)^gama)) - (1/(1+((tempo2+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo2+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura2*log(dens)+
                      (1-censura2)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1.8,0,0.3),vero_idc,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vet_beta1[3] <- v1$par[2]
vet_gamma[3] <- v1$par[3]

vet_epp_beta1[3] <- epp[2]
vet_epp_gamma[3] <- epp[3]

vet_logv[4] <- (-1)*v1$value


############################################################################


vero_idg <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*banco_autism$`Id. do Governo`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(3,0,0.5),vero_idg,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vet_beta1[4] <- v1$par[2]
vet_gamma[4] <- v1$par[3]

vet_epp_beta1[4] <- epp[2]
vet_epp_gamma[4] <- epp[3]

vet_logv[5] <- (-1)*v1$value

############################################################################


vero_pl <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*banco_autism$`Prof. Legislativo`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1,1),vero_pl,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vet_beta1[5] <- v1$par[2]
vet_gamma[5] <- v1$par[3]

vet_epp_beta1[5] <- epp[2]
vet_epp_gamma[5] <- epp[3]

vet_logv[6] <- (-1)*v1$value


############################################################################

vero_simples_na <- function(para){
  
  beta0 <- para[1]
  gama <- para[2]
  alfa <- exp(beta0)
  
  
  dens <- (1/(1+(tempo2/alfa)^gama)) - (1/(1+((tempo2+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo2+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura2*log(dens)+
                      (1-censura2)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1),vero_simples_na,NULL,hessian = TRUE)

semna_logv <- (-1)*v1$value

# Tables

vet_est <- c(vet_beta1[1]/vet_epp_beta1[1], vet_beta1[2]/vet_epp_beta1[2], vet_beta1[3]/vet_epp_beta1[3], vet_beta1[4]/vet_epp_beta1[4], vet_beta1[5]/vet_epp_beta1[5]) 

vet_pvalor <- c(pnorm(vet_est[1],lower.tail = T)*2, pnorm(vet_est[2],lower.tail = F)*2,
                pnorm(vet_est[3],lower.tail = F)*2, pnorm(vet_est[4],lower.tail = T)*2,
                pnorm(vet_est[5],lower.tail = F)*2)



x <- data.frame(vet_beta1,vet_gamma,vet_epp_beta1,vet_epp_gamma,vet_est,vet_pvalor)
x <- round(x,3)
x <- cbind(c("Tipo de Governo", "Dif. Percentual (%)", "Id. do Cidadão", "Id. do Governo", "Prof. Legislativo"), x)
colnames(x) <- c("Variável", "Estimativa", "Gamma", "EP Beta1", "EP Gamma", "Estatística", "P-Valor")
rownames(x) <- c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5")
kable(x,align = 'c', caption = "Modelos com uma variável")

p <- data.frame("ll" = c(vet_logv[1],vet_logv[2], vet_logv[3], vet_logv[4], vet_logv[5], vet_logv[6]),
                
                "lll"= c(0,2*(vet_logv[2] - vet_logv[1]),2*(vet_logv[3] - vet_logv[1]), 2*(vet_logv[4] - semna_logv),2*(vet_logv[5] - vet_logv[1]), 2*(vet_logv[6] - vet_logv[1])),
                
                "llll" = c(0,1-pchisq(2*(vet_logv[2] - vet_logv[1]),1),1-pchisq(2*(vet_logv[3] - vet_logv[1]),1), 1-pchisq(2*(vet_logv[4] - semna_logv),1),
                           1-pchisq(2*(vet_logv[5] - vet_logv[1]),1), 1-pchisq(2*(vet_logv[6] - vet_logv[1]),1)))

p <- round(p,4)

p[1,2] <- ""
p[1,3] <- ""

colnames(p) <- c("LogL-Verossimilhança","Estatística", "P-valor")
rownames(p) <- c("Modelo Simples", "Tipo de Governo", "Diferença de Votos", "Ideologia do Cidadão",
                 "Ideologia do Governo", "Profissionalismo Legislativo")
kable(p,align = 'c', caption = "TRV dos modelos com uma variável")


# Categorized Variables 

vetD_gamma <- c() 
vetD_beta1 <- c()

vetD_epp_gamma <- c()
vetD_epp_beta1 <- c()

vetD_logv <- c()


############################################################################


veroD_difp <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*idp2)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1,1),veroD_difp,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vetD_beta1[1] <- v1$par[2]
vetD_gamma[1] <- v1$par[3]

vetD_epp_beta1[1] <- epp[2]
vetD_epp_gamma[1] <- epp[3]

vetD_logv[1] <- (-1)*v1$value


############################################################################


veroD_idc <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*idc)
  
  
  dens <- (1/(1+(tempo2/alfa)^gama)) - (1/(1+((tempo2+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo2+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura2*log(dens)+
                      (1-censura2)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1,1),veroD_idc,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vetD_beta1[2] <- v1$par[2]
vetD_gamma[2] <- v1$par[3]

vetD_epp_beta1[2] <- epp[2]
vetD_epp_gamma[2] <- epp[3]

vetD_logv[2] <- (-1)*v1$value


############################################################################


veroD_idg <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*idg2)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1,1),veroD_idg,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vetD_beta1[3] <- v1$par[2]
vetD_gamma[3] <- v1$par[3]

vetD_epp_beta1[3] <- epp[2]
vetD_epp_gamma[3] <- epp[3]

vetD_logv[3] <- (-1)*v1$value

############################################################################


veroD_pl <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*idpl2)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1,1),veroD_pl,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vetD_beta1[4] <- v1$par[2]
vetD_gamma[4] <- v1$par[3]

vetD_epp_beta1[4] <- epp[2]
vetD_epp_gamma[4] <- epp[3]

vetD_logv[4] <- (-1)*v1$value


############################################################################

veroD_pop <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*idpop2)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(1,1,1),veroD_pop,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vetD_beta1[5] <- v1$par[2]
vetD_gamma[5] <- v1$par[3]

vetD_epp_beta1[5] <- epp[2]
vetD_epp_gamma[5] <- epp[3]

vetD_logv[5] <- (-1)*v1$value

# Tables

vetD_est <- c(vetD_beta1[1]/vetD_epp_beta1[1], vetD_beta1[2]/vetD_epp_beta1[2], vetD_beta1[3]/vetD_epp_beta1[3], vetD_beta1[4]/vetD_epp_beta1[4], vetD_beta1[5]/vetD_epp_beta1[5]) 

vetD_pvalor <- c(pnorm(vetD_est[1],lower.tail = F)*2, pnorm(vetD_est[2],lower.tail = F)*2,
                 pnorm(vetD_est[3],lower.tail = T)*2, pnorm(vetD_est[4],lower.tail = T)*2,
                 pnorm(vetD_est[5],lower.tail = T)*2)



x <- data.frame(vetD_beta1,vetD_gamma,vetD_epp_beta1,vetD_epp_gamma,vetD_est,vetD_pvalor)
x <- round(x,3)
x <- cbind(c("Dif. Percentual (%)", "Id. do Cidadão", "Id. do Governo", "Prof. Legislativo", "População"),x)
colnames(x) <- c("Variável", "Estimativa", "Gamma", "EP Beta1", "EP Gamma", "Estatística", "P-Valor")
rownames(x) <- c("Modelo 2 Discretizado", "Modelo 3 Discretizado", "Modelo 4 Discretizado", "Modelo 5 Discretizado", "Modelo 6 Discretizado")
kable(x,align = 'c', caption = "Modelos com uma variável")


p <- data.frame("ll" = c(vet_logv[1],vetD_logv[1], vetD_logv[2], vetD_logv[3], vetD_logv[4], vetD_logv[5]),
                
                "lll"= c(0,2*(vetD_logv[1] - vet_logv[1]),2*(vetD_logv[2] - semna_logv), 2*(vetD_logv[3] - vet_logv[1]),2*(vetD_logv[4] - vet_logv[1]), 2*(vetD_logv[5] - vet_logv[1])),
                
                "llll" = c(0,1-pchisq(2*(vetD_logv[1] - vet_logv[1]),1),1-pchisq(2*(vetD_logv[2] - semna_logv),1), 1-pchisq(2*(vetD_logv[3] - vet_logv[1]),1),
                           1-pchisq(2*(vetD_logv[4] - vet_logv[1]),1), 1-pchisq(2*(vetD_logv[5] - vet_logv[1]),1)))

p <- round(p,4)

p[1,2] <- ""
p[1,3] <- ""

colnames(p) <- c("LogL-Verossimilhança","Estatística", "P-valor")
rownames(p) <- c("Modelo Simples", "Diferença de Votos", "Ideologia do Cidadão",
                 "Ideologia do Governo", "Profissionalismo Legislativo", "População")
kable(p,align = 'c', caption = "TRV dos modelos com uma variável")


## Final Model X Complete Model

vero_completo <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  beta2 <- para[3]
  beta3 <- para[4]
  beta4 <- para[5]
  gama <- para[6]
  alfa <- exp(beta0+beta1*banco_modelo$Gov_Dem+beta2*banco_modelo$`Dif_Dem-Rep (%)`+beta3*banco_modelo$`Ideologia do Governo`+beta4*banco_modelo$`Profissionalismo Legislativo`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(3.3,0.04,0,-0.01,0.45,0.5),vero_completo,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)

vet_betas <- v1$par[c(-1,-6)]

vet_epp_betas_comp <- epp[c(-1,-6)]

comp_logv <- (-1)*v1$value

####################################################

vet_estw_comp <- c(vet_betas[1]/vet_epp_betas_comp[1], vet_betas[2]/vet_epp_betas_comp[2],
                   vet_betas[3]/vet_epp_betas_comp[3], vet_betas[4]/vet_epp_betas_comp[4])

vet_pvalor_comp <- c(pnorm(vet_estw_comp[1], lower.tail = T)*2,pnorm(vet_estw_comp[2], lower.tail = F)*2,pnorm(vet_estw_comp[3], lower.tail = F)*2, pnorm(vet_estw_comp[4], lower.tail = T)*2)


df <- data.frame("Estimativas"= vet_betas,
                 "Erro Padrão"=vet_epp_betas_comp,
                 "Estatística" = vet_estw_comp,
                 "P-valor" = vet_pvalor_comp)
df <- round(df,4)
df <- cbind(c("Tipo de Governo","Diferença de Votos (%)", "Ideologia do Governo","Profissionalismo Legislativo"),df)
colnames(df) <- c("Variável","Estimativa","Erro Padrão",
                  "Estatística","P-valor")
row.names(df) <- NULL
kable(df, caption = "Modelo Completo", label = "*Com Gamma = 2.2643 de EP = 0.3896")


g <- data.frame("ll" = c(vet_logv[5],comp_logv),
                "lll"= c(0,2*(comp_logv - vet_logv[5])),
                "llll" = c(0,1-pchisq(2*(comp_logv - vet_logv[5]),1)))

g <- round(g,4)

g[1,2] <- ""
g[1,3] <- ""

colnames(g) <- c("LogL-Verossimilhança","Estatística", "P-valor")
rownames(g) <- c("Modelo Final Provisório", "Modelo Completo")
kable(g,align = 'c', caption ="TRV do Modelo Final Provisório contra o Modelo Completo")

# Manual backwards method

vetB_gamma <- c(0,0,0,0)
vetB_betas <- c()

vetB_epp_betas <- c()

vetB_logv <- c()

# sem pop e sem idc
vero_back_pl <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  beta2 <- para[3]
  beta3 <- para[4]
  beta4 <- para[5]
  gama <- para[6]
  alfa <- exp(beta0+beta1*banco_modelo$Gov_Dem+beta2*banco_modelo$`Dif_Dem-Rep (%)`
              +beta3*banco_modelo$`Ideologia do Governo`+beta4*banco_modelo$`Profissionalismo Legislativo`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(3.3,0.04,0.001,-0.01,0.45,0.5),vero_back_pl,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)


vetB_betas[1] <- v1$par[1]
vetB_betas[2] <- v1$par[2]
vetB_betas[3] <- v1$par[3]
vetB_betas[4] <- v1$par[4]
vetB_betas[5] <- v1$par[5]
vetB_gamma[1] <- v1$par[6]

vetB_epp_betas[1] <- epp[1]
vetB_epp_betas[2] <- epp[2]
vetB_epp_betas[3] <- epp[3]
vetB_epp_betas[4] <- epp[4]
vetB_epp_betas[5] <- epp[5]
vetB_gamma[2] <- epp[6]

vetB_logv[1] <- (-1)*v1$value

vetB_estw <- c(0,vetB_betas[2]/vetB_epp_betas[2],vetB_betas[3]/vetB_epp_betas[3], vetB_betas[4]/vetB_epp_betas[4], vetB_betas[5]/vetB_epp_betas[5]) 
vetB_pvalorw <- c(0,2*(1-pnorm(abs(vetB_estw[2]))), 2*(1-pnorm(abs(vetB_estw[3]))), 2*(1-pnorm(abs(vetB_estw[4]))),
                  2*(1-pnorm(abs(vetB_estw[5]))))
vetB_gamma <- round(vetB_gamma,3)


x <- data.frame(round(vetB_betas,3),round(vetB_epp_betas,3),round(vetB_estw,3),round(vetB_pvalorw,3))
x <- rbind(x,vetB_gamma)
colnames(x) <- c("Coeficientes Estimados", "Erro Padrão", "Estatística","P-Valor")
rownames(x) <- c("Beta0", "Beta1", "Beta2", "Beta3", "Beta4", "Gamma")
x[1,3] <- " "
x[1,4] <- " "
x[6,3] <- " "
x[6,4] <- " "
kable(x,align = 'c')

# Without B0

vetB_gamma <- c(0,0,0,0)
vetB_betas <- c()

vetB_epp_betas <- c()

vetB_logv <- c()

# sem pop e sem idc
vero_back_pl <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  beta2 <- para[3]
  beta3 <- para[4]
  gama <- para[5]
  alfa <- exp(beta0+beta1*banco_modelo$`Dif_Dem-Rep (%)`
              +beta2*banco_modelo$`Ideologia do Governo`+beta3*banco_modelo$`Profissionalismo Legislativo`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(3.5,0,0,0,0.5),vero_back_pl,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)


vetB_betas[1] <- v1$par[1]
vetB_betas[2] <- v1$par[2]
vetB_betas[3] <- v1$par[3]
vetB_betas[4] <- v1$par[4]
vetB_gamma[1] <- v1$par[5]

vetB_epp_betas[1] <- epp[1]
vetB_epp_betas[2] <- epp[2]
vetB_epp_betas[3] <- epp[3]
vetB_epp_betas[4] <- epp[4]
vetB_gamma[2] <- epp[5]

vetB_logv[1] <- (-1)*v1$value


vetB_estw <- c(0,vetB_betas[2]/vetB_epp_betas[2],vetB_betas[3]/vetB_epp_betas[3], vetB_betas[4]/vetB_epp_betas[4]) 
vetB_pvalorw <- c(0,2*(1-pnorm(abs(vetB_estw[2]))), 2*(1-pnorm(abs(vetB_estw[3]))), 2*(1-pnorm(abs(vetB_estw[4]))))
vetB_gamma <- round(vetB_gamma,3)


x <- data.frame(round(vetB_betas,3),round(vetB_epp_betas,3),round(vetB_estw,3),round(vetB_pvalorw,3))
x <- rbind(x,vetB_gamma)
colnames(x) <- c("Coeficientes Estimados", "Erro Padrão", "Estatística","P-Valor")
rownames(x) <- c("Beta0", "Beta1", "Beta2", "Beta3", "Gamma")
x[1,3] <- " "
x[1,4] <- " "
x[5,3] <- " "
x[5,4] <- " "
kable(x,align = 'c')


# Without B1

vetB_gamma <- c(0,0,0,0)
vetB_betas <- c()

vetB_epp_betas <- c()

vetB_logv <- c()

# sem pop e sem idc
vero_back_pl <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  beta2 <- para[3]
  gama <- para[4]
  alfa <- exp(beta0+beta1*banco_modelo$`Dif_Dem-Rep (%)`
              +beta2*banco_modelo$`Profissionalismo Legislativo`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(2.5,0,0.1,0.5),vero_back_pl,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)


vetB_betas[1] <- v1$par[1]
vetB_betas[2] <- v1$par[2]
vetB_betas[3] <- v1$par[3]
vetB_gamma[1] <- v1$par[4]

vetB_epp_betas[1] <- epp[1]
vetB_epp_betas[2] <- epp[2]
vetB_epp_betas[3] <- epp[3]
vetB_gamma[2] <- epp[4]

vetB_logv[1] <- (-1)*v1$value


vetB_estw <- c(0,vetB_betas[2]/vetB_epp_betas[2],vetB_betas[3]/vetB_epp_betas[3]) 
vetB_pvalorw <- c(0,2*(1-pnorm(abs(vetB_estw[2]))), 2*(1-pnorm(abs(vetB_estw[3]))))
vetB_gamma <- round(vetB_gamma,3)


x <- data.frame(round(vetB_betas,3),round(vetB_epp_betas,3),round(vetB_estw,3),round(vetB_pvalorw,3))
x <- rbind(x,vetB_gamma)
colnames(x) <- c("Coeficientes Estimados", "Erro Padrão", "Estatística","P-Valor")
rownames(x) <- c("Beta0", "Beta1", "Beta2", "Gamma")
x[1,3] <- " "
x[1,4] <- " "
x[4,3] <- " "
x[4,4] <- " "
kable(x,align = 'c')


# Without B2
vetB_gamma <- c(0,0,0,0)
vetB_betas <- c()

vetB_epp_betas <- c()

vetB_logv <- c()

# sem pop e sem idc
vero_back_pl <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*banco_modelo$`Dif_Dem-Rep (%)`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(2.5,0,0.5),vero_back_pl,NULL,hessian = TRUE)
invR <- solve(v1$hessian)
variancia <- diag(invR)
epp <- sqrt(variancia)


vetB_betas[1] <- v1$par[1]
vetB_betas[2] <- v1$par[2]
vetB_gamma[1] <- v1$par[3]

vetB_epp_betas[1] <- epp[1]
vetB_epp_betas[2] <- epp[2]
vetB_gamma[2] <- epp[3]

vetB_logv[1] <- (-1)*v1$value


vetB_estw <- c(0,vetB_betas[2]/vetB_epp_betas[2]) 
vetB_pvalorw <- c(0,2*(1-pnorm(abs(vetB_estw[2]))))
vetB_gamma <- round(vetB_gamma,3)


x <- data.frame(round(vetB_betas,3),round(vetB_epp_betas,3),round(vetB_estw,3),round(vetB_pvalorw,3))
x <- rbind(x,vetB_gamma)
colnames(x) <- c("Coeficientes Estimados", "Erro Padrão", "Estatística","P-Valor")
rownames(x) <- c("Beta0", "Beta1", "Gamma")
x[1,3] <- " "
x[1,4] <- " "
x[3,3] <- " "
x[3,4] <- " "
kable(x,align = 'c')


### Residual Analysis

# Cox model
fit5 <- coxph(Surv(banco_modelo$`Tempo de Falha`, banco_modelo$`Falha/Censura`) ~  banco_modelo$`Ideologia do Governo`)

# Martingal
martingal=resid(fit5,type="martingale")
par(mfrow = c(1,2))
plot(banco_modelo$`Ideologia do Governo`,martingal, ylab = "Resíduos",
     xlab = "Valor Observado", main = "Martingal vs Id. do Governo")
plot(banco_modelo$`Tempo de Falha`, martingal, xlab = "Tempo",
     ylab = "Resíduos", main = "Resíduo Martingal")


# Parametric Model
vero_idg <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*banco_modelo$`Ideologia do Governo`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(3,0,0.5),vero_idg,NULL,hessian = TRUE)

beta0 <- v1$par[1]
beta1 <- v1$par[2]
gamma <- v1$par[3]
alfa <- exp(beta0+beta1*banco_modelo$`Ideologia do Governo`)

h <- 1 - (1 + (tempo/alfa)^gamma)/(1 + ((tempo+1)/alfa)^gamma) 
H <- cumsum(h)

resid_martingal_par <- banco_modelo$`Falha/Censura` - H
par(mfrow = c(1,2))
plot(banco_modelo$`Ideologia do Governo`, resid_martingal_par, ylab = "Resíduos",
     xlab = "Valor Observado", main = "Martingal vs Id. do Governo")
plot(banco_modelo$`Tempo de Falha`,resid_martingal_par, ylab = "Resíduos",
     xlab = "Tempo", main = "Resíduo Martingal")


# Backwards Final Model

vero_difp <- function(para){
  
  beta0 <- para[1]
  beta1 <- para[2]
  gama <- para[3]
  alfa <- exp(beta0+beta1*banco_modelo$`Dif_Dem-Rep (%)`)
  
  dens <- (1/(1+(tempo/alfa)^gama)) - (1/(1+((tempo+1)/alfa)^gama))
  sobrev <-  (1/(1+((tempo+1)/alfa)^gama))
  
  if ((alfa > 0) && (gama > 0))
    
    return((-1)*sum(censura*log(dens)+
                      (1-censura)*log(sobrev)))
  else return(-Inf)
  
}

v1 <- optim(c(2.5,0,0.5),vero_difp,NULL,hessian = TRUE)

beta0 <- v1$par[1]
beta1 <- v1$par[2]
gamma <- v1$par[3]
alfa <- exp(beta0+beta1*banco_modelo$`Dif_Dem-Rep (%)`)

h <- 1 - (1 + (tempo/alfa)^gamma)/(1 + ((tempo+1)/alfa)^gamma) 
H <- cumsum(h)

resid_martingal_back <- banco_modelo$`Falha/Censura` - H
par(mfrow = c(1,2))
plot(banco_modelo$`Dif_Dem-Rep (%)`, resid_martingal_back, ylab = "Resíduos",
     xlab = "Valor Observado", main = "Martingal vs Dif. Percentual")
plot(banco_modelo$`Tempo de Falha`, resid_martingal_back, ylab = "Resíduos",
     xlab = "Tempo", main = "Resíduo Martingal")


## Deviance residual

# Cox
par(mfrow = c(1,1))
deviance=resid(fit5,type="deviance")
plot(banco_modelo$`Tempo de Falha`,deviance, ylab = "Resíduos",
     xlab = "Tempo", main = "Resíduo Deviance")

# Parametric
resid_deviance_par <- sign(resid_martingal_par)*sqrt(-2*(resid_martingal_par + banco_modelo$`Falha/Censura`*log(banco_modelo$`Falha/Censura` - resid_martingal_par)))
plot(banco_modelo$`Tempo de Falha`,resid_deviance_par, ylab = "Resíduos",
     xlab = "Tempo", main = "Resíduo Deviance")

# Backward
resid_deviance_back <- sign(resid_martingal_back)*sqrt(-2*(resid_martingal_back + banco_modelo$`Falha/Censura`*log(banco_modelo$`Falha/Censura` - resid_martingal_back)))
plot(banco_modelo$`Tempo de Falha`,resid_deviance_back, ylab = "Resíduos",
     xlab = "Tempo", main = "Resíduo Deviance")


## Proportional risk
# Schoenfeld test 


df <- data.frame( "a" = c("Ideologia do Governo", "Global"),
                  "b" = c(5.47, 5.47),
                  "c" = c(1,1),
                  "d" = c(0.019, 0.019))
colnames(df) <- c("","Estatística", "GL", "P-valor")
knitr::kable(df,align = 'c', caption = "Teste de Schoenfeld")