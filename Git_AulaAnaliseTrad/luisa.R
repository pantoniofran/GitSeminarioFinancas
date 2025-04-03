# Dados extraídos do livro: O Segredo de Luísa
# Rotina construída como apoio a aula de Análise Tradicional de Projeto

#Bibliotecas necessárias

#install.packages('jrvFinance')
#install.packages('dplyr')

library(jrvFinance)
library(dplyr)
#Limpa Ambiente de variáveis
rm(list = ls())

#Dados de entrada
#----------------------------------------------------
#Investimento inicial
camara <- 2000
tacho <- 5730
dosadora <- 7500
tabuleiros <- 525
mesaOper <- 3300
bal_chao <- 450
bal_mesa <- 270
mesaEscrit <- 240
telefax <- 260
cadeiras <- 90
poltrona <- 110
mesaCentro <- 100
veiculo <- 7500
preoper <- 1170
capitalgiro <- 11602.08

numAnos <- 5
taxaCrecAnual <- 0.05
fatorCresc <- matrix(0,1,numAnos)
fatorCresc[1,1] <- 1

for (i in 2:numAnos){
  fatorCresc[1,i] <- fatorCresc[1,i-1] * (1+taxaCrecAnual)
}

#Mao de obra direta
op1 <- 6720
op2 <- 3360
op3 <- 3360
op4 <- 1680

encargos <- 0.862

#Mao de obra indireta
estagiario <- 3600
contador <- 1440
honorarDire <- 4800
encargHonor <- 0.25

#Custos fixos parciais
agluztel <- 1800
alugProd <- 7200
matLimp <- 840
manutencao <-  2323.08
seguros <- 961.92

outrosCustoFixoPerc <- 0.03

#Custos variaveis unitarios
goiabada <- 0.140
xarope <- 0.105
acucar <- 0.13125
celofane <- 0.065625
fretes <- 0.017675
embalagens <- 0.075


depreciacao <- 0.190026714
ipi <- 0.08
pis <- 0.0065
cofins <- 0.03
comissaoVendas <- 0.1
aliqIR <- 0.15

valFinancia <- 0
taxaFinanc <- 0.12
parcAmortiza <- rep(valFinancia/numAnos, numAnos)
parcJuros <- matrix(0, 1, numAnos) 
sdoDev <- valFinancia
for (i in 1:numAnos){
  parcJuros[1,i] <- sdoDev * taxaFinanc
  sdoDev <- sdoDev - parcAmortiza[i]
}


lotesAno <- 192000
precoLote <- 1.5
receitas <- lotesAno*precoLote
taxaDesc <- 0.18



#-----------Fim da entrada de dados

#Construção do Fluxo de caixa livre
#----------------------------------------------------------------
itensDeprec <- camara+tacho+dosadora+tabuleiros+mesaOper+bal_chao+bal_mesa+
  mesaEscrit+telefax+cadeiras+poltrona+mesaCentro+veiculo
depreciaPercAcum <- min(depreciacao*numAnos,1)

valorDepreciacao <- itensDeprec * depreciacao

valResidual <- itensDeprec * (1-depreciaPercAcum)

invInicial <- itensDeprec + preoper + capitalgiro

itensDRE <- c("Receita bruta de vendas", "Deduções", "Receita líquida de vendas", 
              "Custo dos produtos vendidos", "Lucro bruto", "Despesas operacionais",
              "Despesas administrativas", "Despesas gerais", "Depreciação", "Resultado operacional",
              "Despesas financeiras", "Resultado antes do IR", "IR", "LL")
nomeCol <- c("item", "Ano 1", "Ano 2", "Ano 3", "Ano 4", "Ano 5")

maoObraDireta <- (op1 + op2 + op3 + op4) * (1+encargos)
maoObraIndireta <- estagiario + contador + honorarDire * (1+encargHonor) 
custosVariaveisParcial <- (goiabada+xarope+acucar+celofane+fretes+embalagens) * lotesAno
custosVariaveis <- custosVariaveisParcial + maoObraDireta

custosFixos <- (agluztel + alugProd + matLimp + manutencao + 
                  seguros + maoObraIndireta) * fatorCresc + valorDepreciacao
outrosCustoFixo = custosFixos*outrosCustoFixoPerc

custosFixos <- custosFixos +outrosCustoFixo

#DRE
dreReceitas <- receitas * fatorCresc 
dreDeducoes <- (ipi+pis+cofins+comissaoVendas)*dreReceitas
dreReceitaLiq <- dreReceitas - dreDeducoes 
dreCustosProdutos <- (maoObraDireta+custosVariaveisParcial+alugProd)* fatorCresc 
dreLucroBruto <- dreReceitaLiq - dreCustosProdutos
dreDespAdm <- maoObraIndireta * fatorCresc
dreDespGerais <- (agluztel+matLimp+manutencao+seguros)* fatorCresc + outrosCustoFixo

dreDepreciacao <- valorDepreciacao * c(rep(1,numAnos))
dreDespOper <- dreDespAdm+dreDespGerais+dreDepreciacao
dreLajir <- dreLucroBruto - dreDespOper
dreDespFinanc <- parcJuros
dreLair <- dreLajir - dreDespFinanc
dreIR <- dreLair * aliqIR
dreLL <- dreLair - dreIR

dfDRE <- rbind(dreReceitas,dreDeducoes,dreReceitaLiq,dreCustosProdutos,dreLucroBruto)
dfDRE <- rbind(dfDRE,dreDespOper, dreDespAdm, dreDespGerais, dreDepreciacao)
dfDRE <- rbind(dfDRE, dreLajir, dreDespFinanc, dreLair, dreIR, dreLL) 
dfDRE <- round(dfDRE,2)
dfDRE <- data.frame(cbind(itensDRE,dfDRE))
colnames(dfDRE) <- nomeCol
rownames(dfDRE) <- NULL


#Análise 
#Fluxo de caixa livre
ncoldf <- numAnos+1
fluxLivre <- as.numeric(dfDRE[14,2:ncoldf])+dreDepreciacao - parcAmortiza
fluxLivre[numAnos] <- fluxLivre[numAnos] + valResidual

#--------------------------------------------------------
#Cálculo do Valor presente líquido (VPL)
vp<-0
for (i in 1:numAnos){
  vp <- vp + fluxLivre[i]/(1+taxaDesc)^i
}
projvpl <- vp-(invInicial-valFinancia)
print(projvpl)

#Cálculo da Taxa interna de retorno (TIR)
fc0 <- invInicial-valFinancia
project_cf <- data.frame(Year=0:numAnos, cf=c(-fc0,fluxLivre))

irr1 <- project_cf %>%
  select(cf) %>%
  .[[1]] %>%
  irr()
print(irr1*100)