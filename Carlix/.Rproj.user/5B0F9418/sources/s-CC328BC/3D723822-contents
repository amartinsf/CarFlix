


rm(list = ls())




# PACOTES
#######
# install.packages("hrbrthemes") 
library("hrbrthemes")
library("plotly")
library("viridis")
#library("Graphviz")
library("dplyr")
library("stringr")
library("ggplot2")
library("xlsx")
library("factoextra")
library("cluster")
library("fpc")
library("NbClust")
library("clValid")
library("clustertend")
library("htmlwidgets")
library("tidyverse")
library("Hmisc")   #faz describe
library("summarytools") # dfSummary 
library("flexdashboard") 
library("readxl")
library("magrittr")


format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    str_trim() %>%
    str_c("R$ ", .)
}



####### 



#setwd("C:\Users\amart\Documents\R\projetos\Carlix")


getwd()

DF <- read_excel("Base.xlsx", sheet = "Anuncios mercado")



###TRATATIVA DE DADOS###
DF$ID<-0
DF$COUNT <-1

for(i in 1:nrow(DF)){
  
  DF$ID[i] = i
  
  print(i)
}


format_real(DF$Preco)

DF2 <- filter(DF, AnoFabricacao >= 2010)

######## ANÁLISE EXPLORATÓRIA #########

#TODO O PERÍODO

g <-ggplot(DF, aes(x=AnoFabricacao, y=Preco , color = as.factor(Cambio))) + 
  geom_point(size = 1)+
  #theme_ipsum_ps(axis.text.y = element_text(size=10, angle=45))+
  scale_color_viridis(discrete = TRUE)+    scale_y_continuous(limits = quantile(DF$Preco, c(00, 0.95))) +geom_smooth(method='lm', formula= y~x)
#geom_vline(xintercept = x, show.legend=TRUE)+
#geom_hline(yintercept = y, show.legend=TRUE)

g

#Podemos observar o comportamento de veículos semi automáticos, com tendencia de aumento acima dos demais

#POS 2010

f <-ggplot(DF2, aes(x=AnoFabricacao, y=Preco , color = as.factor(Cambio))) + 
  geom_point(size = 1)+
  #theme_ipsum_ps(axis.text.y = element_text(size=10, angle=45))+
  scale_color_viridis(discrete = TRUE)+    scale_y_continuous(limits = quantile(DF2$Preco, c(00, 0.95))) +geom_smooth(method='lm', formula= y~x)
#geom_vline(xintercept = x, show.legend=TRUE)+
#geom_hline(yintercept = y, show.legend=TRUE)

f
#O mesmo se repete quando olhamos somente dados mais recentes



#INVESTIGAÇÃO DE TENDENCIA DE PREÇO POR TIPO DE CAMBIO COM ANÁLISE DE REGRESSÃO 
##PLOTS

DF3 <- filter(DF2, Cambio == "Manual"	)

DF4 <- filter(DF2, Cambio != "Manual")



h <-ggplot(DF2, aes(x=Quilometragem, y=Preco , color = as.factor(Cambio))) + 
  geom_point(size = 1)+
  #theme_ipsum_ps(axis.text.y = element_text(size=10, angle=45))+
  scale_color_viridis(discrete = TRUE)+  scale_x_continuous(limits = quantile(DF2$Quilometragem, c(00, 0.95))) +
  scale_y_continuous(limits = quantile(DF2$Preco, c(00, 0.95))) +geom_smooth(method='lm', formula= y~x)
#geom_vline(xintercept = x, show.legend=TRUE)+
#geom_hline(yintercept = y, show.legend=TRUE)

h


j <-ggplot(DF2, aes(x=CorPrimaria, y=Preco , color = as.factor(Cambio))) + 
  geom_point(size = 1)+
  #theme_ipsum_ps(axis.text.y = element_text(size=10, angle=45))+
  scale_color_viridis(discrete = TRUE)+  
  scale_y_continuous(limits = quantile(DF2$Preco, c(00, 0.95))) +geom_smooth(method='lm', formula= y~x)
#geom_vline(xintercept = x, show.legend=TRUE)+
#geom_hline(yintercept = y, show.legend=TRUE)

j



k <-ggplot(DF2, aes(x=AnoModelo, y=Preco , color = as.factor(Cambio))) + 
  geom_point(size = 1)+
  #theme_ipsum_ps(axis.text.y = element_text(size=10, angle=45))+
  scale_color_viridis(discrete = TRUE)+    scale_y_continuous(limits = quantile(DF$Preco, c(00, 0.95))) +geom_smooth(method='lm', formula= y~x)
#geom_vline(xintercept = x, show.legend=TRUE)+
#geom_hline(yintercept = y, show.legend=TRUE)

k
