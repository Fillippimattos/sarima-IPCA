###### Modelo sSARIMA inflação (IPCA)

# Import packages																									
{ 																									
  # Check if the packages that we need are installed																									
  want = c("fpp3","tidyverse", "forecast", "gridExtra", "xtable", "scales", "tsutils", "tsibble", "feasts","caret","devtools")																									
  have = want %in% rownames(installed.packages())																									
  # Install the packages that we miss																									
  if ( any(!have) ) { install.packages( want[!have] ) }																									
  # Load the packages																									
  junk <- lapply(want, library, character.only = T)																									
  # Remove the objects we created																									
  rm(have, want, junk)																									
  
  
}

# CLETA DOS DADOS
{
devtools::install_github("wilsonfreitas/rbcb")
library(rbcb)
#CRIAÇÃO DA VARIAVEL DE INTERESSE
ipca = get_series(433,start_date = '2007-01-01', end_date= '2020-12-01')
}

# APRESENTAÇÃO INICIAL DOS DADOS
{
  
  
  # grafico do ipca, onde a linha azul representa a média mensal
  ipca = 
    ipca %>%
    mutate(date = yearmonth(date)) %>%
    rename(vmensal='433') %>%
    as_tsibble(index = date)
  
  ipca %>%
    gg_subseries(vmensal)
  
}

# ANALISE EXPLORATORIA DE DADOS 
{
a1 =ggAcf(ipca$vmensal)
a2 = ggPacf(ipca$vmensal)
grid.arrange(a1,a2)
}

#MODELO SARIMA (1,0,0)(0,0,1)
{
inflacao_mensal = ts(ipca$vmensal, start = c(2007,1), freq=12)
sarima = Arima(inflacao_mensal, order = c(1,0,0),
               seasonal = c(0,0,1))

auto = auto.arima(inflacao_mensal)

ipca %>%
  mutate(sarima = fitted(sarima)) %>%
  gather(variavel,valor,-date)%>%
  ggplot(aes(x=date,y=valor,colour= variavel))+
  geom_line(size=1)+
  theme(legend.title = element_blank(),
        legend.position = 'top')


}
    
# PREVISÃO
{
autoplot(resid(sarima))
  
intrain = createDataPartition(inflacao_mensal,p=0.7, list = FALSE)
set.seed(2017)
treino = inflacao_mensal[intrain]
teste = inflacao_mensal[-intrain]

sarima_treino = arima(treino,order = c(1,0,0), seasonal = c(0,0,1))
sarima_teste = forecast(sarima_treino, h = length(teste))

diagnostico = forecast:: accuracy(sarima_teste,teste)
diagnostico


#### forecast

fsarima = forecast(sarima, h=12)
autoplot(fsarima)
fsarima

}
######### comparando o valor estimado com o valor real da inflação registrada em 2021
{
ipca21 = get_series(433,start_date = '2021-01-01', end_date= '2021-05-01')
ipca21 = 
  ipca21 %>%
  mutate(date = yearmonth(date)) %>%
  rename(vmensal='433') %>%
  as_tsibble(index = date)


df_teste = data.frame(ipca21)
df_teste2 = data.frame(fsarima)

df_teste$fsarima = df_teste2$Point.Forecast

df_teste$min = df_teste2$Lo.95
df_teste$max = df_teste2$Hi.95

 ggplot() + 
  geom_line(data = df_teste, aes(x = date, y =fsarima ), color = "blue") +
  geom_point(data = df_teste, aes(x = date, y =vmensal ), color = "red",size = 5 ) +
  geom_ribbon(data = df_teste, aes(x = date,,ymin=min,ymax=max),color="blue", alpha = 0.5) +
  xlab('Dates') +
  ylab('inflação')


}

