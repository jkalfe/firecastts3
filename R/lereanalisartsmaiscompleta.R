#' Função para criação de série temporal com valores mensais a partir da leitura de arquivo em csv que possua cabeçalho com campos separados por vírgula com ano, mês e valor. Além disso, apresenta resultados de sua análise exploratória mais completa.
#'
#' Esta função serve para criar um série temporal com valores mensais e analisá-la estatisticamente mais completa
#'
#' @param a Valor do ano que inicia a série temporal
#' @param m Valor do mês que inicia a série temporal
#' @examples
#' lereanalisartsmaiscompleta(2001,1)
#'
#' @export
lereanalisartsmaiscompleta <- function(a,m){
  # Instalando pacotes

  install.packages("tidyverse")
  install.packages("glue")
  install.packages("stringr")
  install.packages("forecast")
  install.packages("ggplot2")
  library(forecast)
  library(ggplot2)
  library(tidyverse)
  library(glue)
  library(stringr)


  # Importando uma Serie Temporal no R

  FPENACIONAL=read.csv(file.choose(),sep = ",",header = T) # funcao read.csv le um arquivo csv. O primeiro parametro permite abrir caixa para informar onde esta o arquivo. O segundo parametro informa qual o separador dos campos. O terceiro parametro informa se existe cabecalho na primeira linha do arquivo.
  class(FPENACIONAL) # Verifica a classe do objeto recem criado com o nome FPENACIONAL.
  print(FPENACIONAL) # Visualizando o objeto criado no Console
  FPENACIONAL=ts(FPENACIONAL[3],start = c(a,m),frequency = 12) # Criando uma serie temporal com a funcao ts, usando os parametros de dados, incio, fim e frequencia.

  # Análises exploratórias

  class(FPENACIONAL)  # Verificacao do objeto FPENACIONAL em relacao a sua classe. No caso, ela possui uma classe do tipo serie temporal, chamando "ts".
  print(FPENACIONAL)  # Visualizando o objeto criado no Console
  start(FPENACIONAL) # Quando inicia a ts.
  end(FPENACIONAL) # Confirma que pegou o ?ltimo ano e m?s.
  frequency(FPENACIONAL) # Frequência da ts.
  summary(FPENACIONAL) # Resumo de várias estatísticas referentes a série temporal (ts).
  length(FPENACIONAL) # Quantidade de dados

  # Gráficos

  plot(FPENACIONAL)   # Geracao de grafico do objeto FPENACIONAL
  autoplot(FPENACIONAL) # Geracao de grafico do objeto FPENACIONAL
  hist(FPENACIONAL) # Histograma
  boxplot(FPENACIONAL) # Boxplot
  plot(aggregate(FPENACIONAL, FUN=mean)) # Gráfico com suavização em função da média

  # Previsoes

  # Criacao do primeiro modelo (RL) e sua previsao

  modelo1 = tslm(FPENACIONAL~trend+season,data=FPENACIONAL) # Modelo de Regressao Linear
  prev1 = forecast(modelo1,h=12)  # Previsao de 1 ano, usando o modelo1
  prev1
  autoplot(prev1)
  class(prev1$mean)
  prev1$mean # Projecao da RL
  autoplot(prev1)
  CV(modelo1)

  # Criacao do segundo modelo (RN) e sua previsao

  modelo2 = nnetar(FPENACIONAL) # Rede Neural que nao apresenta intervalo de confianca em sua previsao, diferentemente da regressao linear.
  prev2 = forecast(modelo2,h=12)  # Previsao de 1 anos, usando o modelo2
  prev2$mean # Projecao da RN
  autoplot(prev2)

  # Criacao da terceiro previsao (Stlf)

  #prev3=stlf(FPENACIONAL, h=12)
  #class(prev3)
  #prev3$mean # Projecao da stlf
  #autoplot(prev3)


  # Criacao de modelo Arima e sua previsao

  modeloarima=auto.arima(FPENACIONAL,trace = T,stepwise = F,approximation = F) # Os parametros stepwise e approximation com false permitem reduzir as limitacoes dos parametros utilizados pelo auto-arima na busca pelo melhor modelo, fazendo uma busca maior do que o modelo criado sem esses par?metros como "F".
  prev4=forecast(modeloarima,h=12)
  prev4$mean # Projecao do auto.arima
  autoplot(prev4)
  print(modeloarima)


  # Criacao do terceiro modelo (hw multi) e sua previsao

  #mdl6=hw(FPENACIONAL,seasonal = "multiplicative", h=12) # Seasonal igual a multiplicative significa que a varia??o sazonal varia na s?rie.
  #mdl6$mean # Projecao do hw multiplicative (mdl6)
  #autoplot(mdl6)

  # Criacao do terceiro modelo (hw additive) e sua previsao

  #mdl5=hw(FPENACIONAL,seasonal = "additive",h=12) # Usada a funcao hw (holt winter) para projetar, usando esse metodo com o parametro seasonal com a constante additive, significando que sera aditivo, ou seja, variacao sazonal constante.
  #mdl5$mean # Projecao do hw additive (mdl5)
  #autoplot(mdl5)

  # Criacao do terceiro modelo (ets) e sua previsao

  #mdl8=ets(FPENACIONAL) # A funcao ets cria apenas o modelo e nao gera a previsao.
  #prev7=forecast(mdl8, h=12,levels=c(85,90)) # A funcao forecast gera os valores projetados com h intervalos e niveis de confianc igual a levels.
  #prev7
  #prev7$mean # Projecao ets
  #autoplot(prev7)

  # Grafico com a serie historica da serie temporal e as previsoes

  list(
    print('RL'),
    print(prev1$mean),# Predição RL
    print('RN'),
    print(prev2$mean), # Predição RN
    print('ARIMA'),
    print(prev4$mean) # Predição Auto Arima
  )

  plot(FPENACIONAL)
  lines(prev1$mean,col="blue")
  lines(prev2$mean, col="yellow")
  #lines(prev3$mean, col="orange")
  lines(prev4$mean, col="grey")
  #lines(mdl6$mean, col="purple")
  #lines(mdl5$mean, col="red")
  #lines(prev7$mean, col="brown")
  legend("topleft",legend = c("Reg","RN","Arima"),col=c("blue","yellow","grey"),lty=1:2,cex=0.8)

  # Métricas de medição de projeção

  treino=window(FPENACIONAL,start=c(2001,1),end=c(2020,12)) #Subconjunto de treino
  teste=window(FPENACIONAL,start=c(2001,1)) #Subconjunto de teste

  modelo1=tslm(treino~season+trend,data=treino) # Modelo de regressão Linear
  prev1=forecast(modelo1,h=21) #Projeção com modelo de RL
  autoplot(prev1)

  modelo2=nnetar(treino) # Modelo de RN
  prev2=forecast(modelo2,h=21) # Projeção de RN
  autoplot(prev2)

  modeloarima=auto.arima(treino,trace = T,stepwise = F,approximation = F) # Os parametros stepwise e approximation com false permitem reduzir as limitacoes dos parametros utilizados pelo auto-arima na busca pelo melhor modelo, fazendo uma busca maior do que o modelo criado sem esses par?metros como "F".
  prev3=forecast(modeloarima,h=21) # Projeção de auto.arima
  prev3$mean # Projecao do auto.arima
  autoplot(prev4)
  print(modeloarima)

  plot(FPENACIONAL)
  lines(prev1$mean,col="blue")
  lines(prev2$mean,col="red")
  lines(prev3$mean,col="orange")
  lines(teste,col="green")
  legend("topleft",legend = c("RL","RN","Auto Arima","Teste"),col=c("blue","red","orange","green"),lty = 1:2,cex =0.8)

  list(
    accuracy(prev1,teste), # Comparação da previsão com os dados de teste
    accuracy(prev2,teste), # Comparação da previsão com os dados de teste
    accuracy(prev3,teste) # Comparação da previsão com os dados de teste
  )
  # Segue abaixo os índices de comparação de projeção

  # ME = Mean (média) Erro entre o teste e a projeção
  # MEA = Mean Absolut Erro = A média da diferença absoluta entre o realizado e o previsto
  # RMSE = Roat Mean Squared  Error = O desvio padrão da amostra da diferença entre o previsto e o teste
  # MPE = Mean Percentage Error = Diferença percentua de erro
  # MAPE = Mean Absolute Percentage Error  = Diferença Absoluta Percentual de Erro


  # A Regressao Linear e uma tecnica de previsao mais simples do que a Rede Neural.
  # Normalmente tecnicas mais simples de previsao sao melhores.
}
