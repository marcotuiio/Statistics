# Testes de Hipótese ###############

calc_Z = function(media, mu, sd, n) {
  # Com confiança de 95%
  z = (media - mu) / (sd / sqrt(n))
} 

## Região Crítica = > Valor de Referencia ou < -Valor de Referencia
## Como o valor NÃO está na região crítica
## NÃO rejeita-se o H0 e 
## a média NÃO é diferente de Y´

## Em que P-valor é a probabilidade de errar:
## P-valor maior que 0.05 não rejeita-se 
## P-valor menor que 0.05 rejeita-se

## Ex1 Variancia Conhecida: Admite-se que o tempo médio de reaçao de seres vivos a um certo tipo de
# estımulo segue, em geral, o modelo normal com μ = 8s e desvio-padrao σ = 2s.
# O pesquisador desconfia, entretanto, que o tempo medio sofre alteracao por
# influencia de uma certa substancia.
# Assim, para verificar se existe efeito dessa substancia no tempo de reacao,
# um experimento foi desenvolvido com cobaias, inoculadas com a substancia e
# submetidas a um estımulo eletrico, com seus tempos de reacao (em segundos)
# anotados.
# Os seguintes valores foram obtidos:
data = c(9.1, 9.3, 7.2, 7.5, 13.3, 10.9, 7.2, 9.9, 8.0, 8.6)
# Verifique se a desconfiança do pesquisador procede, ao nıvel de 5% de significancia.

### Passo 1: identificar as hipoteses: H0=> mu=8s e H1=> mu!=8s
### Passo 2: calcular Z visto que media e variancia sao conhecidas
### Passo 3: Como os valores criticos estabelecidos são de 5%, usasa-se -1.96 e +1.96
z = calc_Z(mean(data), 8, 2, length(data)); z
#### Conclusão: o valor obtido de z=1.74 está no intervalo de confiança de -1.96 até 1.96, 
#### logo NO há razão para rejeitar H0.

## Ex2 Variancia Desconhecida: Um estudo foi realizado para verificar o nıvel de
# colesterol de adultos fumantes. E desejavel que o nıvel de colesterol esteja 
# abaixo de 200 mg/dL. Foi coletada uma amostra de 16 indivıduos adultos 
# fumantes e o nıvel de colesterol foi mensurado. Os valores obtidos, em mg/dL:
colest = c(215, 190, 282, 186, 184, 231, 240, 230,
           178, 219, 166, 199, 221, 176, 225, 213)

### Passo 1: identificar as hipoteses: H0=> u=200mg/dL e H1=> u>200mg/dL
### Passo 2: Calcular a estatistica T = (Media - mu) / (s / sqrt(n))
### Passo 3: Adota-se α = 5% com 15 gl, e o valor crıtico tabelado é 1.753, portanto 
### maior que tal é a região critica
t = calc_Z(mean(colest), 200, sd(colest), 16); t
#### Conclusão: não há razão para rejeitar H0. Ou seja, o nivel de colesterol não
#### é maior que 200 mg/dL,

t.test(colest, mu=200, alternative='greater')
shapiro.test(colest)  # Garantia para distribuição normal
