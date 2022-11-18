# Testes de Hipótese ###############

calc_Z = function(media, mu, sd, n) {
  # Com confiança de 95%
  z = (media - mu) / (sd / sqrt(n))
} 

## Região Crítica = > Valor de Referencia ou < -Valor de Referencia
## Como o valor NÃO está na região crítica
## NÃO rejeita-se o H0 e 
## a média NÃO é diferente de Y´

# H0: hipotese nula; p-valor > 0.05
# H1: hipotese proposta; p-valor < 0.05

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

## Ex3 Teste para proporção: Um criador tem constatado uma proporçao de 10% do 
# rebanho com verminose. O veterinario alterou a dieta dos animais e acredita que a doenca
# diminuiu de intensidade. Um exame de 100 cabecas do rebanho, escolhidas
# ao acaso, indicou 8 delas com verminose. Ao nıvel de 5%, ha indıcios de que
# a proporçao diminuiu?

### Passo 1: identificar as hipoteses: H0=> p=0.1 e H1=> p<0.1
### Passo 2: Adota-se α = 5%, o valor crıtico tabelado é −1.64

prop.test(x=8, n=100, p=0.1, alternative='less')
#### Conclusão: Como o p-valor é maior que 0.05, não há razão para recusar H0,
#### ou seja, com esses dados não pode-se afirmar que a dieta foi de fato eficaz
#### e reduziu a ocorrencia da verminose

###### Duas Populações
### Para Variâncias homogêneas

## Ex4: Um estudo foi realizado para verificar se o nıvel de colesterol
# de adultos fumantes e nao fumantes diferem. Foi coletada uma amostra
# de 16 indivıduos de cada grupo e o nıvel de colesterol foi mensurado.
# Os valores obtidos, em mg/dL, foram os seguintes:

sim = c(215, 190, 282, 186, 184, 231, 240, 230,
        178, 219, 166, 199, 221, 176, 225, 213)
nao = c(221, 171, 165, 234, 224, 205, 256, 239,
        180, 183, 217, 199, 298, 173, 267, 248)

### Passo 1: Identifar as hipoteses: é esperado que exista diferença nas medias
            # H0=> media_sim - media_nao = 0 e H1=> media_sim - media_nao != 0   
### Passo 2: Verificar se as variancias são homogeneas (var_sim / var_nao) < 4

var.test(nao, sim)  ## Verificando q as variancias são homogeneas
shapiro.test(nao)  ## Verificando que grupo segue distribuição normal
shapiro.test(sim)  ## Verificando que grupo segue distribuição normal
t.test(sim, nao, alternative='two.side', mu=0, var.equal=T)

## Obs: analisar no intervalo de confiança se existe o 0, pois ele indica que 
## nao há diferença nos grupos (eles alternam quem é maior e menor)

### Amostras pareadas
## Ex5: Andrade & Ogliari (2007) apresentam um experimento conduzido para
# estudar o conteudo de hemoglobina no sangue de suınos com
# deficiencia de niacina. Aplicaram-se 20 mg de niacina em oito suınos.
# Os nıveis de hemoglobina no sangue foram mensurados antes e depois
# da aplicaçao da niacina. Os resultados obtidos no experimento foram. 
# Realize o teste de hipotese para 95% de NC:
antes = c(12.4, 13.6, 13.6, 14.7, 12.3, 12.2, 13.0, 11.4)
depois = c(10.4, 11.4, 12.5, 14.6, 13.0, 11.7, 10.3, 9.8)

shapiro.test(diff(antes - depois))  
## se p-valor for menor que 0.05, não há normalidade e portanto não há necessidade
## de realizar testes de hipotese para esse grupo

t.test(depois, antes, alternative='two.side', mu=0, paired=T)
## Conclusão: como p-valor foi menor que 0.05, rejeita-se H0 e pode-se afirmar
## que existe a diferença nos níveis de hemoglobina.

### Para diferença entre Proporções

## Ex6: Um metodo de semeadura de nuvens (cloreto de sodio), usando aviao foi
# bem sucedido em 57 dentre 150 tentativas, enquanto o metodo usando
# foguetes carregados de sal foi eficaz em 33 dentre 100 tentativas.
# Ao nıvel de significancia de 5% pode-se concluir que o primeiro metodo é
# melhor que o segundo? (Z = 1, 64)

metodo = c(57, 33)
tentativas = c(150, 100)
prop.test(metodo, tentativas, correct=F, alternative='greater')
## Conclusão: não vou rejeitar a H0 pois p-valor > 0.05, ou seja, não há evidencias
## para afirmar que o metodo 1 foi melhor que o metodo 2