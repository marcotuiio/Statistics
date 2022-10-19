# Probabilidade 

# Espaço Amostral: TODAS as possibilidades possiveis
# Evento: subconjunto dos espaço amostral

## Conceito Clássico 
# Probabilidade de ocorrer A = p(A) / p(TOTAL), todos devem ter as mesma probab

## Propriedades: 
    # Se A e B não são disjuntos: P(A∪B) = P(A) + P(B) − P(A∩B)  
    # Se A e B são disjuntos: P(A∪B) = P(A) + P(B)

## Probabilidade Condicional: A probabilidade A, sabendo que B ocorreu,
    # A ocorrência de um altera a probabilidade do outro
    # P(A|B) = P(A∩B) / P(B)

## Independência de Eventos: um não depende do outro
    # P(A∩B) = P(A) * P(B)

## Teorema de Bayes
    # P(B) = SOMATORIO( P(B|Ai)P(Ai) ) exemplo das bolas brancas e azuis em 3 urnas
    # P(Ai|B) = P(B|Ai)P(Ai) / SOMATORIO ( P(B|Ai)P(Ai) )  # exemplo das fazendas e leite adulterado

# ---------- Distribuições Discretas ---------- # 

## Distriubuição de variável aleatória (exemplo das ervilhas)
    # Esperança SOMATÓRIO (yi * P(yi))
    # Variância SOMATÓRIO (yi^2 * P(yi))

## Bernoulli: Apenas sucesso e fracasso no espaço amostral
    # Esperança E(Y) = p
    # Variância V (Y) = p(1 − p)
    # Função de probabilidade P(Y = y) = p^y * (1 − p)^1−y  

## Distribução Binomial  Tem limite
    # E(Y) = np
    # V (Y) = np(1 − p)
    # Função de probabilidade P(Y = y) =(Combinação N y a y) * p^y * (1 − p)^n−y

combinatoria = function(n, p) {  # Combinação N p a p
  factorial(n) / (factorial(p) * factorial(n - p))
}

### Exemplo: Uma fazenda sabe que 5% das informa ̧c ̃oes sobre seus animais tem algum
### problema na coleta. Se ela selecionar seis animais ao acaso, determine a
### probabilidade de:

dbinom(2, 6, 0.05)  # Dois com problema
1 - dbinom(0, 6, 0.05)  # Ao menos 1 com problema, exclui a probab. de nenhum ter problema
pbinom(3, 6, 0.05, lower.tail = F)  # No min 4 apresentam problema (conta 4, 5, 6), remove anteriores ao 4
dbinom(5, 6, 0.05)  # 5 sem defeitos = 1 - 1 com problema

## Distribuição Poisson  Taxa de ocorrencia (minimo 0, maximo infinito)
    # P(Y=y) = e^-lambda * lambda^y / y!
    # Esperança == TAXA, que também é a maioria dos casos 
    # Utilizar o complementar ex: 1 ou mais = 1 - (PY=0) 
    # Função de probabilidade P(Y = y) = e^−λ * λ^y / y!,
    # E(Y) = V (Y) = λ

### Em um estudo sobre um organismo aquatico, algumas amostras foram cole-
### tadas de um tanque, e o numero de organismos em cada amostra foi contado.
### A media de organismos por amostra foi de dois. Qual a probabilidade de que:

dpois(3, 2)  # a proxima amostra coletada contera exatamente tres organismos;
ppois(0, 2, lower.tail = F)  # a proxima amostra coletada contera um ou mais organismos, conta a partir do 0

### Exercicio 1 # Binomial
### A probabilidade de um presumıvel cliente, escolhido aleatoriamente, faça uma
### compra, é de 30%. Se o vendedor visita cinco clientes, qual a probabilidade
### que ele realizara:
### a) Exatamente tres vendas?
dbinom(3, 5, 0.3)
### b) Quatro ou mais vendas?
pbinom(3, 5, 0.3, lower.tail = F)
### c) Menos de duas vendas?
dbinom(0, 5, 0.3) + dbinom(1, 5, 0.3)

### Exercicio 2 # Poisson
### Um contador eletronico de bacterias registra, em media, 5 bacterias por cm3
### de um lıquido. Admitindo-se que esta variavel tenha distribuicao de Poisson:
### a) Encontre a probabilidade de que pelo menos duas bacterias ocorram
### num volume de lıquido de 1cm3.
ppois(1, 5, lower.tail = F)  # tira anteriores ao 2
### b) qual e o desvio padrao do numero de bacterias por cm3?
sqrt(5)

## Distriubuição Geométrica Até o 1º sucesso
    # E(Y) = (1-p) / p
    # V(Y) = (1-p) / p^2
    # Função de probabilidade P(Y = y) = (1 − p)^y × p
      # em que Y é o número de falhass até o primeiro sucesso

## Distribuição Hipergeométrica
    # contagem de objetos de certo tipo, retirados ao acaso e sem reposicao, de 
    # um conjunto contendo dois tipos de objetos.
    # E(Y ) = m × r / n
    # Função de probabilidade P(Y = k) = ((m | k) * (n-m | r-k)) / (n | k)

### Exemplo: Em uma sala ha 40 alunos, dos quais 32 sao mulheres. Serao
### selecionados 5 alunos para um estagio. Qual a probabilidade de que 4 sejam
### homens? 
# n = 40; n-m = 32, m = 8, k = 4, r = 5
(combinatoria(8, 4) * combinatoria(32, 1)) / combinatoria(40, 5)
