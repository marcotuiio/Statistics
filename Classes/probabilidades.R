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


## Distribução Binomial  Tem limite
# (N | k) * p^k * (1-p)^N-k

dbinom(2, 6, 0.05)  # Dois com problema

1 - dbinom(0, 6, 0.05)  # Ao menos 1 com problema, exclui a probab. de nenhum ter problema

pbinom(3, 6, 0.05, lower.tail = F)  # No min 4 apresentam problema (conta 4, 5, 6)

dbinom(5, 6, 0.95)  # 5 sem defeitos = 1 - 1 com problema

## Distribuição Poisson  Taxa de ocorrencia (minimo 0, maximo infinito)
# P(Y=y) = e^-lambda * lambda^y / y!
# Esperança == TAXA, que também é a maioria dos casos 
# Utilizar o complementar ex: 1 ou mais = 1 - (PY=0) 

dpois(1, 5)
