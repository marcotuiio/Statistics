# 1) Esperança
1000 - (30000 * 0.03)

# 2) Calcular média de divisores do numero sorteado.
# 1 = 1; 2 = 1, 2; 3 = 1, 3; 4 = 1, 2, 4; 5 = 1, 5
# 6 = 1, 2, 3, 6; 7 = 1, 7; 8 = 1, 2, 4, 8; 9 = 1, 3, 9
# 10 = 1, 2, 5, 10
length(c(1, 1, 2, 1, 3, 1, 2, 3, 1, 2, 3, 4, 1, 5, 1, 2, 3, 6, 1, 7,
  1, 2, 4, 8, 1, 3, 9, 1, 2, 5, 10)); 
10 * (10/31)

# 3) Aposta dos dados
one = 3 * (1/6 * 5/6 * 5/6) # Apenas um dos dados na face 1
two = 3 * (1/6 * 1/6 * 5/6) # Dois dados na face 1
three = (1/6 * 1/6 * 1/6) # Todos os dados na face 1
# Calcular os ganhos dado o valor e a probabilidade de cada resultado
ganhos = 20 - (one * 20 + two * 50 + three * 80); ganhos

# 4) probabilidades de que haja 1, 2, 3, 4 ou 5 = 0.05, 0.20, 0.40, 0.25 e 0.10
# a) Número médio de pessoas por carro
por_carro = 1 * 0.05 + 2 * 0.2 + 3 * 0.4 + 4 * 0.25 + 5 * 0.1
# b) Em 4000 carros/h, numero esperado em 10h
total = por_carro * 4000 * 10; total

# 5) Probabilidade das bolas de 1 a 6 e esperança de lucro
perder = -600 * (4/6 + 1/6 * 4/5)
ganhar_direto = 900 * 1/6 
ganhar_elim = 3000 * (1/6 * 1/5)
lucro = perder + ganhar_direto + ganhar_elim; lucro

# 6) 
y = dbinom(c(5, 10, 15, 20, 30, 50), 100, 0.2); y
plot(y)

combinatoria = function(n, y) {  # Combinação N p a p
  factorial(n) / (factorial(y) * factorial(n - y))
}

# 7) Y ∼ Bin(10, 2/3)
n = 10; p = 2/3; comp_p = 1/3
# a) P(Y=3);
combinatoria(n, 3) * p^3 * comp_p^7
# b) P(Y ≤ 2); P(y=0) + P(y=1) + P(y=2)
combinatoria(n, 2) * p^2 * comp_p^8 + 
combinatoria(n, 1) * p^1 * comp_p^9 + 
combinatoria(n, 0) * p^0 * comp_p^10
# c) P(Y ≥ 4); 1 - (P(y=0) + P(y=1) + P(y=2) + P(y=3))
1 - (combinatoria(n, 3) * p^3 * comp_p^7 +
combinatoria(n, 2) * p^2 * comp_p^8 + 
combinatoria(n, 1) * p^1 * comp_p^9 + 
combinatoria(n, 0) * p^0 * comp_p^10)
# d) P(3 < Y < 5);
combinatoria(n, 4) * p^4 * comp_p^6
# e) Esperança e Variancia
n*p # E(Y) = np
n*p*comp_p # V(Y) = np(1 − p) 

# 8) Uma moeda e lancada 20 vezes. Qual a probabilidade de 8 caras?
# Y ∼ Bin(20, 1/2)
n = 20; p = 1/2
combinatoria(n, 8) * p^8 * p^12
n*p  # esperança
n*p*(1-p) # variancia

# 9) Em 30 bezerros nascidos, 60% foram fêmeas.
30*0.6  # valor esperado
sqrt(30*0.6*0.4)  # desvio padrão

# 10) 20% dos animais submetidos a um certo tratamento nao sobrevivem. Feito com 20
# animais e Y são os não sobreviventes
# a) Numero medio de sobreviventes
20 - 0.2*20
# b) Variancia e desvio padrao 
var = 20*0.8*0.2 # variancia
sd = sqrt(var); var; sd
# c) P(2 < Y ≤ 4); = P(Y=3) + p(Y=4)
combinatoria(20, 3) * 0.2^3 * 0.8^17 + combinatoria(20, 4) * 0.2^4 * 0.8^16
# d) P(Y ≥ 2); = 1 - (P(Y=0)+P(Y=1))
1 - (combinatoria(20, 0) * 0.2^0 * 0.8^20 + combinatoria(20, 1) * 0.2^1 * 0.8^19)

# 11) 10% das vacinas de um determinado lab tem validade vencida. Retira-se 10
# a) Prob de todas vencidas P(y=10)
combinatoria(10, 10) * 0.1^10 * 0.9^0
# b) no maximo tres com validade vencida? P(Y<=3) = P(Y=0) + P(Y=1) + P(Y=2) + P(Y=3)
(combinatoria(10, 0) * 0.1^0 * 0.9^10 + combinatoria(10, 1) * 0.1^1 * 0.9^9 
+ combinatoria(10, 2) * 0.1^2 * 0.9^8 + combinatoria(10, 3) * 0.1^3 * 0.9^7)
# c) existir vacina com validade vencida? 1 - P(y=0)
1 - (combinatoria(10, 0) * 0.1^0 * 0.9^10)
# d) uma vacina com validade vencida? P(y=1)
combinatoria(10, 1) * 0.1^1 * 0.9^9

# 12) 30% de uma certa populacao sao imunes a alguma doenca. Amostra de 10
# a) ela contenha exatamente 4 pessoas imunes? P(y=4)
combinatoria(10, 4) * 0.3^4 * 0.7^6
# b) ela contenha mais que 4 pessoas imunes? 1 - (P(y=0)+P(y=1)+P(y=2)+P(y=3)+P(y=4))
1 - (combinatoria(10, 0) * 0.3^0 * 0.7^10 + combinatoria(10, 1) * 0.3^1 * 0.7^9
     + combinatoria(10, 2) * 0.3^2 * 0.7^8 + combinatoria(10, 3) * 0.3^3 * 0.7^7
     + combinatoria(10, 4) * 0.3^4 * 0.7^6)

# 13) 16% dos membros sao canhotos. Amostra de 10
# a) a probabilidade de que exatamente dois sejam canhotos; P(y=2)
combinatoria(10, 2) * 0.16^2 * 0.84^8
# b) P(Y ≥ 2); 1 - (P(y=0)+P(y=1))
1 - (combinatoria(10, 0) * 0.16^0 * 0.84^10 + combinatoria(10, 1) * 0.16^1 * 0.84^9)
# c) P(Y < 2); = P(y=0) + P(y=1)
(combinatoria(10, 0) * 0.16^0 * 0.84^10 + combinatoria(10, 1) * 0.16^1 * 0.84^9)
# d) P(1 ≤ Y ≤ 4); = P(y=1) + P(y=2) + P(y=3) + P(y=4)
(combinatoria(10, 1) * 0.16^1 * 0.84^9 + combinatoria(10, 2) * 0.16^2 * 0.84^8
+ combinatoria(10, 3) * 0.16^3 * 0.84^7 + combinatoria(10, 4) * 0.16^4 * 0.84^6)

# 14) probabilidade de recuperação para uma certa doença seja conhecida e igual a 0,4. Se 15
# contraem a doença
# a) tres ou mais se recuperem? P(Y >= 3) = 1 - (P(y=0)+P(y=1)+P(y=2))
1 - (combinatoria(15, 0) * 0.4^0 * 0.6^15 + combinatoria(15, 1) * 0.4^1 * 0.6^14
     + combinatoria(15, 2) * 0.4^2 * 0.6^13)
# b) quatro ou mais? P(Y >= 4) = 1 - (P(y=0)+P(y=1)+P(y=2)+P(y=3))
1 - (combinatoria(15, 0) * 0.4^0 * 0.6^15 + combinatoria(15, 1) * 0.4^1 * 0.6^14
     + combinatoria(15, 2) * 0.4^2 * 0.6^13 + combinatoria(15, 3) * 0.4^3 * 0.6^12)
# c) cinco ou mais? P(Y >= 4) = 1 - (P(y=0)+P(y=1)+P(y=2)+P(y=3)+P(y=4))
1 - (combinatoria(15, 0) * 0.4^0 * 0.6^15 + combinatoria(15, 1) * 0.4^1 * 0.6^14
     + combinatoria(15, 2) * 0.4^2 * 0.6^13 + combinatoria(15, 3) * 0.4^3 * 0.6^12
     + combinatoria(15, 4) * 0.4^4 * 0.6^11)
# d) menos que tres? P(y=0)+P(y=1)+P(y=2)
(combinatoria(15, 0) * 0.4^0 * 0.6^15 + combinatoria(15, 1) * 0.4^1 * 0.6^14
  + combinatoria(15, 2) * 0.4^2 * 0.6^13)

# 15) nascer uma crianca do sexo feminino é de 47%, qual a probabilidade
# de que uma famılia de 6 filhos seja constituıda por seis criancas do sexo feminino?
combinatoria(6, 6) * 0.47^6 * 0.53^0 # binomial n=6, p=0.47

# 16) a taxa de mortalidade em clınicas veterinarias é de 12%. 8 amostras
# a) a probabilidade de nao haver obito na amostra; P(y=0)
combinatoria(8, 0) * 0.12^0 * 0.88^8
# b) a probabilidade de haver exatamente 2 obitos na amostra; P(y=2)
combinatoria(8, 2) * 0.12^2 * 0.88^6
# c) a probabilidade de haver ao menos um obito na amostra; 1 - P(y=0)
1 - combinatoria(8, 0) * 0.12^0 * 0.88^8
# d) a probabilidade de haver no maximo 2 obitos na amostra; P(y=0)+P(y=1)+P(Y=2)
(combinatoria(8, 0) * 0.12^0 * 0.88^8 + combinatoria(8, 1) * 0.12^1 * 0.88^7
+ combinatoria(8, 2) * 0.12^2 * 0.88^6)
# e) o numero medio de obitos; esperança n*p
8*0.12
# f) o desvio padrao do numero de obitos. raiz da variancia = sqrt(n*p*(1-p))
sqrt(8*0.12*0.88)

# ===> POISSON = e^−λ * λ^y / y!
e = exp(1)

# 17) distribuiçao de Poisson. Os registros do hospital revelam que as admissoes 
# ao setor de emergencias sao, em media, tres por dia, durante este perıodo.
# a) exatamente duas admissoes ao setor de emergencias ocorrerao em um dado dia;
# lambda = 3, y = 2, e = 2.71
(e^-3 * 3^2) / factorial(2)
# b) nenhuma admissao ao setor de emergencias ocorrera em um dado dia;
# lambda = 3, y = 0, e = 2.71
(e^-3 * 3^0) / factorial(0)
# c) ocorram tres ou quatro admissoes ao setor de emergencias em um dado dia.
(e^-3 * 3^3) / factorial(3) + (e^-3 * 3^4) / factorial(4)

# 18) O numero medio de organismos por amostrafoi encontrado como sendo dois. 
# Assumindo o numero de organismos tendo uma distribuicao de Poisson
# lambda = 2
# a) a proxima amostra coletada contera um ou mais organismos; 1 - P(y=0)
1 - (e^-2 * 2^0) / 1
# b) a proxima amostra coletada contera exatamente tres organismos;
(e^-2 * 2^3) / factorial(3)

# 19) 5 bacterias por cm3 de um lıquido, com Poisson
# a) qual é o desvio padrao do numero de bacterias por cm3; sqrt(variancia) = sqrt(lambda)
sqrt(5)
# b) probabilidade de que pelo menos duas bacterias ocorram num volume de lıquido de 1cm3
# = 1 - (P(y=0)+P(y=1))
1 - ((e^-5 * 5^0) / 1 + (e^-5 * 5^1) / 1)

# 20) resultante da reação de um determinado soro é 0.0001. 
# Determinar a probabilidade de, entre 2000 indivıduos:
# a) exatamente tres sofrerem a reacao;
dbinom(3, 2000, 0.0001)
combinatoria(2000, 3) * 0.0001^3 * 0.9999^1997
# b) mais de dois sofrerem a reação. 1 - P(y=0)+P(y=1)+P(y=2)
1 - pbinom(2, 2000, 0.0001)
1 - (dbinom(0, 2000, 0.0001) + dbinom(1, 2000, 0.0001) + dbinom(2, 2000, 0.0001))

# 21) Num livro de 800 paginas ha 800 erros de impressao. Qual a probabilidade de 
# que uma pagina contenha pelo menos 3 erros?
# 1 erro por pagina media
1 - ((e^-1 * 1^0) / 1 + (e^-1 * 1^1) / 1 + (e^-1 * 1^2) / 2)

# 22) Em um total de n plantas irradiadas, e p = 0.0004 a probabilidade de
# uma planta irradiada apresentar mutação. Poisson com aproximação a Binomial
# a) nao aparecer nenhuma planta com mutacao em 900 plantas;
lambda = 0.0004*900; lambda
(e^-lambda * lambda^0) / 1
# b) de aparecer ao menos uma planta com mutação em 900 plantas; = 1-P(y=0)
1 - ((e^-lambda * lambda^0) / 1)

# 23) Seja Y o numero de arvores de castanha-jaranacom distribuião poisson
# media de tres plantas por hectare
# lambda = 3
# a) encontrar no maximo uma arvore dessa especie, ao se observar 1 hectare; P(y=0)+P(y=1)
(e^-3 * 3^0) / 1 + (e^-3 * 3^1) / 1
# b) de se encontrarem 3 ou mais arvores dessa especie; 1 - (P(y=0)+P(y=1)+P(y=2))
1 - ((e^-3 * 3^0) / 1 + (e^-3 * 3^1) / 1 + (e^-3 * 3^2) / 2)

# ===> Hipergeometrica: P(Y = k) = ((m | k) * (n-m | r-k)) / (n | r)

# 24) De um baralho com 52 cartas, retiram-se 8 cartas ao acaso, sem reposicao. 
# Qual a probabilidade de que quatro sejam ases?
(combinatoria(4, 4) * combinatoria(48, 4)) / combinatoria(52, 8)

# 25) De um lote de 100 animais, dos quais 20 sao estereis, escolhe-se uma 
# amostra de 10 sem reposição.
# a) haver 3 estereis;
(combinatoria(20, 3) * combinatoria(80, 7)) / combinatoria(100, 10)
# a) haver 5 estereis;
(combinatoria(20, 5) * combinatoria(80, 5)) / combinatoria(100, 10)

# 26) Um inspetor de qualidade de um frigorıfico examina cada lote de suınos que 
# chega por amostragem, selecionando sempre 10 animais. Se todos os animais 
# amostrados estiverem acima do peso mınimo estabelecido, o lote é aceito. Se pelo 
# menos um dos animais estiver abaixo do peso mınimo, o lote todo é recusado. Num
# determinado lote de 60 animais, há 6 deles abaixo do peso. Qual a probabilidade
# do lote ser recusado?
1 - (combinatoria(6, 0) * combinatoria(54, 10)) / combinatoria(60, 10)
