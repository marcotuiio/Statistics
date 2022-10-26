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

