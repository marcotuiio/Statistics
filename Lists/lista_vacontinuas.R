# 2) Z = Y - media / desvio
# a) P(-z < Z < z) = 0.95
z1 = 1.7 - 1.96*0.08; z1
z2 = 1.7 + 1.96*0.08; z2
# z1 < Y < z2
c(qnorm(0.025, 1.7, 0.08), 
qnorm(0.975, 1.7, 0.08))

# b) P(1.6 < Y < 1.82)
# P((1.6 - 1.7)/0.08 < Z < (1.82 - 1.7)/0.08)
z1 = (1.6 - 1.7)/0.08; z1
z2 = (1.82 - 1.7)/0.08; z2
# P(z < 1.5) - P(z < -1.25) = P(z < z2) - P(z < z1)
0.9332 - 0.1056 # probabilidade de encontrar alguem alguem 1.6 e 1.82 
pnorm(1.82, 1.7, 0.08) - pnorm(1.6, 1.7, 0.08) 

# c) P(1.58 < z)
z = (1.58 - 1.7)/0.08; z
# P(z < -1.5) = 0.0668
pnorm(1.58, 1.7, 0.08)

# 4) N(90, 20^2), objetivo 80
# a) qual a % a ser aprovada
pnorm(80, 90, 20)  # prob de ser MENOR que 80
# b) tempo para os melhores 5%
qnorm(0.05, 90, 20) # Os 5% melhores são aqueles com os menores tempos, logo quantil de 0.05

# 5) media de 36,8 graus e desvio-padrao de 0,15 graus.
# a) em 1000 pessoas quantas teriam entre 36,8 e 37,2 graus?
1000 * (pnorm(37.2, 36.8, 0.15) - pnorm(36.8, 36.8, 0.15)) 
# b) em qual intervalo estão 98% 
c(qnorm(0.01, 36.8, 0.15), 
qnorm(0.99, 36.8, 0.15))
# 36.45 < Y < 37.15

# 6) 1 desvio, carcaça pior 60, carcaça melhor 80
media = 50
desvio = 2
# peso medio esperado por carcaça deve estar entre 60 e 80 !
(pnorm(48, media, desvio) * 60) + (pnorm(52, media, desvio) * 80)

# 7) Faixa de normalidade para 95% dos dados, intervalo de 95% dos dados
escarro = c(17, 22, 23, 23, 23, 23, 24, 24, 24,
          24, 24, 24, 25, 25, 25, 25, 25, 25,
          25, 26, 28, 28, 29, 30, 30, 31, 31,
          35, 35, 35, 36, 40, 41, 41, 41, 42,
          51, 54, 56, 56, 56, 58, 60, 68, 79)
media = mean(escarro)
desvio = sd(escarro)
c(qnorm(0.025, media, desvio),
qnorm(0.975, media, desvio))

# 8) 10000 alunos, media de 170cm e desvio de 5cm
# a) alunos com estatura superior a 1.65
10000 * pnorm(165, 170, 5, lower.tail = F)
# b) intervalo simetrico em torno da media que contera 75% das estaturas
c(qnorm(.125, 170, 5),
  qnorm(.875, 170, 5))

# 9)
n = 142
media = 390.19
desvio = 45.23
# b) prob de pesos acima de 395kg
n * pnorm(395, media, desvio, lower.tail = F) # 1 - z > (395-390)/45.23
# c) prob de pesos abaixo de 380.17kg
n * pnorm(380.17, media, desvio)
# d) prob de pesos acima de 385kg
n * pnorm(385, media, desvio, lower.tail = F)
# e) prob entre 385 e 390kg
n * (pnorm(390, media, desvio) - pnorm(385, media, desvio))
# f) 25% dos com menor peso abatidos, peso minimo dos remanescentes 
# P(Y < y) = 0.25
# Y = z * desvio + media; z = -0.67 (olhando tabela e procurando 0.25)
y = -0.67 * desvio + media; y
qnorm(0.25, media, desvio)
# g) Peso minimo para os 5% mais pesados
# P(Y < y) = 0.95
y = 1.64 * desvio + media; y
qnorm(0.95, media, desvio)

# 10) n=120, N(80, 12)
# a) 1 desvio para mais e menos a partir da média
120 * (pnorm(80+12, 80, 12) - pnorm(80-12, 80, 12))
# b) pesar entre 72 e 82 kg?
pnorm(82, 80, 12) - pnorm(72, 80, 12)
# c) pesar entre 92 e 104 kg?
pnorm(104, 80, 12) - pnorm(92, 80, 12)

# 11) N = 10000, N(3.5, 0.6)
# 20% dos mais leves = pequenos
pequenos = qnorm(0.2, 3.5, 0.6); pequenos
# 40% seguintes = médios
medios = c(pequenos, qnorm(0.6, 3.5, 0.6)); medios
# 30% seguintes = grandes 
grandes = c(medios[2], qnorm(0.9, 3.5, 0.6)); grandes
# 10% restantes = extras
extras = qnorm(.1, 3.5, 0.6, lower.tail = F); extras
