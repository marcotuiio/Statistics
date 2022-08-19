rm(list=ls());

# Amostra aleatória sistemática
# dados = seq(1:500);
# n = 50;
# k = length(dados) / n;
# set.seed(143);
# inicio = sample(seq(1:k), 1);
# amostra = seq(from=inicio, to=500, by=k); amostra

# Amostra aleória estratificada
# dados = 1:2000; N = 2000; n = 80;
# N1 <- 500 ; N2 <- 1200 ; N3 <- 200; N4 <- 100;
# f <- n / N;
# n <- cbind(N1, N2, N3, N4) * f; n

# FUNÇÃO Amostra proporção
n0_proporcao = function(nc=95, pi=0.5, erro=0.05) {
  if (nc == 95) {
    z = 1.96;
  } else {
    z = 2.58;
  }
  ceiling(z^2 * pi*(1-pi) / erro^2);
}

tamanho_amostral = function(N, n0) {
  ceiling(N * n0 / (N + n0)); # n >= ..
}

amostra_inicial = function(Nt, n) {
  trunc(Nt * n / (Nt - n)) -1;
}

# n0 = n0_proporcao(95, 0.15, 0.03); n0
# tamanho_amostral(25000, n0);

# FUNÇÃO Amostra qualitativa 
n0_qualitativa = function(nc=95, s=400, erro=0.03) {
  if (nc == 95) {
    z = 1.96;
  } else {
    z = 2.58;
  }
  ceiling(z^2 * s^2 / erro^2);
}

# FUNÇÃO ERRO Amostral
erro_amostral = function(nc=95, pi=0.15, n0=500) {
  if (nc == 95) {
    z = 1.96;
  } else {
    z = 2.58;
  }
  aux = z^2 * pi*(1-pi) / n0;
  sqrt(aux);
}

# FUNÇÃO ERRO Qualitativo
erro_quantitativo = function(nc=95, s=400, n0=500) {
  if (nc == 95) {
    z = 1.96;
  } else {
    z = 2.58;
  }
  aux = z^2 * s^2 / n0;
  sqrt(aux);
}

# EXERCICIOS:

# 2) Um levantamento de dados será realizado, para isso será utilizado o 
# procedimento de amostragem. O erro para esse estudo será de 4%, o nível de 
# confiança será de 95% e não sabe-se nunhuma informação sobre a incidência. 
# Encontre quantos e quais elementos deve conter a amostra, sabendo que uma
# população tem cerca de 15000 elementos e é dividida da sequinte maneira.
# • 45% do sexo masculino sendo que 60% estudam de manhã e 40% a noite
# • 55% do sexo feminino sendo que 80% estudam de manhã e 20% a noite

e = 4/100;
nc = 95;
pi = 0.5;
N = 15000;
n0 = n0_proporcao(nc, pi, e); n0
n0_corrigido = tamanho_amostral(N, n0); n0_corrigido
N_Masc_D = 0.45*0.60; N_Masc_D # qntd de homens do dia
N_Masc_N = 0.45*0.40; N_Masc_N # qntd de homens da noite
N_Fem_D = 0.55*0.80; N_Fem_D # qntd de mulheres do dia
N_Fem_N = 0.55*0.20; N_Fem_N # qntd de mulheres da noite
round (578*cbind(N_Masc_D, N_Masc_N, N_Fem_D, N_Fem_N));
n = tamanho_amostral(N, n0_corrigido); n
f = n / N; f
# Quantidade de selecionados em cada estrato
n1 = N * N_Masc_D * f; n1
n2 = N * N_Masc_N * f; n2
n3 = N * N_Fem_D * f; n3
n4 = N * N_Fem_N * f; n4
round(n1+n2+n3+n4)

# 3) Encontre qual o erro para cada uma das situaações abaixo.

# a) 
n = 850; N = 60000; pi = 0.45; nc = 95
n0 = amostra_inicial(N, n); n0
e = erro_amostral(nc, pi, n0); e 

# b)
n = 700; N = 40000; pi = 0.45; nc = 99
n0 = amostra_inicial(N, n); n0
e = erro_amostral(nc, pi, n0); e

# c) 
n = 1200; N = 20000; S = 15; nc = 95
n0 = amostra_inicial(N, n); n
e = erro_quantitativo(nc, S, n0); e

# d) 
n = 350; N = 15000; S = 50; nc = 99
n0 = amostra_inicial(N, n); n0
e = erro_quantitativo(nc, S, n0); e

rm(list=ls());