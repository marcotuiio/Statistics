rm(list=ls());

# Amostra aleatória sistemática
dados = seq(1:500);
n = 50;
k = length(dados) / n;
set.seed(143);
inicio = sample(seq(1:k), 1);
amostra = seq(from=inicio, to=500, by=k); amostra

# Amostra aleória estratificada
dados = 1:2000; N = 2000; n = 80;
N1 <- 500 ; N2 <- 1200 ; N3 <- 200; N4 <- 100;
f <- n / N;
n <- cbind(N1, N2, N3, N4) * f; n

rm(list=ls());

# FUNÇÃO
n0_confianca = function(nc=95, pi=0.5, erro=0.05) {
  if (nc == 95) {
    z = 1.96;
  } else {
    z = 2.58;
  }
  ceiling(z^2 * pi*(1-pi) / erro^2);
}

tamanho_amostral = function(N, n0) {
  ceiling(N * n0 / (N + n0));
}