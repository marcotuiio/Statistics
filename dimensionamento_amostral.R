rm(list=ls());

# - Erro amostral tolerável 
# n0 = 1 / E^2;
# a) E = 10%, b) E = 5%, c) E = 2.5%
# Descomentar qual valor deve ser usado
E = 10/100; 
# E = 5/100;
# E = 2.5/100;
n0 = 1 / E^2; n0

# - Tamanho Amostral
# Com tamanho de população (N) conhecida, erro tolerável (n0) 
# definido, usar a correção para saber quantas pessoas devem
# ser entrevistadas (n):: n >= N * n0 / (N + n0)
# Com erros de 10, 5 e 2.5 porcento, calcule o tamanho amostral para:
# a) 240 b) 1000 c) 1535 d) 18000 e) 200000

erro <- c(10, 5, 2.5);
n_0 <- c(100, 400, 1600); #EXEMPLOS FEITOS PARA n0=100
p240 <- c(71, 150, 209); #ex: n>=240*100/240+100
p1000 <- c(91, 285, 616); #ex: n>=1000*100/1000+100
p1535 <- c(94, 318, 784); #ex: n>=1535*100/1535+100
p18000 <- c(100, 392, 1470); #ex: n>=18000*100/18000+100
p200000 <- c(100, 400, 1588); #ex: n>=200000*100/200000+100

dados <- data.frame(erro, n_0, p240, p1000, p1535, p18000, p200000); dados

# - Nível de confiança
# quando existe um vies para certa resposta, chance de prevalência
# Z = 1.96 se confiança 95%
# z = 2.58 se confiança 99%
# Formula:: n0 >= Z^2 * pi(1-pi) / E^2

# Dados nivel de confiança 95%, margem de erro (E) 3% e incidência
# (pi) de 15%, determine:
# a) a amostra mínima inicial.
Z = 1.96;
pi = 0.15;
E = 0.03;
n0 = Z^2 * pi*(1-pi) / E^2; n0

# b) Amostra final, para N = 25486
N = 25486;
n0 = 545;
n = N * n0 / (N + n0); ceiling(n)

# c) a amostra mínima final se N = 250.
N2 = 250;
n2 = N2 * n0 / (N2 + n0); n2

# - Para variaveis quantitativas
# Formula:: n0 >= Z^2 * S^2 / e^2
# S^2 é a variancia, e é um erro de precisao

# Dados nivel de confiança de 95%, margem de erro de 50 reais
# e um desvio-padrão de 400 reais, resolva:
# a) Tamanho de amostras necessario?
S = 400;
e = 50;
Z = 1.96;
n0 = Z^2 * S^2 / e^2; ceiling(n0)

# b) Com e=25, qual deve ser o valor de n0?
e2 = 25;
n02 = Z^2 * S^2 / e2^2; ceiling(n02)
