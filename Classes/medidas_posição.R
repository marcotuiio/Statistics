rm(list=ls())

# MÉDIA
MEDIA = function(vetor) {
  media = sum(vetor) / length(vetor)
}

pesos = c(10.13, 9.52, 12.45, 6.38, 8.66, 8.66, 10.35, 9.62, 12.34, 5.35)
m = MEDIA(pesos); m

notas = c(7.0, 8.5, 7.0, 7.5, 8.0, 8.5, 6.5, 7.0,
          6.0, 8.5, 6.0, 7.5, 8.0, 6.5, 0.0, 0.5,
          1.0, 1.0, 0.7, 0.0, 0.5)
m = MEDIA(notas); m

# Média aritimética ponderada
MEDIA_POND = function(fi, yi) {
  yf = 0
  for (i in 1:length(fi)) {
    yf = fi[i] * yi[i] + yf  
  }
  yf = yf / sum(fi)
}

pesos = c("40-50", "50-60", "60-70", "70-80", "80-90", "90-100") # intervalos 
fi = c(6, 22, 9, 8, 6, 2) # frequencia de cada intervalo de pesos
yi = c(45, 55, 65, 75, 85, 95) # ponto medio de cada intervalo funciona com media
YF = MEDIA_POND(fi, yi); YF
# Média geométrica
## Quando os valores observados crescem de forma exponencial, a méia
## aritmética pode não representar bem o conjunto de dados.

# MEDIANA
## Agrupados em classe
MD = function(Li, N, Fac, fmd, a) {
  Emd = N / 2; # Mediana espera
  # Li = limite inferior da classe 
  # N = população total
  # Fac = freq acumulada da classe anterior
  # fmd = qntd de elementos na classe da mediana
  # a = amplitude da classe
  Md = Li + ((Emd - Fac) / fmd) * a
}

# MODA
#A) Moda bruta: ponto médio da classe modal

#B) Método de Czuber
czuber = function(Li, a, d1, d2) {
  # d1 = diferença entre a frequência da classe modal e a anterior 
  # d2 = diferença entre a frequência da classe modal e a posterior
  Li + (d1 / (d1+d2)) * a
}

#C) Método de Peason
pearson = function(fi, yi) {
  # Md = mediana
  # y = média amostral
  Md = MD(23, 30, 14, 7, 6);
  y = MEDIA_POND(fi, yi)
  3 * Md - 2 * y
}

fi = c(1, 5, 8, 7, 4, 5)
yi = c(8, 14, 20, 26, 32, 38)

#a) moda bruta = 17|- 23

#b) czuber e pearson
czu = czuber(17, 6, (8-5), (8-7)); czu
pear = pearson(fi, yi); pear


### SIMETRIA DO CONJUNTO DE DADOS
# - cuidado para não tomar uma medida de posição em um conjunto heterogêneo
#   como forma para descrever todo o grupo

# CALCULOS DA MEDIDA DE ASSIMETRIA
# MEDIDAS SEPARATRIZES

## Percentil: divide o conjunto em partes desiguais (ex 1% = 1 <- -> 99)
#  primeiro quartil = P25; segundo quartil = P50; terceiro quartil = P75

penesimo_percentil = function(n, p, yi) {
    # n = numero de elementos
    # p = percentil
    # i = parte inteira de n * p
    # f = parte fracionaria de n * p
    i = as.integer(n * p);
    f = n * p - i;
    if (f == 0) {
        (yi[i] + yi[i+1]) / 2;
    } else if (f > 0) {
        yi[i+1];
    }
}

# Exemplo 14, determinar percentis

tilapias = sort(c(29.0, 29.5, 29.3, 25.8, 29.8, 24.3, 27.0, 29.6, 30.0, 28.5))
#a) p = 0.20
y02 = penesimo_percentil(length(tilapias), 0.20, tilapias); y02
#b) p = 0.34
y034 = penesimo_percentil(length(tilapias), 0.34, tilapias); y034


# Exemplo 15, determinar percentis 10%, 30%

pesos = sort(c(55.0, 70.0, 67.0, 65.0, 61.0, 50.0, 80.0, 65.0, 56.0, 58.0, 63.0,
               57.0, 63.0, 53.0, 76.0, 83.5, 51.0, 49.5, 73.4, 52.0, 59.0, 50.0,
               70.0, 75.0, 57.0, 62.0, 64.0, 53.0, 52.0, 40.0, 82.6, 90.0, 65.0,
               80.0, 60.0, 68.0, 47.0, 65.0, 53.0, 57.0, 73.0, 76.0, 53.2, 70.0,
               65.0, 57.0, 102.0, 57.0, 58.0, 54.0, 95.0, 52.0, 57.0, 51.0, 95.0,
               64.0, 69.0, 56.0, 56.0, 47.0, 58.0, 60.0, 63.0, 58.0, 50.0, 95.0,
               64.0, 88.0, 70.0, 60.0, 52.0, 80.0, 76.0, 60.0, 65.0, 60.0, 57.0,
               60.0, 61.0))
n = length(pesos)

y10 = penesimo_percentil(n, 0.1, pesos); y10 ## 10 % mais leves a esquerda
y30 = penesimo_percentil(n , 0.3, pesos); y30 ## 30% mais leves a esquerda
y70 = penesimo_percentil(n, 0.7, pesos); y70 ## 30% mais pesados a direita

# Quartil para dados agrupados
quartil = function(Li, Fq, Fac, a, i, n) {
    # i = porcento do quartil desejado    
    # Li = limite inferior da classe quantilica
    # a = amplitude das classes
    # Fac = freq acumulada da classe anterior
    # Fq = freq da classe quantilica
    Li + (((i*n - Fac)) * a / Fq); 
}

# Exercicio Considere a distribuição de pesos (kg) dos alunos do curso de 
# Medicina Veterinária da UEL em 2015 a seguir e determine.
pesos_agrupados = c("40|-50", "50|-60", "60|-70", "70|-80", "80|-90", "90|-100", "100|-110")
fi = c(4, 30, 24, 10, 6, 4, 1);
yi = c(45, 55, 65, 75, 85, 95, 105)

media = MEDIA_POND(fi, yi); media
mediana = MD(60, 79, 34, 24, 10); mediana
moda = czuber(50, 10, 26, 6); moda
q1 = quartil(50, 30, 4, 10, 0.25, n); q1  # 25% de 79 q1 entre 50 e 60
q3 = quartil(70, 10, 58, 10, 0.75, n); q3  # 40% de 79 q3

# A distribção é simétrica? Se não, que tipo de assimetria?
assimetria_quartis = function(q1, q3, md) {
    (q3 - 2 * md + q1) / (q3 - q1)
}
assimetria_quartis(q1, q3, mediana);

boxplot(pesos)
