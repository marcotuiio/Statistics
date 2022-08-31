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

#b) czuber
czu = czuber(17, 6, (8-5), (8-7)); czu
pear = pearson(fi, yi); pear
