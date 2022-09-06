rm(list=ls())

### Funções uteis !!

fr = function(fi, N) {
  round((fi / N) * 100, 2);
}

MEDIA = function(vetor) {
  media = sum(vetor) / length(vetor)
}

MEDIA_POND = function(fi, yi) {
  yf = 0
  for (i in 1:length(fi)) {
    yf = fi[i] * yi[i] + yf  
  }
  yf = yf / sum(fi)
}

czuber = function(Li, a, d1, d2) {
  # Li = limite inferior da classe
  # a = amplitude da classe modal
  # d1 = diferença entre a frequência da classe modal e a anterior 
  # d2 = diferença entre a frequência da classe modal e a posterior
  Li + (d1 / (d1+d2)) * a
}

pearson = function(fi, yi, Md) {
  # fi = vetor das frequencias absolutas
  # Md = mediana
  # y = média amostral (vetor dos pontos medios das classes)
  y = MEDIA_POND(fi, yi)
  3 * Md - 2 * y
}

MEDIANA = function(Li, N, Fac, fmd, a) {
  Emd = N / 2; # Mediana espera
  # Li = limite inferior da classe 
  # N = população total
  # Fac = freq acumulada da classe anterior
  # fmd = qntd de elementos na classe da mediana
  # a = amplitude da classe
  Md = Li + ((Emd - Fac) / fmd) * a
}

# 1) Em uma pesquisa sobre diabetes mellitus, tem-se os seguintes dados de 
# glicemia em jejum

glicemia = c(80, 85, 86, 90, 95, 96, 99, 100, 101, 103,
             103, 103, 104, 105, 108, 108, 109, 110, 110, 110)

# a) Agrupar os dados em classes e calcular a média, moda e a mediana;
k = round(sqrt(length(glicemia))); k
a = round((max(glicemia) - min(glicemia)) / k); a
tabela = hist(glicemia, plot=T, breaks=c(80, 88, 96, 104, 112), right=F)

fi = round(tabela$count); fi
frp = round(fi/sum(fi)*100, 2);
Fi = cumsum(fi);
Frp = cumsum(frp);
dados = cbind(fi, frp, Fi, Frp); 
rownames(dados) = c("80|-88", "88|-96", "96|-104", "104|-112"); dados

media = MEDIA(glicemia); media
mediana = MEDIANA(96, 20, 5, 7, a); mediana
moda_czu = czuber(104, a, 1, 8); moda_czu

# b) Determine, sem agrupar em classes (dados brutos): a média, moda e mediana
media_bruta = MEDIA(glicemia); media_bruta
mediana_bruta = (glicemia[10] + glicemia[11]) / 2; mediana_bruta;
moda_bruta = 110

rm(list=ls())
# 2) Em um estudo sobre instituições de atendimento médico, foram obtidos dados 
# da disponibilidade de leitos de 50 dessas instituições:

l = c(48, 53, 58, 62, 64, 66, 69, 71, 77, 81,
           49, 54, 58, 62, 64, 67, 69, 72, 77, 82,
           50, 55, 59, 63, 65, 67, 70, 73, 78, 83,
           52, 56, 60, 64, 65, 67, 70, 74, 78, 86,
           52, 57, 61, 64, 66, 68, 71, 76, 80, 90)
leitos = sort(l); leitos

# a) Determine a média e mediana dos dados e, também, determine o terceiro 
# quartil e interprete-o;
media = MEDIA(leitos); media
mediana = leitos[25]; mediana
moda = 64;
#terceiro quatil = ?

# b) Construa o gráfico de caixas (box plot).
boxplot(leitos);

# c) Agrupar os dados em classes (sqrt(n)) e calcular a média, moda e a mediana.
k = round(sqrt(length(leitos))); k
a = round((max(leitos) - min(leitos)) / k); a
tabela = hist(leitos, plot=T, breaks=c(48, 54, 60, 66, 72, 78, 84, 90), right=F)
fi = round(tabela$count); fi
frp = round(fi/sum(fi)*100, 2);
Fi = cumsum(fi);
Frp = cumsum(frp);
dados = cbind(fi, frp, Fi, Frp);
rownames(dados) = c("48|-54", "54|-60", "60|-66", "66|-72", "72|-78", "78|-84", "84|-90")
dados;

media = MEDIA(leitos); media
mediana = MEDIANA(66, length(leitos), 24, 12, a); mediana
moda = czuber(66, a, 1, 6); moda

rm(list=ls())

# 3) Os dados abaixo representam a largura máxima de amostras de crânios de 
# animais machos. Calcule média, mediana e a moda. 
cr = c(131, 119, 138, 125, 129, 126, 131, 132, 126, 128, 128, 131)
cranios = sort(cr); cranios
len = length(cranios); len

media = MEDIA(cranios);
mediana = (cranios[6] + cranios[7]) / 2; mediana
moda_bruta = 131;

# 4) A amplitude total de um conjunto de números é 500. Se a distribuição de 
# frequências apresenta 20 classes, qual deverá ser o limite inferior e o ponto 
# médio da 5ª classe, se o limite superior da 1ª classe é igual a 35?

AT = 500  # AT = MAX(c) - MIN(c)
k = 20  # qntd de classes 
a = AT/k; a  # amplitude das classes
intervalos = c("35|-60", "60|-85", "85|-110", "110|-135", "135|-160", "...")
classe5_inf = 135;
classe5_medio = (135+12 + 135+13) / 2; classe5_medio  # 24 elementos na classe
