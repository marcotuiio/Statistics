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

quartil = function(Li, Fq, Fac, a, i, n) {
    # i = porcento do quartil desejado    
    # Li = limite inferior da classe quantilica
    # a = amplitude das classes
    # Fac = freq acumulada da classe anterior
    # Fq = freq da classe quantilica
    Li + (((i*n - Fac)) * a / Fq); 
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
q3 = quartil(72, 12, 24, 6, 0.75, 50); q3
## Significa que das 50 instituições analisadas, 75% tem disponibilidade
## de leitos inferior a 78.75; enquanto 25% dessas possuem disponibulidade
## superior a 78.75

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

# 5) Com base nos dados apresentados na Tabela 1, calcular a média, mediana e 
# moda de ácido ascórbico

fi = c(3, 6, 18, 35, 16, 2)
xi = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1)  # ponto medio

media = MEDIA_POND(fi, xi); media
mediana = MEDIANA(0.6, 80, 27, 35, 0.2); mediana
moda = czuber(0.6, 0.2, 17, 19); moda

# 6) Consideremos 12 observações (ordenadas) do tempo de observações (dias) de 
# animais de pequeno porte do Hospital Veterinário da UEL:

observacoes = c(1, 4, 7, 9, 10, 13, 15, 17, 17, 18, 19, 21)
quantile(observacoes); #q1 = 8.5; q2 = 14; q3 = 17.25
boxplot(observacoes)

# 7) Com base nos dados da Tabela 2, calcule o peso médio dos ratos em cada 
# idade. Usando o R, construa o gráfico de caixas para a variável peso, 
# considerando-se cada rato (apenas um gráfico com todos os ratos).
library(readxl)
require(readxl)
dados_ratos = read_excel("C:/Users/marco/OneDrive/Área de Trabalho/AULAS/Estatística/dados_ratos.xlsx");
with(dados_ratos, boxplot(Peso ~ Idade));

# Peso médio para cada idade:
pesos = with(dados_ratos, c(Peso)); 
idades = with(dados_ratos, c(Idade));

i = 1
aux1 = aux2 = aux3 = aux4 = aux5 = 0
medias = c(0, 0, 0, 0, 0)
while(i <= length(idades)) {
    if (idades[i] == 30) {
        medias[1] = medias[1] + pesos[i]
        aux1 = aux1+1
    } else if (idades[i] == 34) {
        medias[2] = medias[2] + pesos[i]
        aux2 = aux2+2
    } else if (idades[i] == 38) {
        medias[3] = medias[3] + pesos[i] 
        aux3 = aux3+1
    } else if (idades[i] == 42) {
        medias[4] = medias[4] + pesos[i]   
        aux4 = aux4+1
    } else if (idades[i] == 46) {
        medias[5] = medias[5] + pesos[i]
        aux5 = aux5+1
    }
    
    i = i+1
}
Medias = c(medias[1]/aux1, medias[2]/aux2, medias[3]/aux3, medias[4]/aux4, medias[5]/aux5)
Medias
points(Medias, pch="+", cex=1.5, col="blue")

media_pesos = sum(pesos)/length(pesos); media_pesos

# 8) Trinta pessoas foram consultadas sobre sua cor favorita.
# Construa uma tabela de frequências para os dados. Qual a escolha modal e a 
# frequência relativa (ou porcentagem) desta cor?

cores = c("vermelho", "azul", "verde", "amarelo", "preto", "marrom", "laranja", "roxo", "totais")
fi = c(4, 11, 4, 3, 5, 1, 1, 1, 30);
frp = c(fr(4, 30), fr(11, 30), fr(4, 30), fr(3, 30), fr(5, 30), fr(1, 30), fr(1, 30), fr(1, 30), 100)
data = data.frame(cores, fi, frp); data
# escolha_modal = azul, com 11 ocorrências
azul_fr = frp[2]; azul_fr

# 9) Calcule para a tabela de frequências abaixo sua respectiva média e mediana.
xi = c(2, 3, 4, 5, 6);    # peso das frequencias 
fi = c(3, 6, 10, 6, 3);   
frp = c(fr(3, 28), fr(6, 28), fr(10, 28), fr(6, 28), fr(3, 28))
Fi = c(3, 9, 19, 25, 28); # frequencias de cada valor
Frp = c(fr(3, 28), fr(9, 28), fr(19, 28), fr(25, 28), fr(28, 28))
data = data.frame(xi, fi, frp, Fi, Frp); data

media_pond = MEDIA_POND(fi, xi); media_pond
# mediana_bruta = 5;
mediana = MEDIANA(4, sum(fi), 9, 10, 1); mediana

# 10) Determinar o quarto decil, o primeiro quartil e o septuagésimo segundo 
# percentil da seguinte distribuição
classes = c("4|-9", "9|-14", "14|-19", "19|-24");
fi = c(8, 12, 17, 3)
frp = c(fr(8, 40), fr(12, 40), fr(17, 30), fr(3, 40))
Fi = c(8, 20, 37, 40);
Frp = c(fr(8, 40), fr(20, 40), fr(37, 40), fr(40, 40))
data = data.frame(classes, fi, frp, Fi, Frp); data

quarto_decil = quartil(9, 12, 8, 5, 0.4, sum(fi)); quarto_decil # 40%
primeiro_quartil = quartil(9, 12, 8, 5, 0.25, sum(fi)); primeiro_quartil # 25%
septuagesimo_segundo_percentil = quartil(14, 17, 20, 5, 0.72, sum(fi)); septuagesimo_segundo_percentil # 72%
