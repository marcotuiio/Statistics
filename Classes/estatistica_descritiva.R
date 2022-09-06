rm(list=ls())

fr = function(fi, N) {
  round((fi / N) * 100, 2);
}
#### VARIÁVEIS QUALITATIVAS

# Tabela Relativa (Frequência Relativa) 
f_canina = fr(8500, 10000); f_canina
f_roedor = fr(36, 10000); f_roedor

# Tabela de Dupla Entrada
racas = c("fecundos", "infecundos", "totais")
charolesa = c(606, 394, 1000)
indubrasil = c(508, 632, 1140)
dados = rbind(racas, charolesa, indubrasil); dados

rm(list=ls())

## Exec Tabela da pecuária nos Estados da Região Sul

n_pr = 17162079
n_sc = 12680508
n_rs = 22121877

bov_pr = fr(9275271, n_pr); bov_pr
bov_sc = fr(4296052, n_sc); bov_sc
bov_rs = fr(12551432, n_rs); bov_rs

bub_pr = fr(33015, n_pr); bub_pr
bub_sc = fr(10776, n_sc); bub_sc
bub_rs = fr(56962, n_rs); bub_rs

eq_pr = fr(282018, n_pr); eq_pr
eq_sc = fr(105448, n_sc); eq_sc
eq_rs = fr(527881, n_rs); eq_rs

sui_pr = fr(6899545, n_pr); sui_pr
sui_sc = fr(7968232, n_sc); sui_sc
sui_rs = fr(5726461, n_rs); sui_rs

cap_pr = fr(115718, n_pr); cap_pr
cap_sc = fr(33372, n_sc); cap_sc
cap_rs = fr(71365, n_rs); cap_rs

ovi_pr = fr(556515, n_pr); ovi_pr
ovi_sc = fr(266628, n_sc); ovi_sc
ovi_rs = fr(3187776, n_rs); ovi_rs

## analisar as similaridades de cada Estado e criar 
## certo padrão para agrupamento

# a) % == bov: rs, bub: rs, eq: rs, sui: sc, cap: pr, ovi: rs
## abs == bov: rs, bub: rs, eq: rs, sui: sc, cap: pr, ovi: rs

# b) % == pr: bov, sc: sui, rs: bov
## abs == pr: bov, sc: sui, rs: bov

#### VARIÁVEIS QUANTITATIVAS

# Numéricas contínuas

# A) Critério de Sturges 
sturges = function(n) {
  k = round(1 + 3.3 * log(n, 10))
}

# B) Critério da Raiz Quadrada
## k = sqrt(n);

#### Exercicio de Sala
# Tabela 7: Distribuição de frequência dos pesos (kg) de 30 cães das raças 
# Brasileiro e Pastor Alemão, coletados no Hospital Veterinário da UEL.
k = sturges(30); k # numero de classes e intervalo entre as classe

peso = c("5|-11", "11|-17", "17|-23", "23|-29", "29|-35", "35|-41", "Total"); peso
pM = c(8, 14, 20, 26, 32, 38, "-"); pM
fi = c(1, 5, 8, 7, 4, 5, 30); fi
frp = c(fr(1, 30), fr(5, 30), fr(8, 30), fr(7, 30), fr(4, 30), fr(5, 30), 100); frp
Fi = c(1, 6, 14, 21, 25, 30, "-"); Fi
Frp = c(fr(1, 30), fr(6, 30), fr(14, 30), fr(21, 30), fr(25, 30), 100, "-"); Frp

dados = data.frame(peso, pM, fi, frp, Fi, Frp); dados


caes = c( 5.5, 19.0, 28.0, 30.0, 33.0, 40.0, 40.0, 40.3,
          40.5, 12.6,12.6, 14.2, 14.2, 17.5, 17.5, 18.0, 19.0, 19.2,
          21.0, 21.0,27.0, 27.0, 27.0, 27.2, 28.0, 28.0, 30.0, 30.0,
          39.8, 13.5); caes
tabela = hist(caes, plot=T, breaks=c(5,11,17,23,29,35,41), right=F)

fi = round(tabela$count)  # freq absoluta
fr = round(fi/sum(fi) * 100, 2)  # freq relativa
Fi = cumsum(fi)  # Freq absoluta acumulada
Fr = cumsum(fr)  # Freq relativa acumulada
final = cbind(fi, fr, Fi, Fr)

rm(list=ls())

## Atividade: Montar tabela dados a qntd de ração (kg) usadas em 40 dias
racao = c(0.71, 2.63, 3.63, 1.94, 3.69, 2.77, 1.42, 2.48, 3.77, 2.75,
          2.04, 2.16, 4.05, 1.80, 2.22, 2.06, 1.20, 1.67, 5.41, 1.57,
          3.09, 2.16, 3.94, 2.06, 3.55, 3.56, 3.57, 2.39, 2.48, 1.53,
          2.67, 2.18, 3.93, 3.34, 2.78, 3.26, 3.06, 3.32, 3.37, 0.75)

N = length(racao); N
# a) Encontre o numero de classes
k = sturges(N); k  # numero de classes

# b) Amplitude total e limite das classes 
maxR = max(racao); maxR
minR = min(racao); minR
a = (maxR - minR) / k; a  # Amplitude de cada intervalo

tabela = hist(racao, plot=T, breaks=c(0.7, 1.5, 2.3, 3.1, 3.9, 4.7, 5.5), right=F)

fi = round(tabela$count); fi
fr = round(fi/sum(fi) * 100, 2); fr
Fi = cumsum(fi); Fi
Fr = cumsum(fr); fr
final = cbind(fi, fr, Fi, Fr); final
rownames(final) = c("0.7|-1.5", "1.5|-2.3", "2.3|-3.1", "3.1|-3.9", "3.9|-4.7", "4.7|-5.5"); final



