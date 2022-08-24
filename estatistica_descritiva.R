rm(list=ls())

fr = function(fi, N) {
  (fi / N) * 100;
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
