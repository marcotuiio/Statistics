# Intervalos de Confiança ###############

###### Uma População
#### Para Variância CONHECIDA
calc_Ic = function(media, z, sd, n) {
  c(media - (z*sd / sqrt(n)), media + (z*sd / sqrt(n))); 
}

## Ex1: Um conjunto de 10 suínos da raça Wessex teve um peso médio de 108 kg.
# Determinar o I.C. para a média de pesos, ao nível de 95% de confiança,
# sabendo que eles tem distribuição normal com desvio-padrão 10 kg.

intervalo = calc_Ic(108, 1.96, 10, 10); intervalo

require(asbio)
ci.mu.z(conf=.95, sigma=10, xbar=108, n=10, summarized=TRUE)

## Ex2: Os tempos (s) para indução de anestesia de uma amostra de 40 tilápias do
# Nilo foram registrados. Sabe-se que a variância dos tempos de indução de
# anestesia para esta espécie de tilápia é de 140s^2. Determine o intervalo de
# confiança para os tempos de indução de anestesia, ao níıvel de 95% de confiança.

til = c(65, 83, 61, 88, 76, 57, 73, 93, 60, 55,
        61, 73, 58, 74, 77, 55, 82, 61, 77, 79,
        80, 80, 55, 63, 90, 67, 70, 60, 66, 57,
        77, 90, 52, 77, 82, 82, 77, 72, 85, 60)
inter = calc_Ic(mean(til), 1.96, sqrt(140), 40); inter

#### Para Variância DESCONHECIDA
## Utiliza-se a Distribuição t de Student; semelhante a Normal porém com "caudas
## mais grossas"

Ic_tStud = function(media, t, s, n) {
  c(media - (t*s / sqrt(n)), media + (t*s / sqrt(n))); 
}

## Ex3: Um experimento foi realizado para verificar a ação da N-acetil cisteína
# associada ao succinato de cloranfenicol no tratamento da doença crônica
# respiratória em aves. Os pesos médios (g) para dez aves de 6 semanas
# foram: y´ = 75g e s = 5,8g. Qual o I.C. para a média ao nível de 95%.

inter = Ic_tStud(75, 2.262, 5.8, 10); inter

#### Para a Proporção
 
Ic_prop = function(p, z, n) {
  c(p - z*sqrt((p*(1-p))/n), p + z*sqrt((p*(1-p))/n))
}

## Ex4: Um estudo foi conduzido para prática da saúde dental de uma certa
# população adulta urbana. De 300 adultos entrevistados, 123 vão ao
# dentista para check-up duas vezes ao ano. Construa um intervalo de
# confiança para a verdadeira proporção, ao nível de 95%.

inter = Ic_prop(123/300, 1.96, 300); inter

###### Duas Populações
#### Para Variâncias Conhecidas

Ic2_var = function(m1, m2, var1, var2, n1, n2, z) {
  c((m1 - m2) - z*sqrt(var1/n1 + var2/n2), (m1 - m2) + z*sqrt(var1/n1 + var2/n2))
}

## Ex5: A variancia dos pesos ao nascer (em kg), de bovinos da raça Guzera, é
# σ^2 = 104,1 kg^2 e da raça Gir é de σ^2 = 70,5 kg^2
# Dez bezerros de cada raça sao aleatoriamente selecionados e pesados e
# as medias de pesos ao nascer, para as raças Guzera e Gir, sao dadas,
# respectivamente, por: y´1 = 28,3 kg e y´2 = 23,5 kg.
# Construa um intervalo de confiança para a diferença entre as medias,
# ao nıvel de 95%.

inter = Ic2_var(28.3, 23.5, 104.1, 70.5, 10, 10, 1.96); inter

#### Para Variâncias Desconhecidas

var_conjunta = function(n1, var1, n2, var2) {
  ((n1-1)*var1 + (n2-1)*var2) / (n1+n2-2)
}

Ic2_conj = function(m1, m2, S, n1, n2, t) {
  sd = sqrt(S)
  c((m1 - m2) - t*sd*sqrt(1/n1 + 1/n2), (m1 - m2) + t*sd*sqrt(1/n1 + 1/n2))
}

## Ex6: Um pesquisador quer verificar se os pesos ao nascer de animais machos das
# raças Gir e Guzera diferem. Foram pesados 10 animais de cada raça.
# Testar, ao nivel de 5% de significancia, se as raças diferem. Os pesos
# observados sao. Construa o intervalo de confiança, ao nıvel de 95% de confiança:
guz = c(30, 26, 25, 23, 25, 29, 34, 30, 30, 31)
gir = c(23, 21, 20, 20, 23, 26, 22, 27, 26, 27)

Sconj = var_conjunta(length(guz), var(guz), length(gir), var(gir)); Sconj
inter = Ic2_conj(mean(guz), mean(gir), Sconj, length(guz), length(gir), 2.1); inter

boxplot(guz, gir, names=c('Guzerá', 'Gir'), ylab='Pesos ao nascer KG', xlab='Raças')
points(c(mean(guz), mean(gir)), pch='+', col='red', cex=2)
t.test(guz, gir, var.equal = T)

#### Caso as populações NÃO sejam HOMEGÊNEAS é aplicada a aproximação
# de Satterthwaite (ou Welch) para os graus de liberdade

### Utilizar a mesma formula de população homogenea com variancia conhecida
### trocando o z pelo t;

## Ex7: Um pesquisador quer comparar o pH de caes diabeticos em dois grupos: um
# com cetose e outro com cetoacidose. O pH de 14 animais de cada grupo foi
# obtido. Construa o intervalo de confianca, ao nıvel de 95% de confianca,
# para a diferenca entre as medias. O que se pode concluir?
cetose = c(7.38, 7.34, 7.42, 7.41, 7.37, 7.39, 7.41,
           7.40, 7.41, 7.39, 7.45, 7.33, 7.33, 7.39)
cetoacidose = c(7.22, 7.32, 7.32, 7.23, 7.00, 7.29, 7.17,
                6.99, 7.15, 7.32, 7.22, 7.06, 7.28, 7.41)

inter = Ic2_var(mean(cetose), mean(cetoacidose), var(cetose), var(cetoacidose), 14, 14, 2.1318); inter

t.test(cetose, cetoacidose)

#### Amostras Pareadas

## Ex8: Andrade & Ogliari (2007) apresentam um experimento conduzido para
# estudar o conteudo de hemoglobina no sangue de suınos com
# deficiencia de niacina. Aplicaram-se 20 mg de niacina em oito suınos.
# Os nıveis de hemoglobina no sangue foram mensurados antes e depois
# da aplicaçao da niacina. Os resultados obtidos no experimento foram. 
# Calcule o IC para 95% de NC:

antes = c(12.4, 13.6, 13.6, 14.7, 12.3, 12.2, 13.0, 11.4)
depois = c(10.4, 11.4, 12.5, 14.6, 13.0, 11.7, 10.3, 9.8)

t.test(depois, antes, paired=T)

#### Para Diferença entre Proporções com Duas pops

Ic2_prop = function(p1, p2, n1, n2, z) {
  v1 = (p1-p2) - z*sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
  v2 = (p1-p2) + z*sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
  c(v1, v2)
}

## Ex9: Uma amostra de 200 animais sofrendo de uma certa doença foram
# aleatoriamente divididos em dois grupos.
# Do primeiro grupo, que recebeu o tratamento padrao (n = 100), 78 se
# recuperaram em 3 dias.
# Do segundo grupo, que recebeu um novo metodo de tratamento, 90 se
# recuperaram em 3 dias.
# Construa um intervalo de confianca, ao nıvel de 95%, para a verdadeira
# diferença entre proporçoes.

inter = Ic2_prop(78/100, 90/100, 100, 100, 1.96); inter
