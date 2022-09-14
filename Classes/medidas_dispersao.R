rm(list=ls())

### MEDIDAS DE DISPERSÃO: variabilidade em relação ao valor médio

# Amplitude total = maior - menor
# Desvio absoluto médio = abs(Media - N[1]) +...+ abs(Media - N[n]) = desvio

# Variância = s^2 = sum((elemento-media)^2) / n - 1 = y1^2 - n*media^2
variancia = function(yi, media, n) {
    soma = 0
    for (i in 1:length(yi)) {
        soma = soma + yi[i]^2
    }
    s = (soma - n * media^2) / (length(yi) - 1);
}

## Var.Dados Agrupados = 1/n-1 * (sum(elemento^2*fi) - (sum(elemento*fi)^2))/n

# Exec 
Y = c(2, 4, 5, 8, 10, 12)
# A) variancia:
s1 = variancia(Y, mean(Y), length(Y)); s1 
# B) acrescente c = 4
c = 4
Y2 = Y+c; Y2
s2 = variancia(Y2, mean(Y2), length(Y2)); s2
# C) subtraia c = 4
Y3 = Y-c; Y3
s3 = variancia(Y3, mean(Y3), length(Y3)); s3
# D) mult c = 4
Y4 = Y*c; Y4
s4 = variancia(Y4, mean(Y4), length(Y4)); s4
# E) div c = 4
Y5 = Y/c; Y5
s5 = variancia(Y5, mean(Y5), length(Y5)); s5
## Somar ou subtrair não altera variancia
## Multiplicar ou dividir é igual a s*c^2 ou s/c^2

# Desvio Padrão = sqrt(s^2)

## Menor CV => Mais Homogeneo
## CV > 30% => ERRADO, alta dispersão
CV = function(s, media) {
    s * 100 / media
}

# Exec: media, variancia, desvio padrao, e cv
Estatura = c(177, 162, 188, 157, 166, 153, 158, 176, 168, 163)
Pesos = c(68.0, 83.0, 72.0, 99.9, 51.0, 52.0, 52.0, 66.5, 80.0, 48.0)
Idades = c(18.0, 20.1, 20.5, 17.7, 19.2, 18.9, 26.9, 20.1, 20.7, 19.3)

media_est = mean(Estatura); media_est
media_pes = mean(Pesos); media_pes
media_id = mean(Idades); media_id

var_est = variancia(Estatura, media_est, length(Estatura)); var_est
var_pes = variancia(Pesos, media_pes, length(Pesos)); var_pes
var_id = variancia(Idades, media_id, length(Idades)); var_id

desv_est = sqrt(var_est); desv_est
desv_pes = sqrt(var_pes); desv_pes
desv_id = sqrt(var_id); desv_id

cv_est = CV(desv_est, media_est); cv_est
cv_pes = CV(desv_pes, media_pes); cv_pes
cv_id = CV(desv_id, media_id); cv_id
