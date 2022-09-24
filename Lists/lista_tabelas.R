rm(list=ls())
# 1) Uma pesquisa sobre leishmaniose em felinos foi conduzida pelo Centro de
# Zoonoses de Bauru. Foram selecionadas 300 fichas de felinos e verificado a
# raça do animal e o resultado do exame. Os felinos foram divididos em três
# grupos e os resultados encontram-se a seguir.
# - Dos gatos SRD 26 apresentaram resultados negativos e 34 positivos
# - Dos gatos Persas 50 resultados positivos 30 resultados negativo
# - Dos outros gatos 75 apresentaram resultados negativos e 85 positivos

# A) Tabela de dupla entrada

fr = function(fi, N) {
  round((fi / N) * 100, 2);
}

RACAS = c("Negativ", "% Neg.", "Positiv", "% Pos.", "Totais");
SRD = c(26, fr(26, 60), 34, fr(34, 60), 60);
PERSAS = c(30, fr(30, 80), 50, fr(50, 80), 80);
OUTROS = c(75, fr(75, 160), 85, fr(85, 160), 160);

#perc_neg = SRD[2]+PERSAS[2]+OUTROS[2];
#perc_pos = SRD[4]+PERSAS[4]+OUTROS[4];
TOTAIS = c(26+30+75, "-", 34+50+85, "-", 60+80+160);

t_dupla_entrada = rbind(RACAS, SRD, PERSAS, OUTROS, TOTAIS); t_dupla_entrada

# B) Podemos dizer que existe uma diferença nas taxas da doença de acordo 
# com as raças?
# R: Não, independente da raça a tendência observada foi a ocorrencia maior
# de resultados positvos

rm(list=ls())
# 2) Uma pesquisa foi realizada com 150 animais para verificar a eficácia
# (Alta, baixa e moderada) de um medicamento em relação ao porte do animal
# (Pequeno, Médio e Grande) em que foi ministrado.

# - Dos animais de pequeno porte 22 tiveram baixa eficácia, 18 tiveram a
# moderada e 10 apresentaram alta eficácia.
# - Dos 80 animais de médio porte 40 tiveram a moderada.
# - Dos animais de grande porte 15 apresentaram alta.
# - Sabe-se que 40 animais apresentaram baixa eficácia de medicamento e 47
# apresentaram alta eficácia.

# A) Tabela de dupla entrada

PORTE = c("ALTA", "% Alta", "BAIXA", "% Baixa", "MODERADA", "% Mod", "TOTAIS")
PEQUENO = c(10, fr(10, 50), 22, fr(22, 50), 18, fr(18, 50), 50);
MEDIO = c(47-25, fr(22, 80), 80-62, fr(18, 80), 40, fr(40, 80), 80);
GRANDE = c(15, fr(15, 20), 40-40, fr(0, 20), 5, fr(5, 20), 150-130);

#a = PEQUENO[2]+MEDIO[2]+GRANDE[2]
#b = PEQUENO[4]+MEDIO[4]+GRANDE[4]
#c = PEQUENO[6]+MEDIO[6]+GRANDE[6]
TOTAIS = c(47, "-", 40, "-", 18+40+5, "-", 150);

t_de = rbind(PORTE,PEQUENO, MEDIO, GRANDE, TOTAIS); t_de

# B) Existe diferença de eficácia olhando para as raças?
# R: Sim, para pequeno porte a ocorrência maior foi de eficiencia Baixa,
# para médio porte foi a eficácia moderada que prevaleceu, enquanto para 
# grande porte destaca-se a alta eficiência

# C) Qual o porte que representa melhor cada determinada taxa de eficácia?
# ALTA=MÉDIO, BAIXA=PEQUENO, MODERADA=MÉDIO

rm(list=ls())
# 3) Fazer Tabela 1: Tempo de resposta de um medicamento

TEMPO = c("1|-3", "3|-5", "5|-7", "7|-9", "TOTAL");
fi = c(100, 50, 30, 20, 200);
frp = c(fr(100, 200), fr(50, 200), fr(30, 200), fr(20, 200), 100);
Fi = c(100, 150, 180, 200, "-");
Frp = c(fr(100, 200), fr(150, 200), fr(180, 200), fr(200, 200), "-");

dados = data.frame(TEMPO, fi, frp, Fi, Frp); dados

# 4) Fazer Tabela 2: Envergadura de asas de aves migratórias

COMPRIMENTO = c("30|-60", "60|-90", "90|-120", "120|-150", "150|-180", "TOTAL")
fi = c(87, 77, 68, 46, 23, 87+77+68+46+23); fi
frp = c(fr(87, 301), fr(77, 301), fr(68, 301), fr(46, 301), fr(23, 301), 100);
Fi = c(87, 164, 232, 278, 301, "-");
Frp = c(fr(87, 301), fr(164, 301), fr(232, 301), fr(278, 301), fr(301, 301), "-");

dados = data.frame(COMPRIMENTO, fi, frp, Fi, Frp); dados

# A) Qual a proporção de aves que tem a envergadura inferior 120 cm?
# R: Olhar Frp, 77.08%

# B) Qual a proporção de aves com envergadura superior a 90 cm?
aux = 100-54.49; aux
# R: Olhar Frp e subtrair do total a partir do desejado, 43.51%

rm(list=ls())
# 5) Fazer Tabela 3: Peso em kg do bezerro ao nascer

PESO = c("35|-42", "42|-49", "49|-56", "56|-63", "63|-70", "TOTAL")
fi = c(12, 61, 72, 50, 15, 12+61+72+50+15); fi
frp = c(fr(12, 210), fr(61, 210), fr(72, 210), fr(50, 210), fr(15, 210), 100)
Fi = c(12, 73, 145, 195, 210, "-");
Frp = c(fr(12, 210), fr(73, 210), fr(145, 210), fr(195, 210), fr(210, 210), "-");

dados = data.frame(PESO, fi, frp, Fi, Frp); dados

# 6) Completar Tabela 4: Número de animais natimortos por cria ao longo de um ano

Natimortos = c(0, 1, 2, 3, 4, 5, "TOTAL");
fi = c(60, 15, 8, 7, 3, 2, 95)
frp = c(fr(60, 95), fr(15, 95), fr(8, 95), fr(7, 95), fr(3, 95), fr(2, 95), 100);
Fi = c(60, 75, 83, 90, 93, 95, "-");
Frp = c(63, 79, fr(83, 95), fr(90, 95), fr(93, 95), 100, "-")

dados = data.frame(Natimortos, fi, frp, Fi, Frp); dados

# 7) O conjunto de dados abaixo refere-se ao comprimento do corpo, em mm, de 
# Penaeus paulensis (Crustacea, Decapoda, Penaidae), obtidos nas despescas dos 
# vieiros do Centro de Ciências Agrárias da UFSC

cm_corpos = c(18, 20, 21, 25, 24, 25, 26, 26, 27, 27, 27, 27, 27,
              30, 32, 32, 33, 33, 34, 34, 35, 35, 36, 37, 38)

# Tabela de distribuição de frequências dos comprimentos dos corpos. (k=sqrt(n))
N = length(cm_corpos); N
k = sqrt(N); k  # Qntd de classes 
a = (max(cm_corpos) - min(cm_corpos)) / k; a  # Amplitude das classes
tabela = hist(cm_corpos, plot=T, breaks=c(18, 22, 26, 30, 34, 38), right=F)

Comprimentos = c("18|-22", "22|-26", "26|-30", "30|-34", "34|-38")
fi = round(tabela$count);
frp = round(fi/sum(fi)*100, 2);
Fi = cumsum(fi);
Frp = cumsum(frp);
dados = data.frame(Comprimentos, fi, frp, Fi, Frp); dados

rm(list=ls())
# 8) Completar Tabela 5: Tempo de procedimento em minutos

Tempo = c("0|-15", "15|-30", "30|-45", "45|-60", "60|-75", "75|-120", "Total");
fi = c(5, 20, 75, 90, 52, 8, 250);
frp = c(2, fr(20, 250), fr(75, 250), fr(90, 250), fr(52, 250), fr(8, 250), 100);
Fi =  c(5, 25, 100, 190, 242, 250, "-");
Frp = c(2, 10, fr(100, 250), fr(190, 250), fr(242, 250), 100, "-");

dados = data.frame(Tempo, fi, frp, Fi, Frp); dados

# 9) Contrua a Tabela de distribuição de frequências do total de oócitos  
# coletados. (k=sqrt(n))

oocitos = c(08, 10, 15, 25, 24, 25, 26, 26, 27, 27, 27, 27, 27,
            30, 32, 32, 33, 33, 34, 34, 35, 35, 36, 37, 38, 48,
            50, 52, 52, 53, 53, 54, 54, 65, 65, 66)

N = length(oocitos); N
k = sqrt(N); k   # Qntd de classes 
a = round((max(oocitos) - min(oocitos)) / k); a  # Amplitude das classes
tabela = hist(oocitos, plot=T, breaks=c(8, 18, 28, 38, 48, 58, 68), right=F)

fi = round(tabela$count);
frp = round(fi/sum(fi)*100, 2);
Fi = cumsum(fi);
Frp = cumsum(frp);

dados = cbind(fi, frp, Fi, Frp);
rownames(dados) = c("8|-18", "18|-28", "28|-38", "38|-48", "48|-58", "58|-68"); dados
