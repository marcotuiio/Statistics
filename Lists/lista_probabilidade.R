# 1) Espaço Amostral {(B,C), (B,K), (V)}

# 2) Espaço Amostral {(C,1), (C,2), (C,3), (C,4), (C,5), (C,6), 
                    # (K,1), (K,2), (K,3), (K,4), (K,5), (K,6)} 

# 3) Probabilidade de em 750 encontrar uma do sangue AB
AB = 220; N = 750
P_AB = AB / N; P_AB

# 4) P(A∪B) = P(A) + P(B) − P(A∩B) 
P_A = 0.2; PuAB = 0.5; PeAB = 0.1
P_B = PuAB - P_A + PeAB

# 5) 
# A: Soma 9 = 3+6, 6+3, 4+5, 5+4 = 4 * (1/6*1/6) = 4 * (1/36) = 1/9
# B: >= 4 no dado1 = 3 * 1/6 = 1/2

# 6) Lançamento de um dado preto e um dado branco
# a) Soma 6: 1+5, 5+1, 2+4, 4+2, 3+3, 3+3
6*(1/6*1/6)
# b) Soma 11: 5+6, 6+5
2*(1/6*1/6)
# c) Soma 2: 1+1, 1+1
2*(1/6*1/6)
#) d) nem soma 2 e nem 8: 1 - (soma2 + soma8)
# Soma 8: 2+6, 6+2, 3+5, 5+3, 4+4, 4+4
1 - (6*(1/6*1/6) + 2*(1/6*1/6))

# 7) Considere dois eventos A e B, mutuamente exclusivos(disjuntos), com: 
P_A = 0.3; P_B = 0.5
# a) P(A∩B);
0
# b) P(A∪B);
P_A + P_B
# c) P(A|B);
0
# d) P(A^c);
1 - P_A
# e) P [(A ∪ B)^c];
1 - (P_A + P_B)

# 8) Se P(A ∪ B) = 0.8; P(A) = 0.5 e P(B) = x, determine o valor de x no caso de:
PuAB = 0.8; PA = 0.5
# a) A e B serem mutuamente exclusivos; x = 0.3
# b) A e B serem independentes; x - PeAB = 0.3

# 9)Suponhamos que um casal ja teve cinco meninos e deseja saber a probabilidade 
# do proximo filho ser menina: P(Menina) = 0.5

# 10) Uma moeda é viciada, de maneira que as caras sao 3 vezes mais provaveis de 
# aparecer do que as coroas. Se esta moeda e lancada duas vezes. Qual a 
# probabilidade de ocorrer cara apenas uma vez?
3/4 * 1/4

# 11) 
N = 10000; esportista = 4000; bioD = 500; bioD_esp = 100; bioN = 700; bioN_esp = 200
# a) ser esportista;
esportista/N
# b) ser esportista e aluno da biologia noturno;
bioN_esp/bioN
# c) nao ser da biologia;
1 - (bioD+bioN) / N
# d) ser esportista ou aluno da biologia;
(esportista/N) + (bioD+bioN)/N
# e) nao ser esportista nem aluno da biologia.
1 - (esportista/N) - (bioD+bioN)/N

# 12) Diagramas de Venn
p1 = 132; ep2 = 86; p1p2 = 120; unico = 54; sop1 = 12; sop2 = 42
acertaram = 12+120+42
erraram = 86-12
n = acertaram + erraram
# a) nao tenha acertado nenhum problema;
erraram/n
# b) tenha acertado apenas o segundo problema;
sop2/n
# c) tenha acertado pelo menos um problema.
1 - erraram/n

# 13) 2% falha humana, 1% falha técnica, 2.5% pelo menos uma das duas falhas
p_AeB = 0.02 * 0.01; p_AeB # errado

# 14) 70% macho, 30% femea; 40% dos machos e 60% das femeas sao nelores. Prob de 
# nelore macho
0.7 * 0.4

# 15) 40% homens e 20% nunca viram o mar; 60% mulher e 50% nunca viram o mar
# a) homem e nunca tenha visto o mar;
0.4 * 0.2
# b) mulher ou nunca tenha visto o mar;
0.6 + 0.2 * 0.4

# 16) Das 8 alunas de uma classe, 3 tem olhos azuis. Escolha duas
# a) ambas terem olhos azuis;
(3/8)^2
# b) nenhuma ter olhos azuis;
(5/8)^2
# c) pelo menos uma ter olhos azuis?
1 - (5/8)^2

# 17) Se os pais forem heterozigotos, a probabilidade de terem um filho de olho 
# escuro é 3/4 e de ter olho claro 1/4. Porém se um deles não tiver ao menos um
# gene recessivo, a probabilide do filho ter olhos escuros é 4/4

# 18) Albinismo, ambos alelos recessivos, ter uma crianca albina
# a) Ambos tem pigmentacao normal, mas cada um tem um genitor albino?
1/4
# b) O homem e albino, a mulher normal, mas o pai dela é albino?
# Aa, Aa, aa, aa
2/4
# c) O homem é albino e a famılia da mulher nao inclui albinos por, pelo menos, 3 geraçoes?
# Nenhuma

# 19) Ambos sao heterozigotos e normais para um dado carater.
# a) Um filho anormal? 
1/2*1/4
# b) Uma filha normal?
1/2*3/4
# c) Duas filhas normais?
(1/2*3/4)^2
# d) Tres filhos anormais?
(1/2*1/4)^3
# e) Duas filhas normais e tres filhos anormais?
(1/2*3/4)^2 * (1/2*1/4)^3

# 20) Um homem mıope e albino casou-se com uma mulher de pigmentacao e visao normais, 
# porem filha de pai tambem mıope e albino. Miopia e o albinismo caracteres recessivos,
# Homem: aa bb; Mulher: Aa Bb
# a) Uma crianca de visao e pigmentacao normais?
# Aa, Aa, aa, aa | Bb, Bb, bb, bb
2/4 * 2/4
# b) Uma filha mıope e albina?
1/2 * 2/4 * 2/4
# c) 2 criancas mıopes e 4 albinas?
(2/4)^2 * (2/4)^4

# 21) Ganha 0.7 se chove, 0.8 se não chove. 0.3 de chuva. Ganhou
ganhar = 0.7*0.3 + 0.8*0.7
# P(A|B) = P(A∩B) / P(B)
ganhar_chuva = 0.7*0.3
choveu = ganhar_chuva / ganhar; choveu 

# 22) 60% castrados, 10% não castrado e com disturbio hormonal, 30% castrado e com disturbio
#a) prob de ter disturbio
0.6*0.3 + 0.4*0.1
# b) tem disturbio, prob de nao castrado # P(A|B) = P(A∩B) / P(B)
x = 0.4*0.1 / 0.22; x

# 23) 20% tem alergia; 50% dos alergicos pratica esporte; 40% dos nao alergicos pratica esporte; 
# a) nao praticar esporte;
n_esporte = 0.2*0.5 + 0.8*0.6
# b) ser alergico dado que nao pratica esportes. # P(A|B) = P(A∩B) / P(B)
x = 0.2*0.5 / n_esporte; x

# 24) 2% é macho e Nelore; 10% é Nelore e 50% é macho.
# Prob de nao sendo macho ser Nelore?
0.5*0.1

# 25) Sabendo-se que 8% de um rebanho tem peso superior a 296 kg e 16% entre 280 e 296 kg, qual a probabilidade
# de que um bovino com peso superior a 280 kg pesar mais que 296 kg?
# n sei

# 27) 50% sao machos e 20% da raca Gir. Dentre os que sao machos, 30% é Gir.
# Prob de não ser macho ne gir
0.5*0.8

# 28) O metodo A da resultado positivo para 80% dos animais portadores da enfermidade 
# e para 10% dos sadios, ao passo que, o metodo B da positivo para 70% dos portadores
# e para 5% dos sadios. Sabendo ser de 15% a taxa dessa enfermidade
# a) de um animal fornecer resultado positivo pelos dois
(0.8*0.15 + 0.1*0.85) * (0.7*0.15 + 0.05*0.85)
# b) de, entre dois animais doentes, pelo menos um fornecer resultado positivo por algum metodo.
1 - ((0.1*0.15) * (0.25*0.15))
