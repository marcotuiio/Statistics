## VARIAVEIS QUALITATIVAS
# Gráfico de colunas
# Gráfico de barras
# Gráfico de colunas justapostas: facil de identificar semelhanças entre grupos
# Gráfico de colunas compostas  
# Gráfico de setores retangulares
# Gráfico de setores circulares  (Teta = 360 / n * fi)

## VARIAVEIS QUANTITATIVAS
# Variaveis discretas: Gráfico em bastão

# Variavies continuas (buscar sempre formato de 'sino' no Gráfico)
    # Histogramas !!
    # Poligonos de frquencias
    # Gráficos de caixas(box plot) !!
    # Gráfico de dispersão e de setores proporcinais (relações e concentrações)
    # Gráfico de linha
rm(list=ls())

caes = c(37, 20, 19, 16, 75, 103);
barplot(caes, main="FREQUENCIAS DE CAES", xlab="caes", ylab="frequencis");

caess = sort(caes); caess
barplot(caess, horiz=T)

## plot()
## boxplot()
## pie()
## symbols