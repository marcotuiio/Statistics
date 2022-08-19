rm(list=ls());
library(readxl)
library(writexl)

require(readxl);

aula_1_dados <- read_excel("C:/Users/marco/OneDrive/Área de Trabalho/AULAS/Estatística/aula_1_dados.xlsx"); aula_1_dados

require(writexl);
write_xlsx(aula_1_dados, "C:/Users/marco/OneDrive/Área de Trabalho/AULAS/Estatística/teste_escrita.xlsx")

teste_leitura <- read_excel("C:/Users/marco/OneDrive/Área de Trabalho/AULAS/Estatística/teste_escrita.xlsx"); teste_leitura

