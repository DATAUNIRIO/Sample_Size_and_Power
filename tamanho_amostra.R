
library(samplingbook)
sample.size.prop(e=0.02, P=0.5, N=100)
sample.size.prop(e=0.02, P=0.5, N=300)
sample.size.prop(e=0.02, P=0.5, N=1000)
sample.size.prop(e=0.01, P = 0.5, N = 200, level = 0.95)
sample.size.prop(e=0.15, P = 0.5, N = 200, level = 0.95)
sample.size.prop(e=0.05, P = 0.5, N = 1200, level = 0.90)


sample.size.prop(e=0.05, P=0.5, N=Inf)
sample.size.prop(e=0.1,  P=0.5, N=1000)
sample.size.prop(e=0.15, P=0.5, N=200)
sample.size.prop(e=0.15, P=0.5, N=80)


# Escolas grandes com erro de 0,1 e 95%
# Escolas pequena com erro de 0,05 e 95%

library(readxl)
library(janitor)

escolas_sorteadas <- read_excel("C:/Users/08451589707/Downloads/escolas_sorteadas.xlsx") %>% clean_names()
escolas_sorteadas = escolas_sorteadas[1:746,]

vetor = escolas_sorteadas$na_escola


resultados_erro_10 <- matrix(NA, ncol=1, nrow=746)

for (i in 1:length(vetor)){
  x <- try(sample.size.prop(e=0.1, P=0.5, N=vetor[i]))
  if (class(x)=='try-error') {resultados_erro_10[i] <- NA}
  else {resultados_erro_10[i] <- x[["n"]]}
}
colnames(resultados_erro_10) <- 'erro_10'


resultados_erro_15 <- matrix(NA, ncol=1, nrow=746)

for (i in 1:length(vetor)){
  x <- try(sample.size.prop(e=0.15, P=0.5, N=vetor[i]))
  if (class(x)=='try-error') {resultados_erro_15[i] <- NA}
  else {resultados_erro_15[i] <- x[["n"]]}
}
colnames(resultados_erro_15) <- 'erro_15'

tamanho_amostra = cbind(escolas_sorteadas$escola,escolas_sorteadas$na_escola,resultados_erro_10,resultados_erro_15)
tamanho_amostra = data.frame(tamanho_amostra)
tamanho_amostra

tamanho_amostra$V2 = as.numeric(tamanho_amostra$V2)
tamanho_amostra$erro_10 = as.numeric(tamanho_amostra$erro_10)
tamanho_amostra$erro_15 = as.numeric(tamanho_amostra$erro_15)
sum(tamanho_amostra$V2)
sum(tamanho_amostra$erro_10)
sum(tamanho_amostra$erro_15)

tamanho_amostra$tamanho_final = ifelse(tamanho_amostra$V2<=1500,
                                       tamanho_amostra$erro_15,
                                       tamanho_amostra$erro_10)

sum(tamanho_amostra$tamanho_final)

# library(dplyr)
# tamanho_amostra = tamanho_amostra %>%
#     mutate(
#       amostra_fixa = case_when(
#         tamanho_amostra$V2 <= 10    ~  V2,
#         tamanho_amostra$V2 <= 50    ~  V2/2,
#         tamanho_amostra$V2 <= 200   ~  V2/4,
#         tamanho_amostra$V2 <= 1000  ~ V2/10,
#         TRUE                        ~  100))
# 
# tamanho_amostra$erro = 1/sqrt(tamanho_amostra$amostra_fixa)
# sum(tamanho_amostra$amostra_fixa)




writexl::write_xlsx(tamanho_amostra,path = "tamanho_amostra.xlsx")











