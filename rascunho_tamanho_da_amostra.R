# Proportions and differences in proportions
#The function nProp will return the sample size required for estimating a proportion with a specified
#CV or variance. For a CV target, Equation (1) is used; the formula for a variance target is similar. The
#function takes the following parameters:
#  CV0 target value of coefficient of variation of the estimated proportion
#  V0 target value of variance of the estimated proportion
#  pU population proportion
#  N number of units in finite population; default is Inf

# A single numeric value, the sample size, is returned. An advance guess is needed for the value of
# the population proportion, pU . By default, the population is assumed to be very large (N = ∞), but
# specifying a finite value of N results in a finite population correction being used in calculating the
# sample size. To estimate a proportion anticipated to be pU = 0.1 with a CV0 of 0.05, the function call
# and resulting output is:

library(PracTools)
nProp(CV0 = 0.05, N = Inf, pU = 0.1)
# If the population has only 500 elements, then the necessary sample size is much smaller:
nProp(CV0 = 0.05, N = 500, pU = 0.1)
# srswor sample size so that half-width of 2-sided 95% CI is 0.005
nProp(V0=(0.005/1.96)^2, N=Inf, pU=0.1)
# srswor sample size so that half-width of 2-sided 95% CI is 0.005
nProp(V0=0.25, N=Inf, pU=0.1)

library(samplingbook)
sample.size.prop(e=0.02, P=0.5, N=100)
sample.size.prop(e=0.02, P=0.5, N=300)
sample.size.prop(e=0.02, P=0.5, N=1000)
sample.size.prop(e=0.01, P = 0.5, N = 200, level = 0.95)


sample.size.prop(e=0.15, P = 0.5, N = 200, level = 0.95)

sample.size.prop(e=0.05, P = 0.5, N = 1200, level = 0.90)


library(pwr)


plot.out <- pwr.t2n.test(n1=50, n2=100, d=0.5, alternative="two.sided")
plot(plot.out)


pwr.p.test(h=0.5,n=100,sig.level=0.05,alternative="two.sided")








pwr.p.test(h=0.2,n=200,sig.level=0.95)










sample.size.prop(e=0.05, P=0.5, N=Inf)

sample.size.prop(e=0.1,  P=0.5, N=1000)
sample.size.prop(e=0.15, P=0.5, N=200)
sample.size.prop(e=0.15, P=0.5, N=80)


# Escolas grandes com erro de 0,1 e 95%
# Escolas pequena com erro de 0,05 e 95%


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













library(readxl)
library(janitor)
escolas_sorteadas <- read_excel("C:/Users/08451589707/Downloads/escolas_sorteadas.xlsx") %>% clean_names()
escolas_sorteadas = escolas_sorteadas[1:746,]

vetor = escolas_sorteadas$na_escola

# resultados com erro de 0,01

resultados_erro_001 <- matrix(NA, ncol=1, nrow=747)

for (i in 1:length(vetor)){
  x <- try(sample.size.prop(e=0.01,, P=0.5, N=vetor[i]))
  if (class(x)=='try-error') {resultados_erro_001[i] <- NA}
  else {resultados_erro_001[i] <- x[["n"]]}
}


colnames(resultados_erro_001) <- 'erro_001'

# resultados com erro de 0,02
resultados_erro_002 <- matrix(NA, ncol=1, nrow=747)

for (i in 1:length(vetor)){
    x <- try(sample.size.prop(e=0.02,, P=0.5, N=vetor[i]))
    if (class(x)=='try-error') {resultados_erro_002[i] <- NA}
    else {resultados_erro_002[i] <- x[["n"]]}
  }


colnames(resultados_erro_002) <- 'erro_002'

# resultados com erro de 0,05
resultados_erro_005 <- matrix(NA, ncol=1, nrow=747)

for (i in 1:length(vetor)){
  x <- try(sample.size.prop(e=0.05, P=0.5, N=vetor[i]))
  if (class(x)=='try-error') {resultados_erro_005[i] <- NA}
  else {resultados_erro_005[i] <- x[["n"]]}
}

colnames(resultados_erro_005) <- 'erro_005'


tamanho_amostra = cbind(escolas_sorteadas$escola, escolas_sorteadas$na_escola, resultados_erro_001,resultados_erro_002,resultados_erro_005)
class(tamanho_amostra)

tamanho_amostra = as.data.frame(tamanho_amostra)
print(tamanho_amostra)

sum(as.numeric(tamanho_amostra$V2))

sum(as.numeric(tamanho_amostra$erro_001))
sum(as.numeric(tamanho_amostra$erro_002))
sum(as.numeric(tamanho_amostra$erro_005))



