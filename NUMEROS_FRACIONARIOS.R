#============================================================================
#
# CALCULO DO NÚMERO BINARIO DE NUMEROS DECIMAIS FRACIONARIOS
#
# Versão 1.00 (2019) - 11.08.2019
# Autor: Prof. Giovanni Leopoldo Rozza (2019)
#
#============================================================================

#  defina o diretorio de trabalho
setwd("C:/Users/Acer/OneDrive/AULAS/Bagozzi/R_SOURCE/CALCULO_NUMERICO/CAP_1_ERROS")
#

# loga console em arquivo
sink("logconsole.txt", type = "output", append = FALSE, split = TRUE)


# defina o o decimal que vc que calcular o valor binário
dDecimal <-0.35 # valor numero fracionario decimal 

dfBinario<- data.frame(     ik  <-integer(),
                            idk <-integer(),
                            irk  <-double()
                      )

ik<-1 
idk<-0                      # D1=0
irk <- dDecimal             # r1=decimal

if( 2*irk >= 1)             # se 2*r1 > 1 --> D1=1
{
  idk<-1
}


bFim<- FALSE                # fim algoritmo

dfBinario <- rbind( dfBinario, data.frame(ik,idk,irk))  # salva 1a iteracao no dataframe
bRepete=FALSE

# algoritmo

while(!bFim)
{
  # calcula pr?ximo rk
  irk_1<- 2*irk - idk
  
  if( isTRUE(all.equal(irk_1,0.0)) )
  # se proximo rk=0, FIM
  {
    bFim <- TRUE
    
  }
  #atualiza valores (ainda n?o salva no dataframe)
  else
  {
    #procura elemento repetido
    for(iCount in seq_len(nrow(dfBinario)) )
    {
      #print(dfBinario$irk[iCount])     #debug
      #print(irk_1)                     #debug
      
      # verifica se tem irk repetido
      if( isTRUE(all.equal(dfBinario$irk[iCount],irk_1)))
      {
        bFim<-TRUE
        bRepete=TRUE
      }
    }
    
    irk <- irk_1
    ik  <- ik + 1
    
    idk<-0
    
    if( 2*irk >= 1.0)              # se 2*ri >= 1 --> Di=1
    {
      idk<-1
    }
    
    dfBinario <- rbind( dfBinario, data.frame(ik,idk,irk))   # atualiza linha no dataframe
      
    
  }
  
}

#imprime tabela
print("|---------------------------|")
linha<- sprintf("| Numero Decimal -> %7.5f |",dDecimal)
print(linha)
print("|---------------------------|")
print("| ik   |   irk    |  idk    |")
print("|------|----------|---------|")
cRepete<-""
for(iCount in seq_len(nrow(dfBinario)))
{
  if(iCount == nrow(dfBinario) & (bRepete) )
  {
    cRepete<-"<-ik=1"
  }
  linha<- sprintf("| %3i  | %7.5f  |   %3i   |  %8s ",dfBinario$ik[iCount],dfBinario$irk[iCount],dfBinario$idk[iCount],cRepete)
  print(linha)
}
# retorna console
sink(NULL, type = "output")

