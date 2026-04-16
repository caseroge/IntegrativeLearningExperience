#DAG

install.packages("dagitty")
install.packages("ggdag")
library(dagitty)
library(ggdag)


dag <- dagitty('dag {
  HIV [pos="-0.523,0.796"]
  MDR_TB [outcome,pos="0.106,0.485"]
  Urban_Rural [adjusted,pos="-0.729,-1.385"]
  Age [adjusted,pos="-1.080,-0.805"]
  Detention [exposure,pos="-1.105,0.524"]
  Education [adjusted,pos="-0.027,-0.853"]
  Homelessness [adjusted,pos="-0.403,-0.840"]
  Occupation [adjusted,pos="-0.341,-1.394"]
  Sex [adjusted,pos="-0.756,-0.844"]
  HIV -> MDR_TB
  Urban_Rural -> MDR_TB
  Urban_Rural -> Detention
  Age -> MDR_TB
  Age -> Detention
  Education -> MDR_TB
  Education -> Detention
  Homelessness -> MDR_TB
  Homelessness -> Detention
  Occupation -> MDR_TB
  Occupation -> Detention
  Sex -> MDR_TB
  Sex -> Detention
}')

ggdag(dag)

adjustmentSets(dag)

impliedConditionalIndependencies(dag)

is