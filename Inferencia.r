library("rprojroot")
data <- read.table(file="C:\\Users\\bruno\\Documents\\R\\Projetos\\Inferência Estatística\\Riverbay.csv",header=FALSE,sep = ';')
votes <- data[,2:7]
candidate_totals <- votes[,6]
time_totals <- apply(votes, 2, sum)
voters <- c(600,1200,2444,3444,4444,5553) #as parciais com a quantidade de votos por bloco de votos
extras <- votes 
extras_voters <- voters
for (j in 2:6){
  extras[,j] <- votes[,j]-votes[,j-1] #subtrai a quantidade do bloco de votos anterior pra pegar a parcial de cada bloco
  extras_voters[j] <- voters[j]-voters[j-1]
}
extras_totals <- apply(extras,2,sum)
names_old <- as.vector(data[,1])
names <- as.vector(data[,8])
winners <- rev(order(candidate_totals))
n_candidates <- length(candidate_totals)
actual <- rep(NA, n_candidates)
expected <- rep(NA, n_candidates)

par(mfrow=c(6,5), mar = c(3,4,2,0), pty='m')
for (i in winners) {
  y <- extras[i,]/extras_voters
  plot(voters, y, ylim=range(0,y), type="l", xlab="", ylab="",
       main=names_old[i])
  p_hat <- candidate_totals[i]/5553
  actual[i] <- sd(as.numeric(y)) #desvio padrão real para os candidatos entre os blocos de votos
  expected[i] <- sqrt(mean(p_hat*(1-p_hat)/extras_voters)) #esperança do desvio padrão entre dos blocos de votos para cada candidato a partir da fórmula do desvio padrão  p(1-p)/n
}

#o plot com as porcentagens de cada candidato a medida que os votos avançavam (cada variação é o novo bloco de votação)
#importante lembrar que cada eleitor vota 6 vezes e portanto por isso as porcentagens são tão altas para os candidatos com somente 5553 eleitores

par(mfrow=c(1,2))
par(pty="s")
plot(expected, actual, xlim=range(expected,actual),
     ylim=range(expected,actual))
abline(0,1)
plot(candidate_totals, actual)
points(candidate_totals, expected, col="red")
#a partir do plot comparando os desvios padrão esperados com os desvio padrão reais para cada candidato vemos que não há uma dispersão tão grande que sugeriria uma manipulação nas eleições assumindo a premissa que cada bloco de votos que foi divulgado continha eleitores de uma amostra aleatória
