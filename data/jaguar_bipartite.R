library (bipartite)
require (sna)
require (igraph)

web<-as.matrix(read.csv("jaguar_bipartite.csv", row.names=1))

# Kawada Kawai - melhor para ver nos centrais
gplot(web,gmode="twomode",usearrows="FALSE",mode="kamadakawai",main="",object.scale = 0.03)

# Fruchterman Reignold - melhor para ver modulos
gplot(web,gmode="twomode",usearrows="FALSE",mode="fruchtermanreingold",main="",object.scale = 0.03)

source("Ordernet.R") # carregando a funcao

order.web=order.net(mat=web) # Organiza a matriz de acordo com o grau das especies

# Decreasing degree - melhor para ver aninhamento
plotweb(web, method="normal",arrow="both",bor.col.interaction="grey", y.width.low=0.02, y.width.high=0.02, col.high="red", col.low="blue", high.lablength=0, low.lablength=0)
mtext("",side=3 , line=1, font=1)

#------------------#
# Conectividade    #
#------------------#

## Grau e Grau Medio ##
K.web.Pred=apply(web,1,sum) #Exemplo de como calcular o grau dos predadores
K.web.Pred
K.web.Prey=apply(web,2,sum) #Exemplo de como calcular o grau das presas
K.web.Prey

KMed.web.Pred=mean(apply(web,1,sum)) #Calculo do grau medio de predadores
KMed.web.Prey=mean(apply(web,2,sum)) #Calculo do grau medio de presas

## Conectancia ##

K.web=apply(web,2,sum) 				        #grau das colunas
E.web=apply(as.matrix(K.web), 2, sum)	#numero de arestas (interacoes) na rede
R.web=dim(web)[1]				             	#numero de linhas
C.web=L=dim(web)[2]					          #numero de colunas
Conec.web=E.web/(R.web*C.web) 			  #calculo da conectancia
Conec.web

## Visualizando os resultados de conectancia da rede:

Connectance=data.frame(c(round(Conec.web,2)))
colnames(Connectance)=c("Connectance")
Connectance

## Distribuicao do grau ##

d.web = degreedistr(web) # Menos que 5 pontos

## Entropias ##

source("Entropy.R")
Entropy.web=entropy(web)

## Visualizando os resultados de entropia:

Entropy=data.frame(c(Entropy.web$Row.Entropy, c(Entropy.web$Col.Entropy), row.names=c(""))
colnames(Entropy)=c("Animal.Entropy", "Plant.Entropy")
Entropy


#-----------------------------#
# Aninhamento e distancias    #
#-----------------------------#

## Aninahmento ##

web.NODF=nested(web, method = "NODF2") # Calcula NODF das matrizes

# Visualizando os resultados de aninhamento:

Nestedness=data.frame(c(round(web.NODF,2)), row.names=c(""))
colnames(Nestedness)=c("NODF")
Nestedness

## Distancias ##

# Criando listas de interaca de uma matriz n x m. Nohs das linhas vao de 0 a n-1. Nohs das colunas vao de n a m-1.

edgelist.web=web2edges(web, return=TRUE) 

write.table(edgelist.web[,1:2]-1,file="web.edge.txt",row.names=F, col.names=F, append=T, quote=F) #cria o arquivo de edgelist

web.igraph=read.graph("web.edge.txt", format="edgelist", direct=FALSE) #le arquivo de edgelist e grava em variavel da classe igraph

#Matriz de distancia do noh da linha ao noh da coluna:
web.paths=shortest.paths(web.igraph)  
web.paths

#Caminho medio:
path1.len.web=average.path.length(web.igraph) #media dos caminhos da rede

Path.1=data.frame(c(round(path1.len.web,2)), row.names=c(""))
colnames(Path.1)=c("Caminho.medio1")
Path.1

#Plot and save the bipartite chart 
png(file = "jaguar_bipartite.png", width = 900, height = 600)
plotweb(web, labsize=1.3, text.rot=90, col.high="#6699CC", 
       	col.low="#999933", col.interaction="#999999")
dev.off()

visweb(web, type="diagonal") 

networklevel(web) # Calculates a variety of indices and values for a bipartite network
specieslevel(web) # Apart from the properties of the entire web, also its participants can be described specifically. Various simple numbers and indices are calculated and returned
nodespec(web) # Calculates a specialisation index based on the node positions for all species in a bipartite network, separately for the higher and lower trophic level
(ex <- second.extinct(web, participant="lower", method="random", nrep=50, details=F))
slope.bipartite(ex)

nullmodel(web, N=1000, method=4)
nullmodel(web, N=1000, method=5)

res <- computeModules(web)
plotModuleWeb(res)
PAC(web) # Quantifies, for each pair of lower trophic level species, the potential for showing apparent competition with another species, mediated through the higher trophic level.

degreedistr(web, plot.it=T)
V.ratio(web)

library(magick)
library(magrittr) 

# Call back the plot
plot <- image_read("jaguar_bipartite.png")
plot2<-image_annotate(plot, "Predator-prey web topology", 
                      color = "black", size = 25,
                      location = "10+50", gravity = "north")
plot3<-image_annotate(plot2, "Data: Palmeira FBL, 2015 (10.11606/T.11.2015.tde-17092015-111206) | Visualization by @fblpalmeira 
                          Image credit: Palmeira, FBL and Trinca CTT", 
                      color = "gray", size = 12, 
                      location = "10+50", gravity = "southeast")
# And bring in a logo
jaguar <- image_read("https://raw.githubusercontent.com/fblpalmeira/jaguar_bipartite/main/data/ponca.png") 
out1<-image_composite(plot3,image_scale(jaguar,"x60"), gravity="north", offset = "-250+40")

puma <- image_read("https://raw.githubusercontent.com/fblpalmeira/jaguar_bipartite/main/data/puma.png") 
out2<-image_composite(out1,image_scale(puma,"x50"), gravity="north", offset = "+20+50")

ocelot <- image_read("https://raw.githubusercontent.com/fblpalmeira/jaguar_bipartite/main/data/pardalis.png") 
out3<-image_composite(out2,image_scale(ocelot,"x50"), gravity="north", offset = "+270+50")

image_browse(out3)

# And overwrite the plot without a logo
image_write(out3, "jaguar_bipartite2.png")
