# 1. Lecture des données taxonomiques sur le site du SANDRE
csv.url <- "http://services.sandre.eaufrance.fr/References/1.0.0/References.php?CdReferentiel=TAX&Filter=++++%3CFilter%3E++++++%3CStatut%3E1%3C%2FStatut%3E++++++%3CStatut%3E0%3C%2FStatut%3E++++%3C%2FFilter%3E&request=getReferenceElements&version=1.0.0&service=References&outputSchema=http%3A%2F%2Fxml.sandre.eaufrance.fr%2Fscenario%2FReferences%2F1&outputFormat=sandre%2Fsimplexml"
datataxon <-read.csv(csv.url,sep=";",header = T,encoding = "UTF-8")

# 2. Liste des codes Taxons des espèces à dessiner
tmpCdTAxon <- data.frame(CdTAxon= c("866","1547","2050", "2071" ,"2110" ,"2113", "2117","2125", "2133" ,"2137", "2167", "2177", "2193" ,"2203"))

# 3. Recherche en cascade des codes Taxon Parents
arbretaxonmerge <- data.frame()
for (j in 1:length(tmpCdTAxon$CdTAxon)){
  arbretaxon <- data.frame()
  ii <- 0
  arbretaxon_parent <- subset (datataxon,X.CdTaxon.==tmpCdTAxon$CdTAxon[j] )$X.CdTaxon. 
  while ( !is.na(arbretaxon_parent) ){
    arbretaxon[1+ii,1] <-  subset(datataxon,X.CdTaxon.== arbretaxon_parent )$X.NomLatinTaxon
    arbretaxon[1+ii,2] <-subset (datataxon,X.CdTaxon.== arbretaxon_parent)$X.CdTaxon.
    arbretaxon[1+ii,3] <-subset (datataxon,X.CdTaxon.== arbretaxon_parent)$X.AbrTaxonomique.
    arbretaxon_parent <-subset (datataxon,X.CdTaxon.== arbretaxon[length(arbretaxon$V2),2])$X.TaxonParent.
    ii <- ii+1
    }
      if (j == 1){
        arbretaxonmerge <- arbretaxon
      }
    arbretaxonmerge <- merge(arbretaxonmerge,arbretaxon,all = T)
}

# 4. Restitution des branches taxons par espèce
arbretaxonmerge <- subset(arbretaxonmerge,V3<16 )
A <- matrix(NA,nrow=length(arbretaxonmerge$V1),ncol=length(unique(arbretaxonmerge$V3)))
A <- data.frame(A)
A[1:length(unique(arbretaxonmerge$V1)),1] <- as.character(unique(arbretaxonmerge$V1))
colnames(A)<- sort(unique(arbretaxonmerge$V3))

for (j in 1:length(tmpCdTAxon$CdTAxon)){
  arbretaxon <- data.frame()
  ii <- 0
  arbretaxon_parent <- subset (datataxon,X.CdTaxon.==tmpCdTAxon$CdTAxon[j] )$X.CdTaxon. 
  while ( !is.na(arbretaxon_parent) ){
    arbretaxon[1+ii,1] <-  subset(datataxon,X.CdTaxon.== arbretaxon_parent )$X.NomLatinTaxon
    arbretaxon[1+ii,2] <-subset (datataxon,X.CdTaxon.==arbretaxon_parent)$X.CdTaxon.
    arbretaxon[1+ii,3] <-subset (datataxon,X.CdTaxon.==arbretaxon_parent)$X.AbrTaxonomique.
    arbretaxon_parent <-subset (datataxon,X.CdTaxon.==arbretaxon[length(arbretaxon$V2),2])$X.TaxonParent.
    ii <- ii+1
  }
  arbretaxon <- subset(arbretaxon,V3<16 )
  A[which(A[,1] == arbretaxon$V1[1]),1:length(arbretaxon$V1)] <- arbretaxon$V1
}

A <- na.omit(A[,1:9])

# 5.1 Définition du niveau des noeuds
A$pathString <- paste(A$`13`,A$`4`,A$`3`,A$`2`,A$`1`,A$`0`,
                    sep = "/")
# 5.2 Noeuds de l'arbre					
Atree <- as.Node(A)

# 6 . Graphique
plot(as.dendrogram(Atree), center = T, horiz = T,xlim=c(100,-50),xaxt ='n',yaxt = 'n')
text(x=90,y=length(A$`0`)+1,label="Embranchement",font = 2)
text(x=60,y=length(A$`0`)+1,label="Classe",font = 2)
text(x=40,y=length(A$`0`)+1,label="Famille",font = 2)
text(x=20,y=length(A$`0`)+1,label="Genre",font = 2)
text(x=0,y=length(A$`0`)+1,label="Espece",font = 2)
