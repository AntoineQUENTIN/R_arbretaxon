#Package nécessaire:
if(!require(data.tree)){  install.packages("data.tree");  require(data.tree)} 
#Arbre du sandre
#http://mdm.sandre.eaufrance.fr/?q=mdm_sandre/treetax/htmlbranchearbre/0
#Dictionnaire des donnÃ©es
#http://sandre.eaufrance.fr/ftp/documents/fr/ddd/tax/2002-1/sandre_dictionnaire_TAX_2002-1.pdf

# 1. Lecture des données taxonomiques sur le site du SANDRE
      # /!\ il faut tenir à jour l'url du SANDRE régulièrement 
csv.url <- "http://services.sandre.eaufrance.fr/References/1.3.0/References.php?CdReferentiel=APT&Filter=%3CFilter%3E%3CStatut%3E1%3C/Statut%3E%3CStatut%3E0%3C/Statut%3E%3C/Filter%3E&request=getReferenceElements&version=1.3.0&service=References&outputSchema=http://xml.sandre.eaufrance.fr/scenario/referentiel/3.1/sandre_sc_referentiel.xsd&outputFormat=sandre/simplexml"
datataxon <-read.csv(csv.url,sep=";",header = T,encoding = "UTF-8")

# 2. Liste des codes Taxons des espéces à dessiner
      # exemple 1
tmpCdTAxon <- data.frame(CdTAxon= c("866","1547","2010","1245","865"))
      # exemple 2
#tmpCdTAxon <- data.frame(CdTAxon= c("5128", "2050", "866","2071" ,"2110" ,"2113", "2117","2125", "2133" ,"2137", "2167", "2177", "2193" ,"2203"))



# 3. Recherche en cascade des codes Taxon Parents
arbretaxonmerge <- data.frame()
for (j in 1:length(tmpCdTAxon$CdTAxon)){
  arbretaxon <- data.frame()
  ii <- 0
  arbretaxon_parent <- paste(subset (datataxon,X.CdAppelTaxon.==as.character(tmpCdTAxon$CdTAxon[j]) )$X.CdAppelTaxon.)
  while ( arbretaxon_parent!="" ){
    arbretaxon[1+ii,1] <-paste(subset(datataxon,X.CdAppelTaxon.== arbretaxon_parent )$X.NomLatinAppelTaxon.)
    arbretaxon[1+ii,2] <-paste(subset (datataxon,X.CdAppelTaxon.== arbretaxon_parent)$X.CdAppelTaxon.)
    arbretaxon[1+ii,3] <-paste(subset (datataxon,X.CdAppelTaxon.== arbretaxon_parent)$X.AbTax.)
    arbretaxon_parent <- paste(subset (datataxon,X.CdAppelTaxon.== arbretaxon[length(arbretaxon$V2),2])$X.AppelTaxonParent.CdAppelTaxon.)
    ii <- ii+1
    }
      if (j == 1){
        arbretaxonmerge <- arbretaxon
      }
    arbretaxonmerge <- merge(arbretaxonmerge,arbretaxon,all = T)
}

# 4. Restitution des branches taxons par espéce
arbretaxonmerge <- subset(arbretaxonmerge,V3<16 )
A <- matrix(NA,nrow=length(arbretaxonmerge$V1),ncol=16)
A <- data.frame(A); colnames(A )<-15:0
A[1:length(unique(arbretaxonmerge$V1)),1] <- as.character(unique(arbretaxonmerge$V1))


for (j in 1:length(tmpCdTAxon$CdTAxon)){
  arbretaxon <- data.frame()
  ii <- 0
  arbretaxon_parent <- paste(subset (datataxon,X.CdAppelTaxon.==as.character(tmpCdTAxon$CdTAxon[j]) )$X.CdAppelTaxon.)
  while ( arbretaxon_parent!="" ){
    arbretaxon[1+ii,1] <-paste(subset(datataxon,X.CdAppelTaxon.== arbretaxon_parent )$X.NomLatinAppelTaxon.)
    arbretaxon[1+ii,2] <-paste(subset (datataxon,X.CdAppelTaxon.== arbretaxon_parent)$X.CdAppelTaxon.)
    arbretaxon[1+ii,3] <-paste(subset (datataxon,X.CdAppelTaxon.== arbretaxon_parent)$X.AbTax.)
    arbretaxon_parent <- paste(subset (datataxon,X.CdAppelTaxon.== arbretaxon[length(arbretaxon$V2),2])$X.AppelTaxonParent.CdAppelTaxon.)
    ii <- ii+1
  }
  arbretaxon <- subset(arbretaxon,V3<16 )
  A[which(A[,1] == arbretaxon$V1[1]),which(names(A)%in%as.character(arbretaxon$V3))] <- arbretaxon$V1
  
}
#On ne garde que ceux dont la chaine est compléte
A <- data.frame(A$`15`,A$`13`,A$`10`,A$`7`,A$`4`,A$`3`,A$`2`,A$`1`,A$`0`)
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
#J'autorise les lignes ou il y a au maximun 4 NA manquant
A <- delete.na(A, 4)

colnames(A)<- c("Espece","Genre","Famille","Ordre","sous-Classe","Classe","Super-Classe","Sous-Embranchement","Embranchement")
# 5.1 Définition du niveau des noeuds
A$pathString <- paste(A$Embranchement,A$Classe,A$Ordre,A$Famille,A$Genre,A$Espece,
                    sep = "/")
# 5.2 Noeuds de l'arbre					
Atree <- as.Node(A)
# 6 . Graphique
plot(as.dendrogram(Atree), center = T, horiz = T,xlim=c(100,-50),xaxt ='n',yaxt = 'n')
text(x=90,y=length(A$Embranchement)+1,label="Embranchement",font = 2)
text(x=60,y=length(A$Embranchement)+1,label="Classe",font = 2)
text(x=40,y=length(A$Embranchement)+1,label="Famille",font = 2)
text(x=20,y=length(A$Embranchement)+1,label="Genre",font = 2)
text(x=0,y=length(A$Embranchement)+1,label="Espece",font = 2)
