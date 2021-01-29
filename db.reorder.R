# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2018. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data 

# Reformat data

# Define function
db.reorder<-function(database, #the previously saved database, coming from db.build()
                     columns='all', #all / date / subject / field / value / source / by / verified
                     exclude=F, #values of fiels to be excluded from the output
                     save.STDOUT=F, #should the STOUT text output be saved to file?
                     filename='debugging' #the base filename
){
  print('Running db.reorder.R ...')
  print('This function expects a registry as produced by db.build()')
  db<-database
  
  print('Currently doing nothing, 9 different attempts to reorder are written in the script...')
  # Reorganize database
  #attempt 1
  db1<-db[substring(db[,1],1,7)!='Aromaat',]
  for(i in 1:ncol(db1)){
    replace<-is.na(db1[,i])
    db1[replace,i]<-0
  }
  row.names(db1)<-db1[,1]
  db1<-db1[,-1]
  h<-hclust(dist(db1[,-1]),method="mcquitty") #single/mcquitty/centroid
  plot(h)
  row.order <- hclust(dist(db[,-1]),method="mcquitty")$order
  col.order <- hclust(dist(t(db[,-1])),method="mcquitty")$order
  col.order<-col.order+1
  db2<-db[row.order, c(1,col.order)]
  write.table(db2,paste('../',today,'_reordered_db.csv',sep=''),row.names=F,sep=';')
  #attempt 2, based on https://www.r-bloggers.com/analyzing-networks-of-characters-in-love-actually/
  db2<-db[substring(db[,1],1,7)!='Aromaat',]
  row.names(db2)<-db2[,1]
  db2<-db2[,-1]
  for(i in 1:ncol(db2)){
    replace<-is.na(db2[,i])
    db2[!replace,i]<-1
    db2[replace,i]<-0
    db2[,i]<-as.numeric(db2[,i])
  }
  db2<-as.matrix(db2)
  h <- hclust(dist(db2, method = "manhattan"))
  plot(h)
  ordering <- h$labels[h$order]
  ordering
  db2<-db2[ordering,]
  cooccur <- db2 %*% t(db2)
  max(cooccur)
  hm<-heatmap(cooccur)
  plot(hm)
  ord<-rownames(db2)[hm$rowInd]
  heatmap(db2[ord,])
  row.order <- hclust(dist(db2,method="manhattan"))$order
  col.order <- hclust(dist(t(db2),method="manhattan"))$order
  heatmap(db2[ord,col.order],Rowv=NA,Colv=NA)
  #attempt 3, based on http://www.sthda.com/english/wiki/print.php?id=240
  db1<-db[substring(db[,1],1,7)!='Aromaat',]
  for(i in 1:ncol(db1)){
    replace<-is.na(db1[,i])
    db1[replace,i]<-0
  }
  row.names(db1)<-db1[,1]
  db1<-db1[,-1]
  if(!require(devtools)) install.packages("devtools")
  install.packages("cluster")
  install.packages('factoextra')
  library(factoextra)
  library(cluster)
  res.dist <- dist(db1, method = "euclidean")
  head(round(as.matrix(res.dist), 2))[, 1:6]
  fviz_dist(res.dist, lab_size = 8)
  res.dist <- dist(db2, method = "euclidean")
  head(round(as.matrix(res.dist), 2))[, 1:6]
  fviz_dist(res.dist, lab_size = 5)
  #attempt 4 https://cran.r-project.org/web/packages/seriation/seriation.pdf p40
  install.packages('seriation')
  library('seriation')
  seriate(db1)
  install.packages('fpc')
  #attempt 5 https://rdrr.io/cran/corrr/man/rearrange.html
  install.packages('corrr')
  library('corrr')
  x<-correlate(db2)
  rearrange(x,method='HC')
  #attempt 6 https://stackoverflow.com/questions/44805607/how-to-sort-and-order-a-dataframe-by-the-similarity-of-its-rows
  library('dplyr')
  a<-dplyr::arrange(db, paste('db[,',seq(2,411,1),']',sep=''))
  #attempt 7 http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
  pca<-prcomp(db2)
  fviz_eig(pca)
  fviz_pca_ind(pca,
               col.ind = "cos2", # Color by the quality of representation
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )
  fviz_pca_var(pca,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )
  fviz_pca_biplot(pca, repel = TRUE,
                  col.var = "#2E9FDF", # Variables color
                  col.ind = "#696969"  # Individuals color
  )
  eig.val <- get_eigenvalue(pca)
  eig.val[1:10,]
  res.var <- get_pca_var(pca)
  res.var$coord[1:10]          # Coordinates
  res.var$contrib[1:10]        # Contributions to the PCs
  res.var$cos2[1:10]           # Quality of representation 
  # Results for individuals
  res.ind <- get_pca_ind(pca)
  res.ind$coord          # Coordinates
  res.ind$contrib        # Contributions to the PCs
  res.ind$cos2           # Quality of representation 
  #attempt 8
  db2<-db2[,which(colSums(db2)!=0)]
  which(colSums(db2)==nrow(db2))
  fviz_nbclust(db2,FUNcluster = kmeans,method='wss')
  km.res<-kmeans(db2,4,nstart=25)
  fviz_cluster(km.res,db2,ellipse.type = "convex")
  mds <- db2 %>%
    dist() %>%          
    cmdscale() %>%
    as_tibble()
  clust <- km.res$cluster %>%
    as.factor()
  mds <- mds %>%
    mutate(groups = clust)
  colnames(mds) <- c("Dim.1", "Dim.2")
  ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
                    label = rownames(db2),
                    size = 1,
                    repel = TRUE)
  # Plot and color by groups
  ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
                    label = rownames(db2),
                    color = "groups",
                    palette = "jco",
                    size = 1, 
                    ellipse = TRUE,
                    ellipse.type = "convex",
                    repel = TRUE)
  #attempt 9
  nrow(unique(db2))
  db.dist<-dist(unique(db2))
  mds<-MASS::isoMDS(db.dist)
  plot(mds$points,type='n')
  text(mds$points, labels = as.character(1:nrow(db2)))
  db.sh <- MASS::Shepard(db.dist, mds$points)
  plot(db.sh, pch = ".")
  lines(db.sh$x, db.sh$yf, type = "S")
  #ik denk dat ik dit voorlopig laat rusten...
  
  return(db)
}