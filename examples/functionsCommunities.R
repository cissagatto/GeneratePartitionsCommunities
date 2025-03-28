#################################################################################################
# BUILD AND TEST df$communities DETECTION METHODS                                                   #
# Copyright (C) 2021                                                                             #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #
# the License, or (at your option) any later version. This code is distributed in the hope       #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# PJacram of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################

##################################################################################################
# Script
##################################################################################################

##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/GeneratePartitionsCommunities"
setwd(FolderRoot)
FolderScripts = paste(FolderRoot, "/R", sep="")

##################################################################################################
# Plot -------------------------------------------------------------
#' Plot Graphs
#'
#' @family
#' @param community
#' @param graph_builted
#' @param title
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
plot_graph <- function(community, graph_builted, title){
  plot(community, graph_builted, vertex.size=18, edge.arrow.size=.5,
       vertex.color="gold", vertex.size=12,
       vertex.shape = "sphere", vertex.frame.color="orange",
       vertex.label.color="black", vertex.label.cex=0.7,
       vertex.label.family = "Times", vertex.label.font = 2,
       edge.color = "gray", edge.width = 0.5)
  title(cex.main= 0.8, main = title)
}


##################################################################################################
# PARTITION -------------------------------------------------------------
#' PARTITION
#'
#' @family
#' @param community
#' @param graph_builted
#' @param title
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
verifyPartition <- function(ds, tamanho, vertices){

  retorno = list()

  if(tamanho==1){
    retorno$numberComm = 1
    retorno$partition = "global"
    return(retorno)

  } else if(tamanho==vertices){
    retorno$numberComm = ds$labels
    retorno$partition = "local"
    return(retorno)

  } else {
    retorno$size = tamanho
    retorno$partition = "hybrid"
    return(retorno)
  }

  gc()
}

##################################################################################################
# HIERARCHICAL PARTITION ---------------------------------------------------
#' HIERARCHICAL PARTITION
#'
#' @family
#' @param community
#' @param graph_builted
#' @param title
#' @return h
#' h returns 0 if merges number equal to 0. It's not possible cut
#' h returns 1 if merges number equal to 1. It's not possible cut
#' h returns 2 if merges number greater than 1 and it's hierarchical
#' h returns 3 if it's not hierarchical
#'
#' @references
#'
#' @export
#'
#' @examples
#'
hierarchicalPartition <- function(comm, string1, string2, FolderSplit){

  setwd(FolderSplit)

  if(is_hierarchical(comm)==TRUE){

      num = nrow(comm$merges)
      labels = c(comm$names)

      if(num==1){
        cat("\nNúmero de merges é igual a 1. Não dá pra cortar")
        h = 1
        return(h)
      } else if(num<1) {
        cat("\nNúmero de merges é igual a 0. Não dá pra cortar")
        h = 0
        return(h)
      } else {
#        dend = as.dendJacram(comm)
#        hc = as.hclust(comm)
        #pdf(paste(string1, "-dendro", sep=""), width = 10, height = 8)
        #print(plot_dendJacram(comm))
        #dev.off()
        #cat("\n")f

        cut = c(0)
        particoes = data.frame(labels, cut)
        k = 2
        m = (comm$vcount-1)
        while(k<=m){
          cat("\n K = ", k)
          cut = data.frame(cut_at(comm, k))
          names(cut) = paste("particao", k, sep="")
          particoes = cbind(particoes, cut)
          k = k + 1
        } # end while

        setwd(FolderSplit)
        particoes = particoes[,-2]
        write.csv(particoes, paste(string2, "-hierarchical.csv", sep=""),
                  row.names = FALSE)
        h = 2
        return(h)
      }

  } else {
    cat("\nNot Hierarchical!\n")
    h = 3
    return(h)
  }

  gc()
} # end function



##################################################################################################
# CREATE DF -------------------------------------------------------------
#' CREATE DF
#'
#' @family
#' @param community
#' @param graph_builted
#' @param title
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
createDF <- function(){

  retorno = list()

  split = c(0)
  sparsification = c("")
  method = c(0)
  numberComm = c(0)
  modularity = c(0)
  hierarchical = c(0)
  partition = c(0)

  infoComm_final = data.frame(split, sparsification, method,
                              numberComm, modularity,
                              hierarchical, partition)

  split = c(0)
  sparsification = c("")
  method = c(0)
  hierarchical = c(0)
  partition = c(0)
  labels = c(0)
  groups = c(0)

  communities_final = data.frame(split, sparsification,
                                 method, hierarchical, partition,
                                 labels, groups)

  retorno$infoComm_final = infoComm_final
  retorno$communities_final = communities_final
  return(retorno)
}


##################################################################################################
# Spin Glass -------------------------------------------------------------
#' Execute Spin Glass
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeSpinGlass <- function(ds, graph_builted, title, fold, FolderSplit){

  cat("\n================================>SPINGLASS\n")

  retorno = list()

  df = createDF()

  setwd(FolderSplit)

  #############################################################################
  sc1 = spinglass.community(graph_builted,
                            weights = graph_builted$weights,
                            implementation = "orig", update.rule = "config")

  sc2 = spinglass.community(graph_builted,
                            weights = graph_builted$weights,
                            implementation = "orig", update.rule = "random")

  sc3 = spinglass.community(graph_builted,
                            weights = graph_builted$weights,
                            implementation = "orig", update.rule = "simple")

  sc4 = spinglass.community(graph_builted,
                            weights = graph_builted$weights,
                            implementation = "neg", update.rule = "config")

  sc5  = spinglass.community(graph_builted,
                             weights = graph_builted$weights,
                             implementation = "neg", update.rule = "random")

  sc6 = spinglass.community(graph_builted,
                            weights = graph_builted$weights,
                            implementation = "neg", update.rule = "simple")

  #############################################################################
  tamanho = sizes(sc1)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(sc1$vcount))
  partition = toString(partition$partition)

  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                              sparsification=title,
                                              method = "sg1",
                                              numberComm = tamanho,
                                              modularity = as.numeric(modularity(sc1)),
                                              hierarchical = is.hierarchical(sc1),
                                              partition = partition))

  df$communities = rbind(df$communities, data.frame(split = fold,
                                                    sparsification=title,
                                                    method = "sg1",
                                                    hierarchical = is.hierarchical(sc1),
                                                    partition = partition,
                                                    labels = sc1$names,
                                                    groups = sc1$membership))

  #############################################################################
  rm(tamanho, partition)
  tamanho = sizes(sc2)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(sc2$vcount))
  partition = toString(partition$partition)

  df$infoComm = rbind(df$infoComm, data.frame(split = fold, sparsification=title,
                                        method = "sg2",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(sc2)),
                                        hierarchical = is.hierarchical(sc2),
                                        partition = partition))

  df$communities = rbind(df$communities, data.frame(split = fold,
                                                    sparsification=title,
                                                    method = "sg2",
                                                    hierarchical = is.hierarchical(sc2),
                                                    partition = partition,
                                                    labels = sc2$names,
                                                    groups = sc2$membership))

  #############################################################################
  rm(tamanho, partition)
  tamanho = sizes(sc3)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(sc3$vcount))
  partition = toString(partition$partition)

  df$infoComm = rbind(df$infoComm, data.frame(split = fold, sparsification=title,
                                        method = "sg3",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(sc3)),
                                        hierarchical = is.hierarchical(sc3),
                                        partition = partition))

  df$communities = rbind(df$communities, data.frame(split = fold,
                                                    sparsification=title,
                                                    method = "sg3",
                                                    hierarchical = is.hierarchical(sc3),
                                                    partition = partition,
                                                    labels = sc3$names,
                                                    groups = sc3$membership))

  #############################################################################
  rm(tamanho, partition)
  tamanho = sizes(sc4)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(sc4$vcount))
  partition = toString(partition$partition)

  df$infoComm = rbind(df$infoComm, data.frame(split = fold, sparsification=title,
                                        method = "sg4",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(sc4)),
                                        hierarchical = is.hierarchical(sc4),
                                        partition = partition))

  df$communities = rbind(df$communities, data.frame(split = fold,
                                                    sparsification=title,
                                                    method = "sg4",
                                                    hierarchical = is.hierarchical(sc4),
                                                    partition = partition,
                                                    labels = sc4$names,
                                                    groups = sc4$membership))

  #############################################################################
  rm(tamanho, partition)
  tamanho = sizes(sc5)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(sc5$vcount))
  partition = toString(partition$partition)

  df$infoComm = rbind(df$infoComm, data.frame(split = fold, sparsification=title,
                                        method = "sg5",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(sc5)),
                                        hierarchical = is.hierarchical(sc5),
                                        partition = partition))

  df$communities = rbind(df$communities, data.frame(split = fold,
                                                    sparsification=title,
                                                    method = "sg5",
                                                    hierarchical = is.hierarchical(sc5),
                                                    partition = partition,
                                                    labels = sc5$names,
                                                    groups = sc5$membership))

  #############################################################################
  rm(tamanho, partition)
  tamanho = sizes(sc6)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(sc6$vcount))
  partition = toString(partition$partition)

  df$infoComm = rbind(df$infoComm, data.frame(split = fold, sparsification=title,
                                        method = "sg6",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(sc6)),
                                        hierarchical = is.hierarchical(sc6),
                                        partition = partition))

  df$communities = rbind(df$communities, data.frame(split = fold,
                                                    sparsification=title,
                                                    method = "sg6",
                                                    hierarchical = is.hierarchical(sc6),
                                                    partition = partition,
                                                    labels = sc6$names,
                                                    groups = sc6$membership))

  #############################################################################

  df$infoComm = df$infoComm[-1,]
  write.csv(df$infoComm, paste(title, "-sg-info.csv",
                               sep=""), row.names = FALSE)

  df$communities = df$communities[-1,]
  write.csv(df$communities, paste(title, "-sg-comm.csv", sep=""),
            row.names = FALSE)

  #############################################################################
  #pdf(paste(title,"-sg1", sep=""), width = 10, height = 8)
  #print(plot_graph(sc1, graph_builted, "orig-config"))
  #dev.off()
  #cat("\n")

  #pdf(paste(title,"-sg2", sep=""), width = 10, height = 8)
  #print(plot_graph(sc2, graph_builted, "orig-random"))
  #dev.off()
  #cat("\n")

  #pdf(paste(title,"-sg3", sep=""), width = 10, height = 8)
  #print(plot_graph(sc3, graph_builted, "orig-simples"))
  #dev.off()
  #cat("\n")

  #pdf(paste(title,"-sg4", sep=""), width = 10, height = 8)
  #print(plot_graph(sc4, graph_builted, "neg-config"))
  #dev.off()
  #cat("\n")

  #pdf(paste(title,"-sg5", sep=""), width = 10, height = 8)
  #print(plot_graph(sc5, graph_builted, "neg-random"))
  #dev.off()
  #cat("\n")

  #pdf(paste(title,"-sg6", sep=""), width = 10, height = 8)
  #print(plot_graph(sc6, graph_builted, "neg-simple"))
  #dev.off()
  #cat("\n")


  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()

}

##################################################################################################
# Edge Betweenness -------------------------------------------------------------
#' Execute Edge Betweenness
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeEdgeBetweenness <- function(ds, graph_builted, title, fold, FolderSplit){

  cat("\n================================>EDGE BETWEENNESS\n")

  retorno = list()

  df = createDF()

  ######################################################################
  setwd(FolderSplit)

  eb = cluster_edge_betweenness(graph_builted,
                                weights = graph_builted$weights,
                                edge.betweenness = TRUE,
                                merges = TRUE,
                                bridges = TRUE,
                                modularity = TRUE,
                                membership = TRUE)

  #######################################################################
  tamanho = sizes(eb)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(eb$vcount))
  partition = toString(partition$partition)

  #######################################################################
  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                        sparsification = title,
                                        method = "eb",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(eb)),
                                        hierarchical = is.hierarchical(eb),
                                        partition = partition))

  ############################################################################3
  df$communities = rbind(df$communities, data.frame(split = fold,
                                              sparsification=title,
                                              method = "eb",
                                              hierarchical = is.hierarchical(eb),
                                              partition = partition,
                                              labels = eb$names,
                                              groups = eb$membership))

  #############################################################################
  nome1 = paste(title,"-eb", sep="")
  nome2 = paste(title, "-eb-partitions", sep="")
  res = hierarchicalPartition(eb, nome1, nome2, FolderSplit)
  cat("\n", res, "\n")

  #############################################################################
  #pdf(paste(title,"-eb", sep=""), width = 10, height = 8)
  #print(plot_graph(eb, graph_builted, "eb"))
  #dev.off()
  #cat("\n")

  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()

}


##################################################################################################
# Label Propagation -------------------------------------------------------------
#' Execute Label Propagation
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeLabelPropagation <- function(ds, graph_builted, title, fold, FolderSplit){

cat("\n================================>LABEL PROPAGATION\n")

  retorno = list()

  df = createDF()

  ######################################################################
  setwd(FolderSplit)

  lp = cluster_label_prop(graph_builted,
                          weights = graph_builted$weights)

  #######################################################################
  tamanho = sizes(lp)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(lp$vcount))
  partition = toString(partition$partition)

  ##############################################################################
  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                        sparsification = title,
                                        method = "lp",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(lp)),
                                        hierarchical = is.hierarchical(lp),
                                        partition = partition))

  ############################################################################3
  df$communities = rbind(df$communities, data.frame(split = fold,
                                              sparsification=title,
                                              method = "lp",
                                              hierarchical = is.hierarchical(lp),
                                              partition = partition,
                                              labels = lp$names,
                                              groups = lp$membership))

  #############################################################################
  nome1 = paste(title,"-lp", sep="")
  nome2 = paste(title, "-lp-partitions", sep="")
  res = hierarchicalPartition(lp, nome1, nome2, FolderSplit)
  cat("\n", res, "\n")

  #############################################################################
  #pdf(paste(title,"-lp", sep=""), width = 10, height = 8)
  #print(plot_graph(lp, graph_builted, "lp"))
  #dev.off()
  #cat("\n")

  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()
}


##################################################################################################
# Walk Trap -------------------------------------------------------------
#' Execute Walk Trap
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeWalkTrap <- function(ds, graph_builted, title, fold, FolderSplit){

cat("\n================================>WALKTRAP\n")

  retorno = list()

  df = createDF()

  ######################################################################
  setwd(FolderSplit)

  # default é 4 ok!
  wt = cluster_walktrap(graph_builted,
                        weights = graph_builted$weights,
                        steps = 4,
                        merges = TRUE,
                        modularity = TRUE,
                        membership = TRUE)


  #######################################################################
  tamanho = sizes(wt)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(wt$vcount))
  partition = toString(partition$partition)

  #######################################################################
  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                        sparsification = title,
                                        method = "wt",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(wt)),
                                        hierarchical = is.hierarchical(wt),
                                        partition = partition))

  #############################################################################
  nome1 = paste(title,"-wt", sep="")
  nome2 = paste(title, "-wt-partitions", sep="")
  res = hierarchicalPartition(wt, nome1, nome2, FolderSplit)
  cat("\n", res, "\n")

  ############################################################################3
  df$communities = rbind(df$communities, data.frame(split = fold,
                                              sparsification=title,
                                              method = "wt",
                                              hierarchical = is.hierarchical(wt),
                                              partition = partition,
                                              labels = wt$names,
                                              groups = wt$membership))

  #############################################################################
  #pdf(paste(title,"-wt", sep=""), width = 10, height = 8)
  #print(plot_graph(wt, graph_builted, "wt"))
  #dev.off()
  #cat("\n")

  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()
}

##################################################################################################
# LEADING EIGENVECTOR -------------------------------------------------------------
#' Execute LEADING EIGENVECTOR
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeLeadingEigenVector <- function(ds, graph_builted, title, fold, FolderSplit){

cat("\n================================>LEADING EIGEN VECTOR\n")

  retorno = list()

  df = createDF()

  ######################################################################
  setwd(FolderSplit)
  #arpack_defaults$ncv = 5
  #arpack_defaults$maxiter = 1000
  le = cluster_leading_eigen(graph_builted,
                             weights = graph_builted$weights,
                             option = arpack_defaults)


  #######################################################################
  tamanho = sizes(le)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(le$vcount))
  partition = toString(partition$partition)

  #######################################################################
  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                        sparsification = title,
                                        method = "le",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(le)),
                                        hierarchical = is.hierarchical(le),
                                        partition = partition))

  ############################################################################
  df$communities = rbind(df$communities, data.frame(split = fold,
                                              sparsification=title,
                                              method = "le",
                                              hierarchical = is.hierarchical(le),
                                              partition = partition,
                                              labels = le$names,
                                              groups = le$membership))

  #############################################################################
  nome1 = paste(title,"-le", sep="")
  nome2 = paste(title, "-le-partitions", sep="")
  res = hierarchicalPartition(le, nome1, nome2, FolderSplit)
  cat("\n", res, "\n")

  #############################################################################
  #pdf(paste(title,"-le", sep=""), width = 10, height = 8)
  #print(plot_graph(le, graph_builted, "le"))
  #dev.off()
  #cat("\n")

  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()
}


##################################################################################################
# optimal -------------------------------------------------------------
#' Execute optimal
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeOptimal <- function(ds, graph_builted, title, fold, FolderSplit){

cat("\n================================>OPTIMAL\n")

  retorno = list()

  df = createDF()

  ######################################################################
  setwd(FolderSplit)

  op = cluster_optimal(graph_builted,
                       weights = graph_builted$weights)


  #######################################################################
  tamanho = sizes(op)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(op$vcount))
  partition = toString(partition$partition)

  #######################################################################
  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                        sparsification = title,
                                        method = "op",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(op)),
                                        hierarchical = is.hierarchical(op),
                                        partition = partition))

  ############################################################################3
  df$communities = rbind(df$communities, data.frame(split = fold,
                                              sparsification=title,
                                              method = "op",
                                              hierarchical = is.hierarchical(op),
                                              partition = partition,
                                              labels = op$names,
                                              groups = op$membership))

  #############################################################################
  nome1 = paste(title,"-op", sep="")
  nome2 = paste(title, "-op-partitions", sep="")
  res = hierarchicalPartition(op, nome1, nome2, FolderSplit)
  cat("\n", res, "\n")

  #############################################################################
  #pdf(paste(title,"-op", sep=""), width = 10, height = 8)
  #print(plot_graph(op, graph_builted, "op"))
  #dev.off()
  #cat("\n")

  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()
}


##################################################################################################
# louvain  -------------------------------------------------------------
#' Execute LOUVAIN
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeLouvain <- function(ds, graph_builted, title, fold, FolderSplit){

cat("\n================================>LOUVAIN\n")

  retorno = list()

  df = createDF()

  ######################################################################
  setwd(FolderSplit)

  lv = cluster_louvain(graph_builted,
                       weights = graph_builted$weights)


  #######################################################################
  tamanho = sizes(lv)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(lv$vcount))
  partition = toString(partition$partition)

  #######################################################################
  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                        sparsification = title,
                                        method = "lv",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(lv)),
                                        hierarchical = is.hierarchical(lv),
                                        partition = partition))

  ############################################################################3
  df$communities = rbind(df$communities, data.frame(split = fold,
                                              sparsification=title,
                                              method = "lv",
                                              hierarchical = is.hierarchical(lv),
                                              partition = partition,
                                              labels = lv$names,
                                              groups = lv$membership))

  #############################################################################
  nome1 = paste(title,"-lv", sep="")
  nome2 = paste(title, "-lv-partitions", sep="")
  res = hierarchicalPartition(lv, nome1, nome2, FolderSplit)
  cat("\n", res, "\n")

  #############################################################################
  #pdf(paste(title,"-lv", sep=""), width = 10, height = 8)
  #print(plot_graph(lv, graph_builted, "lv"))
  #dev.off()
  #cat("\n")

  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()
}



##################################################################################################
# Fast Greedy  -------------------------------------------------------------
#' Execute Fast Greedy
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeFastGreedy <- function(ds, graph_builted, title, fold, FolderSplit){

cat("\n================================>FAST GREEDY\n")

  retorno = list()

  df = createDF()

  ######################################################################
  setwd(FolderSplit)

  g = simplify(graph_builted)

  fg = cluster_fast_greedy(g,
                           merges = TRUE,
                           modularity = TRUE,
                           membership = TRUE,
                           weights = graph_builted$weights)

  #######################################################################
  tamanho = sizes(fg)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(fg$vcount))
  partition = toString(partition$partition)

  #######################################################################
  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                        sparsification = title,
                                        method = "fg",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(fg)),
                                        hierarchical = is.hierarchical(fg),
                                        partition = partition))

  ############################################################################3
  df$communities = rbind(df$communities, data.frame(split = fold,
                                              sparsification=title,
                                              method = "fg",
                                              hierarchical = is.hierarchical(fg),
                                              partition = partition,
                                              labels = fg$names,
                                              groups = fg$membership))

  #############################################################################
  nome1 = paste(title,"-fg", sep="")
  nome2 = paste(title, "-fg-partitions", sep="")
  res = hierarchicalPartition(fg, nome1, nome2, FolderSplit)
  cat("\n", res, "\n")

  #############################################################################
  #pdf(paste(title,"-fg", sep=""), width = 10, height = 8)
  #print(plot_graph(fg, graph_builted, "fg"))
  #dev.off()
  #cat("\n")

  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()
}



##################################################################################################
# InfoMap  -------------------------------------------------------------
#' Execute InfoMap
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeInfoMap <- function(ds, graph_builted, title, fold, FolderSplit){

cat("\n================================>INFOMAP \n")

  retorno = list()

  df = createDF()

  ######################################################################
  setwd(FolderSplit)

  im = cluster_infomap(graph_builted, e.weights = graph_builted$weights,
                       nb.trials = 10, modularity = TRUE)

  #######################################################################
  tamanho = sizes(im)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(im$vcount))
  partition = toString(partition$partition)

  #######################################################################
  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                        sparsification = title,
                                        method = "im",
                                        numberComm = tamanho,
                                        modularity = as.numeric(modularity(im)),
                                        hierarchical = is.hierarchical(im),
                                        partition = partition))

  ############################################################################
  df$communities = rbind(df$communities, data.frame(split = fold,
                                              sparsification=title,
                                              method = "im",
                                              hierarchical = is.hierarchical(im),
                                              partition = partition ,
                                              labels = im$names,
                                              groups = im$membership))

  #############################################################################
  nome1 = paste(title,"-im", sep="")
  nome2 = paste(title, "-im-partitions", sep="")
  res = hierarchicalPartition(im, nome1, nome2, FolderSplit)
  cat("\n", res, "\n")

  #############################################################################
  #pdf(paste(title,"-im", sep=""), width = 10, height = 8)
  #print(plot_graph(im, graph_builted, "im"))
  #dev.off()
  #cat("\n")

  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()
}

##################################################################################################
# LEIDEN  -------------------------------------------------------------
#' Execute LEIDEN
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
executeLeiden <- function(ds, graph_builted, title, fold, FolderSplit){

cat("\n================================>LEIDEN\n")

  retorno = list()

  df = createDF()

  ######################################################################
  setwd(FolderSplit)

  ld = cluster_leiden(graph_builted,
                      objective_function = "modularity",
                      weights = graph_builted$weights)


  #######################################################################
  tamanho = sizes(ld)
  tamanho = as.numeric(length(tamanho))
  partition = verifyPartition(ds, tamanho, as.numeric(ld$vcount))
  partition = toString(partition$partition)

  #######################################################################
  df$infoComm = rbind(df$infoComm, data.frame(split = fold,
                                        sparsification = title,
                                        method = "ld",
                                        numberComm = tamanho,
                                        modularity = as.numeric(ld$quality),
                                        hierarchical = is.hierarchical(ld),
                                        partition = partition))

  ############################################################################
  df$communities = rbind(df$communities, data.frame(split = fold,
                                              sparsification=title,
                                              method = "ld",
                                              hierarchical = is.hierarchical(ld),
                                              partition = partition,
                                              labels = ld$names,
                                              groups = ld$membership))

  #############################################################################
  nome1 = paste(title,"-ld", sep="")
  nome2 = paste(title, "-ld-partitions", sep="")
  res = hierarchicalPartition(ld, nome1, nome2, FolderSplit)
  cat("\n", res, "\n")

  #############################################################################
  #pdf(paste(title,"-ld", sep=""), width = 10, height = 8)
  #print(plot_graph(ld, graph_builted, "ld"))
  #dev.off()
  #cat("\n")

  #############################################################################
  retorno$communities = df$communities
  retorno$infoComm = df$infoComm
  return(retorno)

  gc()
}



##################################################################################################
#' CHOOSE HIERARCHICAL
#'
#' @family
#' @param dataset_name
#' @param FolderResults
chooseHierarchical <- function(dataset_name, number_folds,
                               similarity, FolderResults){

  cat("\n==================>HIERÁRQUICO")

  f = 1
  #while(f <=  number_folds){
  chParalel <- foreach(f = 1:number_folds) %dopar% {

    ##################################################################################################
    # Configures the workspace according to the operating system                                     #
    ##################################################################################################
    FolderRoot = "~/GeneratePartitionsCommunities"
    setwd(FolderRoot)
    FolderScripts = paste(FolderRoot, "/R", sep="")

    ###############################################################################
    # Load sources                                                                #
    ###############################################################################
    cat("\nCarregando os sources\n")
    setwd(FolderScripts)
    source("libraries.R")

    setwd(FolderScripts)
    source("utils.R")

    ###############################################################################
    cat("\ncriando os diretórios: ")
    folder = createDirs2(dataset_name, FolderResults)

    createDF <- function(){
      retorno = list()
      split = c(0)
      sparsification = c("")
      method = c(0)
      numberComm = c(0)
      modularity = c(0)
      hierarchical = c(0)
      partition = c(0)
      infoComm_final = data.frame(split, sparsification, method,
                                  numberComm, modularity,
                                  hierarchical, partition)
      split = c(0)
      sparsification = c("")
      method = c(0)
      hierarchical = c(0)
      partition = c(0)
      labels = c(0)
      groups = c(0)
      communities_final = data.frame(split, sparsification,
                                     method, hierarchical, partition,
                                     labels, groups)
      retorno$infoComm_final = infoComm_final
      retorno$communities_final = communities_final
      return(retorno)
    }

    cat("\n\n------->Fold ", f)

    df_tr = createDF()
    df_knn = createDF()

    folder = createDirs2(dataset_name, FolderResults)

    ##################################################
    FolderSplit = paste(folder$FolderCommunities, "/Split-", f ,sep="")
    FolderPartSplit = paste(folder$FolderPartitions, "/Split-", f, sep="")
    if(dir.exists(FolderPartSplit)==FALSE){dir.create(FolderPartSplit)}

    pasta = paste(folder$FolderDataFrame, "/", dataset_name,
                  "/Split-", f, "/", similarity, sep="")
    setwd(pasta)
    knn = data.frame(read.csv("sparsification-knn-values.csv"))
    n_knn = nrow(knn)

    #########################################################
    # KNN
    i = 1
    while(i<=n_knn){
      cat("\n\tknn", i)

      FolderKnn = paste(FolderSplit, "/knn-", i ,sep="")
      if(dir.exists(FolderKnn)==FALSE){dir.create(FolderKnn)}

      FolderPK = paste(FolderPartSplit, "/knn-", i ,sep="")
      if(dir.exists(FolderPK)==FALSE){dir.create(FolderPK)}

      FolderDF = paste(FolderSplit, "/knn-", i ,sep="")

      # abrindo os arquivos com as informações
      setwd(FolderDF)
      info = data.frame(read.csv(paste("split-", f, "-knn-", i, "-info.csv", sep="")))
      com = data.frame(read.csv(paste("split-", f, "-knn-", i, "-comm.csv", sep="")))

      # pegando apenas os métodos hierárquicos
      info = data.frame(filter(info, hierarchical==1))

      if(nrow(info)==0){
        cat("\nNão tem [ nenhuma ] partição")

        split = f
        sparsification = paste("tr-", j, sep="")
        method = "none"
        numberComm = "none"
        modularity = 0
        hierarchical = 1
        partition = "none"

        knn_choosed =   data.frame(split, sparsification,
                                  method, numberComm, modularity,
                                  hierarchical, partition)

        df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)


      } else if(nrow(info)==1){
        cat("\nTem [ uma única ] partição")
        knn_choosed = info
        df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

      } else {
        cat("\nTem [ mais de uma ] partição")

        # qual é o maior valor de modularidade?
        maximo = info[which.max(info$modularity),]

        # encontrando outros métodos que tem exatamente o mesmo valor
        equal = data.frame(filter(info, modularity==maximo$modularity))
        n = nrow(equal)

        if(n==1){
          cat("\n\t\tn==1 ")
          # Se não houver comunidades com o mesmo valor de modularidade
          # então apenas escolha este mesmo
          knn_choosed = equal
          df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

        } else {
          cat("\nn>1")
          # Se houver mais de uma comunidade com o mesmo valor de
          # modularidade, então escolhe um apenas")
          x = round(n/2)
          knn_choosed = info[x,]
          df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)
        }
      }


      # qual é o método escolhido?
      a = toString(knn_choosed$method)

      # pegando os rótulos para criar a partição
      teste = data.frame(filter(com, method==a))

      # gather only the labels and the groups
      labels = teste$labels
      groups = teste$groups
      teste2 = data.frame(labels, groups)

      setwd(FolderPK)
      write.csv(teste2, paste("knn-", i, "-h-partition.csv", sep=""),
                row.names = FALSE)
      write.csv(knn_choosed, paste("knn-", i, "-h-choosed.csv", sep=""),
                row.names = FALSE)

      i = i + 1
      gc()
    }

    # salva o método para cada esparsificação calculada
    df_knn$infoComm_final = df_knn$infoComm_final[-1,]
    setwd(FolderPartSplit)
    write.csv(df_knn$infoComm_final, paste("fold-", f,
                                           "-knn-h-choosed.csv", sep=""),
              row.names = FALSE)


    #########################################################
    # THRESHOLD
    setwd(pasta)
    tr = data.frame(read.csv("sparsification-new-threshold.csv"))

    if(tr[1,2]=="vazio"){
      cat("\n NÃO TEM DATA FRAME")

    } else {

      n_tr = nrow(tr)
      j = 0
      while(j<n_tr){
        cat("\n\ntr", j)

        FolderTr = paste(FolderSplit, "/Tr-", j ,sep="")
        FolderPK = paste(FolderPartSplit, "/Tr-", j ,sep="")
        if(dir.exists(FolderPK)==FALSE){dir.create(FolderPK)}
        FolderDF = paste(FolderSplit, "/Tr-", j ,sep="")
        res = length(dir(FolderTr))

        if(res == 0){
          cat("\nA pasta está vazia!")
          split = f
          sparsification = paste("tr-", j, sep="")
          method = "none"
          numberComm = "none"
          modularity = 0
          hierarchical = 1
          partition = "none"

          tr_choosed =   data.frame(split, sparsification,
                                    method, numberComm, modularity,
                                    hierarchical, partition)

          df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)


        } else {

          setwd(FolderTr)
          info = data.frame(read.csv(paste("split-", f, "-tr-", j, "-info.csv", sep="")))
          com = data.frame(read.csv(paste("split-", f, "-tr-", j, "-comm.csv", sep="")))

          # separando o que é hierarquico
          info = data.frame(filter(info, hierarchical==1))

          if(nrow(info)==0){
            cat("\nNão tem nenhuma partição")

            cat("\nA pasta está vazia!")
            split = f
            sparsification = paste("tr-", j, sep="")
            method = "none"
            numberComm = "none"
            modularity = 0
            hierarchical = 1
            partition = "none"

            tr_choosed =   data.frame(split, sparsification,
                                      method, numberComm, modularity,
                                      hierarchical, partition)

            df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

          } else if(nrow(info)==0) {
            cat("\nTem [ uma única ] partição")
            tr_choosed = info
            df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

          } else {
            cat("\nTema [ mais de uma ] partição")

            # qual a maior modularidade?
            maximo = info[which.max(info$modularity),]

            # tem mais gente com esse mesmo valor?
            equal = data.frame(filter(info,modularity==maximo$modularity))
            n = nrow(equal)

            if(n==1){
              cat("\nn==1")
              tr_choosed = equal
              df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

            } else {
              cat("\nn>1")
              x = round(n/2)
              tr_choosed = info[x,]
              df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

            }

          }

          a = toString(tr_choosed$method)
          teste = data.frame(filter(com, method==a))

          # gather only the labels and the groups
          labels = teste$labels
          groups = teste$groups
          teste2 = data.frame(labels, groups)

          setwd(FolderPK)
          write.csv(teste2, paste("tr-", j, "-h-partition.csv", sep=""),
                    row.names = FALSE)
          write.csv(tr_choosed, paste("tr-", j, "-h-choosed.csv", sep=""),
                    row.names = FALSE)

        } # fim do else

        j = j + 1
        gc()
      } # fim do while

      setwd(FolderPartSplit)
      df_tr$infoComm_final = df_tr$infoComm_final[-1,]
      write.csv(df_tr$infoComm_final, paste("fold-", f, "-tr-h-choosed.csv", sep=""),
              row.names = FALSE)
      gc()
    } # fim do else

    #f = f + 1
    gc()

  } # fim do while

  gc()

} # fim da função


##################################################################################################
#' CHOOSE NON HIERARCHICAL
#'
#' @family
#' @param dataset_name
#' @param FolderResults
chooseNonHierarchical <- function(dataset_name, number_folds,
                                  similarity, FolderResults){

  cat("\n=======================>NÃO HIERÁRQUICO")

  f = 1
  #while(f <=  number_folds){
  cnhParalel <- foreach(f = 1:number_folds) %dopar% {

    ##################################################################################################
    # Configures the workspace according to the operating system                                     #
    ##################################################################################################
    FolderRoot = "~/GeneratePartitionsCommunities"
    setwd(FolderRoot)
    FolderScripts = paste(FolderRoot, "/R", sep="")

    ###############################################################################
    # Load sources                                                                #
    ###############################################################################
    setwd(FolderScripts)
    source("libraries.R")

    setwd(FolderScripts)
    source("utils.R")

    ###############################################################################
    folder = createDirs2(dataset_name, FolderResults)

    createDF <- function(){

      retorno = list()

      split = c(0)
      sparsification = c("")
      method = c(0)
      numberComm = c(0)
      modularity = c(0)
      hierarchical = c(0)
      partition = c(0)

      infoComm_final = data.frame(split, sparsification, method,
                                  numberComm, modularity,
                                  hierarchical, partition)

      split = c(0)
      sparsification = c("")
      method = c(0)
      hierarchical = c(0)
      partition = c(0)
      labels = c(0)
      groups = c(0)

      communities_final = data.frame(split, sparsification,
                                     method, hierarchical, partition,
                                     labels, groups)

      retorno$infoComm_final = infoComm_final
      retorno$communities_final = communities_final
      return(retorno)
    }

    cat("\n\n============================================")
    cat("\n------->Fold ", f)

    df_tr = createDF()
    df_knn = createDF()

    folder = createDirs2(dataset_name, FolderResults)

    ##################################################
    FolderSplit = paste(folder$FolderCommunities, "/Split-", f ,sep="")

    FolderPartSplit = paste(folder$FolderPartitions, "/Split-", f, sep="")
    if(dir.exists(FolderPartSplit)==FALSE){dir.create(FolderPartSplit)}

    pasta = paste(folder$FolderDataFrame, "/", dataset_name,
                  "/Split-", f, "/", similarity, sep="")
    setwd(pasta)
    knn = data.frame(read.csv("sparsification-knn-values.csv"))
    n_knn = nrow(knn)

    #########################################################
    # KNN
    i = 1
    while(i<=n_knn){
      cat("\n\tknn", i)

      FolderKnn = paste(FolderSplit, "/knn-", i ,sep="")
      if(dir.exists(FolderKnn)==FALSE){dir.create(FolderKnn)}

      FolderPK = paste(FolderPartSplit, "/knn-", i ,sep="")
      if(dir.exists(FolderPK)==FALSE){dir.create(FolderPK)}

      FolderDF = paste(FolderSplit, "/knn-", i ,sep="")

      # abrindo os arquivos com as informações
      setwd(FolderDF)
      info = data.frame(read.csv(paste("split-", f, "-knn-", i, "-info.csv", sep="")))
      com = data.frame(read.csv(paste("split-", f, "-knn-", i, "-comm.csv", sep="")))

      # pegando apenas os métodos não hierárquicos
      info2 = data.frame(filter(info, hierarchical==0))

      # pegando apenas as partições hibridas
      info3 = data.frame(filter(info2, partition=="hybrid"))

      if(nrow(info3)==0){
        cat("\nNão tem [ nenhuma ] partição híbrida")
        split = f
        sparsification = paste("tr-", j, sep="")
        method = "none"
        numberComm = "none"
        modularity = 0
        hierarchical = 0
        partition = "none"

        knn_choosed = data.frame(split, sparsification,
                                method, numberComm, modularity,
                                hierarchical, partition)

        df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

      } else if(nrow(info3)==1) {
        cat("\nTem [ uma única ] partição híbrida")
        knn_choosed = info3
        df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

      } else {
        cat("\nTem [ mais de uma ] partição híbrida")

        # qual é o maior valor de modularidade?
        maximo = info3[which.max(info3$modularity),]

        # encontrando outros métodos que tem exatamente o mesmo valor
        equal = data.frame(filter(info3, modularity==maximo$modularity))
        n = nrow(equal)

        if(n==1){
          cat("\nn==1")
          # Se não houver comunidades com o mesmo valor de modularidade
          # então apenas escolha este mesmo
          knn_choosed = equal
          df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

        } else {
          cat("\nn>1")
          # Se houver mais de uma comunidade com o mesmo valor de
          # modularidade, então escolhe um apenas")
          x = round(n/2)
          knn_choosed = info3[x,]
          df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)
        }

      }

      # qual é o método escolhido?
      a = toString(knn_choosed$method)

      # pegando os rótulos para criar a partição
      teste = data.frame(filter(com, method==a))

      # gather only the labels and the groups
      labels = teste$labels
      groups = teste$groups
      teste2 = data.frame(labels, groups)

      setwd(FolderPK)
      write.csv(teste2, paste("knn-", i, "-nh-partition.csv", sep=""),
                row.names = FALSE)
      write.csv(knn_choosed, paste("knn-", i, "-nh-choosed.csv", sep=""),
                row.names = FALSE)

      i = i + 1
      gc()
    }

    setwd(FolderPartSplit)
    df_knn$infoComm_final = df_knn$infoComm_final[-1,]
    write.csv(df_knn$infoComm_final, paste("fold-", f,
                                           "-knn-nh-choosed.csv", sep=""),
              row.names = FALSE)


    #########################################################
    # THRESHOLD
    ###############################################################
    pasta = paste(folder$FolderDataFrame, "/", dataset_name,
                  "/Split-", f, "/", similarity, sep="")
    setwd(pasta)
    tr = data.frame(read.csv("sparsification-new-threshold.csv"))
    #cat("\nTR=", nrow(tr))

    if(tr[1,2]=="vazio"){
      cat("\nNão há Tresholds válidos!")

    } else {

      n_tr = nrow(tr)
      j = 0
      while(j<n_tr){
        cat("\n\ntr", j)

        FolderTr = paste(FolderSplit, "/Tr-", j ,sep="")
        FolderPK = paste(FolderPartSplit, "/Tr-", j ,sep="")
        if(dir.exists(FolderPK)==FALSE){dir.create(FolderPK)}
        FolderDF = paste(FolderSplit, "/Tr-", j ,sep="")
        res = length(dir(FolderTr))

        if(res == 0){
          cat("\nA pasta está vazia!")
          split = f
          sparsification = paste("tr-", j, sep="")
          method = "none"
          numberComm = "none"
          modularity = 0
          hierarchical = 0
          partition = "none"

          tr_choosed = data.frame(split, sparsification,
                                  method, numberComm, modularity,
                                  hierarchical, partition)

          df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

        } else {
          setwd(FolderTr)
          info = data.frame(read.csv(paste("split-", f, "-tr-", j, "-info.csv", sep="")))
          com = data.frame(read.csv(paste("split-", f, "-tr-", j, "-comm.csv", sep="")))

          # pegando apenas os métodos não hierárquicos
          info2 = data.frame(filter(info, hierarchical==0))

          # pegando apenas as partições hibridas
          info3 = data.frame(filter(info2, partition=="hybrid"))

          if(nrow(info3)==0){
            cat("\nNão tem [ nenhuma ] partição híbrida")
            split = f
            sparsification = paste("tr-", j, sep="")
            method = "none"
            numberComm = "none"
            modularity = 0
            hierarchical = 0
            partition = "none"

            tr_choosed = data.frame(split, sparsification,
                                    method, numberComm, modularity,
                                    hierarchical, partition)

            df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

          } else if(nrow(info3)==1){
            cat("\nTem [ uma única ] partição híbrida")
            tr_choosed = info3
            df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

          } else {
            cat("\nTem [ mais de uma ] partição híbrida")

            # qual é o maior valor de modularidade?
            maximo = info3[which.max(info3$modularity),]

            # encontrando outros métodos que tem exatamente o mesmo valor
            equal = data.frame(filter(info3, modularity==maximo$modularity))
            n = nrow(equal)

            if(n==1){
              cat("\nn==1 ")
              # Se não houver comunidades com o mesmo valor de modularidade
              # então apenas escolha este mesmo
              tr_choosed = equal
              df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

            } else {
              cat("\nn>1")
              # Se houver mais de uma comunidade com o mesmo valor de
              # modularidade, então escolhe um apenas")
              x = round(n/2)
              tr_choosed = info3[x,]
              df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

            } # fim do if else

          } # fim do if else-if else

        } # fim do else

        # qual é o método escolhido?
        a = toString(tr_choosed$method)

        # pegando os rótulos para criar a partição
        teste = data.frame(filter(com, method==a))

        # gather only the labels and the groups
        labels = teste$labels
        groups = teste$groups
        teste2 = data.frame(labels, groups)

        setwd(FolderPK)
        write.csv(teste2, paste("tr-", j, "-nh-partition.csv", sep=""),
                  row.names = FALSE)
        write.csv(tr_choosed, paste("tr-", j, "-nh-choosed.csv", sep=""),
                  row.names = FALSE)

        j = j + 1
        gc()
      }

      setwd(FolderPartSplit)
      df_tr$infoComm_final = df_tr$infoComm_final[-1,]
      write.csv(df_tr$infoComm_final, paste("fold-", f,
                                            "-tr-nh-choosed.csv", sep=""),
                row.names = FALSE)
      gc()
    } # fim do else

    #f = f + 1
    gc()

  } # fim do while

  gc()

} # fim da função



moveFilesPartitions <- function(dataset_name, number_folds, FolderResults){

  f = 1
  mfParalel <- foreach(f = 1:number_folds) %dopar% {
  #while(f<=10){

    cat("\nFold ", f)

    folder = createDirs2(dataset_name, FolderResults)

    ##################################################
    FolderRepSpl = paste(folder$FolderDataFrame, "/", dataset_name,
                         "/Split-", f, sep="")
    setwd(FolderRepSpl)
    knn = data.frame(read.csv("sparsification-knn-values.csv"))
    n_knn = nrow(knn)

    tr = data.frame(read.csv("sparsification-new-threshold.csv"))
    n_tr = nrow(tr)

    ##################################################
    FolderRepSpl = paste(folder$FolderReport, "/", dataset_name,
                         "/Split-", f, sep="")

    #########################################################
    destino = paste(folder$FolderPartitions, "/", dataset_name, sep="")
    if(dir.exists(destino)==FALSE){dir.create(destino)}

    destino1 = paste(destino, "/Split-", f, sep="")
    if(dir.exists(destino1)==FALSE){dir.create(destino1)}

    comando = paste("cp -r ", paste(FolderRepSpl, "/fold-",
                                    f, "-knn-h-choosed.csv", sep="")
                    , " ", destino1, sep="")
    print(system(comando))

    comando = paste("cp -r ", paste(FolderRepSpl, "/fold-",
                                    f, "-knn-nh-choosed.csv", sep="")
                    , " ", destino1, sep="")
    print(system(comando))


    if(tr[1,2]=="vazio"){
      cat("\n não tem data frame")
    } else {
      comando = paste("cp -r ", paste(FolderRepSpl, "/fold-", f, "-tr-h-choosed.csv", sep="")
                      , " ", destino1, sep="")
      print(system(comando))

      comando = paste("cp -r ", paste(FolderRepSpl, "/fold-", f, "-tr-nh-choosed.csv", sep="")
                      , " ", destino1, sep="")
      print(system(comando))
    }


    ################################333
    # KNN
    i = 1
    while(i<=n_knn){

      cat("\nknn\t", i)

      FolderKnn = paste(FolderRepSpl, "/knn-", i ,sep="")
      setwd(FolderKnn)

      ########################################################################################################################
      #cat("\n Copy to Folder Partition")

      destino2 = paste(destino1, "/knn-", i, sep="")
      if(dir.exists(destino2)==FALSE){dir.create(destino2)}

      origem1 = paste(FolderKnn, "/knn-", i, "-h-partition.csv", sep="")
      origem2 = paste(FolderKnn, "/knn-", i, "-nh-partition.csv", sep="")
      origem3 = paste(FolderKnn, "/knn-", i, "-eb-partitions-hierarchical.csv", sep="")
      origem4 = paste(FolderKnn, "/knn-", i, "-fg-partitions-hierarchical.csv", sep="")
      origem5 = paste(FolderKnn, "/knn-", i, "-wt-partitions-hierarchical.csv", sep="")

      comando = paste("cp -r ", origem1, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem2, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem3, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem4, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem5, " ", destino2, sep="")
      print(system(comando))

      i = i + 1
      gc()
    } # fim do knn


    #########################################################
    # THRESHOLD
    if(tr[1,2]=="vazio"){
      cat("\n NÃO TEM DATA FRAME")
    } else {

    n_tr = nrow(tr)
    j = 0
    while(j<n_tr){

      cat("\ntr\t", j)

      FolderTr = paste(FolderRepSpl, "/Tr-", j ,sep="")
      setwd(FolderTr)

      ########################################################################################################################
      #cat("\n Copy to Folder Partition")

      destino2 = paste(destino1, "/Tr-", j, sep="")
      if(dir.exists(destino2)==FALSE){dir.create(destino2)}

      origem1 = paste(FolderTr, "/tr-", j, "-h-partition.csv", sep="")
      origem2 = paste(FolderTr, "/tr-", j, "-nh-partition.csv", sep="")
      origem3 = paste(FolderTr, "/tr-", j, "-eb-partitions-hierarchical.csv", sep="")
      origem4 = paste(FolderTr, "/tr-", j, "-fg-partitions-hierarchical.csv", sep="")
      origem5 = paste(FolderTr, "/tr-", j, "-wt-partitions-hierarchical.csv", sep="")

      comando = paste("cp -r ", origem1, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem2, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem3, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem4, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem5, " ", destino2, sep="")
      print(system(comando))

      j = j + 1
      gc()
      } # fim

    }

    gc()
  }

}



juntaDFs <- function(dataset_name, number_folds, similarity, FolderResults){

  folder = createDirs2(dataset_name, FolderResults)
  resumeKNN = data.frame()
  resumeTR = data.frame()

  f = 1
  while(f<=10){
    cat("\nFOLD: ", f)
    ##################################################
    FolderRepSpl = paste(folder$FolderCommunities, "/Split-", f, sep="")
    setwd(FolderRepSpl)

    nome = paste("fold-",f,"-", similarity, "-conectado-knn.csv", sep="")
    resume = data.frame(read.csv(nome))
    resumeKNN = rbind(resumeKNN, resume)

    nome = paste("fold-",f,"-", similarity, "-conectado-trh.csv", sep="")
    resume = data.frame(read.csv(nome))
    resumeTR= rbind(resumeTR, resume)


    f = f + 1
    gc()
  } # fim do while

  setwd(folder$FolderPartitions)
  write.csv(resumeTR, "All-Conectado-TR.csv", row.names = FALSE)
  write.csv(resumeKNN, "All-Conectado-KNN.csv", row.names = FALSE)

}

# VERIFICAR QUEM OBTEVE MELHOR MODULARIDADE
# modularidade negativa um vértice por comunidade
# modularidade positiva
# modularidade zero é partição global

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
