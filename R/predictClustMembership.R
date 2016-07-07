#' predictClustMembership
#'
#' predictClustMembership will predict/assign cluster membership to new data based on a kmeans() cluster object
#'
#' @param clusterObject R object from kmeans()
#' @param newData data.frame containing the same variables as were used for computing the clusters in the `clusterObject`
#'
#' @return Vector of cluster membership assignment
#' @export
#'
#' @examples
#' df1 <- data.frame(x= rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
#' mod <- kmeans(df1, 2)
#' df2 <- data.frame(x= rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
#' predictClustMembership(mod, df2)
predictClustMembership <- function(clusterObject, newData) {
  perPerson <- function(i) {
    meansClust <- data.frame(clusterObject$centers)
    nms <- names(meansClust)
    names(meansClust) <- paste0(names(meansClust), "_c")
    EucData <- data.frame(row.names(clusterObject$centers),
                          meansClust,
                          newData[i,])
    perVar <- function(j) {
      k <- data.frame(k = (EucData[,j] - EucData[,paste0(j,"_c")]) ^ 2)
      return(k)
    }
    EucData$EucSum <-
      sqrt(rowSums(do.call(cbind, lapply(nms, perVar))))
    predCluster <- EucData[EucData$EucSum == min(EucData$EucSum),1][1]
    return(predCluster)
  }
  AllPredClusters <-
    do.call(c, lapply(1:length(newData[,1]), perPerson))
  return(AllPredClusters)
}