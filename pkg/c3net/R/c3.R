#This file belongs to
#c3net: C3NET, <https://r-forge.r-project.org/projects/c3net/>
#This R package allows inferring regulatory networks from expression data using C3NET.
#The inferred network consists of only direct physical interactions.
## Copyright (C) August 2010 Gokmen Altay <altayscience@gmail.com>
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU LESSER GENERAL PUBLIC LICENSE
## as published by the Free Software Foundation; either version 3
## of the License, or any later version.
##
## This program is distributed WITHOUT ANY WARRANTY; 
## You can get a copy of the GNU LESSER GENERAL PUBLIC LICENSE
## from
## http://www.r-project.org/Licenses/LGPL-3
## See the licence information for the dependent package from
## igraph package itself.


c3 <- function(mim) # symetric MI matrix is input
{
diag(mim) <- 0
c3mim <- mim
c3mim[,] <- 0

numgene <- ncol(mim)

for(i in 1:numgene)
{
if(sum(mim[i,]) != 0)
{
ind <- which(mim[i,]==max(mim[i,]))[1]

c3mim[i,ind] <- mim[i,ind] 
c3mim[ind,i] <- mim[i,ind] 
}
} 

c3mim
}


