#This file belongs to
#c3net: C3NET, <http://www.bioconductor.org/packages/release/Software.html>
#This R package allows inferring regulatory networks from expression data using C3NET.
## Copyright (C) August 2009 Gokmen Altay <altayscience@gmail.com>
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU LESSER GENERAL PUBLIC LICENSE
## as published by the Free Software Foundation; either version 3
## of the License, or any later version.
##
## This program is distributed WITHOUT ANY WARRANTY; 
## You can get a copy of the GNU LESSER GENERAL PUBLIC LICENSE
## from
## http://www.r-project.org/Licenses/LGPL-3


netplot <- function(gnet)
{
gnet[gnet!=0]<-1

x <- graph.adjacency(gnet, mode="undirected", diag=FALSE)
y <- get.edgelist(x)
z <- graph.edgelist(y,directed=FALSE)
V(z)$label <- V(z)$name

tkplot(z)
}



