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


makemim <- function(expdata) # symetric MI matrix is input
{
gnames<-rownames(expdata)
txdata <-t(expdata)
genenum <- ncol(txdata)
	rho2 <- cor(txdata)^2
	for(irh in 1:genenum) for(jrh in 1:genenum) 
		{ if(rho2[irh,jrh]>=1) {rho2[irh,jrh] <- 0.9999999} 
		}
	mim <- -0.5*log(1-rho2)
	mim <- abs(mim)  
	diag(mim)<-0
	rownames(mim) <-gnames
	colnames(mim) <-gnames

mim
}


