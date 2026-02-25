---
title: "BIMM 143 – Protein B-factor Analysis"
author: "Sahar Kamali"
date: "1/25/2026"
output: pdf_document
---



library(bio3d)

# read in PDB files
s1 <- read.pdb("4AKE")  # kinase with drug
s2 <- read.pdb("1AKE")  # kinase no drug
s3 <- read.pdb("1E4Y")  # kinase with drug


# trim to chain A and CA atoms
s1.chainA <- trim.pdb(s1, chain = "A", elety = "CA")
s2.chainA <- trim.pdb(s2, chain = "A", elety = "CA")
s3.chainA <- trim.pdb(s3, chain = "A", elety = "CA")

# extract B-factors
s1.b <- s1.chainA$atom$b
s2.b <- s2.chainA$atom$b
s3.b <- s3.chainA$atom$b
s2.b <- s2.chainA$atom$b

# plot B-factors
plotb3(s1.b, sse = s1.chainA, typ = "l", ylab = "B-factor")
plotb3(s2.b, sse = s2.chainA, typ = "l", ylab = "B-factor")
plotb3(s3.b, sse = s3.chainA, typ = "l", ylab = "B-factor")


# compare B-factor trends
bf.mat <- rbind(s1.b, s2.b, s3.b)

bf.dist <- dist(bf.mat)
bf.hc <- hclust(bf.dist)

plot(bf.hc)





###Q1. What type of object is returned from the read.pdb() function?
A PDB object (a list-like R object) that stores the protein structure data (atoms, coordinates, etc.).

###Q2. What does the trim.pdb() function do?
It cuts down the PDB to only the parts we want (ex: only chain A and only CA atoms).

###Q3. What input parameter would turn off the marginal black and grey rectangles in the plots and what do they represent in this case?
They are secondary structure (helices/sheets) shown along the protein.To turn them off, we need to turn off the SSE display in plotb3()

###Q4. What would be a better plot to compare across the different proteins?
Put all proteins on one plot (overlay the B-factor lines) so we can compare directly.

###Q5. Which proteins are more similar to each other in their B-factor trends. How could you quantify this?
s1 and s2 are most similar (they cluster together first on dendrogram). we Quantify it by using dist() on the B-factor vectors and hclust() to cluster them.
?rbind= Stacks vectors or rows together into a matrix or in our case puts all B-factor vectors into one object.
?dist= Measures how different things are or in our case measures how different B-factor trends are between proteins.
?hclust= Groups similar things together using distances or in our case groups proteins with similar B-factor patterns.

###Q6. How would you generalize the original code above to work with any set of input protein structures?

```{r q6_function, message=FALSE}
library(bio3d)

# Function to analyze B-factors for any PDB structure
analyze_pdb <- function(pdb_id, chain = "A") {
  
  pdb <- read.pdb(pdb_id)
  pdb_trim <- trim.pdb(pdb, chain = chain, elety = "CA")
  
  b_factors <- pdb_trim$atom$b
  
  plotb3(
    b_factors,
    sse = pdb_trim,
    typ = "l",
    ylab = "B-factor",
    main = paste("B-factors for", pdb_id)
  )
  
  return(list(
    pdb_id = pdb_id,
    chain = chain,
    b_factors = b_factors
  ))
}






