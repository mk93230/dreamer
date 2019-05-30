library(dslabs)
data("tissue_gene_expression")
# from the below it is clear it is clear that it has 500 features/predictors and 189 total observations
dim(tissue_gene_expression$x)
tissue_gene_expression$x[189,500]
tissue_gene_expression$y
# lets run a Principal component analysis
pca <- prcomp(tissue_gene_expression$x)
summary(pca)