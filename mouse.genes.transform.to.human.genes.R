mouse.to.human = getLDS(attributes = c("ensembl_gene_id"),
	filters = "ensembl_gene_id", values = x , mart = mouse, attributesL = c("hgnc_symbol"), martL = human, uniqueRows=T)
human.to.mouse = getLDS(attributes = c("hgnc_symbol"),
	filters = "hgnc_symbol", values = x , mart = human, attributesL = c("mgi_symbol"), martL = mouse, uniqueRows=T)
