oslerLite.R: osler_top.R \
	R/osler_install.R \
	R/osler_package_table.R \
	DESCRIPTION \
	make_oslerLite.R 
	Rscript -e "source('make_oslerLite.R')"

clean: 
	rm oslerLite.R