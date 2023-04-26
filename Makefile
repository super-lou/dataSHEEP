all: doc
.PHONY: doc install check

doc:
	R -e 'devtools::document()'

install:
	R -e "remotes::install_github('super-lou/dataSHEEP')"

check:
	R -e 'devtools::check()'

