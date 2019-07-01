TARGET=rapport

.PHONY: paper
$(TARGET).pdf: *.tex *.sty
	TEXINPUTS=.//:style//: BSTINPUTS=.: BIBINPUTS=.: latexmk -pdf -file-line-error -synctex=1 ${TARGET}

spell:
	for f in $(wildcard *.tex) ; do aspell -d en_GB -p aspell.dict -c $$f ; done

.PHONY: clean
clean:
	rm -f $(TARGET).{log,aux,bbl,blg,pdf,out,fls,fdb_latexmk,synctex.gz,rel}

# vi:set noexpandtab:

