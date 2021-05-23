MAN = guile-tap.7
PDF = guile-tap.pdf
HTML = guile-tap.html
INFO = guile-tap.info

COMMONOPTIONS = --self-contained --number-sections --toc -f markdown

TEX_META = --highlight-style=monochrome
HTML_META = -c manual.css --highlight-style=monochrome

all: man html pdf info
man: $(MAN)
pdf: $(PDF)
html: $(HTML)
info: $(INFO)

guile-tap.7: guile-tap.mdwn
	pandoc -t man $(COMMONOPTIONS) -o $@ guile-tap.mdwn
guile-tap.pdf: guile-tap.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) -o $@ guile-tap.mdwn
guile-tap.html: guile-tap.mdwn
	pandoc -t html5 $(HTML_META) $(COMMONOPTIONS) -o $@ guile-tap.mdwn
guile-tap.texi: guile-tap.mdwn
	pandoc -t texinfo $(INFO_META) $(COMMONOPTIONS) -o $@ guile-tap.mdwn
guile-tap.info: guile-tap.texi
	makeinfo --no-validate --force $< -o $@

clean:
	rm -f $(HTML) $(PDF) $(MAN) $(INFO) *.texi *~

.PHONY: all man html info pdf clean
