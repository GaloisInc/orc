
TARGET=hsorc

all : $(TARGET).ps

$(TARGET).pdf : $(TARGET).dvi
	dvipdf $< $@

$(TARGET).ps : $(TARGET).dvi
	dvips $< -o $@

# run latex twice to make sure that the refs are resolved.
$(TARGET).dvi : $(TARGET).tex
	latex $<
	latex $<

$(TARGET).tex : $(TARGET).lhs
	lhs2TeX --verb $< -o $@

clean :
	$(RM) $(TARGET).{aux,dvi,log,ps,pdf,tex}
