ZIP = matchx.zip

all:
	$(MAKE) -C src $@
	-$(MAKE) output.xlsx

ci: all
	git ci . -m "Worked on matchx."
	git push

dist:
	rm -f $(ZIP)
	zip $(ZIP) README.md dune-project src/*.ml src/dune
	cp $(ZIP) /tmp
	-rm -rf /tmp/matchx
	mkdir /tmp/matchx
	cd /tmp/matchx && unzip ../$(ZIP) && dune build

%.xlsx: %.csv
	ssconvert $< $@
