all:	esquilax media

esquilax: .PHONY
	cd cia/esquilax; python3 setup.py build_ext --inplace

media: .PHONY
	python cia/media-src/build.py

clean: .PHONY
	cd cia/esquilax; python3 setup.py clean; rm -rf build/ *.so


.PHONY: