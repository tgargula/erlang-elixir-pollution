zip: clean
	zip -r Gargula_Tomasz.zip src/ test/

clean:
	rm *.zip src/*.beam test/*.beam *.beam || true