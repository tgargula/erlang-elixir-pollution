zip: clean
	zip -r Gargula_Tomasz.zip src/ test/ include/ README.md rebar.config rebar.lock LICENSE .gitignore

clean:
	rm *.zip src/*.beam test/*.beam *.beam || true