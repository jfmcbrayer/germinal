bin/germinal: *lisp *asd
	CL_SOURCE_REGISTRY=$(realpath $(dir germinal.asd)) sbcl --non-interactive \
		--eval '(ql:quickload :germinal)' \
		--eval '(asdf:make :germinal)'

.PHONY: clean
clean:
	rm -rf bin/

PREFIX = /usr/local
.PHONY: install
install: bin/germinal
	install -d $(PREFIX)/lib/germinal
	install bin/* $(PREFIX)/lib/germinal
	echo "#!/bin/sh\ncd $(PREFIX)/lib/germinal && exec ./germinal \$$@" \
		> $(PREFIX)/bin/germinal
	chmod +x $(PREFIX)/bin/germinal
	install -m 644 germinal.service /etc/systemd/system/
	install -d /etc/germinal
	cp --no-clobber config.toml /etc/germinal/
	install -d /var/gemini

.PHONY: uninstall
uninstall:
	rm -f -- $(PREFIX)/bin/germinal /etc/systemd/system/germinal.service
	rm -rf -- $(PREFIX)/lib/germinal
