#!/bin/sh
sbcl  --non-interactive \
      --load germinal.asd \
      --eval '(ql:quickload :germinal)' \
      --eval '(asdf:make :germinal)'
