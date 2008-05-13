#!/bin/bash
latex main
latex main
bibtex main
latex main
dvips -t letter main.dvi -o main.ps
ps2pdf main.ps
