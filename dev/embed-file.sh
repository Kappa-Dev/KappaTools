#!/usr/bin/env bash

sed -e '/JSSIM_CSS/ {' -e 'r js/JsSim.css' -e 'd' -e '}' | less
