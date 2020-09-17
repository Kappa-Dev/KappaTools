#!/bin/sh

set -e

VERSION=$1

curl -L https://github.com/Kappa-Dev/KappaTools/archive/v${VERSION}.tar.gz | tar xz
cd KappaTools-${VERSION}
opam pin add -n -y .
opam install --deps-only -y kappa-agents

## Copy/paste from https://github.com/pypa/python-manylinux-demo/blob/master/travis/build-wheels.sh

# Compile wheels
for PYBIN in /opt/python/*/bin; do
    make clean
    "${PYBIN}/pip" wheel . -w /io/wheelhouse/
done

# Bundle external shared libraries into the wheels
for whl in /io/wheelhouse/*x86_64.whl; do
    auditwheel repair "$whl" --plat manylinux2010_x86_64 -w /io/wheelhouse/
done
