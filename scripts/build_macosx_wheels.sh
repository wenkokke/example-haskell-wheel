#!/bin/bash

eval "$(pyenv init -)"

declare -a python_versions
python_versions=("3.7" "3.8" "3.9" "3.10" "3.11")
for python_version in "${python_versions[@]}"; do
    pyenv shell "${python_version}"
    python -m pip install -r requirements-ci.txt
    python -m build --wheel --outdir=wheelhouselocal
done

mkdir -p wheelhouse
pipx run --spec delocate delocate-wheel --require-archs=x86_64,arm64 --wheel-dir=wheelhouse wheelhouselocal/*.whl
