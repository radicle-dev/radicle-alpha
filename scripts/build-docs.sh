#!/bin/bash -

set -o nounset
set -o errexit

cd docs
pip install -r requirements.txt
make html
