#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

mkdir -p /etc/keyd/

ln -s $SCRIPT_DIR/keyd.conf /etc/keyd/default.conf
