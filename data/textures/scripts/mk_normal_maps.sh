#!/bin/bash
set -eu

gimp -i -b '(load "mk_normal_maps.scm")' -b '(batch-normalmap)' -b '(gimp-quit 0)'
