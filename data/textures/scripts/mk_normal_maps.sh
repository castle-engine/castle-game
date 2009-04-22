#!/bin/bash
set -eu

gimp -i -b '(load "process_textures.scm")' -b '(kam-batch-normalmap)' -b '(gimp-quit 0)'
