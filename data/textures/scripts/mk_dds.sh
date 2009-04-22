#!/bin/bash
set -eu

gimp -i -b '(load "process_textures.scm")' -b '(kam-batch-dds)' -b '(gimp-quit 0)'
