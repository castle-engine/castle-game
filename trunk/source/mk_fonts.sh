#!/bin/bash
set -eu

# Make fonts sources used only by "The Castle"

do_font2pascal ()
{
  font2pascal "$@" --dir .
}

do_font2pascal --font-name 'Bitstream Vera Sans' --grab-to bfnt --font-height -10
