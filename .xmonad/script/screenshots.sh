#!/bin/bash
# File              : screenshots.sh
# License           : MIT
# Author            : Steven Agustinus <steven87.ags@gmail.com>
# Date              : 19.07.2021
# Last Modified Date: 19.07.2021
# Last Modified By  : Steven Agustinus <steven87.ags@gmail.com>
# screenshots.sh
# Copyright (c) 2021 Steven Agustinus <steven87.ags@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

SCREENSHOTFOLDER="$HOME/screenshots"

FILE="${1}"
FILENAME="${FILE##*/}"
FILEBASE="${FILENAME%.*}"


convert "${FILE}" \
      -format 'roundrectangle 1,1 %[fx:w+4],%[fx:h+4] 15,15' \
      -write info:tmp.mvg \
      -alpha set -bordercolor none -border 3 \
      \( +clone -alpha transparent -background none \
         -fill white -stroke none -strokewidth 0 -draw @tmp.mvg \) \
      -compose DstIn -composite \
      \( +clone -alpha transparent -background none \
         -fill none -stroke "#0febff" -strokewidth 2 -draw @tmp.mvg \
         -fill none -stroke white -strokewidth 1 -draw @tmp.mvg \) \
      -compose Over -composite                \
      \( +clone -background black -shadow 80x30+0+0 \) +swap -background "#a7daff" -layers merge +repage "$SCREENSHOTFOLDER/${FILEBASE}.png"

notify-send -u low -t 2000 "${FILEBASE}.png saved."
rm -f tmp.mvg      # Cleanup of temporary file
