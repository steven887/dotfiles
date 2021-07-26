--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( wallpaper
    , background, foreground, cursor
    , color0, color1, color2, color3, color4, color5, color6, color7
    , color8, color9, color10, color11, color12, color13, color14, color15
    ) where

wallpaper="/home/steven/steven_data/mywallpaper/404-not-found.jpg"

-- Special
background="#141012"
foreground="#edeae5"
cursor="#edeae5"

-- Colors
color0="#141012"
color1="#65646A"
color2="#827E7E"
color3="#ADABAB"
color4="#C4C0BD"
color5="#D5D0CD"
color6="#E5DED6"
color7="#edeae5"
color8="#a5a3a0"
color9="#65646A"
color10="#827E7E"
color11="#ADABAB"
color12="#C4C0BD"
color13="#D5D0CD"
color14="#E5DED6"
color15="#edeae5"
