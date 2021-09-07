{-# LANGUAGE ScopedTypeVariables #-}

module Theme.Theme ( background
                   , foreground
                   , brightBlack
                   , brightRed
                   , brightGreen
                   , brightYellow
                   , brightBlue
                   , brightMagenta
                   , brightCyan
                   , brightWhite
                   , brightGray
                   , darkBlack
                   , darkRed
                   , darkGreen
                   , darkYellow
                   , darkBlue
                   , darkMagenta
                   , darkCyan
                   , darkWhite
                   , darkGray
                   , coniferGreen
                   , goldenFizzYellow
                   , electricViolet
                   , ceruleanBlue
                   , pomegranate
                   , frolyPink
                   , selectiveYellow
                   , cyan
                   , pictonBlue
                   , myFont
                  --  , myFontGTK
                  --  , myBigFont
                  --  , myBoldFont
                  --  , myItalicFont
                   ) where

import           Prelude          (String)

-- X11 color names:
-- https://www.wikiwand.com/en/X11_color_names
-- Color Setting
background     :: String
background     = "#1e2127"
foreground     :: String
foreground     = "#FDFDFD"
-- foreground    = "" -- widget ?
brightBlack     :: String
brightBlack     = "#282A36" -- color0
brightRed       :: String
brightRed       = "#F37F97" -- color1
brightGreen     :: String
brightGreen     = "#5ADECD" -- color2
brightYellow    :: String
brightYellow    = "#F2A272" -- color3
brightBlue      :: String
brightBlue      = "#8897F4" -- color4
brightMagenta   :: String
brightMagenta   = "#C574DD" -- color5
brightCyan      :: String
brightCyan      = "#79E6F3" -- color6
brightWhite     :: String
brightWhite     = "#FDFDFD" -- color7
brightGray      :: String
brightGray      = "#C0C0C0"
darkBlack      :: String
darkBlack      = "#2c313a" -- color8
darkRed        :: String
darkRed        = "#FF4971" -- color9
darkGreen      :: String
darkGreen      = "#18E3C8" -- color10
darkYellow     :: String
darkYellow     = "#FF8037" -- color11
darkBlue       :: String
darkBlue       = "#556FFF" -- color12
darkMagenta    :: String
darkMagenta    = "#B043D1" -- color13
darkCyan       :: String
darkCyan       = "#3FDCEE" -- color14
darkWhite      :: String
darkWhite      = "#BEBEC1" -- color15
darkGray       :: String
darkGray       = "#848482"
--
-- name colors http://chir.ag/projects/name-that-color
coniferGreen      :: String
coniferGreen      = "#78ea59"
goldenFizzYellow  :: String
goldenFizzYellow  = "#ffff33"
electricViolet    :: String
electricViolet    = "#cc00ff"
ceruleanBlue      :: String
ceruleanBlue      = "#00a1f1"
pomegranate       :: String
pomegranate       = "#f65314"
frolyPink         :: String
frolyPink         = "#f7786b"
selectiveYellow   :: String
selectiveYellow   = "#fbbc05"
cyan              :: String
cyan              = "#00ffff"
pictonBlue        :: String
pictonBlue        = "#33bdf5"

myFont :: String
myFont = "xft:JetBrainsMono Nerd Font:weight=bold:pixelsize=14:antialias=true:hinting=true"
-- myFont = "xft:Hack:weight=bold:pixelsize=14:antialias=true:hinting=true"
-- myFontGTK :: String
-- myFontGTK = ""
-- myBigFont :: String
-- myBigFont = ""
-- myBoldFont :: String
-- myBoldFont = ""
-- myItalicFont :: string
-- myItalicFont = ""
