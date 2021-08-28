{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase       #-}


module Main (main) where
--
-- If there are more than 3 explicit imports required a qualified import is used
-- If a type has only one constructor it is imported implicitly with (..)
--
--
import           XMonad                        hiding ( (|||) )

import qualified Theme.Theme                         as TH

-- hooks
import           XMonad.Hooks.EwmhDesktops            ( ewmh )
import           XMonad.Hooks.FloatNext               ( floatNextHook )
import qualified XMonad.Hooks.ManageDocks            as ManageDocks
import qualified XMonad.Hooks.ManageHelpers          as ManageHelpers
import qualified XMonad.Hooks.UrgencyHook            as UH
import           XMonad.Hooks.InsertPosition
import qualified XMonad.Hooks.DynamicLog             as DL


import           XMonad.Hooks.StatusBar              (StatusBarConfig,
                                                      statusBarProp, withSB)
import           XMonad.Hooks.StatusBar.PP           (PP (..), filterOutWsPP,
                                                      shorten', wrap,
                                                      xmobarAction,
                                                      xmobarBorder, xmobarColor,
                                                      xmobarFont, xmobarStrip)

-- import           XMonad.Hooks.DynamicIcons           (IconConfig (..), appIcon,
--                                                       dynamicIconsPP,
--                                                       iconsFmtReplace,
--                                                       iconsGetFocus,
--                                                       wrapUnwords)

-- layouts
import           XMonad.Layout.Circle                ( Circle(..) )
import           XMonad.Layout.HintedGrid            ( Grid(GridRatio) )
import           XMonad.Layout.LayoutCombinators     ( JumpToLayout(JumpToLayout) , (|||))
import           XMonad.Layout.Mosaic                ( mosaic )
import           XMonad.Layout.MultiToggle           ( Toggle(..) , mkToggle1 )
import           XMonad.Layout.MultiToggle.Instances ( StdTransformers ( MIRROR , NBFULL))

import           XMonad.Layout.NoBorders             ( noBorders )
import           XMonad.Layout.OneBig                ( OneBig(OneBig) )
import           XMonad.Layout.Reflect               ( REFLECTX(..) , REFLECTY(..))
import           XMonad.Layout.Renamed               ( Rename(Replace) , renamed)
import qualified XMonad.Layout.ResizableTile         as RTile
import           XMonad.Layout.SimpleDecoration      ( shrinkText )
import           XMonad.Layout.Spacing               ( Spacing(..), Border(..), spacingRaw )
import           XMonad.Layout.Spiral                ( spiral )
import qualified XMonad.Layout.Tabbed                as TB
import           XMonad.Layout.ThreeColumns          ( ThreeCol(ThreeColMid) )
import qualified XMonad.Layout.WindowNavigation      as Nav
import           XMonad.Layout.LayoutModifier        (ModifiedLayout)

-- utils
import qualified XMonad.Util.Cursor                  as Cursor
import           XMonad.Util.EZConfig                ( checkKeymap , mkKeymap )
import           XMonad.Util.Run                     ( spawnPipe )
import           XMonad.Util.Scratchpad              ( scratchpadFilterOutWorkspace )
import           XMonad.Util.WorkspaceCompare        ( getSortByIndex )
import           XMonad.Util.Ungrab                  ( unGrab )
import           XMonad.Util.ClickableWorkspaces     (clickablePP)
import           XMonad.Util.NamedScratchpad         (NamedScratchpad (NS),
                                                      customFloating,
                                                      namedScratchpadAction,
                                                      namedScratchpadManageHook,
                                                      scratchpadWorkspaceTag)

-- prompt
import qualified XMonad.Prompt                       as Prompt
import           XMonad.Prompt.ConfirmPrompt         ( confirmPrompt )
import           XMonad.Prompt.Input                 ( inputPromptWithCompl , (?+))
import           XMonad.Prompt.Shell                 ( shellPrompt )
import qualified XMonad.Prompt.Window                as WPrompt

-- actions
import qualified XMonad.Actions.CycleWS              as CycleWS
import           XMonad.Actions.CycleWindows         ( cycleRecentWindows )
import qualified XMonad.Actions.GridSelect           as GS
import           XMonad.Actions.GroupNavigation      ( historyHook )
import           XMonad.Actions.Warp                 ( warpToWindow )
import           XMonad.Actions.WorkspaceNames       ( swapWithCurrent )
import           XMonad.Actions.Submap               ( submap )
import           XMonad.Actions.UpdatePointer        ( updatePointer)

import           XMonad.Config.Desktop               ( desktopConfig )
import qualified XMonad.StackSet                     as W

import           Control.Monad                       ( liftM2 )
import qualified Data.List                           as L
import           Data.List.Split                     ( chunksOf )
import qualified Data.Map                            as M
import qualified Data.Text                           as T
import           Data.Char                           ( toLower )
import           Data.Ratio                          ( (%) )
import           System.Exit                         ( ExitCode(ExitSuccess) , exitWith)
import           System.IO                           ( Handle , hPutStrLn , hClose)
import           XMonad.Util.SpawnOnce               (spawnOnce)
import XMonad.Util.Loggers (xmobarColorL)

------------------------------------------------------------------------
-- Main
--
main :: IO ()
main = do
    -- -- xmobar
--    xmproc <- spawnPipe "xmobar"
--    xmonad $ ewmh $ UH.withUrgencyHook UH.NoUrgencyHook $ myConfig
        -- { logHook =  myLogHook xmproc >> historyHook }
     xmonad . withSB mySB $ ewmh $ UH.withUrgencyHook UH.NoUrgencyHook myConfig
        -- { logHook =  myXmobarLogHook xmproc >> historyHook }


------------------------------------------------------------------------
-- Config
--
myConfig = desktopConfig { borderWidth        = myBorderWidth
               , normalBorderColor  = myNormalBorderColor
               , focusedBorderColor = myFocusedBorderColor
               , focusFollowsMouse  = myFocusFollowsMouse
               , modMask            = myModMask
               , terminal           = myTerminal
               , workspaces         = myWorkspaces
               , mouseBindings      = myMouseBindings
               , keys               = myKeys
               , manageHook         = myManageHook
               , layoutHook         = myLayout
               , startupHook        = myStartupHook
               , handleEventHook    = myHandleEventHook
               }


------------------------------------------------------------------------
-- Default Apps
--
-- Capture Screen
myScreenCapture :: String
myScreenCapture = "$HOME/.scripts/screen_shot.sh"

myTerminal :: String
myTerminal = "alacritty"
-- myTerminal = "xfce4-terminal"

myTmuxTerminal :: String
myTmuxTerminal = myTerminal ++ " -e tmux attach"

-- Launcher
myLauncher :: String
-- myLauncher = "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "

myLauncher = "rofi -no-lazy-grab -show drun -modi run,drun,window"

-- Editor
myTextEditor :: String
-- myTextEditor = "emacsclient -c -a emacs"
myTextEditor = "nvim"

-- Browser
myBrowser :: String
myBrowser = "firefox"

-- File Manager
myFileManager :: String
myFileManager = "thunar"

-- Console File Manager
myConsoleFileManager :: String
myConsoleFileManager = myTerminal ++ " -e vifm"

-- myLayout
myPPLayout :: String -> String
myPPLayout x = case x of
    "Tall"            -> "\xf005" -- 
    "ThreeCol"        -> "\xfa6a" -- 頻
    "Mirror ThreeCol" -> "\xfa6e" -- 﩮
    "Spiral"          -> "\xf306" -- 
    "Mosaic"          -> "\xfa6d" -- 舘
    "Full"            -> "\xf5b5" -- 
    "Mirror Tall"     -> "\xf006" -- 
    "Mirror Mosaic"   -> "\xfa73" -- 侀
    "Tabbed"          -> "\xfd35" -- ﴵ
    "Mirror Spiral"   -> "\xfc06" -- ﰆ
    "Circle"          -> "\xe22e" -- 
    "OneBig"          -> "\xf286" -- 
    "HintedGrid"      -> "\xfb8a" -- ﮊ
    _                 -> x

myWorkspaces :: [String]
myWorkspaces = [show x | x <- [1..16]]
    -- [ "\xE907" -- 
    -- , "\xE905" -- 
    -- , "\xE92D" -- 
    -- , "\xE948" -- 
    -- , "\xE929" -- 
    -- , "\xE93E" -- 
    -- , "\xE926" -- 
    -- , "\xE932" -- 
    -- , "\xE97D" -- 
    -- , "\xE982" -- 
    -- , "\xE922" -- 
    -- , "\xE942" -- 
    -- ]

------------------------------------------------------------------------
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

-- Layouts
--
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'M-S-r') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =
    ManageDocks.avoidStruts
  -- Toggles
        $   mkToggle1 NBFULL
        $   mkToggle1 REFLECTX
        $   mkToggle1 REFLECTY
        $   mkToggle1 MIRROR
        $   Nav.configurableNavigation (Nav.navigateColor myNormalBorderColor)
        $
  -- Layouts
            name "Tall"       myTile
        ||| name "HintedGrid" myHintedGrid
        ||| name "Tabbed"     myTabbed
        ||| name "OneBig"     myOneBig
        ||| name "Circle"     Circle
        ||| name "Mosaic"     myMosaic
        ||| name "ThreeCol"   my3cmi
        ||| name "Spiral"     mySpiral
  where
    name n = renamed [Replace n] . mySpacing 8
    myTile       = RTile.ResizableTall 1 (3 / 100) (4 / 7) []
    my3cmi       = ThreeColMid 1 (3 / 100) (1 / 2)
    mySpiral     = spiral (6 / 7)
    myMosaic     = mosaic 2 [3, 2]
    myHintedGrid = GridRatio (4 / 3) False
    myOneBig     = OneBig (4 / 6) (4 / 6)
    myTabbed     = noBorders ( TB.tabbed shrinkText myTabConfig)

------------------------------------------------------------------------
-- Manage Hooks
--
-----------------------------------------------------------------------------
-- LOGHOOK
-----------------------------------------------------------------------------
myLogHook :: Handle -> X ()
myLogHook b =
    DL.dynamicLogWithPP (myXmobarPP b) >> updatePointer (0.5, 0.5) (0, 0)

-- xmobar
--
myXmobarPP :: Handle -> DL.PP
myXmobarPP h = DL.xmobarPP
    {
      DL.ppOutput  = hPutStrLn h . \s -> " " ++ s
    , DL.ppCurrent = DL.xmobarColor lightWhite "" . DL.wrap "(" ")"
    , DL.ppVisible = DL.xmobarColor lightWhite "" . DL.wrap "[" "]"
    , DL.ppUrgent  = DL.xmobarColor colorRed ""
    , DL.ppLayout  = myPPLayout
    , DL.ppTitle   = DL.shorten 30 . DL.wrap " " " "
    , DL.ppSort    = fmap (. scratchpadFilterOutWorkspace) getSortByIndex
    , DL.ppSep     = " "
    , DL.ppWsSep   = " "
    }

-- myXmobarLogHook :: Handle -> X ()
-- myXmobarLogHook b =
--     DL.dynamicLogWithPP (myXmobarPP b) >> updatePointer (0.5, 0.5) (0, 0)

----------------------------------------------------------------------
-- dzen
--
-- myDzenLogHook :: Handle -> X ()
-- myDzenLogHook h = DL.dynamicLogWithPP $ DL.def
--     {
--         DL.ppCurrent           =   DL.dzenColor "#ebac54" "#1B1D1E" . DL.pad
--       , DL.ppVisible           =   DL.dzenColor "white" "#1B1D1E" . DL.pad
--       , DL.ppHidden            =   DL.dzenColor "white" "#1B1D1E" . DL.pad
--       , DL.ppHiddenNoWindows   =   DL.dzenColor "#7b7b7b" "#1B1D1E" . DL.pad
--       , DL.ppUrgent            =   DL.dzenColor "#ff0000" "#1B1D1E" . DL.pad
--       , DL.ppWsSep             =   " "
--       , DL.ppSep               =   "  |  "
--       , DL.ppLayout            =   DL.dzenColor "#ebac54" "#1B1D1E" .
--                                 (\x -> case x of
--                                     -- "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
--                                     -- "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
--                                     -- "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
--                                     "Simple Float"              ->      "~"
--                                     _                           ->      x
--                                 )
--       , DL.ppTitle             =   (" " ++) . DL.dzenColor "white" "#1B1D1E" . DL.dzenEscape
--       , DL.ppOutput            =   hPutStrLn h
--     }
----------------------------------------------------------------------
-- xmobar
--
-- myXmobarPP :: Handle -> DL.PP
-- myXmobarPP h = DL.xmobarPP
--     { DL.ppOutput  = hPutStrLn h . \s -> " " ++ s
--     , DL.ppCurrent = DL.xmobarColor lightWhite "" . DL.wrap "(" ")"
--     , DL.ppVisible = DL.xmobarColor lightWhite "" . DL.wrap "[" "]"
--     , DL.ppUrgent  = DL.xmobarColor colorRed ""
--     , DL.ppLayout  = myPPLayout
--     , DL.ppTitle   = DL.shorten 30 . DL.wrap " " " "
--     , DL.ppSort    = fmap (. scratchpadFilterOutWorkspace) getSortByIndex
--     , DL.ppSep     = " "
--     , DL.ppWsSep   = " "
--     }



mySB :: StatusBarConfig
mySB = statusBarProp
  "xmobar"
  -- (clickablePP =<< dynamicIconsPP myIconConfig (filterOutWsPP [scratchpadWorkspaceTag] myXmobarPP))
  (clickablePP myXmobarPP)
 where
  myXmobarPP :: PP
  myXmobarPP = def
    { ppSep           = wrapSep " "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent       = blue  . xmobarBorder "Bottom" TH.base06 2 . currentWorkspace
    , ppVisible       = lowWhite .xmobarBorder "Bottom" TH.base07 2 . occupiedWorkspace
    , ppHidden        = lowWhite . occupiedWorkspace
    , ppHiddenNoWindows = lowWhite . emptyWorkspace
    , ppUrgent = magenta . urgentWorkspace
    , ppWsSep         = xmobarColor "" background "  "
    , ppTitle         = magenta . xmobarAction "xdotool key Super+shift+c" "2" . shorten 40
    , ppSort          =  getSortByIndex
    , ppOrder         = \[ws, l, t, ex] -> [ws, l, ex, t]
    , ppExtras        = [xmobarColorL TH.base01 background windowCount]
    , ppLayout        = red . xmobarAction "xdotool key Super+/" "1" . xmobarAction
                            "xdotool key Super+shift+/"
                            "3"
                            . (\case
                                "Tall"            -> "\xf005" -- 
                                "ThreeCol"        -> "\xfa6a" -- 頻
                                "Circle"          -> "\xe22e" -- 
                                "Full"            -> "\xf5b5" -- 
                                "HintedGrid"      -> "\xfb8a" -- ﮊ
                                "Horizon"         -> "?"
                                "Mirror Mosaic"   -> "\xfa73" -- 侀
                                "Mirror Spiral"   -> "\xfc06" -- ﰆ
                                "Mirror Tall"     -> "\xf006" -- 
                                "Mirror ThreeCol" -> "\xfa6e" -- 﩮
                                "Monocle"         -> "?"
                                "Mosaic"          -> "\xfa6d" -- 舘
                                "OneBig"          -> "\xf286" -- 
                                "Spiral"          -> "\xf306" -- 
                                "Tabbed"          -> "\xfd35" -- ﴵ
                                _                 -> "?"
                              )
    }
   where
    shorten :: Int -> String -> String
    shorten = shorten' "…"

    wrapSep :: String -> String
    wrapSep = wrap (xmobarColor TH.base00 "" (xmobarFont 5 "\xe0b4"))
                   (xmobarColor TH.base00 "" (xmobarFont 5 "\xe0b6"))

    currentWorkspace :: String -> String
    currentWorkspace _ = "\61713" -- " "

    occupiedWorkspace :: String -> String
    occupiedWorkspace _ = "\61842" -- " "

    emptyWorkspace :: String -> String
    emptyWorkspace _ = "\61708" -- 

    urgentWorkspace :: String -> String
    urgentWorkspace _ = "\62759" -- 

    background :: String
    background = TH.base00 ++ ":5"

    blue, lowWhite, magenta, red :: String -> String
    magenta  = xmobarColor TH.base05 background
    blue     = xmobarColor TH.base04 background
    -- purple   = xmobarColor "#bd93f9" "#2c323a:5"
    -- lowBlue  = xmobarColor "#8be9fd" "#2c323a:5"
    -- white    = xmobarColor "#f8f8f2" "#2c323a:5"
    -- yellow   = xmobarColor "#f1fa8c" "#2c323a:5"
    red      = xmobarColor TH.base01 background
    lowWhite = xmobarColor TH.base07 background
    -- gray     = xmobarColor "" background
    -- green    = xmobarColor TH.base02 background

    -- Get count of available windows on a workspace
    windowCount :: X (Maybe String)
    windowCount =
      gets
        $ Just
        . show
        . length
        . W.integrate'
        . W.stack
        . W.workspace
        . W.current
        . windowset

  -- myIconConfig :: IconConfig
  -- myIconConfig = def { iconConfigIcons  = myIcons
  --                    , iconConfigFmt    = iconsFmtReplace (wrapUnwords "" "")
  --                    , iconConfigFilter = iconsGetFocus
  --                    }
  --  where
  --   myIcons :: Query [String]
  --   myIcons = composeAll
  --     [ className =? "discord" --> appIcon "<fn=3>\xf392</fn>"
  --     , className =? "Discord" --> appIcon "<fn=3>\xf268</fn>"
  --     , className =? "firefox" --> appIcon "<fn=3>\xf269</fn>"
  --     , className =? "Brave-browser" --> appIcon "<fn=3>\xf268</fn>"
  --     , className =? "St" --> appIcon "<fn=2>\xe795</fn>"
  --     , className =? "Alacritty" --> appIcon "<fn=2>\xe795</fn>"
  --     , className =? "Emacs" --> appIcon "<fn=4>\xe926</fn>"
  --     , className =? "jetbrains-idea-ce" --> appIcon "<fn=4>\xe926</fn>"
  --     , className =? "code-oss" --> appIcon "<fn=4>\xe60c</fn>"
  --     , className =? "Code" --> appIcon "<fn=4>\xe60c</fn>"
  --     , className =? "Org.gnome.Nautilus" --> appIcon "<fn=1>\xf07b</fn>"
  --     , className =? "Spotify" --> appIcon "<fn=3>\xf1bc</fn>"
  --     , className =? "mpv" --> appIcon "<fn=1>\xf03d</fn>"
  --     , className =? "VirtualBox Manager" --> appIcon "<fn=4>\xea3e</fn>"
  --     , className =? "Lutris" --> appIcon "<fn=1>\xf11b</fn>"
  --     , className =? "Sxiv" --> appIcon "<fn=1>\xf03e</fn>"
  --     ]
















-- myLogHook :: D.Client -> DL.PP
-- myLogHook dbus = def
--     { DL.ppOutput = dbusOutput dbus
--     , DL.ppCurrent = DL.wrap ("%{F" ++ lightBlue ++ "} ") " %{F-}"
--     , DL.ppVisible = DL.wrap ("%{F" ++ colorBlue ++ "} ") " %{F-}"
--     , DL.ppUrgent = DL.wrap ("%{F" ++ colorRed ++ "} ") " %{F-}"
--     , DL.ppHidden = DL.wrap " " " "
--     , DL.ppWsSep = ""
--     , DL.ppSep = " "
--     , DL.ppTitle = myAddSpaces 25
--     }

-- -- Emit a DBus signal on log updates
-- dbusOutput :: D.Client -> String -> IO ()
-- dbusOutput dbus str = do
--     let signal = (D.signal objectPath interfaceName memberName) {
--             D.signalBody = [D.toVariant $ UTF8.decodeString str]
--         }
--     D.emit dbus signal
--   where
--     objectPath = D.objectPath_ "/org/xmonad/Log"
--     interfaceName = D.interfaceName_ "org.xmonad.Log"
--     memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = DL.shorten len str


myManageHook :: ManageHook
myManageHook = composeAll
    [
     ManageDocks.manageDocks
    , insertPosition End Newer -- open new windows at the end
    , floatNextHook
    , myManageHook'
    ]

--
-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions
-- xprop fields used in manage hook:
-- resource (also known as appName) is the first element in WM_CLASS(STRING)
-- className is the second element in WM_CLASS(STRING)
-- title is WM_NAME(STRING)
-- https://hackage.haskell.org/package/xmonad-0.15/docs/XMonad-ManageHook.html
myManageHook' :: ManageHook
myManageHook' =
    composeAll
        . concat
        $ [ [ManageHelpers.transience'] -- move transient windows like dialogs/alerts on top of their parents
          , [ className =? c --> doFloat | c <- myClassFloats ]
          , [ className =? c --> ManageHelpers.doFullFloat | c <- myFullFloats ]
          , [ title =? t --> doFloat | t <- myTitleFloats ]
          , [ className =? c --> ManageHelpers.doCenterFloat | c <- myCenterFloats ]
          , [ title =? t --> ManageHelpers.doCenterFloat | t <- myTitleCenterFloats ]
          , [ className =? c --> doShift (myWorkspaces !! ws) | (c, ws) <- myShifts ]
          , [ title =? c --> doShift (myWorkspaces !! ws) | (c, ws) <- myTitleShifts ]
          -- , [ManageHelpers.isDialog --> ManageHelpers.doCenterFloat]
          ]
  where
    myCenterFloats = ["zenity", "Arandr", "Galculator", "albert"]
    myTitleCenterFloats =
        [ "File Operation Progress"
        , "Downloads"
        , "Save as..."
        , "Ulauncher Preferences"
        ]
    myClassFloats = []
    myTitleFloats = ["Media viewer", "Yad"]
    myFullFloats  = []
      -- workspace numbers start at 0
    myShifts =
        [ ("telegram-desktop"  , 10)
        , ("TelegramDesktop"   , 10)
        , ("Slack"             , 9)
        , ("Postman"           , 6)
        , ("DevCenter"         , 6)
        , ("jetbrains-idea-ce" , 2)
        , ("firefox"           , 0)
        , ("Chromium"          , 13)
        , ("Joplin"            , 6)
        , ("Transmission-gtk"  , 11)
        ]
    myTitleShifts =
        [ ("DevCenter"         , 6)
        ]

myStartupHook :: X ()
myStartupHook = do
    checkKeymap myConfig myKeymap
    -- Cursor.setDefaultCursor Cursor.xC_left_ptr
    spawn "$HOME/.config/xmonad/scripts/autostart.sh"
    spawnOnce
      ("stalonetray --geometry 1x1-17+5 --max-geometry 10x1-17+5 --transparent --tint-color '"
      ++ TH.base00
      ++ "' --tint-level 255 --grow-gravity NE --icon-gravity NW --icon-size 20 --sticky --window-type dock --window-strut top --skip-taskbar"
      )

myHandleEventHook =
    ManageDocks.docksEventHook <+> handleEventHook desktopConfig

------------------------------------------------------------------------
-- tabs
--
myTabConfig = def { TB.activeColor         = "#556064"
                  , TB.inactiveColor       = "#2F3D44"
                  , TB.urgentColor         = "#FDF6E3"
                  , TB.activeBorderColor   = "#454948"
                  , TB.inactiveBorderColor = "#454948"
                  , TB.urgentBorderColor   = "#268BD2"
                  , TB.activeTextColor     = "#80FFF9"
                  , TB.inactiveTextColor   = "#1ABC9C"
                  , TB.urgentTextColor     = "#1ABC9C"
                  , TB.fontName = "xft:Noto Sans CJK:size=10:antialias=true"
                  }
------------------------------------------------------------------------
-- Prompt
--
-- Prompt configuration
myPrompt :: Prompt.XPConfig
myPrompt = def
    { Prompt.font = "xft:Fantasque Sans Mono Nerd Font:size=14:antialias=true"
    , Prompt.fgColor           = foreground
    , Prompt.bgColor           = background
    , Prompt.borderColor       = colorPromptbg
    , Prompt.height            = 22
    , Prompt.promptBorderWidth = 0
    , Prompt.autoComplete      = Just 100000
    , Prompt.bgHLight          = colorPromptHLightbg
    , Prompt.fgHLight          = colorPromptHLightfg
    , Prompt.position          = Prompt.Top
    , Prompt.maxComplRows      = Just 5
    , Prompt.searchPredicate   = L.isPrefixOf
    }

myPromptInfix :: Prompt.XPConfig
myPromptInfix = myPrompt { Prompt.searchPredicate = L.isInfixOf }

-- myLayoutPrompt :: X ()
-- myLayoutPrompt =
--     inputPromptWithCompl
--             myPrompt { Prompt.autoComplete = Just 1000 }
--             "Layout"
--             (Prompt.mkComplFunFromList'
--                 [ "1.Tall"
--                 , "2.HintedGrid"
--                 , "3.OneBig"
--                 , "4.Circle"
--                 , "5.Mosaic"
--                 , "6.ThreeCol"
--                 , "7.Spiral"
--                 ]
--             )
--         ?+ \l -> sendMessage $ JumpToLayout $ drop 2 l

-- mySessionPrompt :: X ()
-- mySessionPrompt =
--     inputPromptWithCompl
--             myPrompt { Prompt.autoComplete = Just 1000 }
--             "\x23FB " -- ⏻
--             (Prompt.mkComplFunFromList'
--                 ["1.Lock", "2.Suspend", "3.Reboot", "4.Shutdown", "5.Exit"]
--             )
--         ?+ \l -> prompt $ map toLower $ drop 2 l
--   where
--     prompt = \x -> case x of
--         "lock"     -> noConfirm x
--         "suspend"  -> noConfirm x
--         "reboot"   -> confirm x
--         "shutdown" -> confirm x
--         "exit"     -> confirmPrompt myPrompt x $ io (exitWith ExitSuccess)
--         _ -> noConfirm "lock"
--       where
--         confirm command = confirmPrompt myPrompt command
--             $ spawn ("$HOME/.scripts/i3lock.sh " ++ command)
--         noConfirm command = spawn ("$HOME/.scripts/i3lock.sh " ++ command)

------------------------------------------------------------------------
-- Colors

background :: String
background = "#1D1F28"
foreground :: String
foreground = "#FDFDFD"
lightBlack :: String
lightBlack = "#282A36" -- color0
lightRed :: String
lightRed = "#F37F97" -- color1
lightGreen :: String
lightGreen = "#5ADECD" -- color2
lightYellow :: String
lightYellow = "#F2A272" -- color3
lightBlue :: String
lightBlue = "#8897F4" -- color4
lightMagenta :: String
lightMagenta = "#C574DD" -- color5
lightCyan :: String
lightCyan = "#79E6F3" -- color6
lightWhite :: String
lightWhite = "#FDFDFD" -- color7
lightGray :: String
lightGray = "#C0C0C0"
darkBlack :: String
darkBlack = "#414458" -- color8
darkRed :: String
darkRed = "#FF4971" -- color9
darkGreen :: String
darkGreen = "#18E3C8" -- color10
darkYellow :: String
darkYellow = "#FF8037" -- color11
darkBlue :: String
darkBlue = "#556FFF" -- color12
darkMagenta :: String
darkMagenta = "#B043D1" -- color13
darkCyan :: String
darkCyan = "#3FDCEE" -- color14
darkWhite :: String
darkWhite = "#BEBEC1" -- color15
darkGray :: String
darkGray = "#848482"
--
-- X11 color names:
-- https://www.wikiwand.com/en/X11_color_names
-- Color Setting

colorBlue :: String
colorBlue = "#868bae"

colorGreen :: String
colorGreen = "#00d700"

colorRed :: String
colorRed = "#ff005f"

colorGray :: String
colorGray = "#666666"

colorWhite :: String
colorWhite = "#bdbdbd"

colorPromptbg :: String
colorPromptbg = "#1c1c1c"

colorPromptHLightfg :: String
colorPromptHLightfg = foreground

colorPromptHLightbg :: String
colorPromptHLightbg = background

colorNormalbg :: String
colorNormalbg = background

colorfg :: String
colorfg = foreground

-- Border Styling
myNormalBorderColor :: String
myNormalBorderColor = colorNormalbg

myFocusedBorderColor :: String
myFocusedBorderColor = "#5ADECD"

-- name colors http://chir.ag/projects/name-that-color
colorConiferGreen :: String
colorConiferGreen = "#78ea59"

colorGoldenFizzYellow :: String
colorGoldenFizzYellow = "#ffff33"

colorElectricViolet :: String
colorElectricViolet = "#cc00ff"

colorCeruleanBlue :: String
colorCeruleanBlue = "#00a1f1"

colorPomegranate :: String
colorPomegranate = "#f65314"

colorFrolyPink :: String
colorFrolyPink = "#f7786b"

colorSelectiveYellow :: String
colorSelectiveYellow = "#fbbc05"

colorCyan :: String
colorCyan = "#00ffff"

colorPictonBlue :: String
colorPictonBlue = "#33bdf5"

-- border width
myBorderWidth :: Dimension
myBorderWidth = 3

------------------------------------------------------------------------
-- Floats
--
centerR = W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)

bigCenterR = W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)

leftR = W.RationalRect 0 (1 / 8) (1 / 2) (3 / 4)

rightR = W.RationalRect (4 / 8) (1 / 8) (1 / 2) (3 / 4)

-- GridSelect configuration
myGridSelectConfig :: GS.GSConfig Window
myGridSelectConfig = def { GS.gs_navigate  = myNavigation
                         , GS.gs_colorizer = GS.fromClassName
                         }
  where
    myNavigation = GS.makeXEventhandler $ GS.shadowWithKeymap navKeymap $ const
        GS.defaultNavigation
    navKeymap = M.fromList
        [ ((0, xK_Escape), GS.cancel)
        , ((0, xK_Return), GS.select)
        , ( (0, xK_slash)
          , GS.substringSearch myNavigation
          ) -- search
        , ( (0, xK_h)
          , GS.move (-1, 0) >> myNavigation
          ) -- move left
        , ( (0, xK_l)
          , GS.move (1, 0) >> myNavigation
          ) -- move right
        , ( (0, xK_j)
          , GS.move (0, 1) >> myNavigation
          ) -- move down
        , ( (0, xK_k)
          , GS.move (0, -1) >> myNavigation
          ) -- move up
        , ( (0, xK_y)
          , GS.move (-1, -1) >> myNavigation
          ) -- move diagonal up left
        , ( (0, xK_u)
          , GS.move (1, -1) >> myNavigation
          ) -- move diagonal up right
        , ( (0, xK_b)
          , GS.move (-1, 1) >> myNavigation
          ) -- move diagonal down left
        , ( (0, xK_n)
          , GS.move (1, 1) >> myNavigation
          ) -- move diagonal down right
        , ((0, xK_Tab), GS.moveNext >> myNavigation) -- move next
        ]


-------------------------------------------------------------------------
-- Keybinding hints
--

-- rmHint :: [(a, b, c, d)] -> [(a, b)]
-- rmHint x = [ (t1, t2) | (t1, t2, _, _) <- x ]

-- Is this used ?
-- fmtDesc :: String -> [(String, a, Label, String)] -> Int -> String -> String -> String
-- fmtDesc name keyMap rows fg hl
--     | name == "" = "\"" ++ "\\n" ++ list ++ "\""
--     | otherwise  = "\"" ++ colStr hl ++ name ++ "\\n\\n" ++ list ++ "\""
--   where
--     list         = L.intercalate "\\n" (foldr (zipWithMore (++)) [""] col)
--     col          = chunksOf nRows $ colDesc keyMap
--     --sortKeys  = L.sortBy (\(a,_,_) (b,_,_) -> compare a b)
--     maxChars     = 220
--     lMap         = length keyMap
--     nRows        = min rows lMap
--     nCol         = max 1 $ ceiling $ fromIntegral lMap / fromIntegral nRows
--     charsPerCol  = quot maxChars nCol
--     charsPerICol = quot charsPerCol 2

--     descAlign    = charsPerICol
--     keyAlign     = charsPerICol

--     colDesc :: [(String, a, Label, String)] -> [String]
--     colDesc x =
--         [ colStr hl
--               ++ rAlign keyAlign key
--               ++ " "
--               ++ colStr fg
--               ++ lAlign descAlign desc
--         | (key, _, _, desc) <- x
--         ]

    -- colStr :: String -> String
    -- colStr col = "^fg(" ++ col ++ ")"

    -- rAlign :: Int -> String -> String
    -- rAlign = textAlign T.justifyRight

    -- lAlign :: Int -> String -> String
    -- lAlign = textAlign T.justifyLeft

    -- textAlign :: (Int -> Char -> T.Text -> T.Text) -> Int -> (String -> String)
    -- textAlign fAlign n = T.unpack . fAlign n ' ' . T.pack

    zipWithMore :: (a -> a -> a) -> [a] -> [a] -> [a]
    zipWithMore f (a : as) (b : bs) = f a b : zipWithMore f as bs
    zipWithMore _ []       bs       = bs -- if there's more in bs, use that
    zipWithMore _ as       []       = as -- if there's more in as, use that


-------------------------
fmtHint :: [(String, a, Label, String)] -> String -> String -> String -> Int -> String
fmtHint keyMap colorBinding colorDesc colorTitle maxChars =
   "\"\\n" ++ L.intercalate "\\n" listKeyMap ++ "\""
  where
    colSize = charsPerCol maxChars
    emptyColRow = rAlign colSize "" ++ lAlign colSize ""
    sortedKeymap = sortKeymap keyMap
    colDescription = colDesc colorBinding colorDesc colorTitle colSize
    listKeyMap = buildKeyMap (buildColumns (map colDescription sortedKeymap)) emptyColRow

    colDesc ::  String -> String -> String -> Int -> [(String, a, Label, String)] -> [String]
    colDesc colorBinding colorDesc colorTitle colSize bindings=
        (colStr colorTitle ++ rAlign colSize (getLabel bindings) ++ lAlign (colSize + 1) "") :
        [ colStr colorBinding
              ++ rAlign colSize key
              ++ " "
              ++ colStr colorDesc
              ++ lAlign colSize desc
        | (key, _, _, desc) <- bindings
        ]

charsPerCol :: Int -> Int
charsPerCol maxChars = quot (quot maxChars 3) 2 -- 3 columns by 2 (key, description)

getLabel :: [(a, b, Label, c)] -> String
getLabel ((_, _, label, _):_) =  show label
getLabel [] = ""

colStr :: String -> String
colStr col = "^fg(" ++ col ++ ")"

textAlign :: (Int -> Char -> T.Text -> T.Text) -> Int -> (String -> String)
textAlign fAlign n = T.unpack . fAlign n ' ' . T.pack

rAlign :: Int -> String -> String
rAlign = textAlign T.justifyRight

lAlign :: Int -> String -> String
lAlign = textAlign T.justifyLeft

trd :: (a, b, c, d) -> c
trd (_, _, c, _) =  c

buildColumns :: [[String]] -> [[String]]
buildColumns keyGroups = columns
  where
    keyCol = concat keyGroups
    columnsLength = ceiling (fromIntegral(length keyCol) / 3)
    columns = chunksOf columnsLength keyCol


buildKeyMap :: [[String]] -> String -> [String]
buildKeyMap [a] filler = buildSection a [] [] filler
buildKeyMap [a,b] filler = buildSection a b [] filler
buildKeyMap [a,b,c] filler = buildSection a b c filler
buildKeyMap (a:b:c:xs) filler = buildSection a b c filler ++ buildKeyMap xs filler

buildSection :: [String] -> [String] -> [String] -> String -> [String]
buildSection a b c filler = zipWith3 (\x y z -> x ++ y ++ z) (fillColumn a)  (fillColumn b)  (fillColumn c)
  where rows = max (length a) $ max (length b) (length c)
        fillColumn col = col ++ replicate (rows - length col) filler

sortKeymap :: [(String, a, Label, String)] -> [[(String, a, Label, String)]]
sortKeymap = map sortByKeyBinding . groupByLabel

sortByKeyBinding :: [(String, a, Label, String)] -> [(String, a , Label, String)]
sortByKeyBinding = L.sortBy (\(a, _, _, _) (b, _, _ ,_ ) -> compare a b)

groupByLabel :: [(String, a, Label, String)] -> [[(String, a, Label, String)]]
groupByLabel = L.groupBy (\a b -> trd a == trd b) . L.sortBy (\a b -> compare (trd a)  (trd b))

showHelp :: X ()
showHelp = spawn $ unwords
    [ "$HOME/.config/xmonad/scripts/showHintForKeymap.sh"
    , desc
    ]
    where desc = fmtHint myKeymapH lightBlue lightGreen lightRed 220

-- Order displayed
-- Label used for displaying keys
data Label =
  ClientLabel
  | LayoutLabel
  | TagLabel
  | ScreenLabel
  | LauncherLabel
  | MediaLabel
  | MiscLabel
  deriving (Eq, Ord)

instance Show Label where
  show ClientLabel = "Client"
  show LayoutLabel = "Layout"
  show TagLabel = "Tag"
  show ScreenLabel = "Screen"
  show LauncherLabel = "Launcher"
  show MediaLabel = "MediaLabel"
  show MiscLabel = "MiscLabel"

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modMask } = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys config' = mkKeymap config' $ myKeymap ++ [ (t1, t2) | (t1, t2, _, _) <- myKeymapH]

myKeymap :: [(String, X ())]
myKeymap = myWorkspaceMovementKeys

-- Keys with hints
myKeymapH :: [(String, X (), Label, String)]
myKeymapH = concat
    [ myControlKeys
    , myLauncherKeys
    , myLayoutKeys
    , myWorkspaceKeys
    , myMovementKeys
    , myMediaLabelKeys
    , myFloatKeys
    ]

myWorkspaceMovementKeys :: [(String, X ())]
myWorkspaceMovementKeys =
    [ (prefix ++ key, func ws)
    | (prefix, func) <-
        [ ( "M-"
          , windows . W.greedyView
          ) -- go to workspace
        , ( "M-S-"
          , windows . viewShift
          ) -- go to workspace taking current window
        , ( "M-C-"
          , windows . W.shift
          ) -- send window to workspace
        , ("M-C-S-", swapWithCurrent) -- change workspace number
        ]
    , (key   , ws  ) <- zip keys' myWorkspaces
    ]
  where
    keys'      = fmap return $ ['1' .. '9'] ++ ['0', '-', '=', '[', ']', ';' ,'\'']
    viewShift = liftM2 (.) W.greedyView W.shift

myMovementKeys :: [(String, X (), Label, String)]
myMovementKeys =
    myWindowMovementKeys
        ++ myWorkspaceMovementKeys'
        ++ myScreenMovementKeys
        ++ myGotoLayoutKeys

myWindowMovementKeys :: [(String, X (), Label, String)]
myWindowMovementKeys =
    [ ("M-<D>", windowGo Nav.D, ClientLabel, "Focus down")
    , ("M-<U>", windowGo Nav.U, ClientLabel, "Focus up")
    , ("M-<L>", windowGo Nav.L, ClientLabel, "Focus left")
    , ("M-<R>", windowGo Nav.R, ClientLabel, "Focus right")
    , ("M-j"  , windowGo Nav.D, ClientLabel, "Focus down")
    , ("M-k"  , windowGo Nav.U, ClientLabel, "Focus up")
    , ("M-h"  , windowGo Nav.L, ClientLabel, "Focus left")
    , ("M-l"  , windowGo Nav.R, ClientLabel, "Focus right")
    , ( "M-S-m"
      , windows W.focusMaster, ClientLabel
      , "Focus master"
      ) -- Focus master
    , ( "M1-<Tab>"
      , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab, ClientLabel
      , "Cycle recent windows"
      )
    ]
    where windowGo = sendMessage . Nav.Go
myWorkspaceMovementKeys' :: [(String, X (), Label, String)]
myWorkspaceMovementKeys' =
    [ ( "M-C-<R>"
      , CycleWS.nextWS
      , TagLabel
      , "Next workspace"
      ) -- Go to the next
    , ( "M-C-<L>"
      , CycleWS.prevWS
      , TagLabel
      , "Previos workspace"
      ) --  Go to previous workspace
    , ( "M-C-l"
      , CycleWS.nextWS
      , TagLabel
      , "Next workspace"
      ) -- Go to the next
    , ("M-C-h", CycleWS.prevWS, TagLabel, "Previous workspace") --  Go to previous workspace
    ]
myScreenMovementKeys :: [(String, X (), Label, String)]
myScreenMovementKeys =
    [ ( "M-s"
      , sequence_ [CycleWS.nextScreen, warpToWindow (1 % 2) (1 % 2)]
      , ScreenLabel
      , "Next screen"
      ) -- Move the focus to next screen (multi screen)
    , ("M-o"  , CycleWS.swapNextScreen , ScreenLabel, "Swap next screen")
    , ("M-S-o", CycleWS.shiftNextScreen, ScreenLabel, "Shift next screen")
    ]
myGotoLayoutKeys :: [(String, X (), Label, String)]
myGotoLayoutKeys =
    [ ("M-g 1", jumpToLayout "Tall"      , LayoutLabel, "Tall")
    , ("M-g 2", jumpToLayout "HintedGrid", LayoutLabel, "HintedGrid")
    , ("M-g 3", jumpToLayout "Tabbed"    , LayoutLabel, "Tabbed")
    , ("M-g 4", jumpToLayout "OneBig"    , LayoutLabel, "OneBig")
    , ("M-g 5", jumpToLayout "Circle"    , LayoutLabel, "Circle")
    , ("M-g 6", jumpToLayout "Mosaic"    , LayoutLabel, "Mosaic")
    , ("M-g 7", jumpToLayout "ThreeCol"  , LayoutLabel, "Threecol")
    , ("M-g 8", jumpToLayout "Spiral"    , LayoutLabel, "spiral")
    ]
    where jumpToLayout = sendMessage . JumpToLayout

myLayoutKeys :: [(String, X (), Label, String)]
myLayoutKeys = myLayoutKeys' ++ myLayoutSwapKeys ++ myLayoutTransformKeys

myLayoutSwapKeys :: [(String, X (), Label, String)]
myLayoutSwapKeys =
    [ ("M-S-<D>", layoutSwap Nav.D, ClientLabel, "Swap down")
    , ("M-S-<U>", layoutSwap Nav.U, ClientLabel, "Swap up")
    , ("M-S-<L>", layoutSwap Nav.L, ClientLabel, "Swap left")
    , ("M-S-<R>", layoutSwap Nav.R, ClientLabel, "Swap right")
    , ("M-S-j"  , layoutSwap Nav.D, ClientLabel, "Swap down")
    , ("M-S-k"  , layoutSwap Nav.U, ClientLabel, "Swap up")
    , ("M-S-h"  , layoutSwap Nav.L, ClientLabel, "Swap left")
    , ("M-S-l"  , layoutSwap Nav.R, ClientLabel, "Swap right")
    ]
    where layoutSwap = sendMessage . Nav.Swap

myLayoutKeys' :: [(String, X (), Label, String)]
myLayoutKeys' =
    [ ( "M-f"
      , sendMessage $ Toggle NBFULL
      , ClientLabel
      , "Toggle full screen"
      ) -- Toggle Fullscreen mode
    , ( "M-C-,"
      , sendMessage $ IncMasterN (-1)
      , LayoutLabel
      , "Decrease master"
      ) -- Decrease the number of master pane
    , ( "M-C-."
      , sendMessage $ IncMasterN 1
      , LayoutLabel
      , "Increase master"
      ) -- Increase the number of master pane
    , ( "M-\\"
      , sendMessage NextLayout
      , LayoutLabel
      , "Next layout"
      ) -- Rotate through the available layout algorithms
    -- , ( "M-S-\\"
    --   , sendMessage FirstLayout
    --   , LayoutLabel
    --   , "First layout"
    --   )
    , ("M-m", windows W.shiftMaster, ClientLabel, "Shift with master") -- Shift the focused window to the master window
    ]

myLayoutTransformKeys :: [(String, X (), Label, String)]
myLayoutTransformKeys =
    [ ("M-,"  , sendMessage Shrink            , LayoutLabel, "Decrease horizontally")
    , ("M-."  , sendMessage Expand            , LayoutLabel, "Increase vertically")
    , ("M-S-.", sendMessage RTile.MirrorShrink, LayoutLabel, "Decrease vertically")
    , ("M-S-,", sendMessage RTile.MirrorExpand, LayoutLabel, "Increase vertically")
    , ("M-g x", sendMessage $ Toggle REFLECTX , LayoutLabel, "Reflect horizontally")
    , ("M-g y", sendMessage $ Toggle REFLECTY , LayoutLabel, "Reflect vertically")
    , ("M-g m", sendMessage $ Toggle MIRROR   , LayoutLabel, "Toggle mirror") -- Toggle Mirror layout
    ]

myWorkspaceKeys :: [(String, X (), Label, String)]
myWorkspaceKeys =
    [ ( "M-C-S-<R>"
      , CycleWS.shiftToNext
      , TagLabel
      , "Shift to next workspace"
      ) -- Shift the focused window to the next workspace
    , ( "M-C-S-<L>"
      , CycleWS.shiftToPrev
      , TagLabel
      , "Shift to previous workspace"
      ) -- Shift the focused window to the previous workspace
    , ( "M-C-S-l"
      , CycleWS.shiftToNext
      , TagLabel
      , "Shift to next workspace"
      ) -- Shift the focused window to the next workspace
    , ( "M-C-S-h"
      , CycleWS.shiftToPrev
      , TagLabel
      , "Shift to previous workspace"
      ) -- Shift the focused window to the previous workspace
    , ("M-<Tab>", CycleWS.toggleWS, TagLabel, "Toggle last workspace") -- toggle last workspace
    ]

myFloatKeys :: [(String, X (), Label, String)]
myFloatKeys =
    [ ("M-c s", withFocused $ windows . W.sink, ClientLabel, "Sink floating")
    , ("M-c b", withFocused $ windows . flip W.float bigCenterR, ClientLabel, "Float big center")
    , ("M-c c", withFocused $ windows . flip W.float centerR, ClientLabel, "Float center")
    , ("M-c l", withFocused $ windows . flip W.float leftR, ClientLabel, "Float left")
    , ("M-c r", withFocused $ windows . flip W.float rightR, ClientLabel, "Float right")
    ]

myLauncherKeys :: [(String, X (), Label, String)]
myLauncherKeys = myLauncherKeys' ++ myScreenCaptureKeys

myLauncherKeys' :: [(String, X (), Label, String)]
myLauncherKeys' =
    [ ( "M-<Return>"
      , spawn myTerminal
      , LauncherLabel
      , "Terminal"
      ) -- Launch terminal
    , ( "M-S-<Return>"
      , spawn myFileManager
      , LauncherLabel
      , "File Manager"
      ) -- Launch FileManager
    -- , ( "M-, b"
    --   , spawn myBrowser
    --   , Launcher
    --   , "Browser"
    --   ) -- Launch browser
    -- , ( "M-, e"
    --   , spawn myTextEditor
    --   , Launcher
    --   , "Text Editor"
    --   ) -- Launch text editor
    -- , ( "M-, f"
    --   , spawn myFileManager
    --   , Launcher
    --   , "File Manager"
    --   ) -- Launch File Manager
    -- , ( "M-, k"
    --   , spawn "xkill"
    --   , Launcher
    --   , "Kill Window"
    --   ) -- Kill window
    -- , ( "M-, r"
    --   , spawn myConsoleFileManager
    --   , Launcher
    --   , "Ranger"
    --   ) -- Launch text editor
    -- , ( "M-, t"
    --   , spawn myTmuxTerminal
    --   , Launcher
    --   , "Tmux"
    --   ) -- Launch tmux terminal
    -- , ( "M-, v"
    --   , spawn "nvim"
    --   , Launcher
    --   , "Neovim"
    --   ) -- Launch text editor
    , ( "M-S-C-="
      , spawn "$HOME/.scripts/xbacklight-toggle.sh"
      , LauncherLabel
      , "Toggle backlight"
      )
    ]

myScreenCaptureKeys :: [(String, X (), Label, String)]
myScreenCaptureKeys =
    [ ( "<Print>"
      , spawn $ myScreenCapture ++ " root && notify-send 'Desktop captured'"
      , MiscLabel
      , "Take a screenshot (desktop)"
      )
    , ( "S-<Print>"
      , spawn
          $  "notify-send 'Select Area';sleep 0.2;"
          ++ myScreenCapture
          ++ " area && notify-send 'Area captured'"
      , MiscLabel
      , "Take a screenshot (area)"
      )
    , ( "C-<Print>" --
      , spawn
          $  myScreenCapture
          ++ " window && notify-send 'Focused window captured'"
      , MiscLabel
      , "Take a screenshot (window)"
      )
    ]

myMediaLabelKeys :: [(String, X (), Label, String)]
myMediaLabelKeys =
    -- Play / Pause media
    [ ("<XF86AudioPlay>", spawn "playerctl play-pause", MediaLabel, "MediaLabel play/pause")
    , ("<XF86AudioStop>", spawn "playerctl pause"     , MediaLabel, "MediaLabel pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous"  , MediaLabel, "MediaLabel previous")
    , ( "<XF86AudioNext>"
      , spawn "playerctl next"
      , MediaLabel
      , "MediaLabel next"
      )
  -- Volume
    , ( "<XF86AudioRaiseVolume>"
      , spawn "$HOME/.scripts/volume.sh up"
      , MediaLabel
      , "Volume up"
      )
    , ( "<XF86AudioLowerVolume>"
      , spawn "$HOME/.scripts/volume.sh down"
      , MediaLabel
      , "Volume down"
      )
    , ( "<XF86AudioMute>"
      , spawn "$HOME/.xmonad/scripts/XMMute.sh"
      , MediaLabel
      , "Mute"
      )
  -- Brightness
    , ( "<XF86MonBrightnessUp>"
      , spawn
          "xbacklight + 5 -time 100 -steps 1 && notify-send \"brightness up $(xbacklight -get)\""
      , MiscLabel
      , "Brightness up"
      )
    , ( "<XF86MonBrightnessDown>"
      , spawn
          "xbacklight - 5 -time 100 -steps 1 && notify-send \"brightness down $(xbacklight -get)\""
      , MiscLabel
      , "Brightness down"
      )
  -- Touchpad
    , ( "<XF86TouchpadToggle>"
      , spawn "$HOME/.scripts/touchpad_toggle.sh"
      , MiscLabel
      , "Toggle touchpad"
      ) -- Touch pad
  -- Browser
    , ("<XF86Explorer>", spawn myBrowser, LauncherLabel, "Browser") -- Browser
    ]

myControlKeys :: [(String, X (), Label, String)]
myControlKeys =
    [ ( "M-S-q"
      , kill
      , ClientLabel
      , "Kill focused"
      ) -- Close the focused window
       -- Toggle struts
    , ( "M-b"
      , sendMessage ManageDocks.ToggleStruts
      , MiscLabel
      , "Toggle statusbar"
      )
       -- grid selection
    , ( "M-g s"
      , GS.goToSelected myGridSelectConfig
      , MiscLabel
      , "Grid selection"
      )
       -- Search a window and focus into the window
    -- , ( "M-g g"
    --   , WPrompt.windowPrompt myPromptInfix WPrompt.Goto WPrompt.allWindows
    --   , MiscLabel
    --   , "Search and go to client"
    --   )
       -- Search a window and bring to the current workspace
    -- , ( "M-g b"
    --   , WPrompt.windowPrompt myPromptInfix WPrompt.Bring WPrompt.allWindows
    --   , MiscLabel
    --   , "Search and bring client"
    --   )
    -- , ( "M-g l"
    --   , myLayoutPrompt
    --   , MiscLabel
    --   , "LayoutLabel menu"
    --   )
       -- Resize viewed windows to the correct size
  --, ("M-r", refresh)
    --  Reset the layouts on the current workspace to default
    --, ("M-S-<Space>", setLayoutLabel $ XMonad.layoutHook conf)
    , ( "M-C-r" -- Restart xmonad
      , spawn
          "xmonad --recompile && xmonad --restart && notify-send 'Xmonad restarted' || notify-send 'Xmonad failed to restart'"
      , MiscLabel
      , "Compile and restart"
      )
    , ( "M-S-r"
      , spawn "xmonad --restart"
      , MiscLabel
      , "Restart"
      ) -- restart xmonad w/o recompiling
    -- , ( "M-S-<Space>"
    --   , shellPrompt myPrompt
    --   , MiscLabel
    --   , "Shell launcher"
    --   ) -- launch apps
    , ( "M-<Space>"
      , spawn myLauncher
      , MiscLabel
      , "Launcher"
      ) -- launch apps
    -- , ("M-<Esc>", mySessionPrompt   ,  MiscLabel, "Log menu")
    , ( "M-u"
      , UH.focusUrgent
      , ClientLabel
      , "Focus urgent"
      ) -- focus urgent window
    , ( "M-S-u"
      , UH.clearUrgents
      , ClientLabel
      , "Clear urgent"
      ) -- clear urgents
    , ("M-<F1>", unGrab >> showHelp, MiscLabel, "Show help")
    ]
