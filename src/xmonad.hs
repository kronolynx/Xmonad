{-# LANGUAGE UnicodeSyntax #-}


module Main (main) where
--
-- If there are more than 3 explicit imports required a qualified import is used
-- If a type has only one constructor it is imported implicitly with (..)
--
--
import           XMonad                              hiding ( (|||) )

import qualified Theme.Theme                         as TH

-- hooks
import           XMonad.Hooks.EwmhDesktops            ( ewmh, ewmhFullscreen )
import           XMonad.Hooks.FloatNext               ( floatNextHook )
import qualified XMonad.Hooks.ManageDocks            as ManageDocks
import qualified XMonad.Hooks.ManageHelpers          as ManageHelpers
import qualified XMonad.Hooks.UrgencyHook            as UH
import XMonad.Hooks.InsertPosition                   ( insertPosition, Focus(Newer),
                                                      Position(End) )

import           XMonad.Hooks.StatusBar              (StatusBarConfig,
                                                      statusBarProp, withSB)
import           XMonad.Hooks.StatusBar.PP           (PP (..),
                                                      shorten', wrap,
                                                      xmobarAction,
                                                      xmobarBorder, xmobarColor,
                                                      xmobarFont, xmobarStrip)

-- layouts
import           XMonad.Layout.Circle                ( Circle(..) )
import           XMonad.Layout.HintedGrid            ( Grid(GridRatio) )
import           XMonad.Layout.LayoutCombinators     ( (|||))
import           XMonad.Layout.Mosaic                ( mosaic )
import           XMonad.Layout.MultiToggle           ( Toggle(..) , mkToggle1 )
import           XMonad.Layout.MultiToggle.Instances ( StdTransformers ( MIRROR , NBFULL))

import           XMonad.Layout.NoBorders             ( noBorders, smartBorders )
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
import           XMonad.Layout.Magnifier             (magnifiercz')


-- utils
import           XMonad.Util.EZConfig                ( checkKeymap , mkKeymap )
import           XMonad.Util.WorkspaceCompare        ( getSortByIndex )
import           XMonad.Util.Ungrab                  ( unGrab )
import qualified XMonad.Util.Hacks as Hacks

-- prompt
import qualified XMonad.Prompt                       as Prompt
import           XMonad.Prompt.ConfirmPrompt         ( confirmPrompt )
import           XMonad.Prompt.Input                 ( inputPromptWithCompl , (?+))
import           XMonad.Prompt.Shell                 ( shellPrompt )
import qualified XMonad.Prompt.Window                as WPrompt

-- actions
import qualified XMonad.Actions.CycleWS              as CycleWS
import           XMonad.Actions.CycleWindows         ( cycleRecentWindows )
import           XMonad.Actions.Warp                 ( warpToWindow )
import           XMonad.Actions.WorkspaceNames       ( swapWithCurrent )
import           XMonad.Actions.RotSlaves            (rotSlavesDown, rotAllDown)
import           XMonad.Actions.WithAll              (sinkAll, killAll)

import qualified XMonad.StackSet                     as W

import           Control.Monad                       ( liftM2 )
import qualified Data.List                           as L
import           Data.List.Split                     ( chunksOf )
import qualified Data.Map                            as M
import qualified Data.Text                           as T
import           Data.Char                           ( toLower )
import           Data.Ratio                          ( (%) )
import           XMonad.Util.SpawnOnce               (spawnOnce)
import           XMonad.Util.Loggers                 (xmobarColorL)
import           Data.Semigroup (All)
import qualified XMonad.Config.Prime                 as ManageHelpers

import           System.Exit                         (exitSuccess)

------------------------------------------------------------------------
-- Main
--
main :: IO ()
main = do
     xmonad . Hacks.javaHack . withSB mySB . ewmhFullscreen . ewmh $ UH.withUrgencyHook UH.NoUrgencyHook  myConfig


------------------------------------------------------------------------
-- Config
--
myConfig = def { borderWidth        = myBorderWidth
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

myTmuxTerminal :: String
myTmuxTerminal = myTerminal ++ " -e tmux attach"

-- Launcher
myLauncher :: String
myLauncher = "rofi -show drun -modi run,drun,window"

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
myWorkspaces = map show [1..16 :: Int]

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
        $ smartBorders
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
    my3cmi       = magnifiercz' 1.4 $ ThreeColMid 1 (3 / 100) (1 / 2)
    mySpiral     = spiral (6 / 7)
    myMosaic     = mosaic 2 [3, 2]
    myHintedGrid = GridRatio (4 / 3) False
    myOneBig     = OneBig (4 / 6) (4 / 6)
    myTabbed     = noBorders ( TB.tabbed shrinkText myTabConfig)



mySB :: StatusBarConfig
mySB = statusBarProp "xmobar" (pure myXmobarPP)
 where
  myXmobarPP :: PP
  myXmobarPP = def
    { ppSep             = wrapSep " "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = xmobarFont 5 . green  . xmobarBorder "Bottom" TH.darkGreen  2 . currentWorkspace
    , ppVisible         = xmobarFont 5 . lowWhite .xmobarBorder "Bottom" TH.darkGray 2 . occupiedWorkspace
    , ppHidden          = xmobarFont 5 . blue . occupiedWorkspace
    , ppHiddenNoWindows = xmobarFont 5 . gray . emptyWorkspace
    , ppUrgent          = xmobarFont 5 . red . urgentWorkspace
    , ppWsSep           = xmobarColor "" background "  "
    , ppTitle           = brightBlue. xmobarAction "xdotool key Super+shift+c" "2" . shorten 40
    , ppSort            =  getSortByIndex
    , ppOrder           = \[ws, l, t, ex] -> [ws, l, ex, t]
    , ppExtras          = [xmobarColorL TH.darkWhite background windowCount]
    , ppLayout          = blue . xmobarAction "xdotool key Super+/" "1" . xmobarAction
                            "xdotool key Super+shift+/"
                            "3" . xmobarFont 5 . myPPLayout
    }
   where
    shorten :: Int -> String -> String
    shorten = shorten' "…"

    wrapSep :: String -> String
    wrapSep = wrap (xmobarColor TH.darkBlack "" (xmobarFont 5 "\xe0b4"))
                   (xmobarColor TH.darkBlack "" (xmobarFont 5 "\xe0b6"))

    currentWorkspace :: String -> String
    currentWorkspace _ = "\61713" -- " "

    occupiedWorkspace :: String -> String
    occupiedWorkspace _ = "\61842" -- " "

    emptyWorkspace :: String -> String
    emptyWorkspace _ = "\61708" -- 

    urgentWorkspace :: String -> String
    urgentWorkspace _ = "\62759" -- 

    background :: String
    background = TH.darkBlack  ++ ":5"

    magenta  = xmobarColor TH.darkMagenta  background
    brightBlue  = xmobarColor TH.brightBlue background
    blue     = xmobarColor TH.darkBlue  background
    red      = xmobarColor TH.darkRed  background
    lowWhite = xmobarColor TH.darkWhite  background
    gray     = xmobarColor TH.darkGray background
    green    = xmobarColor TH.darkGreen  background

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
------------------------------------------------------------------------
-- Prompt
--
-- Prompt configuration
myPrompt :: Prompt.XPConfig
myPrompt = def
    { Prompt.font = TH.myFont
    , Prompt.fgColor           = TH.brightBlue
    , Prompt.bgColor           = TH.background
    , Prompt.borderColor       = TH.darkBlack
    , Prompt.height            = 22
    , Prompt.promptBorderWidth = 0
    , Prompt.autoComplete      = Just 100000
    , Prompt.bgHLight          = TH.darkBlack
    , Prompt.fgHLight          = TH.brightBlack
    , Prompt.position          = Prompt.Top
    , Prompt.maxComplRows      = Just 5
    , Prompt.searchPredicate   = L.isPrefixOf
    }

myPromptInfix :: Prompt.XPConfig
myPromptInfix = myPrompt { Prompt.searchPredicate = L.isInfixOf }

myLayoutPrompt :: X ()
myLayoutPrompt =
    inputPromptWithCompl
            myPrompt { Prompt.autoComplete = Just 1000 }
            "Layout"
            (Prompt.mkComplFunFromList'
                myPrompt { Prompt.autoComplete = Just 1000 } -- TODO why do I need to pass this twice ??
                [ "1.Tall"
                , "2.HintedGrid"
                , "3.OneBig"
                , "4.Circle"
                , "5.Mosaic"
                , "6.ThreeCol"
                , "7.Spiral"
                ]
            )
        ?+ \l -> sendMessage $ JumpToLayout $ drop 2 l

mySessionPrompt :: X ()
mySessionPrompt =
    inputPromptWithCompl
            myPrompt { Prompt.autoComplete = Just 1000 }
            "\x23FB " -- ⏻
            (Prompt.mkComplFunFromList'
            myPrompt { Prompt.autoComplete = Just 1000 } -- TODO why do I need to pass this twice ??
            ["1.Lock", "2.Suspend", "3.Reboot", "4.Shutdown", "5.Exit"])
        ?+ \l -> prompt $ map toLower $ drop 2 l
  where
    prompt = \x -> case x of
        "lock"     -> noConfirm x
        "suspend"  -> noConfirm x
        "reboot"   -> confirm x
        "shutdown" -> confirm x
        "exit"     -> confirmPrompt myPrompt x $ io exitSuccess
        _ -> noConfirm "lock"
      where
        confirm command = confirmPrompt myPrompt command
            $ spawn ("$HOME/.scripts/i3lock.sh " ++ command)
        noConfirm command = spawn ("$HOME/.scripts/i3lock.sh " ++ command)


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
          , [(className =? "firefox" <&&> resource =? "Dialog") --> doFloat ] -- Float Firefox Dialog
          , [ManageHelpers.isFullscreen -->  ManageHelpers.doFullFloat]
          , [ManageHelpers.isDialog --> ManageHelpers.doFloat]
          ]
  where
    myCenterFloats = ["zenity", "Arandr", "Galculator", "albert"]
    myTitleCenterFloats =
        [ "File Operation Progress"
        , "Downloads"
        , "Save as..."
        , "Ulauncher Preferences"
        ]
    myClassFloats =
      [ "confirm"
      , "file_progress"
      , "dialog"
      , "download"
      , "error"
      , "Gimp"
      , "notification"
      , "pinentry-gtk-2"
      , "splash"
      , "toolbar"
      ]
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
      ++ TH.darkBlack
      ++ "' --tint-level 255 --grow-gravity NE --icon-gravity NW --icon-size 20 --sticky --window-type dock --window-strut top --skip-taskbar"
      )

myHandleEventHook :: Event -> X All
myHandleEventHook =
    ManageDocks.docksEventHook <+> handleEventHook def

------------------------------------------------------------------------
-- tabs
--
myTabConfig :: TB.Theme
myTabConfig = def { TB.activeColor         = myFocusedBorderColor -- "#556064"
                  , TB.inactiveColor       = myNormalBorderColor -- "#2F3D44"
                  , TB.urgentColor         = TH.brightMagenta  -- "#FDF6E3"
                  , TB.activeBorderColor   = myFocusedBorderColor -- "#454948"
                  , TB.inactiveBorderColor = myNormalBorderColor -- "#454948"
                  , TB.urgentBorderColor   = TH.brightMagenta  -- "#268BD2"
                  -- , TB.activeTextColor     =  -- "#80FFF9"
                  -- , TB.inactiveTextColor   =  -- "#1ABC9C"
                  -- , TB.urgentTextColor     =  -- "#1ABC9C"
                  , TB.fontName = TH.myFont -- "xft:Noto Sans CJK:size=10:antialias=true"
                  }
-- ------------------------------------------------------------------------

myNormalBorderColor :: String
myNormalBorderColor = TH.background
myFocusedBorderColor :: String
myFocusedBorderColor = TH.darkCyan
-- border width
myBorderWidth :: Dimension
myBorderWidth = 3



-------------------------------------------------------------------------
-- Keybinding hints
--
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
    where desc = fmtHint myKeymapH TH.brightBlue TH.brightGreen TH.brightRed 220

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
    [ ( "M-C-o"
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
    , ( "M-S-\\"
      , sendMessage FirstLayout
      , LayoutLabel
      , "First layout"
      )
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
    , ("M-g i", sendMessage $ Toggle MIRROR   , LayoutLabel, "Toggle mirror") -- Toggle Mirror layout
    , ("M-S-<Tab>", rotSlavesDown, LayoutLabel, "Rotate slave windows")
    , ("M-C-<Tab>", rotAllDown, LayoutLabel, "Rotate all windows")
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
    , ("M-c a", sinkAll , ClientLabel, "Sink all floating")
    ]
    where
      centerR = W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
      bigCenterR = W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)
      leftR = W.RationalRect 0 (1 / 8) (1 / 2) (3 / 4)
      rightR = W.RationalRect (4 / 8) (1 / 8) (1 / 2) (3 / 4)

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
    , ( "M-C-<Return>"
      , spawn myConsoleFileManager
      , LauncherLabel
      , "Vifm"
      )
    , ( "M-, b"
      , spawn myBrowser
      , LauncherLabel
      , "Browser"
      ) -- Launch browser
    , ( "M-, k"
      , spawn "xkill"
      , LauncherLabel
      , "Kill Window"
      ) -- Kill window
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
    [ ( "M-S-q" , kill , ClientLabel , "Kill focused")
    , ( "M-S-C-q" , killAll , ClientLabel , "Kill all in workspace")
       -- Toggle struts
    , ( "M-b"
      , sendMessage ManageDocks.ToggleStruts
      , MiscLabel
      , "Toggle statusbar"
      )
      --  Search a window and focus into the window
    , ( "M-g g"
      , WPrompt.windowPrompt myPromptInfix WPrompt.Goto WPrompt.allWindows
      , MiscLabel
      , "Search and go to client"
      )
      --  Search a window and bring to the current workspace
    , ( "M-g b"
      , WPrompt.windowPrompt myPromptInfix WPrompt.Bring WPrompt.allWindows
      , MiscLabel
      , "Search and bring client"
      )
    , ( "M-g l"
      , myLayoutPrompt
      , MiscLabel
      , "LayoutLabel menu"
      )
       -- Resize viewed windows to the correct size
    , ("M-r",  refresh, MiscLabel, "Reset window size to default")
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
    , ( "M-S-<Space>"
      , shellPrompt myPrompt
      , MiscLabel
      , "Shell launcher"
      ) -- launch apps
    , ( "M-<Space>"
      , spawn myLauncher
      , MiscLabel
      , "Launcher"
      ) -- launch apps
    , ("M-<Esc>", mySessionPrompt   ,  MiscLabel, "Log menu")
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
