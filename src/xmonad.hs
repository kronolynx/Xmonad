{-# LANGUAGE UnicodeSyntax #-}


module Main (main) where
--
-- If there are more than 3 explicit imports required a qualified import is used
-- If a type has only one constructor it is imported implicitly with (..)
--
--
import           XMonad                              hiding ( (|||) )
import           XMonad.Config.Mate                 (mateConfig)
import           XMonad.Config.Xfce                 (xfceConfig)

import qualified Theme.Theme                         as TH

-- config
import           XMonad.Config.Desktop              (desktopConfig)

-- hooks
import           XMonad.Hooks.EwmhDesktops            ( ewmhFullscreen, ewmh, setEwmhActivateHook, addEwmhWorkspaceSort )
import           XMonad.Hooks.FloatNext               ( floatNextHook )
import qualified XMonad.Hooks.ManageDocks            as ManageDocks
import qualified XMonad.Hooks.ManageHelpers          as ManageHelpers
import qualified XMonad.Hooks.UrgencyHook            as UH
import           XMonad.Hooks.InsertPosition         ( insertPosition, Focus(Newer),
                                                      Position(End) )

import           XMonad.Hooks.StatusBar              (StatusBarConfig,
                                                      statusBarProp, withSB)
import           XMonad.Hooks.StatusBar.PP           (PP (..),
                                                      shorten', wrap,
                                                      xmobarColor, xmobarStrip, xmobarAction, filterOutWsPP)

-- layouts
import           XMonad.Layout.HintedGrid            ( Grid(GridRatio) )
import           XMonad.Layout.LayoutCombinators     ( (|||))
import           XMonad.Layout.Mosaic                ( mosaic )
import           XMonad.Layout.MultiToggle           ( Toggle(..) , mkToggle1 )
import           XMonad.Layout.MultiToggle.Instances ( StdTransformers ( MIRROR , NBFULL))

import           XMonad.Layout.NoBorders             ( noBorders, smartBorders, hasBorder )
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
import           XMonad.Actions.EasyMotion (EasyMotionConfig (..), selectWindow, textSize)

import qualified XMonad.StackSet                     as W

import           XMonad.Util.SpawnOnce               (spawnOnce)
import           XMonad.Util.Loggers                 (xmobarColorL, wrapL)
import           XMonad.Util.NamedScratchpad
                                                     (customFloating,
                                                      namedScratchpadAction,
                                                      namedScratchpadManageHook,
                                                      NamedScratchpad(NS),
                                                      scratchpadWorkspaceTag
                                                      )
import           XMonad.Util.WorkspaceCompare        (filterOutWs)

import           Control.Monad                       ( liftM2 )
import qualified Data.List                           as L
import           Data.List.Split                     ( chunksOf )
import qualified Data.Map                            as M
import qualified Data.Text                           as T
import           Data.Char                           ( toLower )
import           Data.Ratio                          ( (%) )

import           System.Environment                  (lookupEnv)
import           Data.Maybe                          (fromMaybe)
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

import qualified DBus as D
import qualified DBus.Client as D
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)

------------------------------------------------------------------------
-- Main
--
main :: IO ()
main = do
    session <- lookupEnv "DESKTOP_SESSION"
    let defDesktopConfig = maybe desktopConfig desktop session
    let myDesktopConfig = defDesktopConfig {
            borderWidth        = myBorderWidth
          , normalBorderColor  = myNormalBorderColor
          , focusedBorderColor = myFocusedBorderColor
          , focusFollowsMouse  = myFocusFollowsMouse
          , modMask            = myModMask
          , terminal           = myTerminal
          , workspaces         = myWorkspaces
          , mouseBindings      = myMouseBindings
          , keys               = myKeys
          , manageHook         = myManageHook <+> manageHook desktopConfig
          , layoutHook         = myLayout
          , startupHook        = myStartupHook <+> checkKeymap myDesktopConfig myKeymap
          --  , logHook = refocusLastLogHook
          --           >> nsHideOnFocusLoss myScratchPads
        }

    if session == Just "xmonad"
      then do
        dbus <- D.connectSession
        -- Request access to the DBus name
        _ <- D.requestName dbus (D.busName_ "org.xmonad.Log")
            [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

        let urgencyConfig = UH.UrgencyConfig UH.Focused UH.Dont
        let urgencyStyle = UH.BorderUrgencyHook TH.brightMagenta
        xmonad . Hacks.javaHack . setEwmhActivateHook UH.doAskUrgent . ManageDocks.docks . ewmhFullscreen . ewmh  $ UH.withUrgencyHookC urgencyStyle urgencyConfig myDesktopConfig {
          startupHook = myStartupHook <+> do
            checkKeymap myDesktopConfig myKeymap
            spawnOnce "polybar bar-xmonad"
          , logHook = myPolybarLogHook dbus
        }
    else if session == Just "xmonad-xmobar"
      then do
        let urgencyConfig = UH.UrgencyConfig UH.Focused UH.Dont
        let urgencyStyle = UH.BorderUrgencyHook TH.brightMagenta
        xmonad . Hacks.javaHack . withSB myXmobarSB . ewmhFullscreen . ewmh  $ UH.withUrgencyHookC urgencyStyle urgencyConfig myDesktopConfig {
          startupHook = myStartupHook <+> do
            checkKeymap myDesktopConfig myKeymap
            spawnOnce "stalonetray"
        }
    else do xmonad . addEwmhWorkspaceSort (pure myFilter) . setEwmhActivateHook UH.doAskUrgent . ManageDocks.docks . ewmhFullscreen . ewmh $ UH.withUrgencyHook UH.NoUrgencyHook $ myDesktopConfig
    where
      myFilter = filterOutWs [scratchpadWorkspaceTag]





desktop "mate"   = mateConfig
desktop "xfce"   = xfceConfig
desktop _        = desktopConfig

-- Read environment variables or use default
envVar :: String -> String -> String
envVar envName defaultVar =
   unsafeDupablePerformIO var
   where
     var = do
       maybeEnv <- lookupEnv envName
       return $ fromMaybe defaultVar maybeEnv

-- envVar :: String -> String -> IO (Maybe String)
-- envVar envName defaultValue =
--    lookupEnv envName

------------------------------------------------------------------------
-- Default Apps
--
-- Capture Screen
myScreenCapture :: String
myScreenCapture = "$HOME/.scripts/screen_shot.sh"

myTerminal :: String
myTerminal = envVar "TERMINAL" "terminator"

-- Launcher
myLauncher :: String
myLauncher = "rofi -show drun -modi run,drun -show-icons"

myWindowSelector :: String
myWindowSelector = "rofi -show window -show-icons"

myClipboard :: String
myClipboard = "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"

-- Browser
myBrowser :: String
myBrowser = envVar "BROWSER" "firefox"

-- File Manager
myFileManager :: String
myFileManager = envVar "FILE_MANAGER" "thunar"

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
    "OneBig"          -> "\xf286" -- 
    "HintedGrid"      -> "\xfb8a" -- ﮊ
    _                 -> x

myWorkspaces :: [String]
myWorkspaces = map show [1..16 :: Int]

------------------------------------------------------------------------
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

-------------------------------------------------------------------------
-- Easymotion configuration
-------------------------------------------------------------------------
emConfig :: EasyMotionConfig
emConfig =
  def
    { txtCol = TH.brightYellow
    , bgCol = TH.background
    , borderCol = myFocusedBorderColor
    , overlayF = textSize
    , cancelKey = xK_Escape
    , emFont = TH.myFont
    , borderPx = 1
    }



-------------------------------------------------------------------------
-- Polybar
-------------------------------------------------------------------------
-- Requires https://github.com/xintron/xmonad-log
-- Enable module xmonad (replaces ewmh xwindow)
-- Formatting https://github.com/polybar/polybar/wiki/Formatting
myPolybarLogHook :: D.Client -> X ()
myPolybarLogHook dbus = dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] def {
  ppOutput           = dbusOutput dbus
  , ppSep             = " "
  , ppCurrent         = underLine TH.brightGreen  . fgColor TH.brightGreen . currentWorkspace
  , ppVisible         = underLine TH.brightBlue . fgColor TH.brightBlue . occupiedWorkspace
  , ppHidden          = fgColor TH.darkWhite . occupiedWorkspace
  , ppHiddenNoWindows = fgColor TH.darkGray . emptyWorkspace
  , ppUrgent          = fgColor TH.darkRed . urgentWorkspace
  , ppLayout          = wrap "%{A1:xdotool key super+\\ &:}%{T4}" "%{T-}%{A}" . myPPLayout
  , ppWsSep           = ""
  , ppTitle           =  const ""
  , ppSort            =  getSortByIndex
  , ppOrder           = \[ws, l, t, ex] -> [ws, l, ex, t]
  , ppExtras           = [wrapL "%{A1:rofi -show window -dpi 150 &:} " " %{A}" windowCount]
}
    where
      -- Emit a DBus signal on log updates
      dbusOutput :: D.Client -> String -> IO ()
      dbusOutput dbus' str = do
          let signal = (D.signal objectPath interfaceName memberName) {
                  D.signalBody = [D.toVariant str]
              }
          D.emit dbus' signal
        where
          objectPath = D.objectPath_ "/org/xmonad/Log"
          interfaceName = D.interfaceName_ "org.xmonad.Log"
          memberName = D.memberName_ "Update"

      shorten :: Int -> String -> String
      shorten = shorten' "…"

      wrapSep :: String -> String
      wrapSep = wrap (bgColor TH.darkBlack "\xe0b4")
                      (bgColor TH.darkBlack "\xe0b6")

      -- fontWrap font = wrap ("%{B" ++ font ++ "} ") " %{B-}"

      bgColor color = wrap ("%{B" ++ color ++ "} ") " %{B-}"
      fgColor color = wrap ("%{F" ++ color ++ "} ") " %{F-}"
      overLine color = wrap ("%{o" ++ color ++ "}%{+o} ") " %{-o}"
      underLine color = wrap ("%{u" ++ color ++ "}%{+u} ") " %{-u}"
      -- bgColor color =  ""



-------------------------------------------------------------------------
-- Xmobar
-------------------------------------------------------------------------
myXmobarSB :: StatusBarConfig
myXmobarSB = statusBarProp "xmobar"
     $ pure $ filterOutWsPP [scratchpadWorkspaceTag] myXmobarPP
 where
  myXmobarPP :: PP
  myXmobarPP = def
    { ppSep             = wrapSep " "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = xmobarColor TH.darkGreen  background . currentWorkspace
    , ppVisible         = xmobarColor TH.darkWhite  background . occupiedWorkspace
    , ppHidden          = xmobarColor TH.darkBlue  background . occupiedWorkspace
    , ppHiddenNoWindows = xmobarColor TH.darkGray background . emptyWorkspace
    , ppUrgent          = xmobarColor TH.darkRed  background . urgentWorkspace
    , ppWsSep           = xmobarColor "" background "  "
    , ppTitle           = xmobarColor TH.brightBlue background. xmobarAction "xdotool key Super+shift+c" "4" . shorten 60
    , ppSort            =  getSortByIndex
    , ppOrder           = \[ws, l, t, ex] -> [ws, l, ex, t]
    , ppExtras          = [xmobarColorL TH.darkWhite background windowCount]
    , ppLayout          = xmobarColor TH.darkBlue  background . xmobarAction "xdotool key Super+/" "1" . xmobarAction
                            "xdotool key Super+shift+/"
                            "3" . myPPLayout
    }
   where
    shorten :: Int -> String -> String
    shorten = shorten' "…"

    wrapSep :: String -> String
    wrapSep = wrap (xmobarColor TH.darkBlack ""  "\xe0b4")
                   (xmobarColor TH.darkBlack ""  "\xe0b6")

    background :: String
    background = TH.darkBlack  ++ ":5"


currentWorkspace :: String -> String
currentWorkspace _ = "\61713" -- " "

occupiedWorkspace :: String -> String
occupiedWorkspace _ = "\61842" -- " "

emptyWorkspace :: String -> String
emptyWorkspace _ = "\61708" -- 

urgentWorkspace :: String -> String
urgentWorkspace _ = "\62759" -- 

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

-------------------------------------------------------------------------
-- Prompt
-------------------------------------------------------------------------
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
                , "4.Mosaic"
                , "5.ThreeCol"
                , "6.Spiral"
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
            ["1.Lock", "2.Suspend", "3.ScreenOff", "4.Reboot", "5.Shutdown", "6.Logout"])
        ?+ \l -> prompt $ map toLower $ drop 2 l
  where
    prompt = \x -> case x of
        "lock"      -> noConfirm x
        "suspend"   -> noConfirm x
        "screenoff" -> noConfirm x
        "reboot"    -> confirm x
        "shutdown"  -> confirm x
        "logout"    -> confirm x
        _ -> noConfirm "lock"
      where
        confirm command = confirmPrompt myPrompt command
            $ spawn ("$HOME/.scripts/i3lock.sh " ++ command)
        noConfirm command = spawn ("$HOME/.scripts/i3lock.sh " ++ command)


-------------------------------------------------------------------------
-- Layouts
-------------------------------------------------------------------------
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
        ||| name "OneBig"     myOneBig
        ||| name "ThreeCol"   my3cmi
        ||| name "Mosaic"     myMosaic
        ||| name "Spiral"     mySpiral
        ||| name "Tabbed"     myTabbed
  where
    name n = renamed [Replace n] . mySpacing 8
    myTile       = RTile.ResizableTall 1 (3 / 100) (4 / 7) []
    -- my3cmi       = magnifiercz' 1.1 $ ThreeColMid 1 (3 / 100) (1 / 2)
    my3cmi       = ThreeColMid 1 (3 / 100) (1 / 2)
    mySpiral     = spiral (6 / 7)
    myMosaic     = mosaic 2 [3, 2]
    myHintedGrid = GridRatio (4 / 3) False
    myOneBig     = OneBig (4 / 6) (4 / 6)
    myTabbed     = noBorders ( TB.tabbed shrinkText myTabConfig)


-------------------------------------------------------------------------
-- Manage hook
-------------------------------------------------------------------------
      --
myManageHook :: ManageHook
myManageHook = composeAll
    [
     ManageDocks.manageDocks
     -- open windows at the end if they are not floating
    , fmap not (willFloat <||> checkModal) --> insertPosition End Newer
    , floatNextHook
    , myManageHook'
    ]

-- | Check if window is modal
checkModal :: Query Bool
checkModal = ManageHelpers.isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODAL"

checkSkipTaskbar :: Query Bool
checkSkipTaskbar = ManageHelpers.isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR"

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
          , [ className =? c --> doIgnore <+> hasBorder False | c <- myIgnores ]
          , [ title =? t --> doFloat | t <- myTitleFloats ]
          , [ className =? c --> ManageHelpers.doCenterFloat | c <- myCenterFloats ]
          , [ title =? t --> ManageHelpers.doCenterFloat | t <- myTitleCenterFloats ]
          , [ className =? c --> doShift (myWorkspaces !! ws) | (c, ws) <- myShifts ]
          , [ title =? c --> doShift (myWorkspaces !! ws) | (c, ws) <- myTitleShifts ]
          , [ className =? c --> hasBorder False  | c <- myClassNoBorder]
          , [ role =? r --> ManageHelpers.doCenterFloat | r <- myRoleCenterFloats]
          , [(className =? "firefox" <&&> resource =? "Dialog") --> doFloat ] -- Float Firefox Dialog
          , [ManageHelpers.isFullscreen -->  ManageHelpers.doFullFloat]
          , [ManageHelpers.isDialog --> doFloat]
          , [namedScratchpadManageHook myScratchPads]
          , [checkModal --> ManageHelpers.doCenterFloat]
          , [(className =? "plasmashell" <&&> checkSkipTaskbar) --> doIgnore <+> hasBorder False ] -- Ignore kde widgets
          ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    myIgnores = [ ]
    myCenterFloats = ["zenity"
      ,"Arandr"
      , "Galculator"
      ]
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
      , "Peek"
      , "yakuake"
      , "gpclient"
      ]
    myRoleCenterFloats = ["GtkFileChooserDialog"]
    myTitleFloats = ["Media viewer", "Yad"]
    myFullFloats  = []
      -- workspace numbers start at 0
    myShifts =
      [ ("Slack"             , 9)
      , ("Postman"           , 6)
      , ("DevCenter"         , 6)
      , ("jetbrains-idea-ce" , 1)
      , ("Chromium"          , 13)
      -- , ("Joplin"            , 6)
      , ("Transmission-gtk"  , 11)
      ]
    myTitleShifts =
      [ ("DevCenter"         , 6)
      ]
    myClassNoBorder =
      [
        "Yad"
      ]

-------------------------------------------------------------------------
-- Scratchpads
-------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [
    NS "terminal" spawnTerm findTerm manageScratch
  , NS "notes" spawnNotes findNotes manageScratch
 ]
  where
    spawnTerm  = "terminator -T scratchpad"
    findTerm   = title =? "scratchpad"
    spawnNotes = "joplin-desktop"
    findNotes  = className =? "Joplin"
    manageScratch = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w


-- Add things to start up here
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "$HOME/.config/xmonad/scripts/autostart.sh"


-------------------------------------------------------------------------
-- tabs
-------------------------------------------------------------------------
myTabConfig :: TB.Theme
myTabConfig = def { TB.activeColor         = myFocusedBorderColor -- "#556064"
                  , TB.inactiveColor       = myNormalBorderColor -- "#2F3D44"
                  , TB.urgentColor         = TH.brightMagenta  -- "#FDF6E3"
                  , TB.activeBorderColor   = myFocusedBorderColor -- "#454948"
                  , TB.inactiveBorderColor = myNormalBorderColor -- "#454948"
                  , TB.urgentBorderColor   = TH.brightMagenta  -- "#268BD2"
                  , TB.fontName = TH.myFont -- "xft:Noto Sans CJK:size=10:antialias=true"
                  }
-- ----------------------------------------------------------------------

myNormalBorderColor :: String
myNormalBorderColor = TH.background
myFocusedBorderColor :: String
myFocusedBorderColor = TH.brightGreen
-- border width
myBorderWidth :: Dimension
myBorderWidth = 3



-------------------------------------------------------------------------
-- Keybinding hints
-------------------------------------------------------------------------
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
  show ClientLabel   = "Client"
  show LayoutLabel   = "Layout"
  show TagLabel      = "Tag"
  show ScreenLabel   = "Screen"
  show LauncherLabel = "Launcher"
  show MediaLabel    = "Media"
  show MiscLabel     = "Misc"

type KeySequence = String
type Description = String
type KeyHint = (KeySequence, Label, Description)
type KeyMap = (KeySequence, X ())
type KeyMapHint = (KeySequence, X (), Label, Description)

fmtHint :: [KeyHint] -> String -> String -> String -> Int -> String
fmtHint keyMap colorBinding colorDesc colorTitle maxChars =
   "\"\\n" ++ L.intercalate "\\n" listKeyMap ++ "\""
  where
    colSize = charsPerCol maxChars
    emptyColRow = rAlign colSize "" ++ lAlign colSize ""
    sortedKeymap = sortKeymap keyMap
    colDescription = colDesc colorBinding colorDesc colorTitle colSize
    listKeyMap = buildKeyMap (buildColumns (map colDescription sortedKeymap)) emptyColRow

    colDesc ::  String -> String -> String -> Int -> [KeyHint] -> [String]
    colDesc colorBinding' colorDesc' colorTitle' colSize' bindings =
        (colStr colorTitle' ++ rAlign colSize (getLabel bindings) ++ lAlign (colSize + 1) "") :
        [ colStr colorBinding'
              ++ rAlign colSize' key
              ++ " "
              ++ colStr colorDesc'
              ++ lAlign colSize' desc
        | (key, _, desc) <- bindings
        ]

    charsPerCol :: Int -> Int
    charsPerCol maxChars' = quot (quot maxChars' 3) 2 -- 3 columns by 2 (key, description)

    getLabel :: [KeyHint] -> String
    getLabel ((_, label, _):_) =  show label
    getLabel [] = ""


    textAlign :: (Int -> Char -> T.Text -> T.Text) -> Int -> (String -> String)
    textAlign fAlign n = T.unpack . fAlign n ' ' . T.pack

    rAlign :: Int -> String -> String
    rAlign = textAlign T.justifyRight

    lAlign :: Int -> String -> String
    lAlign = textAlign T.justifyLeft

    trd :: (a, b, c) -> b
    trd (_, b, _) =  b

    buildColumns :: [[String]] -> [[String]]
    buildColumns keyGroups = columns
      where
        keyCol = concat keyGroups
        columnsLength = ceiling (fromIntegral(length keyCol) / 3 :: Double)
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

    sortKeymap :: [KeyHint] -> [[KeyHint]]
    sortKeymap = map sortByKeyBinding . groupByLabel

    sortByKeyBinding :: [KeyHint] -> [KeyHint]
    sortByKeyBinding = L.sortBy (\(a, _, _) (b, _ ,_ ) -> compare a b)

    groupByLabel :: [KeyHint] -> [[KeyHint]]
    groupByLabel = L.groupBy (\a b -> trd a == trd b) . L.sortBy (\a b -> compare (trd a)  (trd b))

colStr :: String -> String
colStr col = "^fg(" ++ col ++ ")"

showHelp :: X ()
showHelp = spawn $ unwords
    [ "$HOME/.config/xmonad/scripts/showHintForKeymap.sh"
    , desc
    ]
    where
      desc = fmtHint myKeyHints TH.brightBlue TH.brightGreen TH.brightRed 180
      myKeyHints = extraHints ++ [(keySeq, label, description') | (keySeq, _, label, description') <- myKeymapH]
      extraHints = [
          ("M " ++ myTagKeys, TagLabel, "Go to Tag")
          , ("M-S " ++ myTagKeys, TagLabel, "Move window to Tag")
          , ("M-C " ++ myTagKeys, TagLabel, "Send window to Tag")
          , ("M-C-S " ++ myTagKeys, TagLabel, "Swap with Tag")
        ]
      -- TODO find a way to make the key seq look better
      -- if I try to separate with a color then the string is not centered because of the length of the string
      myTagKeys = unwords ["1..9","0", "-", "=", "[", "]", ";" ,"'"]
      -- separator = colStr TH.darkBlack ++ "/" ++ colStr TH.brightBlue

------------------------------------------------------------------------
-- Mouse bindings
------------------------------------------------------------------------
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modMask } = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> do
      floats <- gets $ W.floating . windowset
      if (w `M.member` floats) then
        focus w >> mouseMoveWindow w >> windows W.shiftMaster
      else
        focus w >> windows W.shiftMaster
        )

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Key bindings
------------------------------------------------------------------------
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys config' = mkKeymap config' myKeymap

myKeymap :: [KeyMap]
myKeymap = myWorkspaceMovementKeys ++ [(keySeq, keyMap) | (keySeq, keyMap, _, _) <- myKeymapH]

-- Keys with hints
myKeymapH :: [KeyMapHint]
myKeymapH = concat
    [ myControlKeys
    , myLauncherKeys
    , myLayoutKeys
    , myWorkspaceKeys
    , myMovementKeys
    , myMediaLabelKeys
    , myFloatKeys
    ]

myWorkspaceMovementKeys :: [KeyMap]
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
        , ("M-C-S-", swapWithCurrent) -- swap workspace windows
        ]
    , (key   , ws  ) <- zip keys' myWorkspaces
    ]
  where
    keys'      = fmap return $ ['1' .. '9'] ++ ['0', '-', '=', '[', ']', ';' ,'\'']
    viewShift = liftM2 (.) W.greedyView W.shift

myMovementKeys :: [KeyMapHint]
myMovementKeys =
    myWindowMovementKeys
        ++ myWorkspaceMovementKeys'
        ++ myScreenMovementKeys
        ++ myGotoLayoutKeys

myWindowMovementKeys :: [KeyMapHint]
myWindowMovementKeys =
    [ ("M-<D>", windowGo Nav.D, ClientLabel, "Focus down")
    , ("M-<U>", windowGo Nav.U, ClientLabel, "Focus up")
    , ("M-<L>", windowGo Nav.L, ClientLabel, "Focus left")
    , ("M-<R>", windowGo Nav.R, ClientLabel, "Focus right")
    , ("M-j"  , windowGo Nav.D, ClientLabel, "Focus down")
    , ("M-k"  , windowGo Nav.U, ClientLabel, "Focus up")
    , ("M-h"  , windowGo Nav.L, ClientLabel, "Focus left")
    , ("M-l"  , windowGo Nav.R, ClientLabel, "Focus right")
    , ("M-s", selectWindow emConfig >>= (`whenJust` windows . W.focusWindow),
      ClientLabel, "Easymotion"
    )
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
myWorkspaceMovementKeys' :: [KeyMapHint]
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
myScreenMovementKeys :: [KeyMapHint]
myScreenMovementKeys =
    [ ("M-o"  , CycleWS.swapNextScreen , ScreenLabel, "Swap next screen")
    , ( "M-S-o"
      , sequence_ [CycleWS.nextScreen, warpToWindow (1 % 2) (1 % 2)]
      , ScreenLabel
      , "Focus next screen"
      ) -- Move the focus to next screen (multi screen)
    , ("M-C-o", CycleWS.shiftNextScreen, ScreenLabel, "Send client to next screen")
    ]
myGotoLayoutKeys :: [KeyMapHint]
myGotoLayoutKeys =
    [ ("M-g 1", jumpToLayout "Tall"      , LayoutLabel, "Tall")
    , ("M-g 2", jumpToLayout "HintedGrid", LayoutLabel, "HintedGrid")
    , ("M-g 3", jumpToLayout "Tabbed"    , LayoutLabel, "Tabbed")
    , ("M-g 4", jumpToLayout "OneBig"    , LayoutLabel, "OneBig")
    , ("M-g 5", jumpToLayout "Mosaic"    , LayoutLabel, "Mosaic")
    , ("M-g 6", jumpToLayout "ThreeCol"  , LayoutLabel, "Threecol")
    , ("M-g 7", jumpToLayout "Spiral"    , LayoutLabel, "spiral")
    ]
    where jumpToLayout = sendMessage . JumpToLayout

myLayoutKeys :: [KeyMapHint]
myLayoutKeys = myLayoutKeys' ++ myLayoutSwapKeys ++ myLayoutTransformKeys

myLayoutSwapKeys :: [KeyMapHint]
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

myLayoutKeys' :: [KeyMapHint]
myLayoutKeys' =
    [ ( "M-a"
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

myLayoutTransformKeys :: [KeyMapHint]
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

myWorkspaceKeys :: [KeyMapHint]
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
    , ("M-<Tab>", CycleWS.toggleWS' ["NSP"], TagLabel, "Toggle last workspace") -- toggle last workspace
    ]

myFloatKeys :: [KeyMapHint]
myFloatKeys =
    [ ("M-w s", withFocused $ windows . W.sink, ClientLabel, "Sink floating")
    , ("M-w b", withFocused $ windows . flip W.float bigCenterR, ClientLabel, "Float big center")
    , ("M-w c", withFocused $ windows . flip W.float centerR, ClientLabel, "Float center")
    , ("M-w l", withFocused $ windows . flip W.float leftR, ClientLabel, "Float left")
    , ("M-w r", withFocused $ windows . flip W.float rightR, ClientLabel, "Float right")
    , ("M-w a", sinkAll , ClientLabel, "Sink all floating")
    ]
    where
      centerR = W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
      bigCenterR = W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)
      leftR = W.RationalRect 0 (1 / 8) (1 / 2) (3 / 4)
      rightR = W.RationalRect (4 / 8) (1 / 8) (1 / 2) (3 / 4)

myLauncherKeys :: [KeyMapHint]
myLauncherKeys = myLauncherKeys' ++ myScreenCaptureKeys

myLauncherKeys' :: [KeyMapHint]
myLauncherKeys' =
    [ ( "M-<Return>"
      , spawn myTerminal
      , LauncherLabel
      , "Terminal"
      ) -- Launch terminal
    , ( "M-e"
      , spawn myFileManager
      , LauncherLabel
      , "File Manager"
      ) -- Launch FileManager
    , ( "M-S-e"
      , spawn myConsoleFileManager
      , LauncherLabel
      , "Vifm"
      )
    , ( "M-; b"
      , spawn myBrowser
      , LauncherLabel
      , "Browser"
      ) -- Launch browser
    , ( "M-; k"
      , spawn "xkill"
      , LauncherLabel
      , "Kill Window"
      ) -- Kill window
    , ( "M-C-S-x"
      , spawn "$HOME/.scripts/xbacklight-toggle.sh"
      , LauncherLabel
      , "Toggle backlight"
      )
    ]

myScreenCaptureKeys :: [KeyMapHint]
myScreenCaptureKeys =
    [ ( "<Print>"
      , spawn $ myScreenCapture ++ " desktop"
      , MiscLabel
      , "Take a screenshot (desktop)"
      )
    , ( "S-<Print>"
      , spawn $ myScreenCapture ++ " area"
      , MiscLabel
      , "Take a screenshot (area)"
      )
    , ( "C-<Print>" --
      , spawn $ myScreenCapture ++ " window"
      , MiscLabel
      , "Take a screenshot (window)"
      )
    ]

myMediaLabelKeys :: [KeyMapHint]
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

myControlKeys :: [KeyMapHint]
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
    , ( "M-C-S-<Space>"
      , shellPrompt myPrompt
      , MiscLabel
      , "Shell launcher"
      ) -- launch apps
    , ( "M-<Space>"
      , spawn myLauncher
      , MiscLabel
      , "Launcher"
      ) -- launch apps
    , ( "M-C-<Space>"
      , spawn myClipboard
      , MiscLabel
      , "Clipboard"
      )
    , ( "M-S-<Space>"
      , spawn myWindowSelector
      , MiscLabel
      , "window selector"
      )
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
    , ("M-q", namedScratchpadAction myScratchPads "terminal", MiscLabel, "Scratchpad")
    , ("M-n", namedScratchpadAction myScratchPads "notes", MiscLabel, "Scratchpad notes")
    , ("M-<F1>", unGrab >> showHelp, MiscLabel, "Show help")
    ]
