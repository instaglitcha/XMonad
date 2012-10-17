import Control.Monad (liftM2)

import Data.Char (isSpace)

import Dzen

import System.IO (Handle, hPutStrLn)

import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders

import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowArranger

import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Define terminal
myTerminal :: String
myTerminal = "urxvtc"

-- Define focused window border color
myFocusedBorderColor :: String
myFocusedBorderColor = "#ffffff"

-- Define border width
myBorderWidth :: Dimension
myBorderWidth = 2

-- Define workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["main","web","chat","media","graph","browse", "text", "dev","mail"]

-- The layout that tiles all windows
tiled = spacing 10 $ Tall nmaster delta ratio
    where
        -- Default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        -- Percentage of screen to increment when resizing panes
        delta   = 3/100

-- Layout that maximizes all windows
fullscreen = noBorders $ Full

-- Layout that floats all windows
floating :: Eq a => (ModifiedLayout WindowArranger SimplestFloat) a
floating = simplestFloat

-- Defaults set of layouts to be used on workspaces
def  = tiled ||| Mirror tiled ||| fullscreen
full = fullscreen ||| tiled ||| Mirror tiled

-- Define layouts to be used on workspaces
myLayouts = avoidStruts $ onWorkspace "web" full $ onWorkspace "chat" floating $ onWorkspace "media" full $
    onWorkspace "graph" full $ onWorkspace "browse" floating $ onWorkspace "text" full $
    onWorkspace "dev" full $ onWorkspace "mail" full $ def

-- Define the workspace an application has to go to
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [
        -- Applications that go to web
          [ className =? b --> viewShift "web"    | b <- myClassWebShifts ]
        -- Applications that go to chat
        , [ className =? c --> viewShift "chat"   | c <- myClassChatShifts ]
        -- Applications that go to media
        , [ className =? d --> viewShift "media"  | d <- myClassMediaShifts ]
        -- Applications that go to graph
        , [ className =? e --> viewShift "graph"  | e <- myClassGraphShifts ]
        -- Applications that go to browse
        , [ className =? f --> viewShift "browse" | f <- myClassBrowseShifts ]
        -- Applications that go to text
        , [ className =? g --> viewShift "text"   | g <- myClassTextShifts ]
        -- Applications that go to dev
        , [ className =? h --> viewShift "dev"    | h <- myClassDevShifts ]
        -- Applications that go to mail
        , [ className =? i --> viewShift "mail"   | i <- myClassMailShifts ]
        -- Applications that need floating regardless of workspace
        , [ className =? j --> doCenterFloat      | j <- myClassFloats ]
    ]
    where
        viewShift           = doF . liftM2 (.) W.greedyView W.shift
        myClassWebShifts    = ["Firefox","Filezilla"]
        myClassChatShifts   = ["Pidgin"]
        myClassMediaShifts  = ["mplayer2"]
        myClassGraphShifts  = ["Gimp"]
        myClassBrowseShifts = ["Nautilus","File-roller"]
        myClassTextShifts   = ["Zathura"]
        myClassDevShifts    = ["jetbrains-idea"]
        myClassMailShifts   = ["Thunderbird"]
        myClassFloats       = ["Run.py", "feh", "Zenity","Pavucontrol"]

-- Define scratchpads
myScratchPads :: NamedScratchpads
myScratchPads =
    [
        NS "terminal" spawnTerm findTerm manageTerm
    ]
    where
        spawnTerm  = myTerminal ++ " -name scratchpad"
        findTerm   = resource  =? "scratchpad"
        manageTerm = customFloating $ W.RationalRect 0.3 0.2 0.4 0.4

-- Define new key combinations to be added
keysToAdd x =
    [
        -- Run dialog
          ((mod5Mask, xK_r), spawn "~/.run/run.py --interface=full")
        -- Gnome close window
        , ((modMask x, xK_c), kill)
        -- Shift to prevous workspace
        , (((modMask x .|. controlMask), xK_Left), moveTo Prev (WSIs notSP))
        -- Shift to next window
        , (((modMask x .|. controlMask), xK_Right), moveTo Next (WSIs notSP))
        -- Shift window to previous workspace
        , (((modMask x .|. shiftMask), xK_Left), shiftTo Prev (WSIs notSP))
        -- Shift window to next workspace
        , (((modMask x .|. shiftMask), xK_Right), shiftTo Next (WSIs notSP))
        -- Focus most recent urgent window
        , ((modMask x, xK_u), focusUrgent)
        -- Open a scratchpad terminal
        , ((modMask x, xK_o), namedScratchpadAction myScratchPads "terminal")
        -- Lock and turn off screen
        , (((modMask x .|. controlMask), xK_l), spawn "i3lock -d -c 000000")
        -- Toggle volume
        , ((modMask x, xK_m), spawn "/home/bart/volume_pulse_audio.sh mute")
        -- Decrease volume
        , ((modMask x, xK_KP_Subtract), spawn "/home/bart/volume_pulse_audio.sh down")
        -- Increase volume
        , ((modMask x, xK_KP_Add), spawn "/home/bart/volume_pulse_audio.sh up")
        -- Play / pause song in mpd
        , ((modMask x, xK_p), spawn "ncmpcpp toggle")
        -- Play previous song in mpd
        , ((modMask x, xK_Left), spawn "ncmpcpp prev")
        -- Play next song in mpd
        , ((modMask x, xK_Right), spawn "ncmpcpp next")
        -- Increment the number of windows in the master area
        , ((modMask x .|. shiftMask, xK_KP_Add), sendMessage (IncMasterN 1))
        -- Deincrement the number of windows in the master area
        , ((modMask x .|. shiftMask, xK_KP_Subtract), sendMessage (IncMasterN (-1)))
        -- Remove borders
        , ((modMask x,  xK_d), withFocused toggleBorder)
        -- Gnome system monitor
        , (((modMask x .|. controlMask), xK_Delete), spawn "gnome-system-monitor")
        -- Rebind mod + q: custom restart xmonad script
        , ((modMask x, xK_q), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
        -- Rebind mod + shift + q: custom shutdown script
        , (((modMask x .|. shiftMask), xK_q), spawn "leave")
        -- Toggle struts
        , ((modMask x, xK_b), sendMessage ToggleStruts)
        -- Take screenshot
        , ((0, xK_Print), spawn "gnome-screenshot --interactive")
    ]
    ++
    [
        -- Focus workspace / shift workspace
        ((modMask x .|. m, k), windows $ f i)
        | (i,k) <- zip (myWorkspaces) [xK_F1 .. xK_F9]
        , (f,m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
    where
        notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)

-- Define existing key combinations to be removed
keysToRemove x =
    [
        -- Old mod + p behavior: launch dmenu
           (modMask x, xK_p)
        -- Old mod + r behavior: switch multihead
        ,  (mod5Mask, xK_r)
        -- Unused close window binding
        , (modMask x .|. shiftMask, xK_c)
        -- Unused gmrun binding
        , (modMask x .|. shiftMask, xK_p)
        -- Unused increase master pane count
        , (modMask x, xK_comma)
        -- Unused decrease master pane count
        , (modMask x, xK_period)
        -- Old mod + q behavior: restart xmonad
        , (modMask x, xK_q)
        -- Old mod + shift + q behavior: shutdown xmonad
        , (modMask x .|. shiftMask, xK_q)
        -- Old mod + m behavior
        , (modMask x, xK_m)
        -- Old mod + enter behavior
        , (modMask x, xK_Return)
    ]
    ++
    [
        (shiftMask .|. modMask x, k) | k <- [xK_1 .. xK_9]
    ]
    ++
    [
        (modMask x, k) | k <- [xK_1 .. xK_9]
    ]

-- Delete the key combinations to be removed from the original keys
newKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)

-- Merge new key combinations with existing keys
myKeys x = M.union (newKeys x) (M.fromList (keysToAdd x))

-- Log hook that prints out everything to a dzen handler
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ myPrettyPrinter h

-- Pretty printer for dzen workspace bar
myPrettyPrinter :: Handle -> PP
myPrettyPrinter h = dzenPP
    {
      ppOutput          = hPutStrLn h
    , ppCurrent         = dzenColor "#000000" "#e5e5e5" . pad
    , ppHidden          = dzenColor "#e5e5e5" "#000000" . pad . clickable myWorkspaces . trimSpace . noScratchPad
    , ppHiddenNoWindows = dzenColor "#444444" "#000000" . pad . clickable myWorkspaces . trimSpace . noScratchPad
    , ppUrgent          = dzenColor "#ff0000" "#000000". pad . clickable myWorkspaces . trimSpace . dzenStrip
    , ppWsSep           = " "
    , ppSep             = "  |  "
    , ppTitle           = (" " ++) . dzenColor "#ffffff" "#000000" . shorten 50 . dzenEscape
    , ppLayout          = dzenColor "#964AFF" "#000000" . pad .
                          (\x -> case x of
                            "Spacing 10 Tall"           -> "Tall"
                            "SimplestFloat"             -> "Float"
                            "Mirror Spacing 10 Tall"    -> "Mirror"
                            _                           -> x
                          )
    }
    where
        noScratchPad ws = if ws /= "NSP" then ws else ""

-- Wraps a workspace name with a dzen clickable action that focusses that workspace
clickable :: [String] -> String -> String
clickable workspaces workspace = clickableExp workspaces 1 workspace

clickableExp :: [String] -> Int -> String -> String
clickableExp [] _ ws = ws
clickableExp (ws:other) n l | l == ws = "^ca(1,xdotool key alt+F" ++ show (n) ++ ")" ++ ws ++ "^ca()"
                            | otherwise = clickableExp other (n+1) l

-- Trims leading and trailing white space
trimSpace :: String -> String
trimSpace = f . f
    where f = reverse . dropWhile isSpace

myDzenFont :: String
myDzenFont = "Bitstream Sans Vera:pixelsize=11"

-- Workspace dzen bar
myWorkDzen :: DzenConf
myWorkDzen = DzenConf {
      x_position    = Just 0
    , y_position    = Just 0
    , width         = Just 1920
    , height        = Just 24
    , alignment     = Just LeftAlign
    , font          = Just myDzenFont
    , fg_color      = Just "#ffffff"
    , bg_color      = Just "#000000"
    , exec          = []
    , addargs       = []
}

-- Mail dzen bar
myMailDzen :: DzenConf
myMailDzen = DzenConf {
      x_position    = Just 0
    , y_position    = Just 1176
    , width         = Just 135
    , height        = Just 24
    , alignment     = Just LeftAlign
    , font          = Just myDzenFont
    , fg_color      = Just "#06a1ff"
    , bg_color      = Just "#000000"
    , exec          = []
    , addargs       = []
}

-- Mpd dzen bar
myMpdDzen :: DzenConf
myMpdDzen = DzenConf {
      x_position    = Just 135
    , y_position    = Just 1176
    , width         = Just 825
    , height        = Just 24
    , alignment     = Just LeftAlign
    , font          = Just myDzenFont
    , fg_color      = Just "#ffd000"
    , bg_color      = Just "#000000"
    , exec          = []
    , addargs       = []
}

-- Time and date bar
myClockDzen :: DzenConf
myClockDzen = DzenConf {
      x_position    = Just 960
    , y_position    = Just 1176
    , width         = Just 960
    , height        = Just 24
    , alignment     = Just RightAlign
    , font          = Just myDzenFont
    , fg_color      = Just "#000000"
    , bg_color      = Just "#000000"
    , exec          = []
    , addargs       = []
}

-- Main function that launches xmonad
main :: IO ()
main =  do
    workspaceBar <- spawnDzen myWorkDzen
    spawnToDzen "conky -c .conkyrc_mail" myMailDzen
    spawnToDzen "conky -c .conkyrc_mpd" myMpdDzen
    spawnToDzen "conky -c .conkyrc_date" myClockDzen
    spawn "/home/bart/transparency"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
          terminal              = myTerminal
        , focusedBorderColor    = myFocusedBorderColor
        , borderWidth           = myBorderWidth
        , workspaces            = myWorkspaces
        , keys                  = myKeys
        , layoutHook            = myLayouts
        , logHook               = takeTopFocus >> myLogHook workspaceBar
        , manageHook            = manageDocks <+> myManageHook <+> namedScratchpadManageHook myScratchPads
        , startupHook           = ewmhDesktopsStartup >> setWMName "LG3D"
        , focusFollowsMouse     = False
    }
