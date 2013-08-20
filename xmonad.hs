import XMonad
import Data.Monoid
import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.WindowNavigation

import XMonad.Layout.LayoutCombinators hiding ( (|||) )

import XMonad.Util.Run(spawnPipe)

myTerminal = "rxvt-unicode -e /usr/bin/tmux"
myBrowser = "conkeror"
myFocusFollowsMouse = True
myBorderWidth = 1
myModifier = mod4Mask
myWorkspaces = ["1:dev","2:web","3:skype"] ++ map show [3..9]
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

runOrRaiseIdiom modm key action windowClassName =
  [((modm .|. shiftMask, key), action),
   ((modm, key), raiseMaybe action (className =? windowClassName))]

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
  runOrRaiseIdiom modm xK_u (spawn $ XMonad.terminal conf) "URxvt" ++
  runOrRaiseIdiom modm xK_e (spawn "emacs") "Emacs" ++
  runOrRaiseIdiom modm xK_o (spawn myBrowser) "Conkeror" ++
  [ ((modm,                 xK_b),     bringMenuArgs ["-nb", "black", "-nf", "grey", "-sb", "#ee9a00"])
  , ((modm,                 xK_p),     spawn "dmenu_run")
  , ((modm .|. shiftMask,   xK_c),     kill)
  , ((modm,                 xK_space), sendMessage NextLayout)
  , ((modm .|. shiftMask,   xK_space), setLayout $ XMonad.layoutHook conf)
  , ((modm,                 xK_Tab),   windows W.focusDown)
  , ((modm,                 xK_j),     windows W.focusDown)
  , ((modm,                 xK_k),     windows W.focusUp)
  , ((modm,                 xK_m),     windows W.focusMaster)
  , ((modm .|. shiftMask,   xK_j),     windows W.swapDown)
  , ((modm .|. shiftMask,   xK_k),     windows W.swapUp)
  , ((modm .|. shiftMask,   xK_h),     sendMessage Shrink)
  , ((modm .|. shiftMask,   xK_l),     sendMessage Expand)
  , ((modm,                 xK_t),     withFocused $ windows . W.sink)
  , ((modm,                 xK_l),     sendMessage $ Go R)
  , ((modm,                 xK_h),     sendMessage $ Go L)
  , ((modm .|. controlMask, xK_l),     sendMessage $ Move R)
  , ((modm .|. controlMask, xK_h),     sendMessage $ Move L)
  , ((modm .|. controlMask, xK_s),     sendMessage SwapWindow)
  , ((modm .|. shiftMask,   xK_q),     io exitSuccess)
  , ((modm,                 xK_q),     spawn "xmonad --recompile; xmonad --restart")
  ]
  ++
  [((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_r] [0, 1]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                            >> windows W.shiftMaster)
  , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                            >> windows W.shiftMaster)
  ]

myManageHook =
  composeAll
  [ className =? "vlc"  --> doFloat
  , className =? "Gimp" --> doFloat
  , isFullscreen        --> doF W.focusDown <+> doFullFloat ]

myEventHook = fullscreenEventHook -- For Chrome fullscreen
myStartupHook = return ()
myLogHook xmproc = do
  dynamicLogWithPP $ xmobarPP {
    ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "#FFB6B0" "" . shorten 100
    , ppCurrent = xmobarColor "#CEFFAC" ""
    , ppSep = " | "
    , ppOrder = \(ws:_:t:_) -> [ws,t]
    }

myHook = windowNavigation . smartBorders . avoidStruts $ 
       (twoPanes ||| simpleTabbed ||| Mirror twoPanes)
  where
     ratio   = 1/2 -- default fraction of screen occupied by master pane
     delta   = 3/100 -- percentage of screen to increment by when resizing panes
     twoPanes = combineTwo (TwoPane delta ratio) simpleTabbed simpleTabbed


myDefaults spawnproc = defaultConfig {
  terminal             = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , borderWidth        = myBorderWidth
  , modMask            = myModifier
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , keys               = myKeys
  , mouseBindings      = myMouseBindings
  , layoutHook         = myHook
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , startupHook        = myStartupHook
  , logHook            = myLogHook spawnproc
  }

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad (myDefaults xmproc)
