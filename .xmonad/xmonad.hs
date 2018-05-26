
----
-- import
import qualified Data.Map as M
import System.IO
import System.Exit

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)

import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders

import XMonad.Hooks.FadeInactive

import qualified XMonad.StackSet as W

import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS

----
-- main
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig


myLogHook = fadeInactiveLogHook 0x333333

-- xmobar
myBar = "xmobar $HOME/.xmonad/xmobarrc"

colorBlue      = "#868bae"
colorBlue2     = "#335555"
colorGreen     = "#00d700"
colorRed       = "#ff005f"
colorGray      = "#666666"
colorWhite     = "#bdbdbd"
colorNormalbg  = "#1b1b1b"
colorfg        = "#a8b6b8"
myPP = xmobarPP { ppOrder           = \(ws:l:t:_)  -> [ws,t]
                , ppCurrent         = xmobarColor colorRed     colorNormalbg . \s -> "●"
                , ppUrgent          = xmobarColor colorGray    colorNormalbg . \s -> "●"
                , ppVisible         = xmobarColor colorRed     colorNormalbg . \s -> "◎"
                , ppHidden          = xmobarColor colorGray    colorNormalbg . \s -> "●"
                , ppHiddenNoWindows = xmobarColor colorBlue2    colorNormalbg . wrap "" ""
                , ppTitle           = xmobarColor colorGreen   colorNormalbg
                , ppOutput          = putStrLn
                , ppWsSep           = " "
                , ppSep             = " | "
                }


-- struct key
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


-- Hook
myStartupHook :: X ()
myStartupHook = do
  spawn "feh --bg-center ~/.xmonad/data/MetaTronC.png"
  -- spawn "fcitx"

-- layout

myLayoutHook = avoidStruts $ ( toggleLayouts (noBorders Full)
                                            $ onWorkspace "3" simplestFloat
                                            $ myLayout)

myLayout = spacing gapwidth $ gaps [(U, gwU),(D, gwD),(L, gwL),(R, gwR)]
           $ (ResizableTall 1 (1/205) (120/205) [])
           ||| (TwoPane (1/205) (120/2))
           ||| Simplest
  where gapwidth  = 6
     	gwU = 2
	gwD = 2
	gwL = 24
	gwR = 24

-- workspace
myWorkspaces :: [String]
myWorkspaces = map show [1..9] 



-- config
myConfig = def {terminal           = "urxvt"
	       , focusFollowsMouse  = True
	       , borderWidth        = 2
	       , modMask            = mod4Mask
  	       -- numlockMask deprecated in 0.9.1
	       -- numlockMask        = myNumlockMask,
	       , workspaces         = myWorkspaces
	       , normalBorderColor  = "#cccccc"
	       , focusedBorderColor = "#cd8b00"

	       -- key bindings
	       ,keys               = myKeys
	       -- mouseBindings      = myMouseBindings,
                 
	       -- hooks, layouts
	       -- , layoutHook = avoidStruts  $  layoutHook def
	       , layoutHook = myLayoutHook
	       , manageHook = manageDocks <+> manageHook def
	       , handleEventHook = handleEventHook def <+> docksEventHook
	       , logHook            = myLogHook
	       , startupHook        = myStartupHook
	       } 


      
-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- default key config from
    -- https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) 
    , ((modMask,               xK_p     ), spawn "dmenu_run")
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") 
    , ((modMask .|. shiftMask, xK_c     ), kill) 

    , ((modMask,               xK_space ), sendMessage NextLayout) 
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) 
    , ((modMask,               xK_n     ), refresh)

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) 
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) 
    , ((modMask,               xK_j     ), windows W.focusDown) 
    , ((modMask,               xK_k     ), windows W.focusUp  ) 
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) 
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) 

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) 
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) 

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    ---- self added configs
    [ ((modMask , xK_c     ), kill)
    --
    , ((modMask .|. controlMask, xK_e), runOrRaise "emacs" (className =? "Emacs"))
    , ((modMask .|. controlMask, xK_t), runOrRaise "gnome-terminal" (className =? "Gnome-terminal"))
    , ((modMask .|. controlMask, xK_f), runOrRaise "firefox" (className =? "Navigator"))
    -- sound keys
    , ((0                       , 0x1008ff13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%" >> spawn "play ~/.xmonad/data/decide.wav")
    , ((0                       , 0x1008ff11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -1.5%" >> spawn "play ~/.xmonad/data/decide.wav")
    , ((0                       , 0x1008ff12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle" >> spawn "play ~/.xmonad/data/decide.wav")
    -- Brightness Keys
    , ((0                       , 0x1008FF02), spawn "xbacklight + 5")
    , ((0                       , 0x1008FF03), spawn "xbacklight - 5")
    -- screen, workspace
    , ((modMask,               xK_Right), nextWS)
    , ((modMask,               xK_Left),  prevWS)
    , ((modMask .|. shiftMask, xK_Down),  shiftToNext)
    , ((modMask .|. shiftMask, xK_Up),    shiftToPrev)
    , ((modMask,               xK_Down),  nextScreen)
    , ((modMask,               xK_Up),    prevScreen)
    , ((modMask .|. shiftMask, xK_Right), shiftNextScreen)
    , ((modMask .|. shiftMask, xK_Left),  shiftPrevScreen)
    , ((modMask,               xK_z),     toggleWS)
    
    ]
