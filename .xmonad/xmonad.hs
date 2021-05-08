
-- ##################################################################
--      _____ ___                                                  ##
--     / ___//   |    # My Personal XMonad Config                  ##
--     \__ \/ /| |    # Author      : steven Agustinus             ## 
--    ___/ / ___ |    # Find me on  : https://github.com/steven887 ##                            #
--   /____/_/  |_|                                                 ##
--                                                                 ## 
-- ##################################################################


--------------------- # Import Module Section # -------------------- 

--------------------------------------------------------------------
--                             CORE                               --
--------------------------------------------------------------------
import XMonad
import System.Exit
import qualified XMonad.StackSet as W
import System.IO (hPutStrLn)
-------------------------------------------------------------------
------                          DATA                         ------               
-------------------------------------------------------------------
import Data.Maybe (fromJust)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.Monoid
import Data.Ratio

-------------------------------------------------------------------
------                        UTILLITIES                     ------               
-------------------------------------------------------------------
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86
-------------------------------------------------------------------
------                          HOOKS                        ------ 
-------------------------------------------------------------------
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten,pad, PP(..))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))

-------------------------------------------------------------------
------                         LAYOUTS                       ------
-------------------------------------------------------------------
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat


-------------------------------------------------------------------
------                 LAYOUTS MODIFIERS                     ------
-------------------------------------------------------------------
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName -- This is a layout modifier that will show the workspace name
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
-------------------------------------------------------------------
------                         ACTIONS                       ------
-------------------------------------------------------------------
import XMonad.Actions.MouseResize
import XMonad.Actions.OnScreen
------------------ # End Import Section # -------------------------
-------------------------------------------------------------------

-------------------- # Variables Section # ------------------------    

----  what terminal emulator you use?
myTerminal = "st"
myTerminal2 = "alacritty"
myTerminal3 = "kitty"

-- Focus follow your mouse pointer or not
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Clicking on a window to focus ?? set True if you want it
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Your window border in pixels.
myBorderWidth = 2

-- Your border color
myNormalBorderColor  = "#22d964"
myFocusedBorderColor = "#3ef008" 

-- Set Your ModKey -> mod1Mask = left alt, mod3Mask = right alt, mod4Mask = window
myModMask :: KeyMask
myModMask = mod4Mask

altMask :: KeyMask
altMask = mod1Mask


-- Set your workspaces
--myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
--myWorkspaces = ["חתא","םיתש","שלש","עברא","שמח","שש","עבש","הנומש","עשת"]
myWorkspaces = ["一","二","三","四","五","六","七","八","九"]
--myWorkspaces = [" \xf10c "," \xf10c "," \xf10c "," \xf10c "," \xf10c "," \xf10c "," \xf10c "," \xf10c "," \xf10c"]

-----------  make your workspace on xmobar clickable ------------
myWorkspacesIndices = M.fromList $ zipWith (,) myWorkspaces [1..] 

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspacesIndices
------------------------------------------------------------------

------------------ # End Variables Section # ----------------------    


-------------------------------------------------------------------
------                        LAYOUTS                        ------               
-------------------------------------------------------------------

-- this will show the workspace name everytime you move or change to another workspace
myshowWNameTheme :: SWNConfig
myshowWNameTheme = def
  {
  --swn_font         = "xft:MiriamMonoCLM:bold:size=60:antialias=true:hinting=true"
  swn_font         = "xft:NotoSansCJKSC:bold:size=60:antialias=true:hinting=true"
  ,swn_fade         = 0.7
  ,swn_bgcolor      = "#191c21"
  ,swn_color        = "#23A611"
  }


mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayout = mouseResize $ windowArrange  $ mkToggle (NBFULL ?? FULL ?? EOT) $ avoidStruts  (
           --monocle      |||
           tall         ||| 
           grid         |||
           mirror       |||
           threeCol 
           )            ||| 
           noBorders Full

  where 
    tall     = renamed [Replace "Tall"] 
               $ windowNavigation 
               $ subLayout [] (smartBorders Simplest)
               $ mySpacing 8 
               $ ResizableTall 1 (3/100) (1/2) [] 

    grid     = renamed [Replace "Grid"] 
               $ windowNavigation 
               $ mySpacing 8
               $ Grid

    mirror   = renamed [Replace "Mirror"]
               $ windowNavigation
               $ mySpacing 8
               $ Mirror (Tall 1 (3/100) (3/5))

    threeCol = renamed [Replace "ThreeCol"]
               $ windowNavigation
               $ mySpacing 8
               $ ThreeCol 1 (3/100) (1/2)

--    monocle = renamed [Replace "monocle"]
--              $ smartBorders
--              $ subLayout [] (smartBorders Simplest)
--              $ Full


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

-------------------------------------------------------------------
------                   WINDOW RULES                        ------               
-------------------------------------------------------------------

-- doRectFloat :: W.RationalRect -> ManageHook

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , className =? "xdman-Main"        --> doFloat
    ]
   <+>composeOne
   [className =? "Pcmanfm"  -?> doRectFloat $ (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2) ) 
   --,className =? "xdman-Main"  -?> doRectFloat $ (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2) ) 
   ]
-------------------------------------------------------------------
------                   EVENT HANDLING                      ------               
-------------------------------------------------------------------

--myEventHook = mempty

-------------------------------------------------------------------
------                  STATUS BARS & LOGGING                ------               
-------------------------------------------------------------------

myLogHook :: X ()
myLogHook = return () -- fadeInactiveLogHook fadeAmount
   where fadeAmount = 1.0

-------------------------------------------------------------------
------                     STARTUP HOOK                      ------               
-------------------------------------------------------------------

myStartupHook = do
   spawnOnce "nitrogen --restore &"
   spawnOnce "picom &"
   spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request  --transparent true --alpha 55 --tint 0x000000  --height 22 --monitor 0 --iconspacing 2 &"

-------------------------------------------------------------------
------                     KEY BINDINGS                      ------               
-------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

   -- launch a terminal (alacritty)
   -- [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
      [ 
      ((shiftMask .|. controlMask , xK_Return), spawn $ XMonad.terminal conf)
    , ((0, xK_Print), spawn "scrot 'scrot-%Y-%m-%d_$wx$h.png' -s -e   'mv $f ~/screenshoots'") 
   -- launch kitty terminal
    , ((modm .|. shiftMask, xK_Return), spawn myTerminal3)
   

   -- launch vscode
    , ((modm,              xK_v  ), spawn "code")

   -- launch firefox private
    , ((altMask .|. controlMask   ,       xK_f  ), spawn "firefox --private-window")

   -- launch firefox 
    , ((modm   ,       xK_f  ), spawn "firefox")
   
   -- launch brave browser incgonito
    , ((modm,                xK_b    ),  spawn "brave")
   
   -- launch brave browser
    , ((altMask .|. controlMask  ,      xK_b    ),  spawn "brave --incognito")

   -- launch pcmanfm
    , ((modm,               xK_p   ),   spawn "pcmanfm")
  
   -- launch dmenu
    , ((modm,               xK_d     ), spawn "dmenu_run -h 24")


    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- MirrorShrink the master area
    , ((modm .|. altMask ,               xK_j     ), sendMessage MirrorShrink)

    -- MirrorExpand the master area
    , ((modm .|. altMask ,               xK_k     ), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    ------------------------------- EXTRA KEYS ------------------------------
    -- Media Player keybindings --
    -- launch spotify
    , ((0,      xF86XK_Tools        ), spawn "spotify-adblock")

    -- PlayPause
    , ((0,      xF86XK_AudioPlay        ), spawn "playerctl play-pause")
   
    -- Stop
    , ((0,      xF86XK_AudioStop        ), spawn "playerctl stop")
  
    -- Next
    , ((0,      xF86XK_AudioNext        ), spawn "playerctl next")
  
    -- Previous
    , ((0,      xF86XK_AudioPrev        ), spawn "playerctl previous")

    -- Volume Key - mute/on
    ,  ((0, xF86XK_AudioMute             ), spawn "amixer set Master toggle")
   
    -- Volume Key - decrease volume 
    ,  ((0, xF86XK_AudioLowerVolume      ), spawn "amixer set Master 2%-")

    -- Volume Key - increase volume 
    ,  ((0, xF86XK_AudioRaiseVolume      ), spawn "amixer set Master 2%+")
    
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
     , ((modm .|. controlMask             , xK_t     ), sendMessage ToggleStruts)
    
    -- Toggle Layout
     , ((modm .|. shiftMask               , xK_f    ), sendMessage $ Toggle NBFULL)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or smartBordere3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
        


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
   ] 
-------------------------------------------------------------------
------                        MAIN                           ------               
-------------------------------------------------------------------

main :: IO ()
main = do
	xmproc0 <- spawnPipe "xmobar -x 0 /home/steven/.config/xmobar/xmobarrc" 
	xmproc1 <- spawnPipe "xmobar -x 1 /home/steven/.config/xmobar/xmobarrc2" 
 	xmonad $  ewmh defaults {
        logHook = dynamicLogWithPP $ def
        {
          ppOutput  = \x ->  hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
        , ppCurrent = xmobarColor "#cbe500" "#078202"  . wrap " "  " " -- "#71fe00" -- . wrap "[" "]"
        , ppVisible = xmobarColor "#2ba402" "" .  clickable
        , ppHidden  = xmobarColor "#498236" "" . wrap "" "*" . clickable
        , ppTitle   = xmobarColor "#22d964" "" . shorten 50
        , ppSep     =  "<fc=#666666> | </fc>"
        , ppHiddenNoWindows = xmobarColor "#373b41" "" . wrap "|" " " 
        , ppExtras  = [windowCount] 
        , ppOrder   = \(ws:l:t:ex) -> [ws, l]++ex++[t]
        }
        }

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = showWName' myshowWNameTheme $ myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = docksEventHook,
        logHook            = myLogHook ,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
