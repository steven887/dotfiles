
-- ##################################################################
--      _____ ___                                                  ##
--     / ___//   |    # My Personal XMonad Config                  ##
--     \__ \/ /| |    # Author      : steven Agustinus             ## 
--    ___/ / ___ |    # Find me on  : https://github.com/steven887 ##                            
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
import XMonad.Util.NamedScratchpad
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
import XMonad.Layout.Tabbed

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
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
-------------------------------------------------------------------
------                         ACTIONS                       ------
-------------------------------------------------------------------
import XMonad.Actions.MouseResize
import XMonad.Actions.OnScreen
import XMonad.Actions.WithAll

------------------ # End Import Section # -------------------------
-------------------------------------------------------------------

-------------------- # Variables Section # ------------------------    

----  what terminal emulator you use?
s_Term = "st"
s_Term2 = "kitty"

-- Focus follow your mouse pointer or not
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Clicking on a window to focus ?? set True if you want it
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Your window border in pixels.
myBorderWidth = 2

-- Your border color
myNormalBorderColor  = "#BADFFD"
myFocusedBorderColor = "#0Febff" 

---- neon-green setup border
--myNormalBorderColor  = "#22d964"
--myFocusedBorderColor = "#3ef008" 

-- Set Your ModKey -> mod1Mask = left alt, mod3Mask = right alt, mod4Mask = window
myModMask :: KeyMask
myModMask = mod4Mask

altMask :: KeyMask
altMask = mod1Mask

-- Fonts
myFont :: String
myFont = "xft:JetBrainsMono Nerd Font:pixelsize=10:Bold:antialias=true"

-- Set your workspaces
--myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
--myWorkspaces = ["חתא","םיתש","שלש","עברא","שמח","שש","עבש","הנומש","עשת"]
--myWorkspaces = ["一","二","三","四","五","六","七","八","九"]
--
-- use icon for workspaces
-- set variables for icon
-- fn=2 is font from xmobar additional font
a ="\xf120" 
b ="\xf008" 
c ="\xf001" 
d ="\xf447" 
e ="\xf441" 
f ="\xf43a" 
g ="\xf43f" 
h ="\xf445" 
i ="\xf443" 

--a ="\61728" 
--b ="\61448" 
--c ="\61441" 
--d ="\62535" 
--e ="\62529" 
--f ="\62522" 
--g ="\62527" 
--h ="\62533" 
--i ="\62531" 
   

myWorkspaces = [ a,b,c,d,e,f,g,h,i] 

-----------  make your workspace on xmobar clickable ------------
myWorkspacesIndices = M.fromList  $ zipWith (,) myWorkspaces [1..] 
--
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
  --swn_font         = "xft:NotoSansCJKSC:bold:size=60:antialias=true:hinting=true"
  swn_font         = "xft:Font Awesome 5 Free Solid:Solid:pixelsize=120:antialias=true:hinting=true"
  --swn_font          = "xft:MesloLGL Nerd Font:size=60:Regular:antialias=true"
  ,swn_fade         = 0.50
  ,swn_bgcolor      = "#191c21"
  ,swn_color        = "#0CBCF7"
  }


mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayout = mouseResize $ windowArrange  $ mkToggle (NBFULL ??NOBORDERS ?? FULL ?? EOT) $ T.toggleLayouts floats $ avoidStruts  (
           --monocle      |||
           tall         ||| 
           grid         |||
           mirror       |||
           threeCol     |||
           tab          |||
           floats
           )            ||| 
           noBorders Full

  where 
    tall     = renamed [Replace "Tall"] 
               $ windowNavigation 
               -- $ subLayout [] (smartBorders Simplest)
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

    tab      = renamed [Replace "Tabs"]
               $ tabbed shrinkText myTabConfig 

    floats   = renamed [Replace "Floats"]
               $ smartBorders 
               $ simplestFloat

               

--    monocle = renamed [Replace "monocle"]
--              $ smartBorders
--              $ subLayout [] (smartBorders Simplest)
--              $ Full


--- tabTheme config for tabbed layout ---
myTabConfig = def { fontName          =  myFont 
               , activeColor          = "#0CBCF7"
               , inactiveColor        = "#373b41" 
               , activeBorderColor    = "#0CBCF7"
               , inactiveBorderColor  = "#373b41"
               , activeTextColor      = "#ffffff"
               , inactiveTextColor    = "#666666"
               }


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
------                     SCRATCHPAD                        ------               
-------------------------------------------------------------------
-- notes : 
-- customFloating :: RationalRect l t w h
--where
--h = 0.6 -- height 60%
--w = 0.6 -- width 60%
--t = 0.2 -- (1 - h)/2  --> center from top/bottom
--l = 0.2 -- (1 - w)/2  --> center from left/right

scratchpads = [ NS "terminal" spawnSt findSt stLayout
                
              ]
     where
     spawnSt  = s_Term  ++ " -n st-terminal" 
     findSt   = resource =? "st-terminal"
     stLayout = customFloating $ W.RationalRect (0.1)(0.1)(0.8)(0.8) 

-------------------------------------------------------------------
------                   WINDOW RULES                        ------               
-------------------------------------------------------------------

-- doRectFloat :: W.RationalRect -> ManageHook

myManageHook = composeAll
    [ className =? "MPlayer"            -->   doFloat
    , className =? "Gimp"               -->   doFloat
    --, resource  =? "desktop_window"     -->   doIgnore
    --, resource  =? "kdesktop"           -->   doIgnore 
    , className =? "xdman-Main"         -->   doFloat
    , className =? "kitty"              -->   hasBorder False

    ]
   <+>composeOne
   [
     className =? "Pcmanfm"                 -?>  doRectFloat $ (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2) ) 
   , className =? "Blueman-manager"         -?>  doCenterFloat  
   , className =? "Pavucontrol"             -?>  doCenterFloat  
   , className =? "Nm-connection-editor"    -?>  doCenterFloat  
--,className =? "xdman-Main"  -?> doRectFloat $ (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2) ) 
   ]
   <+> namedScratchpadManageHook scratchpads
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
   spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request  --transparent true --alpha 127 --tint 0x000000  --height 22 --monitor 0 --iconspacing 2 &"


-------------------------------------------------------------------
------                     KEY BINDINGS                      ------               
-------------------------------------------------------------------
s_Keys :: [(String, X ())]
s_Keys =
   -- Apps Section
         [ ("C-S-<Return>",                     spawn s_Term  )
         , ("M-S-<Return>",                     spawn s_Term2 )
         , ("M-S-v",                             spawn "code" ) 
         , ("M-S-b",                            spawn "brave" )
         , ("M-S-f",                          spawn "firefox" )
         , ("M-p",                            spawn "pcmanfm" )
         , ("M-d",                    spawn "dmenu_run -h 24" )
         
  -- Window Section
  -- Kill windows
        , ("M-S-c",                                      kill )
        , ("M-S-a",                                   killAll )

  -- Window Resizing      
        , ("M-h",                          sendMessage Shrink )
        , ("M-l",                           sendMessage Expand)
        , ("M-M1-j",                  sendMessage MirrorShrink)
        , ("M-M1-k",                  sendMessage MirrorExpand)

  -- Window Focus
        , ("M1-<Tab>",                     windows W.focusDown)
        , ("M-j",                          windows W.focusDown)
        , ("M-k",                          windows W.focusUp  )
        , ("M-m",                       windows W.focusMaster )
        , ("M-<Return>",                  windows W.swapMaster)
        , ("M-S-j",                        windows W.swapDown )
        , ("M-S-k",                          windows W.swapUp )

  -- Window push back into tiling/float
        , ("M-t",               withFocused $ windows . W.sink) -- push from floating windows back to tile
        , ("M-S-t",                                   sinkAll ) -- push all floating windows back to tile
        
  -- Layout Toggle
        , ("M-S-t",            sendMessage (T.Toggle "Floats"))
        , ("M-S-f",                sendMessage $ Toggle NBFULL) -- toggle Full noborders
        , ("M-S-_b",            sendMessage $ Toggle NOBORDERS) -- toggle noBorders
        , ("M-C-t"                  , sendMessage ToggleStruts) -- toggle status bar gap

  -- Changging Layout
  
       , ("M-<Tab>",                   sendMessage NextLayout )
      -- , ("M-S-<space>",   setLayout $ XMonad.layoutHook conf )
        
  -- Increase / Decrease Master
        , ("M-,",                   sendMessage (IncMasterN 1))
        , ("M-.",                sendMessage (IncMasterN (-1)))

  -- Media Keys / Extra Keys

    , ("<XF86Tools>",                  spawn "spotify-adblock")
    , ("<XF86AudioPlay>",         spawn "playerctl play-pause")
    , ("<XF86AudioStop>",               spawn "playerctl stop")
    , ("<XF86AudioNext>",               spawn "playerctl next")
    , ("<XF86AudioPrev>",           spawn "playerctl previous")
    , ("<XF86AudioMute>",     spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")

  -- XMonad
    , ("M-S-q",                      io (exitWith ExitSuccess))
    , ("M-q",     spawn "xmonad --recompile; xmonad --restart")

  -- SCRATCHPADS
    , ("M1-C-<Return>", namedScratchpadAction scratchpads "terminal")

      ]
------------------------------------------------------------------
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
        --, ppCurrent = xmobarColor "#cbe500" "#078202"  . wrap " "  " " -- "#71fe00" -- . wrap "[" "]"
        -- without background
        , ppCurrent = xmobarColor "#7bf9f2" ""  . wrap " "  " " -- "#71fe00" -- . wrap "[" "]"
        , ppVisible = xmobarColor "#0CBCF7" ""  .  clickable
        --, ppHidden  = xmobarColor "#498236" "" . wrap "" "*"  . clickable
        , ppHidden  = xmobarColor "#fff" ""  .wrap " " " " . clickable
        , ppTitle   = xmobarColor "#9fffff" "" . shorten 50
        , ppSep     =  "<fc=#666666> | </fc>"
        , ppHiddenNoWindows = xmobarColor "#373b41" ""   . wrap " " " " 
        , ppExtras  = [windowCount] 
        , ppOrder   = \(ws:l:t:ex) -> [ws, l]++ex++[t]
        }
        } `additionalKeysP` s_Keys 

defaults = def {
      -- simple stuff
        terminal           = s_Term,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
     --   keys               = myKeys,
     --   mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = showWName' myshowWNameTheme $ myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = docksEventHook,
        logHook            = myLogHook ,
        startupHook        = myStartupHook
    }

