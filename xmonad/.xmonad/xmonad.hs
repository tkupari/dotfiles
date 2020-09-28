import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Util.NamedScratchpad
import System.IO

myManageHook = insertPosition Below Newer <+> composeAll
    [ isDialog --> doF W.swapUp
    , className =? "Gimp"      --> doFloat
    , resource =? "nm-connection-editor" --> doFloat
    , resource =? "pavucontrol" --> doFloat
    , resource =? "JetBrains Toolbox" --> doFloat
    , resource =? "blueman-manager" --> doFloat
    ]

myDmenu = "dmenu_run -nb \"#1d2021\" -sb \"#fabd2f\" -nf \"#d5c4a1\" -sf \"#1d2021\" -fn \"Hack:pixelsize=16\""
myModMask = mod4Mask
myWorkspaces = ["1:web", "2:term"] ++ map show [3..9]
myNormalBorderColor = "#665c54"
myFocusedBorderColor = "#d5c4a1"

myScratchpads = [ NS "irssi" spawnIrssi findIrssi manageIrssi
                , NS "spotify" "spotify" (resource =? "spotify") defaultFloating
                , NS "slack" "slack" (resource =? "slack") defaultFloating
                ]
  where
  spawnIrssi = "alacritty --class irssi --title irssi"
  findIrssi = resource =? "irssi"
  manageIrssi = customFloating $ W.RationalRect l t w h
                where
                h = 0.5
                w = 0.5
                t = 0.25
                l = 0.25

myLayout = tiled ||| Full ||| ThreeColMid 1 (3/100) (1/2)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks defaultConfig
        { manageHook = myManageHook <+> namedScratchpadManageHook myScratchpads <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ smartBorders( myLayout )
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = myModMask
        , terminal = "alacritty"
        , workspaces = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        } `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawn "lock_and_suspend")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((myModMask, xK_b), sendMessage ToggleStruts)
        , ((0, xK_Print), spawn "scrot")
        , ((myModMask, xK_p), spawn myDmenu)
        ] `additionalKeysP`
        [ ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86MonBrightnessUp>", spawn "lux -a 10%")
        , ("<XF86MonBrightnessDown>", spawn "lux -s 10%")
        , ("M-C-<Space>", spawn "toggle_keyboard")
        , ("M-c", namedScratchpadAction myScratchpads "irssi")
        , ("M-x", namedScratchpadAction myScratchpads "spotify")
        , ("M-s", namedScratchpadAction myScratchpads "slack")
        , ("M-S-a", spawn "autorandr -c")
        , ("M-C-S-a", spawn "autorandr -l internal")
        ]
