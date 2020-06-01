import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Util.NamedScratchpad
import System.IO

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , resource =? "slack"      --> doShift "3:slack"
    , resource =? "nm-connection-editor" --> doFloat
    ]

myDmenu = "dmenu_run -nb \"#1d2021\" -sb \"#fabd2f\" -nf \"#d5c4a1\" -sf \"#1d2021\" -fn \"Hack:pixelsize=16\""
myModMask = mod4Mask
myWorkspaces = ["1:web", "2:term", "3:slack"] ++ map show [4..9]
myNormalBorderColor = "#665c54"
myFocusedBorderColor = "#d5c4a1"

myScratchpads = [ NS "irssi" spawnIrssi findIrssi manageIrssi
                , NS "spotify" spawnSpotify findSpotify manageSpotify
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
  spawnSpotify = "spotify"
  findSpotify = resource =? "spotify"
  manageSpotify = defaultFloating -- FIXME: xprops are not set on launch?

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks defaultConfig
        { manageHook = myManageHook <+> namedScratchpadManageHook myScratchpads <+> manageHook defaultConfig -- make sure to include myManageHook definition from above
        , layoutHook = avoidStruts $ smartBorders( layoutHook defaultConfig )
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
        [ ((myModMask .|. shiftMask, xK_z), spawn "dunst_lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((myModMask, xK_b), sendMessage ToggleStruts)
        , ((0, xK_Print), spawn "scrot")
        , ((myModMask, xK_p), spawn myDmenu)
        ] `additionalKeysP`
        [ ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+ unmute")
        , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%- unmute")
        , ("M-S-<Space>", spawn "toggle_keyboard")
        , ("M-i", namedScratchpadAction myScratchpads "irssi")
        , ("M-m", namedScratchpadAction myScratchpads "spotify")
        ]
