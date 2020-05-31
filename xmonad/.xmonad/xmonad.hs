import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import System.IO

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , resource =? "nm-connection-editor" --> doFloat
    ]

myDmenu = "dmenu_run -nb \"#1d2021\" -sb \"#fabd2f\" -nf \"#d5c4a1\" -sf \"#1d2021\" -fn \"Hack:pixelsize=16\""
myModMask = mod4Mask
myWorkspaces = ["web", "term"] ++ map show [3..9]
myNormalBorderColor = "#665c54"
myFocusedBorderColor = "#d5c4a1"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks defaultConfig
        { manageHook = myManageHook <+> manageHook defaultConfig -- make sure to include myManageHook definition from above
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
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
        [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((myModMask, xK_b), sendMessage ToggleStruts)
        , ((0, xK_Print), spawn "scrot")
        , ((myModMask, xK_p), spawn myDmenu)
        ] `additionalKeysP`
        [ ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+ unmute")
        , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%- unmute")
        , ("M-S-<Space>", spawn "toggle_keyboard")
        ]
