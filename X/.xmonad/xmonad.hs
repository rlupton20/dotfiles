-- Custom XMonad config file
-- Richard Lupton 2015

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe, runProcessWithInput)
import XMonad.Util.CustomKeys
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed (simpleTabbed)
import System.IO
import Data.Default

-- The main xmonad modifier key is set here
defaultMask = mod4Mask
-- We use other modifiers sometimes too
hyper = mod3Mask
control = controlMask
terminalCmd = "urxvt -tr -sh 25"

toggleMouse :: String
toggleMouse = "xinput --list-props 11 | awk '/Device Enabled/ {toggle = 1 - $4; print toggle}' | xargs xinput set-int-prop 11 \"Device Enabled\" 8"

main = do
  xmproc <- spawnPipe "xmobar"
  _ <- spawn "xmodmap ~/.Xmodmap"
  _ <- spawn "feh --bg-scale ~/.Wallpaper"
  xmonad $ customConfig xmproc
  where
    customConfig xmproc = def { terminal = terminalCmd
                              , manageHook = manageDocks <+> manageHook def
                              , layoutHook = defaultLayouts
                              , handleEventHook = docksEventHook <+> handleEventHook def
                              , startupHook = docksStartupHook <+> startupHook def
                              , logHook = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc
                                                                    , ppTitle = xmobarColor "#69FF24" "" . shorten 50
                                                                    , ppCurrent = (\w -> xmobarColor "#FF6A00" "" $ "[" ++ w ++ "]")
                                                                    , ppHidden  = xmobarColor "#828282" ""
                                                                    , ppSep = xmobarColor "#FF0088" "" " : "
                                                                    , ppLayout = xmobarColor "#828282" ""
                                                                    }
                              , focusedBorderColor = "#6AD4F7"
                              , modMask = defaultMask
                              , keys = customKeys (\_ -> []) insertkeys }

    defaultLayouts = (avoidStruts $ smartBorders $ layoutHook def) ||| (noBorders Full)

    insertkeys :: XConfig l -> [((KeyMask,KeySym), X ())]
    insertkeys _ = [ ((defaultMask, xK_p), spawn "rofi -show drun")
                   , ((defaultMask, xK_s), spawn "i3lock && xset dpms force off")
                   , ((noModMask, xK_Print), spawn "maim -s -c 1,0,0,0.6 --format png /dev/stdout | tee ~/.screenshots/$(date +%F-%T).png | xclip -selection clipboard -t image/png -i")
                   , ((defaultMask, xK_Print), spawn "~/scripts/super-screenshot.sh")
                   , ((shiftMask, xK_Print), spawn "ls ~/.screenshots | sort -r | rofi -dmenu | xargs -I{} rifle ~/.screenshots/{}")
                   , ((hyper, xK_space), spawn toggleMouse) ]

launchInTerminal cmd = terminalCmd ++ " -e " ++ cmd
