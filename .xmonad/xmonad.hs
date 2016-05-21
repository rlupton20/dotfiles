-- Custom XMonad config file
-- Richard Lupton 2015

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe, runProcessWithInput)
import XMonad.Util.CustomKeys
import XMonad.Layout.NoBorders
import System.IO

-- The main xmonad modifier key is set here
defaultMask = mod4Mask

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ customConfig xmproc
  where
    customConfig xmproc = defaultConfig { terminal = "urxvt"
                                 , manageHook = manageDocks <+> manageHook defaultConfig
                                 , layoutHook = defaultLayouts
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

    defaultLayouts = (avoidStruts $ smartBorders $ layoutHook defaultConfig) ||| (noBorders Full)

    makeEmacsPanel :: X ()
    makeEmacsPanel = do
      setLayout (Layout $ avoidStruts $ noBorders (Full :: Full Window))
      spawn "emacs-client"
      

    insertkeys :: XConfig l -> [((KeyMask,KeySym), X ())]
    insertkeys _ = [ ((defaultMask, xK_p), spawn "$(yeganesh -x -- -h 30 -y 400 -nb '#303030' -nf '#505050' -sb '#0099FF' -sf '#303030' -fn 'LiberationMono-10:bold')")
                   , ((controlMask, xK_i), spawn "xcalib -i -a")
                   , ((defaultMask, xK_e), makeEmacsPanel)
                   , ((defaultMask, xK_t), spawn "tmux-launcher") ]
