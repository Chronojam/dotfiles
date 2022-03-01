import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import System.IO

main = do
    xmproc <- spawnPipe "xmobar /home/calliope/.xmobbarc"
    xmonad $ defaultConfig
      { modMask = mod4Mask
      , manageHook = manageDocks <+> manageHook defaultConfig
      , layoutHook = avoidStruts $ layoutHook defaultConfig
      , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
      , logHook = dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppTitle  = xmobarColor "darkgreen" "" . shorten 20
          }
      }
      `additionalKeysP`
      [ ("M-s", unGrab *> spawn "scrot -s")
      , ("M-r", spawn "dmenu_run")
      ]
