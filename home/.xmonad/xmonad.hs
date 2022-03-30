import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing

import System.IO

-- layouts
myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = spacing 10 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 1/100
    

main = do
    xmproc <- spawnPipe "xmobar /home/calliope/.xmobbarc"
    xmproc <- spawnPipe "feh --bg-fill /home/calliope/background.jpg"
    xmonad $ defaultConfig
      { modMask = mod4Mask
      , manageHook = manageDocks <+> manageHook defaultConfig
      , layoutHook = avoidStruts $ myLayout
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
