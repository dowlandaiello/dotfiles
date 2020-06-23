-------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
--
-- Author: Dowland Aiello
-- License: MIT License
--
-- Notes: font-awesome should be installed via the ttf-font-
-- awesome community repo, while the siji bitmap font should be
-- installed via the siji-git AUR repo.
--
-- Enjoy!
-------------------------------------------------------------------

import XMonad
import XMonad.Util.Types
import XMonad.Util.SpawnOnce
import XMonad.Layout.Minimize
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Decoration
import XMonad.Actions.Minimize
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet as Windows
import XMonad.Config.Desktop (desktopLayoutModifiers, desktopConfig)

import qualified Data.Map as M

-- Use alacritty as the default terminal
myTerminal = "alacritty"

-- Use windows key instead of alt
myModMask    = mod4Mask
myWorkspaces = ["docs", "dev", "web", "b", "c", "d", "e", "f", "g"]

-- Custom keybindings + the default ones
myKeys (XConfig {modMask = modm}) = M.fromList $
        [ ((modm, xK_x), spawn "dm-tool lock")

        -- Reload polybar configuration
        , ((modm .|. mod1Mask, xK_q), spawnOnce "$HOME/.config/polybar/launch.sh")

        -- Keybindings for rofi
        , ((controlMask, xK_space), spawn "rofi -show window")
        , ((controlMask .|. shiftMask, xK_space), spawn "rofi -show run")
        , ((controlMask .|. shiftMask, xK_2), spawn "rofi -show ssh")

        -- Media controls
        -- XF86AudioRaiseVolume, XF86AudioLowerVolume, XF86AudioMute, XFAudioPlay, XFAudioPrev, XFAudioNext
        , ((0, 0x1008ff13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
        , ((0, 0x1008ff11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -1.5%")
        , ((0, 0x1008ff12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ((0, 0x1008ff14), spawn "playerctl play-pause")
        , ((0, 0x1008ff16), spawn "playerctl previous")
        , ((0, 0x1008ff17), spawn "playerctl next")]

-- Border width
myBorderWidth = 1

myFocusedBorderColor = "#bb8aff"

-- Gaps between windows
mySpacing = spacingRaw True
        (Border 5 5 5 5)
        True
        (Border 5 5 5 5)
        True

-- Layouts available via mod + space
myLayoutHook =
        avoidStruts $ smartBorders $ mySpacing $ 
        Tall 1 (10/100) (60/100)
        ||| Grid

-- Start polybar when xmonad starts
myStartupHook = do
        spawnOnce "$HOME/.config/polybar/launch.sh"

main = do
        -- Don't override the default configuration--extend it
        xmonad $ ewmh desktopConfig
                { modMask            = myModMask
                , terminal           = myTerminal
                , workspaces         = myWorkspaces
                , focusedBorderColor = myFocusedBorderColor
                , borderWidth        = myBorderWidth
                , layoutHook         = desktopLayoutModifiers $ myLayoutHook
                , startupHook        = myStartupHook
                , handleEventHook    = fullscreenEventHook <+> handleEventHook desktopConfig
                , keys               = \c -> myKeys c `M.union` keys def c
                }
