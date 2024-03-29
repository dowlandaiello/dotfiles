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
{-# LANGUAGE DeriveDataTypeable #-}

import XMonad
import XMonad.Util.Types
import XMonad.Util.SpawnOnce
import XMonad.Util.Paste
import XMonad.Util.ExtensibleState as XS
import XMonad.Layout.Minimize
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Decoration
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Actions.Minimize
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.WindowSwallowing
import qualified XMonad.StackSet as Windows
import XMonad.Config.Desktop (desktopLayoutModifiers, desktopConfig)

import qualified Data.Map as M
import qualified Data.Bool as B

-- Use alacritty as the default terminal
myTerminal = "alacritty"

-- Use windows key instead of alt
myModMask    = mod4Mask
myWorkspaces = ["docs", "dev", "web", "misc", "I", "II", "III", "IV", "V"]

-- Custom keybindings + the default ones
myKeys (XConfig {modMask = modm}) = M.fromList $
        [ ((modm, xK_x), spawn "dm-tool lock")

        -- Reload polybar configuration
        , ((modm .|. mod1Mask, xK_q), spawnOnce "$HOME/.config/polybar/launch.sh")

        -- Keybindings for rofi
        , ((controlMask .|. shiftMask, xK_space), spawn "rofi -show run")
        , ((controlMask .|. shiftMask, xK_2), spawn "rofi -show ssh")
        , ((controlMask .|. shiftMask, xK_1), spawn "rofi -show window")

        -- Media controls
        -- XF86AudioRaiseVolume, XF86AudioLowerVolume, XF86AudioMute, XFAudioPlay, XFAudioPrev, XFAudioNext
        , ((0, 0x1008ff13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
        , ((0, 0x1008ff11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -1.5%")
        , ((0, 0x1008ff12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ((0, 0x1008ff14), spawn "playerctl play-pause")
        , ((0, 0x1008ff16), spawn "playerctl previous")
        , ((0, 0x1008ff17), spawn "playerctl next")

        -- Screenshots with scrot
        , ((modm, xK_Print), spawn "sleep 0.5 && scrot ~/Pictures/Screenshots/%b%d::%H%M%S.png -d 1")
        , ((modm .|. shiftMask, xK_Print), spawn "sleep 0.5 && scrot ~/Pictures/Screenshots/%b%d::%H%M%S.png -s")

        -- Minimize + maximize via mod + n
        , ((modm, xK_n), sequence_ [withFocused minimizeWindow, spawn "echo temp\n >> ~/.xmonad/temp"])
        , ((modm .|. shiftMask, xK_n), sequence_ [withLastMinimized maximizeWindow, spawn "sed -i.bak 'ld' ~/.xmonad/temp"])

        -- Change screen brightness with F11 and F12
        , ((0, 0x1008ff02), spawn "xrandr --output eDP-1 --brightness 1")
        , ((0, 0x1008ff03), spawn "xrandr --output eDP-1 --brightness 0.25")

        -- Make window fullscreen with mod + f
        , ((modm, xK_f), sendMessage $ Toggle FULL)]

-- Border width
myBorderWidth = 2

-- Active vs inactive
myFocusedBorderColor = "#ed60ba"
myInactiveBorderColor = "#1b1720"

-- Gaps between windows
mySpacing = spacingRaw False
        (Border 80 80 80 80)
        True
        (Border 20 20 20 20)
        True

-- Layouts available via mod + space
myLayoutHook =
        avoidStruts $
        mySpacing $
        minimize $
        mkToggle (NOBORDERS ?? FULL ?? EOT) $
                Tall 1 (10/100) (60/100)
                ||| Grid

-- Start polybar when xmonad starts
myStartupHook = do
        spawn "$HOME/.config/polybar/launch.sh"

-- Swallow alacritty windows (replace with whatever's running)
myHandleEventHook = swallowEventHook (className =? "Alacritty") (return True)

main = do
        -- Don't override the default configuration--extend it
        xmonad $ docks $ ewmhFullscreen . ewmh $ def
                { modMask             = myModMask
                , terminal            = myTerminal
                , workspaces          = myWorkspaces
                , normalBorderColor   = myInactiveBorderColor
                , focusedBorderColor  = myFocusedBorderColor
                , borderWidth         = myBorderWidth
                , layoutHook          = desktopLayoutModifiers $ myLayoutHook
                , startupHook         = myStartupHook
                , handleEventHook     = myHandleEventHook <+> handleEventHook desktopConfig
                , keys                = \c -> myKeys c `M.union` keys def c
                }
