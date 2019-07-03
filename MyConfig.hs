{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Config

import Prelude hiding (Either(..), mod)

import Data.Bifunctor(first, second)

makeLayout :: Layout -> Command
makeLayout (Split axis) = DoSplit axis
makeLayout l = DoSplit Horizontal `Seq` ChangeLayout l

-- it just somehow feels better to have this with lowercase
changeLayout :: Layout -> Command
changeLayout l = ChangeLayout l

-- likewise
toggleSplit :: Command
toggleSplit = ToggleSplit

-- do I want to make lower case aliases for everything? Or do I want to change my syntax highlighting?
focus :: Direction -> Command
focus = Focus

hsplit, vsplit :: Layout
hsplit = Split Horizontal
vsplit = Split Vertical

mirror :: Char -> Char
mirror 'q' = ';'
mirror 'w' = 'y'
mirror 'f' = 'y'
mirror 'p' = 'l'
mirror 'g' = 'j'
mirror 'a' = 'o'
mirror 'r' = 'i'
mirror 's' = 'e'
mirror 't' = 'n'
mirror 'd' = 'h'
mirror 'z' = '/'
mirror 'x' = '.'
mirror 'c' = ','
mirror 'v' = 'm'
mirror 'b' = 'k'
mirror 'j' = 'g'
mirror 'l' = 'p'
mirror 'u' = 'f'
mirror 'y' = 'w'
mirror ';' = 'q'
mirror 'o' = 'a'
mirror 'i' = 'r'
mirror 'e' = 's'
mirror 'n' = 't'
mirror 'h' = 'd'
mirror '/' = 'z'
mirror '.' = 'x'
mirror ',' = 'c'
mirror 'm' = 'v'
mirror 'k' = 'b'
mirror _ = undefined

config :: Config
config =  Config { keybinds = defaultMap
                 , modes = myModes
                 , startupCommands = commands
                 , otherOptions = other
                 , statusBar = Just (BarSettings { statusCommand = "i3status"
                                                 , trayOutput = Primary })
                 , floatingModifier = Just mod
                 , font = Just "pango:DejaVu Sans Mono 8"
                 }
  where
    mod = Mod 4
    baseMap = concat
      [ mapWith Shift moveKeys
      , focusKeys
      , layoutKeys
      , programKeys
      , controlKeys
      , modeKeys
      ]
    moveKeys = directionBinds Move
    focusKeys = directionBinds Focus
                ++ [ (key 'w', focus Parent)
                   , (key 'a', focus Child)
                   ]
    directionBinds f = map (second f . first toChord) dirKeys
    dirKeys = map (first Key)
              [ ('f', Up)
              , ('s', Down)
              , ('r', Left)
              , ('t', Right)
              ]
    -- TODO tidy up: key, Key, chord, toChord
    --      maybe make some typeclass for key chords (KeyThing Char etc)
    layoutKeys = [ (key 'n', makeLayout hsplit)
                 , (key 'u', makeLayout vsplit)
                 , (key 'e', toggleSplit)
                 , (key 'y', changeLayout Stacked)
                 , (key 'i', changeLayout Tabbed)
                 , (key 'l', ToggleFullscreen)
                 ]
    programKeys = [ (chord [Shift, Key 'q'], Kill)
                  , (chord [Return], exec [] "konsole -e fish")
                  , (key 'p', exec [] "dmenu_run")
                  ]
    controlKeys = [ (chord [Shift, Key 'c'], Reload)
                  , (chord [Shift, Mod 1, Key 'x'], Restart)
                  , (chord [Shift, Key 'x'], exitCmd)
                  ]
    modeKeys = [ (key 'h', ChangeMode "insert") ]
    noModKeys = [ (toChord Print, exec []
                     "scrot $(echo \"$HOME/Pictures/Screens/%Y-%m-%d-%H:%M:%S-\\$wx\\$h.png\")")
                ]
    commands = map (const [NoStartupId] >*< id) nostartupid
               ++ map (const [] >*< id) plainCmds
    nostartupid = []
    plainCmds = [ "xsession-settings" ]
    other = [ WorkspaceLayout Stacked
            , DefaultOrientation Auto
            ]
    -- TODO bind modes somewhere
    defaultMap = baseMap ++ noModKeys
    myModes =
      [ -- ("normal", escapedMap)
      -- insert mode needs modifier even for noModKeys
        ("insert", mapWith mod $ escapedMap)
      ]
    escapedMap = (chord [Escape], ChangeMode "default"):baseMap ++ noModKeys
    exitCmd =  exec [] "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"
      
main :: IO ()
-- main = putStrLn . printConfig $ myconfig (Mod 4) navKeys layoutKeys
main = putStrLn . printConfig $ config

{- TODO/DONE

DONE?
# use Mouse+$mod to drag floating windows to their wanted position
floating_modifier $mod

TODO
# border style/size
# NOTE this is deprecated in newer i3 versions
new_window pixel 4
# For newer versions:
# default_border pixel 4

# start eshell
bindsym $mod+Return exec emacsclient -e "(open-eshell)" -c

# toggle tiling / floating
bindsym $mod+Shift+space floating toggle

# change focus between tiling / floating windows
bindsym $mod+space focus mode_toggle

# move the currently focused window to the scratchpad
bindsym $mod+Shift+minus move scratchpad

# Show the next scratchpad window or hide the focused scratchpad window.
# If there are multiple scratchpad windows, this command cycles through them.
bindsym $mod+minus scratchpad show

# switch to workspace
bindsym $mod+1 workspace 1
bindsym $mod+2 workspace 2
bindsym $mod+3 workspace 3
bindsym $mod+4 workspace 4
bindsym $mod+5 workspace 5
bindsym $mod+6 workspace 6
bindsym $mod+7 workspace 7
bindsym $mod+8 workspace 8
bindsym $mod+9 workspace 9
bindsym $mod+0 workspace 10

# move focused container to workspace
bindsym $mod+Shift+1 move container to workspace 1; workspace 1
bindsym $mod+Shift+2 move container to workspace 2; workspace 2
bindsym $mod+Shift+3 move container to workspace 3; workspace 3
...
bindsym $mod+Shift+0 move container to workspace 10; workspace 10

# resize window (you can also use the mouse for that)
mode "resize" {
        # These bindings trigger as soon as you enter the resize mode

        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym $left       resize shrink width 5 px or 5 ppt
        bindsym $down       resize grow height 5 px or 5 ppt
        bindsym $up         resize shrink height 5 px or 5 ppt
        bindsym $right      resize grow width 5 px or 5 ppt

        # same bindings, but for the arrow keys
        bindsym Left        resize shrink width 5 px or 5 ppt
        bindsym Down        resize grow height 5 px or 5 ppt
        bindsym Up          resize shrink height 5 px or 5 ppt
        bindsym Right       resize grow width 5 px or 5 ppt

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
}

bindsym $mod+d mode "resize"

-}
