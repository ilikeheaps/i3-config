{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- module containing abstraction layer over i3 config format
-- the goal is to have expressive config (e.g. operations on keymaps like adding a modifier to all keys) and safe config checked statically (so you can't define multiple status bars -- unless it makes sense to).
-- data constructors and types are added as needed so many options are missing

module Config ( module Config
              , BarSettings (..)
              , Command(..)
              , Key(..)
              , Output(..)
              , Direction(..)
              , TreeRelation(..)
              , Axis(..)
              , Layout(..)
              , exec
              , ExecFlag(..)
              , Option(WorkspaceLayout, DefaultOrientation)
              , Orientation(..)
              )
where


import RawConfig hiding (Config, printConfig)
import qualified RawConfig as R

import Prelude (String, Char
               --, Int
               --, unlines
               , map
               --, fmap
               , (++), ($), (.)
               , uncurry
               -- , undefined
               , Maybe(..), maybe
               )
import Data.Bifunctor(first)


data Config = Config
                { keybinds :: [(KeyChord, Command)]
                , modes :: [(String, [(KeyChord, Command)])]
                -- TODO this or [Exec]?
                , startupCommands :: [([ExecFlag], String)]
                -- TODO would be nice to specify this
                , otherOptions :: [Option]
                , statusBar :: Maybe BarSettings
                , floatingModifier :: Maybe Key
                , font :: Maybe String
                }

emptyConfig :: Config
emptyConfig = Config
              { keybinds = []
              , modes = []
              , startupCommands = []
              , otherOptions = []
              , statusBar = Nothing
              , floatingModifier = Nothing
              , font = Nothing
              }

singleton :: a -> [a]
singleton = (:[])

mkConfig :: Config -> R.Config
mkConfig c = R.Config
  $ [exec f cmd | (f, cmd) <- startupCommands c]
  ++ otherOptions c
  ++ maybeOptionComment "no font option" Font (font c)
  ++ maybeOptionComment "no status bar option" BarDefinition (statusBar c)
  ++ maybeOptionComment "no floating modifier" FloatingModifier (floatingModifier c)
  ++ [Comment "default keymap:"]
  ++ configureMap (keybinds c)
  ++ [Comment "modes:"]
  ++ map (uncurry configureModeMap) (modes c)
  where
    maybeOptionComment str f m =
      maybe [Comment str] (singleton . f) m


printConfig :: Config -> String
printConfig c = R.printConfig $ mkConfig c

with :: Key -> (KeyChord, Command) -> (KeyChord, Command)
with m b = first (m:) b

mapWith :: Key -> [(KeyChord, Command)] -> [(KeyChord, Command)]
mapWith k = map (with k)

-- TODO could check if bindings don't overlap, modes are reachable etc.
configureMap :: MetaKeyBinding a => [(KeyChord, Command)] -> [a]
configureMap bs = map (\(k, c) -> bindsym k c) bs

configureModeMap :: String -> [(KeyChord, Command)] -> Option
configureModeMap name binds = ModeDefinition name . configureMap $ binds

-- TODO rename these: toChord, chord, key [possibly change the data type]
toChord :: Key -> KeyChord
toChord k = chord [k]

chord :: [Key] -> KeyChord
chord ks = ks

key :: Char -> KeyChord
key c = [Key c]

(>*<) :: (a -> b) -> (a -> c) -> a -> (b, c)
(>*<) f g x = (f x, g x)
