{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RawConfig where
-- module containing raw i3 config types
-- data constructors and types are added as needed so many options are missing
-- To add new option, first add it to Option type, then add a print function for it and include it in printOption

{- TODO
It's getting uncomfortable with so many closely-related types.
E.g. you'd want: data Workspace = Named String | Numbered Int
but "Named" and "Numbered" are pretty general so you don't want to use these and end up with:
data Workspace = Workspace String
which is pretty uninformative.
data Workspace = NamedWorkspace String seems kinda superfluous
Could you define
data Workspace = Named String and later use Workspace.Named to avoid conflicts?
-> this is actually a bit dumb when you can define a new conflicting name making all previous uses ambiguous
-}

import Prelude (String, Char, Int
               , unlines, lines, map, fmap, (++), ($), (.)
               -- , (<$>), (<*>), pure
               , show, concatMap
               -- , undefined
               , (==)
               )
import Data.List(intersperse, concat)

-- NOTE sum types would be cool here (-> data types a la carte)

-- this is a type representing i3 config files as they are
-- TODO maybe should instead make a more abstract type? Or maybe it's good for compatibility to have this type closer to output format and build abstractions on top of it?
newtype Config = Config [Option]

data Option = ModeDefinition String [KeyBinding]
            | BindingDefinition KeyBinding
            | FloatingModifier Key -- TODO check if only a single key is allowed
            | GlobalExec Exec
            | Font String
            | WorkspaceLayout Layout
            | DefaultOrientation Orientation
            | BarDefinition BarSettings
            | Comment String

data BarSettings = BarSettings {statusCommand :: String, trayOutput :: Output}

data Output = Primary

data Exec = Exec [ExecFlag] String
-- TODO actually could make Stacked and Tabbed one constructor parametrised by Axis
data Layout = Stacked | Tabbed | Split Axis

data Axis = Horizontal | Vertical

data Orientation = Auto

data KeyBinding = KeyBinding KeyChord Command

{- TODO Move can't accept Parent | Child
It would be cool if it could though:
- Move Parent seems obvious
- Move Child would create a new node with a single child

Possible solution:

class MetaSimpleDirection a where
  up :: a ; down :: a ; left :: a ; right :: a
class MetaTreeDirection a where
  parent :: a; child :: a
data SimpleDirection = Up | Down | Left | Right
data TreeDirection = Parent | Child  --  | NextSibling | PrevSibling
data FocusDirection = Simple SimpleDirection | Tree TreeDirection
instance MetaSimpleDirection SimpleDirection where
  up = Up; down = Down; left = Left; right = Right
instance MetaSimpleDirection FocusDirection where
  up = Simple Up; down = Simple Down; left = Simple Left; right = Simple Right
instance MetaTreeDirection FocusDirection where
  parent = Tree Parent ; child = Tree Child

~~ class MoveArgument a where...

Another one:
data Direction = U | D | L | R
data TreeDirection = ...
Focus (Either Direction TreeDirection)

Another one:
data Direction
data TreeDirection
data FocusArg = Visual Direction | Tree TreeDirection
Focus FocusArg
-}

-- Move (Either Direction String)
-- Move (Direction :+: String)
-- MoveToWorkspace String
-- Move {from :: Movable, target :: Either Direction Movable}
-- type Movable = Workspace :+: Direction

data Command = Focus Direction
             | Move Direction
             | ChangeMode String
             | ChangeLayout Layout
             | ToggleSplit
             | DoSplit Axis
             | DoExec Exec
             | Kill
             | ToggleFullscreen
             | Reload
             | Restart
             | FocusWorkspace Workspace
             -- TODO check/set precedence `Seq`
             | Seq Command Command

data Workspace = NamedWorkspace String

data Direction = Up | Down | Left | Right | Parent | Child

type KeyChord = [Key]

data Key = Key Char
         | Mod Int -- should be only from 1 to 7
         | Shift
         | Escape
         | Return
         | Print

data ExecFlag = NoStartupId 

indent :: String -> String
indent = ("  "++)

unlines1 :: [String] -> String
unlines1 ss = concat . intersperse "\n" $ ss

makeBlock :: String -> [String] -> [String]
makeBlock lab ss = (if lab == "" then "{" else lab ++" {") : map indent ss ++ ["}"]

-- TODO this is slow. Would need some stuff like String -> String for fast append
printConfig :: Config -> String
printConfig (Config options) = unlines $ fmap printOption options

printOption :: Option -> String
printOption (ModeDefinition name bindings) =
  unlines1 $ makeBlock ("mode " ++ printString name) (fmap printKeyBinding bindings)
printOption (BindingDefinition binding) = printKeyBinding binding
printOption (GlobalExec e) = printExec e
printOption (Font s) = "font "++s
printOption (WorkspaceLayout l) = "workspace_layout "++printLayout l
printOption (DefaultOrientation o) = "default_orientation "++printOrientation o
  where printOrientation Auto = "auto"
printOption (BarDefinition b) = printBarSettings b
printOption (Comment s) = unlines1 . fmap ("# "++) .  lines $ s
printOption (FloatingModifier m) = "floating_modifier "++printKey m

printBarSettings :: BarSettings -> String
printBarSettings b =
  unlines1 $ makeBlock "bar" [ "status_command " ++ statusCommand b
                             , "tray_output " ++ printOutput (trayOutput b)
                             ]

printOutput :: Output -> String
printOutput Primary = "primary"

printExec :: Exec -> String
printExec (Exec flags command) =
  "exec "++ concatMap ((++" ") . printFlag) flags ++ printString command
  where printFlag NoStartupId = "--no-startup-id"

printLayout :: Layout -> String
printLayout Stacked = "stacking"
-- TODO confirm
printLayout Tabbed = "tabbed"
printLayout (Split a) = "split "++printAxis a

printAxis :: Axis -> String
printAxis Horizontal = "horizontal"
printAxis Vertical = "vertical"

printString :: String -> String
printString s = "\"" ++ s ++ "\""

printKeyBinding :: KeyBinding -> String
printKeyBinding (KeyBinding keys cmd) = "bindsym "++printKeyChord keys++" "++printCommand cmd

printKeyChord :: KeyChord -> String
printKeyChord keys = concat . intersperse "+" . map printKey $ keys

-- TODO what's the proper way to cast a Char onto String?
printKey :: Key -> String
printKey (Key c) = [c]
printKey (Mod n) = "Mod" ++ show n
printKey Shift = "Shift"
printKey Escape = "Escape"
printKey Return = "Return"
printKey Print = "Print"

printCommand :: Command -> String
printCommand (Focus d) = "focus "++printDirection d
printCommand (Move d) = "move "++printDirection d
printCommand (ChangeMode name) = "mode "++name
printCommand (DoSplit a) = "split " ++ printAxis a
printCommand ToggleSplit = "layout toggle split"
printCommand (ChangeLayout l) = "layout "++printLayout l
printCommand (DoExec e) = printExec e
printCommand Kill = "kill"
printCommand ToggleFullscreen = "fullscreen toggle"
printCommand Reload = "reload"
printCommand Restart = "restart"
printCommand (FocusWorkspace (NamedWorkspace s)) = "focus workspace "++s
-- TODO check if parens aren't needed
printCommand (c1 `Seq` c2) = printCommand c1 ++ "; " ++ printCommand c2

printDirection :: Direction -> String
printDirection Up = "up"
printDirection Down = "down"
printDirection Left = "left"
printDirection Right = "right"
printDirection Parent = "parent"
printDirection Child = "child"

-- TODO I don't think one should do this (classes purely for overloading)
-- could call it AbstractKeyBinding to remind of terrible languages
class MetaKeyBinding a where
  bindsym :: [Key] -> Command -> a

instance MetaKeyBinding KeyBinding where
  bindsym keys = KeyBinding keys

instance MetaKeyBinding Option where
  bindsym keys cmd = BindingDefinition $ bindsym keys cmd

class MetaExec a where
  exec :: [ExecFlag] -> String -> a

instance MetaExec Option where
  exec f c = GlobalExec $ Exec f c

instance MetaExec Exec where
  exec = Exec

instance MetaExec Command where
  exec f c = DoExec $ Exec f c
