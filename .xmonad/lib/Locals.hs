module Locals where

import Graphics.X11.Xlib
import XMonad

myFont :: String
myFont = "-*-terminus-*-*-*-*-32-*-*-*-*-*-*-*"
myPromptHeight :: Dimension
myPromptHeight = 50
myBorderWidth :: Dimension
myBorderWidth = 2
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["con","edit","www"] ++ map show [4 .. 9]
