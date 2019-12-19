import Locals

import System.Exit
import System.IO

import XMonad
-- import XMonad.ManageHook
-- import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
-- import XMonad.Actions.UpdateFocus

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

-- import XMonad.Layout.Gaps
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
-- import XMonad.Layout.Fullscreen
-- import XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
-- import XMonad.Layout.Tabbed
-- import XMonad.Layout.ThreeColumns
-- import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
-- import XMonad.Layout.NoFrillsDecoration
-- import XMonad.Layout.Renamed
-- import XMonad.Layout.Simplest
-- import XMonad.Layout.SubLayouts
-- import XMonad.Layout.WindowNavigation
-- import XMonad.Layout.ZoomRow

import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Prompt.Pass

import XMonad.Util.Run(spawnPipe)

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Color of current window title in xmobar.
xmobarTitleColor = "#C678DD"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#51AFEF"

base02  = "#073642"
yellow  = "#cd8b00"



myPromptConfig = def {
    font = Locals.myFont
  , height = Locals.myPromptHeight
  -- , promptKeymap = M.union [((modMask, xK_Up), historyUpMatching def)
  --                          ,((modMask, xK_Down), historyDownMatching def)]
  --                          (promptKeymap def)
  -- , maxComplRows = 7
  }


myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    ((modMask .|. controlMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Lock the screen using command specified by myScreensaver.
  , ((modMask, xK_0),
     spawn "screenlock.sh")

  -- spawn the launcher using command specified by mylauncher.
  -- use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn "rofi -show")

  , ((controlMask .|. modMask, xK_m),
     spawn "emacs --eval '(gnus)'")

  , ((mod4Mask,xK_Print),spawn "scrot '%F-%H-%M-%S.png' -e 'mv $f ~/pic/'") -- screenshot

  , ((modMask .|. shiftMask, xK_g),
     windowPrompt myPromptConfig Goto wsWindows)

  -- , ((modMask .|. shiftMask, xK_b),
  --    windowPrompt myPromptConfig
  --     { autoComplete = Just 500000 }
  --     Goto allWindows)

  , ((modMask .|. controlMask, xK_s),
     passPrompt myPromptConfig)


  , ((modMask .|. controlMask .|. shiftMask, xK_s),
     passGeneratePrompt myPromptConfig)

  , ((modMask .|. controlMask, xK_x),
     xmonadPrompt myPromptConfig)

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  -- , ((modMask .|. shiftMask, xK_p),
  --    spawn "")

  -- Take a full screenshot using the command specified by myScreenshot.
  -- , ((modMask .|. controlMask .|. shiftMask, xK_p),
  --    spawn "")

  -- Toggle current focus window to fullscreen
  , ((modMask .|. shiftMask, xK_f), sendMessage $ Toggle FULL)

  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "/usr/bin/pactl set-sink-mute alsa_output.pci-0000_00_1f.3.analog-stereo toggle")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo '-10%'")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo '+10%'")

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  -- , ((modMask, xK_n),
  --    refresh)

  -- Move focus to the next window.
  , ((modMask, xK_k),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_j),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((controlMask .|. modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  -- switching workspaces, and moving windows between spaces
  ++
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

-- WM_CLASS(STRING) = "emacs", "Emacs"  <-- "emacs" is 'resource' (appName) name and "Emacs" is 'className'
myManageHook :: ManageHook
myManageHook = composeAll
    [
       manageDocks
    , className =? "Steam"       --> doCenterFloat
    , className =? "Pavucontrol" --> doCenterFloat
    , stringProperty "WM_NAME" =? "Microsoft Teams Notification" --> doIgnore
    , stringProperty "WM_WINDOW_ROLE" =? "browser-window" --> doFloat
    , resource =? "gcr-prompter" --> doCenterFloat -- Pinentry, etc. GnuPG
    , resource =? "vlc" --> doCenterFloat
    , className =? "zoom" --> doFloat
    , isDialog --> doCenterFloat
    , isFullscreen               --> doFullFloat
    , manageHook def
    ]


-- myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> hintsEventHook <+> docksEventHook
myEventHook = docksEventHook


main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ def
        {
        -- manageHook = manageDocks <+> manageHook def
          manageHook = myManageHook
        -- smartborders is added to remove "focus border" when only one window or fullscreen
        , layoutHook = smartBorders . avoidStruts  $  layoutHook def
        -- , handleEventHook = handleEventHook def <+> docksEventHook
        , handleEventHook = myEventHook
        , logHook = dynamicLogWithPP xmobarPP {
                  ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
                , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
                , ppSep = "   "
                , ppOutput = hPutStrLn xmproc
         } >> updatePointer (0.75, 0.75) (0.75, 0.75)
          -- mod1Mask = "left alt"
          -- mod3Mask = "right alt"
          -- mod4Mask = "windows key"
          -- mod5Mask = "alt gr"
        , modMask = mod4Mask
        , normalBorderColor  = base02
        , focusedBorderColor = yellow
        , keys = myKeys
        , workspaces = myWorkspaces
        , mouseBindings = myMouseBindings
        , terminal = "urxvt"
        , startupHook = spawn "~/bin/xmonad-startup.sh"
        -- more changes
        }
