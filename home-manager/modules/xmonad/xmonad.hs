-- SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
--
-- SPDX-License-Identifier: LGPL-3.0-or-later

--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.DynamicLog
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FloatKeys

import Control.Monad

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "xterm"

xmobarCmd :: String
xmobarCmd = "@xmobar@ @xmobarConfig@"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False


-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = map show [0..9]

toggleFloat = withFocused (\windowId -> do
                              { floats <- gets (W.floating . windowset);
                                if windowId `M.member` floats
                                then withFocused $ windows . W.sink
                                else float windowId })

modm :: KeyMask
modm = mod4Mask

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeymap c =
    -- launch a terminal
    [ ("M-S-<Return>", spawn "alacritty")

    -- launch dmenu
    , ("M-e", spawn "@dmenu_run@")

    -- close focused window
    , ("M-S-q", io (exitWith ExitSuccess))

     -- Rotate through the available layout algorithms
    , ("M-<Space>", sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    -- , ("M-S-Space", setLayout $ XMonad.layoutHook c)

    -- Resize viewed windows to the correct size
    , ("M-b", refresh)

    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)

    -- Move focus to the next window
    , ("M-n", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-r", windows W.focusUp)

    -- Move focus to the master window
    , ("M-p", windows W.focusMaster)

    -- Swap the focused window and the master window
    , ("M-<RET>", windows W.swapMaster)

    -- Swap the focused window with the next window
    , ("M-S-n", windows W.swapDown)

    -- Swap the focused window with the previous window
    , ("M-S-r", windows W.swapUp)

    -- Shrink the master area
    , ("M-t", sendMessage Shrink)

    -- Expand the master area
    , ("M-s", sendMessage Expand)

    -- Push window back into tiling
    , ("M-y", toggleFloat)

    -- Increment the number of windows in the master area
    , ("M-w", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-m", sendMessage (IncMasterN (-1)))

    -- , ("M-b", spawn ("pkill xmobar && " ++ xmobarCmd))
    , ("<Print>", spawn "sleep 0.1 ; @screenshot@ select")
    , ("S-<Print>", spawn "@screenshot@ screen")
    , ("C-S-<Print>", spawn "@screenshot@ focused")

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ("M-S-k", kill)

    -- Restart xmonad
    , ("M-k", spawn "xmonad --recompile; xmonad --restart")

    -- float keys
     , ("M-g", withFocused (keysResizeWindow (-10,  0) (0, 0)))
     , ("M-c", withFocused (keysResizeWindow (  0, 10) (0, 0)))
     , ("M-l", withFocused (keysResizeWindow (  0,-10) (0, 0)))
     , ("M-ß", withFocused (keysResizeWindow ( 10,  0) (0, 0)))
     , ("M-S-g", withFocused (xMoveWindow (-10,  0)))
     , ("M-S-c", withFocused (xMoveWindow (  0, 10)))
     , ("M-S-l", withFocused (xMoveWindow (  0,-10)))
     , ("M-S-ß", withFocused (xMoveWindow ( 10,  0)))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [("M-" ++ m ++ [k], windows $ f i)
        | (i, k) <- zip (XMonad.workspaces c) "1234567890"
        , (f, m) <- [(W.view, ""), (W.shift, "S-")]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [("M-"++m++[key], screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ".o," [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "S-")]]
    where
      xMoveWindow
        :: (Position, Position)
        -> Window
        -> X ()
      xMoveWindow (x, y) w = withDisplay (\d -> do
        (_, ox, oy, _, _, _, _) <- io $ getGeometry d w
        io $ moveWindow d w (ox + x) (oy + y))


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = smartBorders tiled ||| smartBorders simpleTabbed ||| smartBorders emptyBSP ||| noBorders Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ -- className =? "MPlayer"        --> doFloat
   -- , className =? "Gimp"           --> doFloat
    -- , resource  =? "desktop_window" --> doIgnore
    -- , resource  =? "kdesktop"       --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = updatePointer (0.5, 0.5) (1, 1)

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  when @enableDunst@ (spawnOnce "@dunst@ -config @dunstConfig@")
  when @enablePicom@ (spawnOnce "@picom@ --config @picomConfig@ @experimentalBackends@")
  when @enableKeynav@ (spawnOnce "@keynav@")
  spawnOnce xmobarCmd

myPP = def
   { ppLayout = const ""  -- Don't show the layout name
   , ppSort = getSortByXineramaRule  -- Sort left/right screens on the left, non-empty workspaces after those
   , ppTitle = const ""  -- Don't show the focused window's title
   , ppTitleSanitize = const ""  -- Also about window's title
   , ppVisible = wrap "(" ")"  -- Non-focused (but still visible) screen
   }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
-- main = xmonad $ ewmh $ docks $ defaults
main = xmonad . ewmh =<< statusBar xmobarCmd myPP toggleStrutsKey defaults
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = let c = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        modMask            = modm,
        workspaces         = myWorkspaces,

      -- key bindings
        -- keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts $ myLayout,
        manageHook         = manageDocks <+> myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook,

      -- Looks
        focusedBorderColor = "#5c5c5c",
        normalBorderColor = "#222222",
        borderWidth = 2
    } in additionalKeys (additionalKeysP c (myKeymap c)) [ ((mod1Mask, xK_v), return ()) ]

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
