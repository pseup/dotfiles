-- pseup's Xmonad Config (0.9)

-- Import {{{
import XMonad
import Graphics.X11.Xlib
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import System.IO (Handle, hPutStrLn)

import XMonad.Hooks.DynamicLog ( PP(..), dynamicLogWithPP, dzenColor, shorten, wrap, defaultPP )
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Combo
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

import XMonad.Operations

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad

import XMonad.Util.Run (spawnPipe)
-- }}}
--------------------------------
-- Settings {{{
pIconDir = "/home/pseup/.xmonad/icons/"
pFont    = "-*-mintsstrong-*-*-*-*-8-*-*-*-*-*-*-*"
pFont2   = "-*-snap-*-*-*-*-10-*-*-*-*-*-*-*"

pTerm = "urxvtc"

dzenOptions = " -h 18 -fn '" ++ pFont ++ "' -fg '#B5B5B5' -bg '#131313'"
dzenWorkspaces  = "dzen2 -ta l -w 960 -y 1182" ++ dzenOptions

pModMask = mod4Mask
pBorder = 3
normalBorderColor'  = "#131313"
focusedBorderColor' = "#131313"

-- }}}
--------------------------------
-- Main {{{
main = do
       spWorkspaces <- spawnPipe dzenWorkspaces
       xmonad $ withUrgencyHook NoUrgencyHook
              $ defaultConfig
              { workspaces = pWorkspaces
              , modMask = pModMask
              , borderWidth = pBorder
              , normalBorderColor = normalBorderColor'
              , focusedBorderColor = focusedBorderColor'
              , terminal = pTerm
              , keys = pKeys
              , logHook = dynamicLogWithPP $ customPP spWorkspaces
              --, manageHook = manageHook defaultConfig <+> pManageHook <+> xPropManageHook pPropHook
              , manageHook = pManageHook
              , layoutHook = pLayout
              }
-- }}}
--------------------------------
-- Dzen Pretty Printer {{{
customPP i = defaultPP
           { ppCurrent = wrap "^fg(#131313)^bg(#3A78C8)^p(6)" "^p(6)^fg()^bg()"
           , ppHidden  = wrap "^p(6)" "^p(6)"
           , ppHiddenNoWindows = wrap "^fg(#555555)^p(6)" "^p(6)^fg()"
           , ppUrgent  = wrap "^fg(#AE3232)^bg(#131313)" "^fg()^bg()"
           , ppLayout  = (\x -> case x of
                                     "Tall"        -> "^i(" ++ pIconDir ++ "/tileright.xbm)"
                                     "Mirror Tall" -> "^i(" ++ pIconDir ++ "/tilebottom.xbm)"
                                     "Full"        -> "^i(" ++ pIconDir ++ "/max.xbm)"
                                     "ThreeCol"    -> "^i(" ++ pIconDir ++ "/threecol.xbm)"
                                     _             -> x
                         )
           , ppTitle   = wrap "^p(6)" "^p(6)" . shorten 100
           , ppSep     = " "
           , ppWsSep   = ""
           , ppOutput  = hPutStrLn i
           }
-- }}}
--------------------------------
-- Prompt Setup {{{
pXPConfig :: XPConfig
pXPConfig = defaultXPConfig
          { position    = Top
          , font        = pFont2
          , bgColor     = "#4C8EA1"
          , fgColor     = "#131313"
          , fgHLight    = "#B5B5B5"
          , bgHLight    = "#131313"
          , promptBorderWidth = 0
          , historyFilter = deleteConsecutive
          }
-- }}}
--------------------------------
-- Workspaces & Layouts {{{
pWorkspaces :: [WorkspaceId]
pWorkspaces = ["Code", "Web", "Media", "Util", "Misc"]

pLayout = onWorkspace "Code" threecol
        $ onWorkspace "Media" empty
        $ gap (Mirror tiled) ||| gap tiled ||| nogaps ||| Tall 1 (1/100) (3/4)
  where
    tiled    = named "Tall" $ spacing 3 $ ResizableTall 1 (1/100) (3/4) []
    threecol = named "ThreeCol" $ gap $ spacing 3 $ ThreeCol 1 (3/100) (1/3)
    gap      = gaps [(D,21), (U,3), (L,3), (R,3)]
    nogaps   = gaps [(D, 0), (U,0)] $ smartBorders Full
    empty    = smartBorders Full

-- }}}
--------------------------------
-- Hooks {{{
--pManageHook :: ManageHook
pManageHook = composeAll . concat $
  [ [ isFullscreen --> doFloat ]
  , [ className =? c --> placeHook (fixed (0.5,0.5)) <+> doFloat | c <- cFloats ]
  , [ title     =? t --> doFloat | t <- tFloats ]
  , [ className =? "MPlayer" --> doF (W.shift "Media") ]
  , [ resource  =? "rtorrent" --> doF (W.shift "Util") ]
  , [ resource  =? "ncmpc"    --> doF (W.shift "Util") ]
  , [ className =? "XCalc" --> placeHook (fixed (1,0.5)) <+> doFloat ]
  , [ resource  =? "popTerm"  --> placeHook (fixed (0.5,0.5)) <+> doFloat ]
  ]
  where
    cFloats = ["feh", "Mpdtab", "Blender:Render", "MPlayer", "Gimp" ]
    tFloats = ["Downloads", "_<<codetest" ]
-- }}}
--------------------------------
-- Key Bindings {{{

--pKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
pKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_t         ), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask,               xK_p         ), shellPrompt pXPConfig) -- Open shell prompt
    , ((modMask .|. shiftMask, xK_p         ), spawn "urxvtc -name \"popTerm\" -geometry 84x8")
--    , ((0,                     xK_Multi_key ), shellPrompt pXPConfig) -- ^
--    , ((modMask .|. shiftMask, xK_p         ), xmonadPrompt pXPConfig)
    , ((modMask .|. shiftMask, xK_c         ), kill) -- %! Close the focused window
    , ((modMask .|. shiftMask, xK_v         ), spawn "urxvtc -e vim")
    , ((modMask .|. shiftMask, xK_w         ), spawn "firefox")
    , ((modMask .|. shiftMask, xK_m         ), spawn "xterm")
--    , ((modMask,               xK_b         ), withFocused toggleBorder )
    , ((modMask,               xK_Print     ), spawn "scrot ~/Pictures/Screenshots/%y%m%d_%H-%M-%S.png")
--    , ((0,                     xK_Menu      ), shellPrompt pXPConfig)
    , ((0, 0x1008ff14                       ), spawn "mpc toggle")
--    , ((0, 0x1008ff2f                       ), )
--    , ((0, 0x1008ff2a                       ), )
    , ((modMask,               xK_space     ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space     ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
    , ((modMask,               xK_n         ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- mpd controls
    , ((modMask .|. controlMask,  xK_h     ), spawn "mpc prev")
    , ((modMask .|. controlMask,  xK_t     ), spawn "mpc pause")
    , ((modMask .|. controlMask,  xK_n     ), spawn "mpc play")
    , ((modMask .|. controlMask,  xK_s     ), spawn "mpc next")

    , ((modMask .|. controlMask,  xK_g     ), spawn "mpc seek -2%")
    , ((modMask .|. controlMask,  xK_c     ), spawn "mpc volume -5")
    , ((modMask .|. controlMask,  xK_r     ), spawn "mpc volume +5")
    , ((modMask .|. controlMask,  xK_l     ), spawn "mpc seek +2%")


    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True) -- %! Restart xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{a,o} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,o} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_o] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
-- }}}
