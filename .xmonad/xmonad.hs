--
--  xmonad.hs - i.pseup@gmail.com
--

-- Import {{{
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import System.IO

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

import XMonad.Prompt
import XMonad.Prompt.Shell
-- }}}

-- Settings {{{
pTerm       = "urxvtc"
pModMask    = mod4Mask
pBorder     = 3
pNormColor  = "#131313"
pFocusColor = "#131313" --"#1677B4"

pIconDir = "/home/pseup/.xmonad/icons/"
pFont    = "-*-mintsstrong-*-*-*-*-8-*-*-*-*-*-*-*"
pFont2   = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"

dzOptions = " -h 18 -fn '" ++ pFont ++ "' -fg '#BCC2AD' -bg '#131313' -e 'onstart=lower'"
dzWorkspaces  = "dzen2 -ta l -w 960 -y 1182" ++ dzOptions
-- }}}

-- Main {{{
main = xmonad =<< statusBar dzWorkspaces dzPP kb conf
  where
    uhook = withUrgencyHook NoUrgencyHook
    conf  = uhook myConfig
    kb    = toggleStrutsKey
-- }}}

-- Config {{{
myConfig = ewmh defaultConfig { workspaces = pWorkspaces
                              , layoutHook = pLayout
                              , manageHook = pManageHook
                              , borderWidth = pBorder
                              , normalBorderColor  = pNormColor
                              , focusedBorderColor = pFocusColor
                              , terminal = pTerm
                              , modMask = pModMask
                              , keys = pKeys
                              }
-- }}}

-- dzen PP {{{
dzPP = defaultPP
     { ppCurrent = dzenColor "#131313" "#49AAE7" . pad
     , ppHidden  = pad
     , ppHiddenNoWindows = dzenColor "#4B5457" "" . pad
     , ppUrgent  = dzenColor "#AE3232" "". pad
     , ppLayout  = dzenColor "#49AAE7" "" .
                   (\x -> case x of
                     "Tall"        -> "^i(" ++ pIconDir ++ "/tileright.xbm)"
                     "Mirror Tall" -> "^i(" ++ pIconDir ++ "/tilebottom.xbm)"
                     "Full"        -> "^i(" ++ pIconDir ++ "/max.xbm)"
                     "TriCol"      -> "^i(" ++ pIconDir ++ "/threecol.xbm)"
                     _             -> "^i(" ++ pIconDir ++ "/unknown.xbm)"
                     )
     , ppTitle   = pad . shorten 100
     , ppSep     = " "
     , ppWsSep   = ""
     }
  where
    pad = wrap "^p(6)" "^p(6)"

-- }}}

-- Prompt Settings {{{
pXPConfig = defaultXPConfig
          { position    = Bottom
          , font        = pFont2
          , bgColor     = "#131313"
          , fgColor     = "#B1B1B1"
          , fgHLight    = "#49AAE7"
          , bgHLight    = "#131313"
          , promptBorderWidth = 0
          , historyFilter = deleteConsecutive
          }
-- }}}

-- Workspace Layouts {{{
pWorkspaces = ["code", "web", "media", "util", "misc"]
pLayout = onWorkspace "code" (gap tricol)
        $ onWorkspace "media" full
        $ gap (Mirror tiled ||| tiled) ||| full
  where
    named' n l = named n $ spacing 3 l
    tiled      = named' "Tall" $ ResizableTall 1 (1/100) (3/4) []
    tricol     = named' "TriCol" $ ThreeCol 1 (1/100) (1/3)
    gap        = gaps [(D,3), (U,3), (L,3), (R,3)]
    full       = smartBorders Full
-- }}}

-- Hooks {{{
pManageHook = composeAll . concat $
  [ [ isDialog --> doFloat ]
  , [ isFullscreen --> doFullFloat ]
  , [ className =? c --> doCenterFloat | c <- cFloats ]
  , [ className =? "MPlayer"  --> doF (W.shift "media") ]
  , [ resource  =? "irc"      --> doF (W.shift "web") ]
  , [ resource  =? "rtorrent" --> doF (W.shift "util") ]
  , [ resource  =? "ncmpc"    --> doF (W.shift "util") ]
  , [ className =? "Gimp"     --> doFloat ]
  , [ resource  =? "popTerm"  --> doCenterFloat ]
  , [ resource  =? "+>>"      --> doCenterFloat ]
  , [ title     =? "++>_"     --> doCenterFloat ]
  ]
  where
    cFloats = ["feh", "Mpdtab", "Blender:Render", "MPlayer"]
-- }}}

-- Keys {{{
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

pKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- apps
  [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
  , ((modMask,               xK_p     ), shellPrompt pXPConfig)
  , ((modMask .|. shiftMask, xK_p     ), spawn "urxvtc -name \"popTerm\"")
  , ((modMask .|. shiftMask, xK_v     ), spawn "urxvtc -e vim")
  , ((modMask .|. shiftMask, xK_w     ), spawn "firefox")
  , ((modMask,               xK_Print ), spawn "~/Scripts/scrshot.sh")
  , ((modMask .|. shiftMask, xK_c     ), kill)

  -- media keys
   , ((0, stringToKeysym "XF86AudioPlay" ), spawn "mpc toggle")

  -- mpd
  , ((modMask .|. controlMask,  xK_h  ), spawn "mpc toggle")
  , ((modMask .|. controlMask,  xK_j  ), spawn "mpc prev")
  , ((modMask .|. controlMask,  xK_k  ), spawn "mpc next")

  -- layouts
  , ((modMask,               xK_space ), sendMessage NextLayout)
  , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
  , ((modMask,               xK_n     ), refresh)

  -- focus
  , ((modMask,               xK_Tab   ), windows W.focusDown)
  , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
  , ((modMask,               xK_j     ), windows W.focusDown)
  , ((modMask,               xK_k     ), windows W.focusUp)
  , ((modMask,               xK_m     ), windows W.focusMaster)

  -- ordering
  , ((modMask .|. shiftMask, xK_m     ), windows W.swapMaster)
  , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
  , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

  -- resizing
  , ((modMask,               xK_h     ), sendMessage Shrink)
  , ((modMask,               xK_l     ), sendMessage Expand)
  , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
  , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

  -- floating layer support
  , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

  -- +/- master windows
  , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
  , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

  -- quit, or restart
  , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
  , ((modMask              , xK_q     ), restart "xmonad" True)
  ]
  ++
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- }}}
