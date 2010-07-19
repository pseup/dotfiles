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
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

import XMonad.Prompt
import XMonad.Prompt.Shell
-- }}}

-- Settings {{{
pTerm       = "urxvtc"
pModMask    = mod4Mask
pBorder     = 3
pNormColor  = "#131313"
pFocusColor = "#131313"

pIconDir = "/home/pseup/.xmonad/icons/"
pFont    = "-*-mintsstrong-*-*-*-*-8-*-*-*-*-*-*-*"
pFont2   = "-*-snap-*-*-*-*-10-*-*-*-*-*-*-*"
pFont3   = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"

dzenOptions = " -h 18 -fn '" ++ pFont ++ "' -fg '#B5B5B5' -bg '#131313' -e 'onstart=lower'"
dzenWorkspaces  = "dzen2 -ta l -w 960 -y 1182" ++ dzenOptions
-- }}}

-- Main {{{
main = xmonad =<< statusBar cmd pp kb conf
  where
    uhook = withUrgencyHookC NoUrgencyHook urgConfig
    cmd   = dzenWorkspaces
    pp    = dzPP
    kb    = toggleStrutsKey
    conf  = uhook myConfig
-- }}}

-- Config {{{
myConfig = defaultConfig { workspaces = pWorkspaces
                         , layoutHook = pLayout
                         , manageHook = pManageHook
                         , borderWidth = pBorder
                         , normalBorderColor  = pNormColor
                         , focusedBorderColor = pFocusColor
                         , terminal = pTerm
                         , modMask = pModMask
                         , keys = pKeys
                         }


urgConfig = UrgencyConfig { suppressWhen = Focused
                          , remindWhen   = Dont }
-- }}}

-- dzen PP {{{
dzPP = defaultPP
     { ppCurrent = dzenColor "#131313" "#3A78C8" . pad
     , ppHidden  = pad
     , ppHiddenNoWindows = dzenColor "#555555" "" . pad
     , ppUrgent  = dzenColor "#AE3232" "". pad
     , ppLayout  = (\x -> case x of
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
          { position    = Top
          , font        = pFont3
          , bgColor     = "#323333"
          , fgColor     = "#B1B1B1"
          , fgHLight    = "#DE8C19"
          , bgHLight    = "#323333"
          , promptBorderWidth = 0
          , historyFilter = deleteConsecutive
          }
-- }}}

-- Workspace Layouts {{{
pWorkspaces = ["Code", "Web", "Media", "Util", "Misc"]
pLayout = onWorkspace "Code" (gap tricol)
        $ onWorkspace "Media" full
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
  , [ title     =? t --> doFloat | t <- tFloats ]
  , [ className =? c --> doCenterFloat | c <- cFloats ]
  , [ className =? "MPlayer" --> doF (W.shift "Media") ]
  , [ resource  =? "irc"      --> doF (W.shift "Web") ]
  , [ resource  =? "rtorrent" --> doF (W.shift "Util") ]
  , [ resource  =? "ncmpc"    --> doF (W.shift "Util") ]
  , [ resource  =? "popTerm"  --> doCenterFloat]
  ]
  where
    cFloats = ["feh", "Mpdtab", "Blender:Render", "MPlayer", "Gimp" ]
    tFloats = ["Downloads", "-$> codetest" ]
-- }}}

-- Keys {{{
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

pKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- apps
  [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
  , ((modMask,               xK_p     ), shellPrompt pXPConfig)
  , ((modMask .|. shiftMask, xK_p     ), spawn "urxvtc -name \"popTerm\" -geometry 84x8")
  , ((modMask .|. shiftMask, xK_v     ), spawn "urxvtc -e vim")
  , ((modMask .|. shiftMask, xK_w     ), spawn "firefox")
  , ((modMask,               xK_Print ), spawn "scrot ~/Pictures/Screenshots/%y%m%d_%H%M%S.png")
  , ((modMask .|. shiftMask, xK_c     ), kill)

  -- mpd
  , ((0, 0x1008ff14                   ), spawn "mpc toggle")
  , ((modMask .|. controlMask,  xK_h  ), spawn "mpc toggle")
  , ((modMask .|. controlMask,  xK_j  ), spawn "mpc prev")
  , ((modMask .|. controlMask,  xK_k  ), spawn "mpc next")

  -- layouts
  , ((modMask,               xK_space ), sendMessage NextLayout)
  , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
  , ((modMask,               xK_n     ), refresh)

  -- focus
  , ((modMask,               xK_Tab   ), windows W.focusDown)
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
  ++
  -- mod-{a,o} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{a,o} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_o] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
-- }}}
