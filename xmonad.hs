import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Spacing
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#504945"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua = "#8ec07c"


main :: IO ()
main = do
	dbus <- D.connectSession
	 -- Request access to the DBus name
	D.requestName dbus (D.busName_ "org.xmonad.Log")
		[D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

	-- config <- statusBar myConfig
	xmonad  $ ewmh $ docks myConfig { logHook = dynamicLogWithPP (dbusLogHook dbus)}

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myLauncher = "$(rofi -theme fortytw2 -combi-modi window,run -show combi)"

myScreenshot = "maim -s | tee ~/screenshots/$(date +%s).png | xclip -selection clipboard -t image/png && notify-send 'Screenshot Taken'"

dbusLogHook :: D.Client -> PP
dbusLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap "<" ">"
    , ppVisible = wrap "[" "]"
    , ppUrgent = wrap "*" "*"
    , ppHidden = wrap "-" "-"
    , ppHiddenNoWindows = wrap "" ""
    , ppWsSep = " "
    , ppSep = "  "
	, ppTitle = (\str -> "")
	, ppLayout = (\str -> "")
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"

memberName = D.memberName_ "Update"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
	 spawn $ XMonad.terminal conf)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn myLauncher)

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  , ((0, xK_Print),
     spawn myScreenshot)

  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "pactl set-sink-mute 0 toggle && notify-send 'mute toggled'")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "pactl set-sink-volume 0 -5% && notify-send \"volume $(pamixer --get-volume)\"")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "pactl set-sink-volume 0 +5% && notify-send volume $(pamixer --get-volume)")

  -- raise brightness
  , ((0, xF86XK_MonBrightnessUp),
     spawn "xbacklight +10% && notify-send brightness $(xbacklight)")

    -- lower brightness
  , ((0, xF86XK_MonBrightnessDown),
     spawn "xbacklight -10% && notify-send brightness $(xbacklight)")

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
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
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

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))


	 , ((modMask .|. controlMask, xK_0), sendMessage $ ToggleStrut U)

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]



-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig { terminal = "kitty"
	, layoutHook = myLayoutHook
	, normalBorderColor = "#252932"
	, focusedBorderColor = "#d08770"
	, handleEventHook = fullscreenEventHook
	, keys = myKeys
	, clickJustFocuses = False
	, workspaces = myWorkspaces
	, manageHook = composeOne [
             isKDETrayWindow -?> doIgnore,
             transience,
			   isFullscreen -?> doFullFloat,
             resource =? "stalonetray" -?> doIgnore
        ] <+> manageDocks
	}

myWorkspaces = ["1","2","3","4", "5"]

myLayoutHook = avoidStruts $ smartBorders $ smartSpacing 4 $ Tall 1 (3/100) (1/4)
