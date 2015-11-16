allow desktop composition=i:<0 or 1>
allow font smoothing=i:<0 or 1>
alternate shell=s:<string>
audiomode=i:<0, 1, or 2>
authentication level=i:<0, 1, or 2>
connect to console=i:<0 or 1>
disable cursor settings=i:<0 or1>
disable full window drag=i:<0 or 1>
disable menu anims=i:<0 or 1>
disable themes=i:<0 or 1>
disable wallpaper=i:<0 or 1>
drivestoredirect=s:* (this is the only supported value)
desktopheight=i:<value in pixels>
desktopwidth=i:<value in pixels>
domain=s:<string>
full address=s:<string>
gatewayhostname=s:<string>
gatewayusagemethod=i:<1 or 2>
prompt for credentials on client=i:<0 or 1>
loadbalanceinfo=s:<string>
redirectprinters=i:<0 or 1>
remoteapplicationcmdline=s:<string>
remoteapplicationmode=i:<0 or 1>
remoteapplicationprogram=s:<string>
shell working directory=s:<string>
Use redirection server name=i:<0 or 1>
username=s:<string>
screen mode id=i:<1 or 2>
session bpp=i:<8, 15, 16, 24, or 32>
use multimon=i:<0 or 1>

rdp://full%20address=s:mypc:3389&audiomode=i:2&disable%20themes=i:1

data Attribute =
  AllowDesktopComposition Bool
  | AllowFontSmoothing Bool
