{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module RDPURI (
  -- Our Type
  RDPURI(..),
  Attribute(..),
  -- The lenses
  rdpuriAddress,
  rdpuriAttributes,
  -- Rendering
  renderRDPURI,
  -- Support
  addAttribute,
  -- Data types
  ZeroOne(..), ZeroOneTwo(..)
  ) where

import           Control.Lens
import           Data.ByteString        as BS
import           Data.ByteString.Lens
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           Network.HTTP.Types.URI

type Address = (Text, Maybe Int)

data RDPURI = RDPURI { _rdpuriAddress :: Address, _rdpuriAttributes :: [Attribute] }

data Attribute
  = AllowDesktopComposition ZeroOne
  | AllowFontSmoothing ZeroOne
  | AlternateShell Text
  | AudioMode ZeroOneTwo
  | AuthenticationLevel ZeroOneTwo
  | ConnectToConsole ZeroOne
  | DisableCursorSettings ZeroOne
  | DisableFullWindowDrag ZeroOne
  | DisableMenuAnims ZeroOne
  | DisableThemes ZeroOne
  | DisableWallpaper ZeroOne
  | DriveStoreDirect
  | DesktopHeight Int
  | DesktopWidth Int
  | Domain Text
  | GatewayHostname Text
  | GateWayUsageMethod OneTwo
  | PromptForCredentialsOnClient ZeroOne
  | LoadBalanceInfo Text
  | RedirectPrinters ZeroOne
  | RemoteApplicationCmdLine Text
  | RemoteApplicationMode ZeroOne
  | RemoteApplicationProgram Text
  | ShellWorkingDirectory Text
  | UseRedirectionServerName ZeroOne
  | Username Text
  | ScreenModeId OneTwo
  | SessionBPP BPP
  | UseMultimon ZeroOne

data ZeroOne = Zero | One
data ZeroOneTwo = Zero' | One' | Two'
data OneTwo = One'' | Two''
data BPP = BPP8 | BPP15 | BPP16 | BPP24 | BPP32

makeLenses ''RDPURI

renderRDPURI :: RDPURI -> BS.ByteString
renderRDPURI (RDPURI address as) = "rdp://" <> render attributes
                where attributes = renderAddress address : fmap renderAttribute as
                      render = BS.intercalate "&" . fmap f
                               where f (a,b) = urlEncode False a <> "=" <> b

addAttribute :: Attribute -> RDPURI -> RDPURI
addAttribute a = rdpuriAttributes <>~ [a]

renderAddress :: (Text, Maybe Int) -> (ByteString, ByteString)
renderAddress (a, v) = ("full address", x)
                    where x = "s:" <> encodeUtf8 a <> portBit
                          portBit = maybe "" (\i -> ":" <> (view packedChars . show $ i)) v

-- Maybe do this with a Prism? Make it 2-way
class RdpValue a where
  renderValue :: a -> ByteString

instance RdpValue Text where
  renderValue v = "s:" <> encodeUtf8 v

instance RdpValue ZeroOne where
  renderValue Zero = "i:0"
  renderValue One = "i:1"

instance RdpValue ZeroOneTwo where
  renderValue Zero' = "i:0"
  renderValue One' = "i:1"
  renderValue Two' = "i:2"

instance RdpValue OneTwo where
  renderValue One'' = "i:1"
  renderValue Two'' = "i:2"

instance RdpValue Int where
  renderValue i = "i:" <> (show i ^. packedChars)

instance RdpValue BPP where
  renderValue BPP8 = renderValue (8::Int)
  renderValue BPP15 = renderValue (15::Int)
  renderValue BPP16 = renderValue (16::Int)
  renderValue BPP24 = renderValue (24::Int)
  renderValue BPP32 = renderValue (32::Int)

-- https://technet.microsoft.com/en-us/library/dn690096.aspx

renderAttribute :: Attribute -> (ByteString, ByteString)
renderAttribute = \case
 AllowDesktopComposition z -> ("allow desktop composition", renderValue z)
 AllowFontSmoothing z -> ("allow font smoothing", renderValue z)
 AlternateShell z -> ("alternate shell",renderValue z)
 AudioMode z -> ("audiomode", renderValue z)
 AuthenticationLevel z -> ("authentication level", renderValue z)
 ConnectToConsole z -> ("connect to console", renderValue z)
 DisableCursorSettings z -> ("disable cursor settings", renderValue z)
 DisableFullWindowDrag z -> ("disable full window drag", renderValue z)
 DisableMenuAnims z -> ("disable menu anims", renderValue z)
 DisableThemes z -> ("disable themes", renderValue z)
 DisableWallpaper z -> ("disable wallpaper", renderValue z)
 DriveStoreDirect -> ("drivestoredirect", renderValue ("*" :: Text))
 DesktopHeight z -> ("desktopheight", renderValue z)
 DesktopWidth z -> ("desktopwidth", renderValue z)
 Domain z -> ("domain", renderValue z)
 -- FullAddress z -> ("full address", renderValue z)
 GatewayHostname z -> ("gatewayhostname", renderValue z)
 GateWayUsageMethod z -> ("gatewayusagemethod", renderValue z)
 PromptForCredentialsOnClient z -> ("prompt for credentials on client", renderValue z)
 LoadBalanceInfo z -> ("loadbalanceinfo", renderValue z)
 RedirectPrinters z -> ("redirectprinters", renderValue z)
 RemoteApplicationCmdLine z -> ("remoteapplicationcmdline", renderValue z)
 RemoteApplicationMode z -> ("remoteapplicationmode", renderValue z)
 RemoteApplicationProgram z -> ("remoteapplicationprogram", renderValue z)
 ShellWorkingDirectory z -> ("shell working directory", renderValue z)
 UseRedirectionServerName z -> ("Use redirection server name", renderValue z)
 Username z -> ("username", renderValue z)
 ScreenModeId z -> ("screen mode id", renderValue z)
 SessionBPP z -> ("session bpp", renderValue z)
 UseMultimon z -> ("use multimon", renderValue z)


