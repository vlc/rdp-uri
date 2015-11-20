
rdp-uri
===

`rdp-uri` is a library for building URIs that follow the Remote Desktop Connection URI Scheme.

The scheme is documented here: https://technet.microsoft.com/en-us/library/dn690096.aspx


Building URIs
---

```haskell

import RDPURI

ex1 = RDPURI ("localhost", Nothing) []
ex2 = ex1 & rdpuriAttributes <>~ [DesktopWidth 1024, DesktopHeight 768]
ex3 = addAttribute (SessionBPP BPP32) ex2

```

Printing URIs
---

```
λ> renderRDPURI ex1
"rdp://full%20address=s:localhost"
λ> renderRDPURI ex2
"rdp://full%20address=s:localhost&desktopwidth=i:1024&desktopheight=i:768"
λ> renderRDPURI ex3
"rdp://full%20address=s:localhost&desktopwidth=i:1024&desktopheight=i:768&session%20bpp=i:32"
```
