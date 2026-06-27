`https://gitlab.com/nonguix/nonguix`

```lisp
(channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (introduction
    (make-channel-introduction
      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
      (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
```

The **Nonguix** channel is your _primary source_ for _non-free_ software and drivers that can’t be included in the main Guix repository. 

You can find the following packages (and more!) there:

- **full** Linux kernel
    - non-free modules
- NVIDIA drivers
- Non-free drivers
-/Steam client
- Firefox latest
- Clojure’s Leiningen tool
- Wine (windows)

If you use the linux package, it requires that the kernel be rebuilt every time you update your system! This can take... quite some time.
