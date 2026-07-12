
(append
  (list (channel
          (name 'guix-past)
          (url "https://codeberg.org/guix-science/guix-past.git")
          (branch "master")
          (introduction
            (make-channel-introduction
              "0c119db2ea86a389769f4d2b9c6f5c41c027e336"
              (openpgp-fingerprint
                "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5")))))
  %default-channels)
  
