### guix-science

`https://codeberg.org/guix-science/guix-science.git`

```lisp
(append
  (list (channel
          (name 'guix-science)
          (url "https://codeberg.org/guix-science/guix-science.git")
          (branch "master")
          (introduction
            (make-channel-introduction
              "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
              (openpgp-fingerprint
                "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446")))))
  %default-channels)
```



### substitutes

`https://guix.bordeaux.inria.fr`


```lisp
(public-key
 (ecc
  (curve Ed25519)
  (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))
  ```
