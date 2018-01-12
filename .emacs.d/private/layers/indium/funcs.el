(defun indium-toggle-v8-cache ()
  "toggle browser cache"
  (interactive)
  (if indium-v8-cache-disabled (indium-v8-enable-cache)
    (indium-v8-disable-cache)))