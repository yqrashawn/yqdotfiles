(prodigy-define-service
  :name "lmv front"
  :command "npm"
  :cwd "~/workspace/gltflmvviewer/"
  :args '("run" "dev")
  :tags '(front lmv)
  )

(prodigy-define-service
  :name "peacock front"
  :command "npm"
  :cwd "~/workspace/peacock/client"
  :args '("run" "dev")
  :tags '(front peacock)
  )

(prodigy-define-service
  :name "peacock back"
  :command "npm"
  :cwd "~/workspace/peacock/server"
  :args '("run" "dev")
  :tags '(back peacock)
  )

(prodigy-define-service
  :name "sso back"
  :command "npm"
  :cwd "~/workspace/BimsopSSO/sso-server"
  :args '("run" "dev")
  :tags '(back nodemon)
  )

(prodigy-define-service
  :name "sso front"
  :command "npm"
  :cwd "~/workspace/BimsopSSO/sso-client"
  :args '("run" "dev")
  :tags '(front webpack)
  )

(prodigy-define-service
  :name "sso front"
  :command "npm"
  :cwd "~/workspace/BimsopSSO/sso-client"
  :args '("run" "dev")
  :tags '(front webpack)
  )

(prodigy-define-service
  :name "gbim-diff server"
  :command "npm"
  :cwd "~/workspace/gbim-diff"
  :args '("run" "dev")
  :tags '(nodemon back)
  )

(prodigy-define-service
  :name "mongodb sso"
  :command "mongod"
  :cwd "~/workspace/BimsopSSO/"
  :args '("-dbpath" "./dbpath")
  :tags '(mongod back)
  )


;; macbook
(prodigy-define-service
  :name "lmv front"
  :command "npm"
  :cwd "~/workspace/OFFICE/gltflmvviewer/"
  :args '("run" "dev")
  :tags '(front lmv)
  )

(prodigy-define-service
  :name "peacock front"
  :command "npm"
  :cwd "~/workspace/OFFICE/peacock/client"
  :args '("run" "dev")
  :tags '(front peacock)
  )

(prodigy-define-service
  :name "peacock back"
  :command "npm"
  :cwd "~/workspace/OFFICE/peacock/server"
  :args '("run" "dev")
  :tags '(back peacock)
  )

(prodigy-define-service
  :name "sso back"
  :command "npm"
  :cwd "~/workspace/OFFICE/BimsopSSO/sso-server"
  :args '("run" "dev")
  :tags '(back nodemon)
  )

(prodigy-define-service
  :name "sso front"
  :command "npm"
  :cwd "~/workspace/OFFICE/BimsopSSO/sso-client"
  :args '("run" "dev")
  :tags '(front webpack)
  )

(prodigy-define-service
  :name "sso front"
  :command "npm"
  :cwd "~/workspace/OFFICE/BimsopSSO/sso-client"
  :args '("run" "dev")
  :tags '(front webpack)
  )

(prodigy-define-service
  :name "gbim-diff server"
  :command "npm"
  :cwd "~/workspace/OFFICE/gbim-diff"
  :args '("run" "dev")
  :tags '(nodemon back)
  )

(prodigy-define-service
  :name "mongodb sso"
  :command "mongod"
  :cwd "~/workspace/OFFICE/BimsopSSO/"
  :args '("-dbpath" "./dbpath")
  :tags '(mongod back)
  )
