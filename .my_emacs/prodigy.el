(prodigy-define-service
  :name "lmv front dev"
  :command "npm"
  :cwd "~/workspace/OFFICE/gltflmvviewer/"
  :args '("run" "dev")
  :tags '(front lmv)
  )

(prodigy-define-service
  :name "lmv front watch"
  :command "cross-env"
  :cwd "~/workspace/OFFICE/gltflmvviewer/"
  :args '("NODE_ENV=BUILD" "./node_modules/.bin/webpack" "-w" "--config" "webpack.config.js")
  :tags '(front lmv)
  )

(prodigy-define-service
  :name "peacock front"
  :command "npm"
  :cwd "~/workspace/OFFICE/peacock/client"
  :args '("run" "local")
  :tags '(front peacock)
  )

(prodigy-define-service
  :name "peacock back"
  :command "npm"
  :cwd "~/workspace/OFFICE/peacock/server"
  :args '("run" "local")
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
  :args '("run" "local")
  :tags '(front webpack)
  )

(prodigy-define-service
  :name "gbim-diff server"
  :command "npm"
  :cwd "~/workspace/OFFICE/gbim-diff"
  :args '("run" "local")
  :tags '(nodemon back)
  )

(prodigy-define-service
  :name "mongodb sso"
  :command "mongod"
  :cwd "~/workspace/OFFICE/BimsopSSO/"
  :args '("-dbpath" "./dbpath")
  :tags '(mongod back)
  )
