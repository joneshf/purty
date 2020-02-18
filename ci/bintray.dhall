let Config
    : Type
    = { date : Text, tarFile : Text, uploadedFilename : Text, version : Text }

in    λ(config : Config)
    → { files =
        [ { includePattern = config.tarFile
          , uploadPattern = config.uploadedFilename
          }
        ]
      , package =
          { licenses = [ "BSD 3-Clause" ]
          , name = "purty"
          , repo = "generic"
          , subject = "joneshf"
          , vcs_url = "https://github.com/joneshf/purty.git"
          }
      , publish = True
      , version = { name = config.version, released = config.date }
      }
