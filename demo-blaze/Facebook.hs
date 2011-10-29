module Facebook where

import qualified Data.Text as Text
import Web.Authenticate.Facebook 

facebook :: Facebook
facebook = Facebook { facebookClientId     = Text.pack "157639254022"
                    , facebookClientSecret = Text.pack "7380e71b401e1207f517855648fbe22c"
                    , facebookRedirectUri  = Text.empty
                    }
