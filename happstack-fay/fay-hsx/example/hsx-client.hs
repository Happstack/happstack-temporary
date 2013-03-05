{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import AJAX
import HTML
import Prelude
import JQuery
import FFI

main :: Fay ()
main =
  ready $
    do div <- select "#thediv"
       newContents <- <div>
                       <p class="red">This paragraph was added on the client-side via Fay.</p>
                       <span></span>
                       <p class="red">As was this one Fay.</p>
                      </div>
       newContentsJQ <- renderHTML newContents
       append newContentsJQ div
       return ()
