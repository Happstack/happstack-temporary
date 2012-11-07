{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}
module Language.Fay.AJAX where

import Language.Fay.FFI
import Language.Fay.Prelude

data ResponseType a = ResponseType
    deriving (Eq, Read, Show, Data, Typeable)
instance Foreign (ResponseType a)

-- | Asynchronously call a command
call :: (Foreign cmd, Foreign res) =>
        String
     -> (ResponseType res -> cmd)
     -> (res -> Fay ())
     -> Fay ()
call uri f g =
    ajaxCommand uri (f ResponseType) g

-- | Run the AJAX command.
--
-- Note: if the server returns 404 or some other non-success exit
-- code, the callback function will never be run.
ajaxCommand :: (Foreign cmd, Foreign res) =>
               String
            -> cmd
            -> (res -> Fay ())
            -> Fay ()
ajaxCommand =
    ffi "jQuery['ajax']({\
        \ \"url\": %1, \
        \ \"type\": 'POST', \
        \ \"data\": { \"json\": JSON.stringify(%2) }, \
        \ \"dataType\": 'json', \
        \ \"success\" : %3 \
        \})"

{-
ffiE :: Foreign a => JExpr -> a
ffiE expr = ffi (show $ renderJs expr)

ajaxCommand :: (Foreign cmd, Foreign res, Foreign a) => cmd -> (res -> Fay a) -> Fay a
ajaxCommand =
    ffiE ajaxCommand'

ajaxCommand' :: JExpr
ajaxCommand' =
    [jmacroE|
       jQuery['ajax']({
           url:  '/json',
           type: 'POST',
           data: { json: JSON.Stringify(`(arg 1)`) },
           dataType: 'json',
           success: `(arg 2)`
       })
    |]

arg :: Int -> JVal
arg n = JVar $ StrI ('%': show n)
-}