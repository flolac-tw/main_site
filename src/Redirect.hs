module Redirect
    ( Redirect (..)
    , createRedirects
    ) where

import           Control.Applicative    ((<$>))
import           Control.Monad          (forM_, when)
import           Data.Binary            (Binary (..))
import           Data.List              (sort, group)
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules
import           Hakyll.Core.Writable   (Writable (..))

-- An example of a valid association list would be:
--
-- > brokenLinks =
-- >     [ ("projects.html", "http://github.com/gwern")
-- >     , ("/Black-market archive", "Black-market%20archives")
-- >     ]
--
-- In which case the functionality can then be used in `main` with a line like:
--
-- > version "redirects" $ createRedirects brokenLinks
--
-- The 'version' is recommended to separate these items from your other pages.
--
-- The on-disk files can then be uploaded with HTML mimetypes
-- (either explicitly by generating and uploading them separately, by
-- auto-detection of the filetype, or an upload tool defaulting to HTML
-- mimetype, such as calling @s3cmd@ with @--default-mime-type=text/html@) and
-- will redirect browsers and search engines going to the old/broken URLs.
--
-- See also <https://groups.google.com/d/msg/hakyll/sWc6zxfh-uM/fUpZPsFNDgAJ>.
createRedirects :: [(Identifier, String)] -> Rules ()
createRedirects redirects =
 do -- redirects are many-to-fewer; keys must be unique, and must point somewhere else:
    let gkeys = group $ sort $ map fst redirects
    forM_ gkeys $ \gkey -> case gkey of
        (k : _ : _) -> fail $
            "Duplicate 301 redirects; " ++ show k ++ " is ambiguous."
        _           -> return ()

    forM_ redirects $ \(r, t) ->
        when (toFilePath r == t) $ fail $
            "Self-redirect detected: " ++ show r ++ " points to itself."

    forM_ redirects $ \(ident, to) ->
        create [ident] $ do
            route idRoute
            compile $ makeItem $! Redirect to

-- | This datatype can be used directly if you want a lower-level interface to
-- generate redirects.  For example, if you want to redirect @foo.html@ to
-- @bar.jpg@, you can use:
--
-- > create ["foo.html"] $ do
-- >     route idRoute
-- >     compile $ makeItem $ Redirect "bar.jpg"
data Redirect = Redirect
    { redirectTo :: String
    } deriving (Eq, Ord, Show)

instance Binary Redirect where
    put (Redirect to) = put to
    get = Redirect <$> get

instance Writable Redirect where
    write path = write path . fmap redirectToHtml

redirectToHtml :: Redirect -> String
redirectToHtml (Redirect working) =
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"/><meta name=\"generator\" content=\"hakyll\"/>" ++
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">" ++
    "<meta http-equiv=\"refresh\" content=\"0; url=" ++ working ++
    "\"><link rel=\"canonical\" href=\"" ++ working ++
    "\"><title>Permanent Redirect</title></head><body></body></html>"