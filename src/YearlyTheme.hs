module YearlyTheme where

import           Control.Monad
import           Data.Monoid
import           Data.List

-- import           Hakyll hiding (Context)
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Compiler

import           Hakyll.Web.ExtendedTemplate

data Theme = Ocean | Mountain

themeCtx :: Theme -> Context a
themeCtx th = case th of
                Ocean -> stringField "yearly_theme" (const $ return "ocean")
                Mountain -> stringField "yearly_theme" (const $ return "mountain")
