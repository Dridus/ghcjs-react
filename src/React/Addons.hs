{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module React.Addons where

import Data.Maybe (catMaybes)
import GHCJS.Types (JSString, jsval)
import React.DOM (className_)
import React.Primitive (ToProps(toProps), ToPropsOf, ToChildren, Node, Class, Prop, PropName(PropName), (.:), createFactory, runFactory)

foreign import javascript unsafe "React.addons.CSSTransitionGroup" js_cssTransitionGroup :: Class Prop

cssTransition :: (ToProps props, ToPropsOf props ~ Prop, ToChildren children) => props -> children -> Node
cssTransition = runFactory fact
  where
    fact = createFactory js_cssTransitionGroup
{-# NOINLINE cssTransition #-}

cssTransitionGroup :: ToChildren children => JSString -> TransitionGroup -> children -> Node
cssTransitionGroup cl g = cssTransition props
  where
    props = catMaybes
      [ Just (className_ cl)
      , Just (PropName "transitionName" .: (either jsval (jsval . toProps . map (uncurry (\k v -> PropName k .: v))) $ transitionName g))
      , (PropName "transitionEnterTimeout" .:) <$> transitionEnterTimeout g
      , (PropName "transitionLeaveTimeout" .:) <$> transitionLeaveTimeout g
      , (PropName "transitionEnter" .:) <$> transitionEnter g
      , (PropName "transitionLeave" .:) <$> transitionLeave g
      , (PropName "transitionAppear" .:) <$> transitionAppear g
      ]

data TransitionGroup = TransitionGroup
  { transitionName         :: Either JSString [(JSString, JSString)]
  , transitionEnterTimeout :: Maybe Int
  , transitionLeaveTimeout :: Maybe Int
  , transitionEnter        :: Maybe Bool
  , transitionLeave        :: Maybe Bool
  , transitionAppear       :: Maybe Bool
  }

simpleTransition :: JSString -> TransitionGroup
simpleTransition str = TransitionGroup (Left str) Nothing Nothing Nothing Nothing Nothing

complexTransition :: [(JSString, JSString)] -> TransitionGroup
complexTransition strs = TransitionGroup (Right strs) Nothing Nothing Nothing Nothing Nothing
