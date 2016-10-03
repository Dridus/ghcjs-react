{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
module React (
  -- * Component Specs
    Spec(..), defaultDisplayName, spec, statefulSpec, buildSpec
  , SpecMaintenance(..)
  -- * Component Classes and Factories
  , Class(..)
  , makeClass, createClass
  , Factory(..), createFactory, runFactory
  -- * React Node and Elements
  , Node(..), createElement, cloneElement, isValidElement, (//)
  , ToNode(..), ToChildren(..)
  -- * React Component Instances
  , Instance(..), instanceThis, instanceElement
  , This(..)
  -- * Component Props and Properties
  , Properties(..), getProperties, readProperties, getProp
  -- ** Prop Construction
  , PropName(..), suppressContentEditableWarning, dangerouslySetInnerHTML, key, children
  , Prop(..), (.:), noProps, inheritProp, inheritProp'
  , ReadProp(..), ToProps(..)
  -- ** Event Props
  , EventProp, eventProp
  , SyntheticEvent(..)
  -- *** Clipboard Event Props
  , ClipboardEvent(..), onCopy, onCut, onPaste
  -- *** Composition Event Props
  , CompositionEvent(..), onCompositionEnd, onCompositionStart, onCompositionUpdate
  -- *** Keyboard Event Props
  , KeyboardEvent(..), onKeyDown, onKeyPress, onKeyUp
  -- *** Focus Event Props
  , FocusEvent(..), onFocus, onBlur
  -- *** Form Event Props
  , FormEvent(..), onChange, onInput, onSubmit
  -- *** Mouse Event Props
  , MouseEvent(..), onClick, onContextMenu, onDoubleClick, onDrag, onDragEnd, onDragEnter, onDragExit
  , onDragLeave, onDragOver, onDragStart, onDrop, onMouseDown, onMouseEnter, onMouseLeave, onMouseMove
  , onMouseOut, onMouseOver, onMouseUp
  -- *** Selection Event Props
  , SelectionEvent(..), onSelect
  -- *** Touch Event Props
  , TouchEvent(..), onTouchCancel, onTouchEnd, onTouchMove, onTouchStart
  -- *** Scroll Event Prop
  , UIEvent(..), onScroll
  -- *** Wheel Event Prop
  , WheelEvent(..), onWheel
  -- *** Media Event Props
  , MediaEvent(..), onAbort, onCanPlay, onCanPlayThrough, onDurationChange, onEmptied, onEncrypted, onEnded
  , onError, onLoadedData, onLoadedMetadata, onLoadStart, onPause, onPlay, onPlaying, onProgress
  , onRateChange, onSeeked, onSeeking, onStalled, onSuspend, onTimeUpdate, onVolumeChange, onWaiting
  -- *** Image Loaded Event Prop
  , ImageEvent(..), onLoad
  -- *** Animation Event Props
  , AnimationEvent(..), onAnimationStart, onAnimationEnd, onAnimationIteration
  -- *** Transition End Event Prop
  , TransitionEvent(..), onTransitionEnd
  -- * Component State
  , getState, readState, replaceState, modifyState
  -- * ReactM and RendererM
  , ReactM(..), runReactM, getChildren
  , RendererM(..)
  -- * Event handlers and callbacks
  , EventHandler, eventHandler, jsEventHandler, callback
  -- * Backing Node @refs@
  , ref, unsafeGetRef
  -- * Miscellaneous
  , SanitizedHtml(..)
  , OnlyAttributes(..)
  , Array(..), array
  , maybeSetProp
  , printWhatever
  , render
  , unmountComponentAtNode
  , findDOMNode
  ) where

import Control.Monad.Reader
import Control.Monad.State (runStateT)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Traversable
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types
import qualified JavaScript.Object as Object
import qualified JavaScript.Object.Internal as OI
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as THSyntax
import React.Primitive hiding (Props, Spec, State, getProps, getState, replaceState)
import qualified React.Primitive as Prim
import System.IO.Unsafe
import Unsafe.Coerce

-- |Get the props of this React component in some Haskell type @ps@ by using its 'FromJSVal' instance to
-- recover it from the props.
--
-- See the 'Properties' type for details on how the type is decoded from the raw JS props.
getProperties :: FromJSVal ps => ReactM ps st (Properties ps)
getProperties = Prim.getProps >>= liftIO . readProperties . coerce

-- |Get the state of the React component by coercing the JS state to the state of type @st@ using its
-- 'FromJSVal' instance.
--
-- The state value will be decoded directly from the state of the React component, which must always be a JS
-- object.
getState :: FromJSVal st => ReactM ps st st
getState = Prim.getState >>= liftIO . readState . coerce

-- |Set the state of the React component by coercing the state of type @st@ to JS using its 'ToJSVal' instance.
--
-- The state will be coerced without wrapping, so it must be a JS object as required by the React specification.
replaceState :: ToJSVal st => st -> ReactM ps st ()
replaceState = Prim.replaceState . Prim.State . coerce <=< liftIO . toJSVal

-- |Update a typed state value using an update function.
modifyState :: (ToJSVal st, FromJSVal st) => (st -> st) -> ReactM ps st ()
modifyState f = fmap f getState >>= replaceState

-- Component Specs and Lifecycle
{-
newtype ReactM propPerms statePerms refPerms a = ReactM (ReaderT ReactThis IO a)
-}

-- |Empty props or state type which converts to 'JSVal' as an empty object.
data OnlyAttributes = OnlyAttributes
  deriving (Show, Eq)

instance ToJSVal OnlyAttributes where
  toJSVal _ = unsafeCoerce <$> Object.create

instance FromJSVal OnlyAttributes where
  fromJSVal _ = return $ Just OnlyAttributes
  fromJSValUnchecked _ = return OnlyAttributes

-- |A React component specification for a component with props type @ps@ and state type @st@, used as input to
-- create a 'ReactClass' using either the 'makeClass' splice for static component classes which never get
-- released and the 'createClass' function for dynamic component classes where the callbacks are explicitly
-- released.
data Spec ps st = Spec
  { renderSpec                :: RendererM (ReactM ps st Node)
  -- ^Action to bind callbacks and then yield the action to perform on each update to render the component.
  , getInitialState           :: Maybe (ReactM ps st st)
  -- ^Action to determine the initial state of the component, if any. By default an empty object is used for
  -- the state.
  , getDefaultProps           :: Maybe (IO (Properties ps))
  -- ^Action run when the class is created to make the default values for props of the component. For each
  -- field of the default props that is not specified when creating an element using the class, the default will
  -- be used.
  -- , propTypes                 :: Maybe PropTypechecker
  -- , mixins                    :: Maybe (Array Mixin)
  , statics                   :: Maybe Object.Object
  -- ^Static functions attached to the class
  , displayName               :: Maybe JSString
  -- ^Name of the component class shown in diagnostic logging and the React devtools. For classes made using
  -- 'makeClass', the @displayName@ will be defaulted to the Haskell symbol being bound.
  , componentWillMount        :: Maybe (ReactM ps st ())
  -- ^ Invoked once, both on the client and server, immediately before the initial rendering occurs. If you
  -- call 'setState' within this method, 'render' will see the updated state and will be executed only once
  -- despite the state change.
  , componentDidMount         :: Maybe (ReactM ps st ())
  -- ^ Invoked once, only on the client (not on the server), immediately after the initial rendering occurs. At
  -- this point in the lifecycle, you can access any refs to your children (e.g., to access the underlying DOM
  -- representation). The @componentDidMount()@ method of child components is invoked before that of parent
  -- components.
  --
  -- If you want to integrate with other JavaScript frameworks, set timers using @setTimeout@ or @setInterval@,
  -- or send AJAX requests, perform those operations in this method.
  , componentWillReceiveProps :: Maybe (Properties ps -> ReactM ps st ())
  -- ^ Invoked when a component is receiving new props. This method is not called for the initial render.
  -- Use this as an opportunity to react to a prop transition before render() is called by updating the state
  -- using this.setState(). The old props can be accessed via this.props. Calling this.setState() within this
  -- function will not trigger an additional render.

  , shouldComponentUpdate     :: Maybe (Properties ps -> st -> ReactM ps st Bool)
  -- ^Determines whether a rerender is required for some new properties or state. Defaults to 'True' so all
  -- changes to props or state will cause a rerender.
  , componentWillUpdate       :: Maybe (Properties ps -> st -> ReactM ps st ())
  -- ^Called immediately prior to rendering in response to a new props or state. The state should not be changed
  -- by this callback.
  , componentDidUpdate        :: Maybe (Properties ps -> st -> ReactM ps st ())
  -- ^Called immediately after update render's DOM updates are committed
  , componentWillUnmount      :: Maybe (ReactM ps st ())
  -- ^Called when the component is about to be removed from the DOM to allow cleanup specific for the component.
  }

-- |Set the 'displayName' of the spec with the given name if that spec has no @displayName@ already.
defaultDisplayName :: JSString -> Spec ps st -> Spec ps st
defaultDisplayName n s@(Spec { displayName }) =
  maybe (s { displayName = Just n }) (const s) displayName

-- |Create a spec with no callbacks bound but for the mandatory 'render' callback and has no state.
spec :: RendererM (ReactM ps OnlyAttributes Node) -> Spec ps OnlyAttributes
spec f = Spec f Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- |Create a spec with some action to produce the initial state of components created using it along with the
-- mandatory 'render' callback.
statefulSpec :: ReactM ps st st -> RendererM (ReactM ps st Node) -> Spec ps st
statefulSpec st f = Spec f (Just st) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- |An action to clean up the callbacks bound during creation of a 'ReactSpec' from a 'Spec' by 'buildSpec'.
data SpecMaintenance = SpecMaintenance { specMaintenanceFinalize :: IO () }

-- |Typed wrapper around 'ReactProps' which can be used to easily manage the Haskell type representing a component's
-- properties.
--
-- Many callbacks provide an instance of this type, and you can get, and decode props with this type using
-- 'getProperties' and 'readProperties' respectively.
data Properties ps = Properties
  { propVal  :: ps
  , allProps :: Prim.Props ps
  }

instance PFromJSVal ps => PFromJSVal (Properties ps) where
  pFromJSVal jv = Properties (pFromJSVal jv) (pFromJSVal jv)
instance PToJSVal ps => PToJSVal (Properties ps) where
  pToJSVal = pToJSVal . toProps
instance FromJSVal ps => FromJSVal (Properties ps) where
  fromJSVal jv = (fmap . fmap) (\ ps -> Properties ps $ coerce jv) (fromJSVal jv)
instance ToJSVal ps => ToJSVal (Properties ps) where
  toJSVal (Properties {..}) = do
    jsPropVal <- toJSVal propVal
    pure . coerce $ allProps `mappend` coerce jsPropVal

instance PToJSVal ps => ToProps (Properties ps) where
  type ToPropsOf (Properties ps) = ps
  toProps (Properties {..}) = coerce $ allProps `mappend` coerce (pToJSVal propVal)

-- |Decode a 'JSVal' representing props for a component into a 'Properties' using the 'FromJSVal' instance for @ps@
--
-- Differs from 'fromJSVal' in that it applies the 'Properties' wrapper which includes the raw 'ReactProps', and also
-- logs a diagnostic message on parse failure.
readProperties :: FromJSVal ps => JSVal -> IO (Properties ps)
readProperties v = do
  ps <- fromJSVal v
  case ps of
    Nothing -> printWhatever v >> Prelude.error "Failed to parse props"
    Just r -> return (Properties r $ coerce v)

-- |Decode a 'JSVal' representing state for a component into an @st@ using the 'FromJSVal' instance.
--
-- Differs from 'fromJSVal' only in that this function logs a diagnostic error when parsing fails.
readState :: FromJSVal st => JSVal -> IO st
readState v = do
  st <- fromJSVal v
  case st of
    Nothing -> printWhatever v >> Prelude.error "Failed to parse state"
    Just r -> return r

-- |Given a 'Spec' using Haskell typed callbacks build a 'ReactSpec' for use with 'createClass' by binding all
-- callbacks and making the object expected by React.
--
-- Yields the 'ReactSpec' along with a 'SpecMaintenance' action to release the bound callbacks.
--
-- __Note:__ This function is only used when building component classes dynamically and you have the obligation
-- of releasing the callbacks to prevent leaks. For the majority of uses, instead make static named components
-- with the 'makeClass' TH splice.
buildSpec :: (ToJSVal ps, FromJSVal ps, ToJSVal st, FromJSVal st) => Spec ps st -> IO (Prim.Spec ps st, SpecMaintenance)
buildSpec s = do
  (renderer, cleanup) <- runStateT (fromRendererM $ renderSpec s) (return ())
  o <- Object.create
  renderCb <- syncCallback1' $ \this -> do
    fromNode <$> runReaderT (fromReactM renderer) (This this)

  Object.setProp "render" (captureThis renderCb) o

  mGetInitialStateCb <- for (getInitialState s) $ \f -> do
    getInitialStateCb <- syncCallback1' ((runReaderT (fromReactM f) . This) >=> toJSVal)
    Object.setProp "getInitialState" (captureThis getInitialStateCb) o
    return getInitialStateCb

  mGetDefaultPropsCb <- for (getDefaultProps s) $ \m -> do
    getDefaultPropsCb <- syncCallback' (toJSVal =<< m)
    Object.setProp "getDefaultProps" (jsval getDefaultPropsCb) o
    return getDefaultPropsCb

  maybeSetProp "statics" (unsafeCoerce <$> statics s) o
  maybeSetProp "displayName" (jsval <$> displayName s) o

  -- TODO refcount increment
  mWillMountCb <- for (componentWillMount s) $ \f -> do
    willMountCb <- syncCallback1 ThrowWouldBlock (runReaderT (fromReactM f) . This)
    Object.setProp "componentWillMount" (captureThis willMountCb) o
    return willMountCb

  mDidMountCb <- for (componentDidMount s) $ \f -> do
    didMountCb <- syncCallback1 ThrowWouldBlock (runReaderT (fromReactM f) . This)
    Object.setProp "componentDidMount" (captureThis didMountCb) o
    return didMountCb

  mWillReceiveProps <- for (componentWillReceiveProps s) $ \f -> do
    willReceivePropsCb <- syncCallback2 ThrowWouldBlock $ \t ps -> do
      ps' <- readProperties ps
      runReaderT (fromReactM $ f ps') $ This t
    Object.setProp "componentWillReceiveProps" (captureThis willReceivePropsCb) o
    return willReceivePropsCb

  mShouldComponentUpdate <- for (shouldComponentUpdate s) $ \f -> do
    cb <- syncCallback3' $ \t ps st -> do
      st' <- readState st
      ps' <- readProperties ps
      res <- runReaderT (fromReactM (f ps' st')) $ This t
      return $ pToJSVal res

    Object.setProp "shouldComponentUpdate" (captureThis cb) o
    return cb

  mWillUpdate <- for (componentWillUpdate s) $ \f -> do
    cb <- syncCallback3 ThrowWouldBlock $ \t ps st -> do
      st' <- readState st
      ps' <- readProperties ps
      runReaderT (fromReactM $ f ps' st') $ This t
    Object.setProp "componentWillUpdate" (captureThis cb) o
    return cb

  mDidUpdate <- for (componentDidUpdate s) $ \f -> do
    cb <- syncCallback3 ThrowWouldBlock $ \t ps st -> do
      st' <- readState st
      ps' <- readProperties ps
      runReaderT (fromReactM $ f ps' st') $ This t
    Object.setProp "componentDidUpdate" (captureThis cb) o
    return cb

  -- TODO refcount decrement
  mWillUnmount <- for (componentWillUnmount s) $ \f -> do
    cb <- syncCallback1 ThrowWouldBlock (runReaderT (fromReactM f) . This)
    Object.setProp "componentWillUnmount" (captureThis cb) o
    return cb

  let finalizer = do
        -- TODO return failure instead of freeing if refcount /= 0
        releaseCallback renderCb
        traverse_ releaseCallback mGetInitialStateCb
        traverse_ releaseCallback mGetDefaultPropsCb
        traverse_ releaseCallback mWillMountCb
        traverse_ releaseCallback mDidMountCb
        traverse_ releaseCallback mWillReceiveProps
        traverse_ releaseCallback mShouldComponentUpdate
        traverse_ releaseCallback mWillUpdate
        traverse_ releaseCallback mDidUpdate
        traverse_ releaseCallback mWillUnmount
        cleanup

  return (Prim.Spec o, SpecMaintenance finalizer)


-- FIXME document what this is for!
-- Type constrained in order to avoid ambiguous types in DOM structures
(//) :: (Maybe (Array Node) -> Node) -> [Node] -> Node
(//) lhs rhs = lhs $ Just $ array rhs

-- |TH splice to set up a static named component class.
--
-- This is the regular kind of class generation, the dynamic alternative being to use 'createClass' with
-- 'buildSpec', the latter requiring you to take care of releasing the generated callbacks when you're done
-- in order to avoid leaking them.
--
-- This splice defines @myComponent@ and arranges for it to not be inlined by the compiler so that callbacks
-- aren't leaked since there can only be as many as defined by the application.
--
-- Example:
--
-- > {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- >
-- > myComponentSpec :: Spec MyProps OnlyAttributes
-- > myComponentSpec = spec $ do
-- >   click <- eventHandler $ \ clickEvent -> …
-- >   pure $ do
-- >     pure [button_ [] ["Click me!"]]
-- >
-- > makeClass "myComponent" [| myComponentSpec |]
-- > myComponent :: ReactClas MyProps OnlyAttributes
--
-- Where the use of @makeClass@ is equivalent to:
--
-- > myComponent =
-- >   createClass . fst . unsafePerformIO . buildSpec . defaultDisplayName "myComponent" $
-- >     myComponentSpec
-- > {-# NOINLINE myComponent #-}
makeClass :: String -> ExpQ -> DecsQ
makeClass nameStr specExp = do
  ds <- funD nameName [ clause [] (normalB expr) [] ]
  return [ds, noInlineFun]
  where
    nameName = mkName nameStr
    noInlineFun = PragmaD $ InlineP nameName NoInline FunLike AllPhases
    expr = [e| createClass . fst . unsafePerformIO . buildSpec . defaultDisplayName $(THSyntax.lift nameStr) $ $specExp |]

instance ReadProp (Properties a) where
  readProp name = readProp name . allProps
  unsafeReadProp name = unsafeReadProp name . allProps

