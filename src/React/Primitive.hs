{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE NamedFieldPuns             #-}
module React.Primitive (
  -- * Component Specs
    Spec(..)
  -- * Component Classes and Factories
  , Class(..)
  , createClass
  , Factory(..), createFactory, runFactory
  -- * React Node and Elements
  , Node(..), createElement, cloneElement, isValidElement
  , ToNode(..)
  , ToChildren(..), asChildrenJSVal
  -- * React Component Instances
  , Instance(..), instanceThis, instanceElement
  , This(..), captureThis
  -- * Component Props and Properties
  , Props(..), getProps
  , GetProp(..)
  , ToProps(..)
  -- ** Prop Construction
  , PropName(..), suppressContentEditableWarning, dangerouslySetInnerHTML, key
  , Prop(..), (.:), buildProps, props, unsafeProps, noProps, inheritProp, inheritProp'
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
  , State(..), getState, setState, replaceState, setState'
  -- * ReactM and RendererM
  , ReactM(..), runReactM
  , RendererM(..)
  -- * Event handlers and callbacks
  , EventHandler, eventHandler, jsEventHandler, callback
  -- * Backing Node @refs@
  , ref, unsafeGetRef
  -- * Miscellaneous
  , SanitizedHtml(..)
  , Array(..), array
  , maybeSetProp
  , printWhatever
  , render
  , unmountComponentAtNode
  , findDOMNode
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State (StateT, modify)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)
import Data.JSString.Text
import Data.IORef
import qualified Data.JSString as JSString
import Data.String (IsString(..))
import qualified Data.Text      as S
import qualified Data.Text.Lazy as L
import Data.Typeable
import qualified GHCJS.DOM.AnimationEvent   as Animation
import qualified GHCJS.DOM.KeyboardEvent    as Keyboard
import qualified GHCJS.DOM.FocusEvent       as Focus
import qualified GHCJS.DOM.MouseEvent       as Mouse
import qualified GHCJS.DOM.TouchEvent       as Touch
import qualified GHCJS.DOM.TransitionEvent  as Transition
import GHCJS.DOM.Element
import GHCJS.DOM.EventTarget (EventTarget)
import GHCJS.DOM.Types (GObject(..), IsGObject(..), IsEventTarget)
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Export
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Nullable (Nullable(..), nullableToMaybe)
import GHCJS.Types
import qualified JavaScript.Object as Object
import qualified JavaScript.Object.Internal as OI
import JavaScript.Array
import Prelude hiding (exp)
import System.IO.Unsafe
import Unsafe.Coerce

-- |A React class with the given type of props.
--
-- Created either by using the 'makeClass' TH splice for regular classes, or using 'createClass' to create
-- dynamic classes.
newtype Class ps = Class { fromClass :: JSVal }
  deriving (ToJSVal, PToJSVal)

instance IsJSVal (Class ps)

-- |Some value which qualifies as a React node.
--
-- Can be a:
--
--    * String
--    * React element, as created by 'createElement' or 'runFactory' or one of the 'React.DOM' factories
--    * Array of same
newtype Node = Node { fromNode :: JSVal }
  deriving (ToJSVal, PToJSVal, FromJSVal)

instance IsJSVal Node

instance IsString Node where
  fromString = Node . pToJSVal . JSString.pack

-- TODO these should probably be conditonally included only for GHCJS
instance IsGObject Node where
  toGObject (Node val) = GObject val
  unsafeCastGObject (GObject val) = Node val

instance IsEventTarget Node


-- |An instance of a React component with the given props and state type.
newtype Instance ps st = Instance { fromInstance :: JSVal }

-- |Given an instance of a React component, get its 'this'
instanceThis :: Instance ps st -> This ps st
instanceThis = This . fromInstance

instanceElement :: Instance ps st -> Element
instanceElement = unsafeCastGObject . GObject . fromInstance


-- |A property which accepts values of type @v@
newtype PropName v = PropName { fromPropName :: JSString }

-- |Type of actions which set up callbacks and produce a rendering callback action of type 'ReactM'.
--
-- The various callback binding functions operate in this type and produce the rendering lifecycle callback
-- while assembling a separate IO action to release the bound callbacks.
--
-- See the 'makeClass' splice and 'createClass' functions for the two usual ways classes are created and
-- callback allocations handled.
newtype RendererM a = RendererM { fromRendererM :: StateT (IO ()) IO a }
  deriving (Functor, Applicative, Monad)

-- |A callback function for handling events. Can wrap either a Haskell callback or a raw JS one.
--
-- @ps@ is the type of component properties provided to the event handler.
-- @st@ is the type of component state provided to the event handler.
-- @a@ is the type of event expected (and 'unsafeCoerce'd!) as an argument to the handler.
data EventHandler ps st a = HsEventHandler (Callback (JSVal -> JSVal -> IO ())) JSVal
                          | JSEventHandler JSVal

instance PToJSVal (EventHandler ps st a) where
  pToJSVal (HsEventHandler _ v) = v
  pToJSVal (JSEventHandler v) = v

-- |Bind a Haskell event callback function to be run in the context of the component when an event of type @e@
-- occurs.
--
-- Because Haskell callbacks need to be explicitly released, binding such callbacks is done in the 'RendererM'
-- monad. See that type and 'makeClass' for more.
--
-- __Note:__ Performing a blocking action in the event handler prior to calling 'preventDefault' will allow the
-- event to occur.
eventHandler :: (e -> ReactM ps st ()) -> RendererM (EventHandler ps st e)
eventHandler f = RendererM $ do
  cb <- liftIO $ syncCallback2 ContinueAsync (\this e -> runReaderT (fromReactM $ f $ unsafeCoerce e) $ This this)
  let cleaner = (>> releaseCallback cb)
  modify cleaner
  return $ HsEventHandler cb (jsval cb)

-- |Wrap up a raw JS event handler as an 'EventHandler'.
--
-- Because raw JS callbacks don't need explicit releasing, wrapping up a JS callback is a pure function.
jsEventHandler :: JSVal -> EventHandler ps st a
jsEventHandler = JSEventHandler

callback :: (Typeable a, Typeable b, Show a, Show b) => (a -> b) -> RendererM (Callback (Export a -> IO (Export b)))
callback f = RendererM $ do
  cb <- liftIO $ syncCallback1' $ \exp -> do
    v <- derefExport (unsafeCoerce exp :: Export a)
    case v of
      Nothing -> Prelude.error "Mismatched type or freed export value"
      Just x -> do
        print x
        let res = f x
        print res
        jsval <$> export res
  let cleaner = (>> releaseCallback cb)
  modify cleaner
  return $ unsafeCoerce cb

-- |A props object with a phantom type to distinguish props for different components.
newtype Props ps = Props { fromProps :: Object.Object }
  deriving (IsJSVal)

instance PToJSVal (Props ps) where
  pToJSVal = coerce
instance PFromJSVal (Props ps) where
  pFromJSVal = coerce
instance ToJSVal (Props ps) where
  toJSVal = pure . coerce
instance FromJSVal (Props ps) where
  fromJSVal = pure . Just . coerce

foreign import javascript "Object.assign.apply(null, $1)" assign :: JSArray -> IO Object.Object

instance Monoid (Props ps) where
  mempty = Props $ unsafePerformIO Object.create
  mappend (Props o1) (Props o2) = unsafePerformIO $ do
    o <- Object.create
    oo <- fromListIO [jsval o, jsval o1, jsval o2]
    res <- assign oo
    return $ Props res

-- |Wrapper around some (hopefully) sanitized HTML to be used as @innerHTML@.
--
-- When converted to a 'JSVal' using 'pToJSVal' or 'toJSVal', an object in the special form React prescribes
-- for sanitized inner HTML will be generated: <https://facebook.github.io/react/tips/dangerously-set-inner-html.html>
newtype SanitizedHtml = SanitizedHtml { fromSanitizedHtml :: JSString }
  deriving (Eq, Show)

instance PToJSVal SanitizedHtml where
  pToJSVal (SanitizedHtml h) = unsafePerformIO $ do
    o <- Object.create
    Object.setProp "__html" (jsval h) o
    return $ jsval o

instance FromJSVal SanitizedHtml where
  fromJSVal r = case jsonTypeOf r of
    JSONObject -> do
      let o = OI.Object r
      str <- Object.getProp "__html" o
      fmap SanitizedHtml <$> fromJSVal str
    _ -> return Nothing
  {-# INLINE fromJSVal #-}

instance ToJSVal SanitizedHtml where
  toJSVal = return . pToJSVal
  {-# INLINE toJSVal #-}

-- |Phantom typed wrapper around 'JSArray' used to represent @children@ arrays.
newtype Array a = Array { fromArray :: JSArray }
  deriving (IsJSVal)

instance PToJSVal (Array a) where
  pToJSVal = jsval

-- |Make an 'Array' of @a@s which can be converted ot 'JSVal's
array :: PToJSVal a => [a] -> Array a
array = Array . fromList . map pToJSVal

foreign import javascript unsafe "captureThis($1)" captureThis :: Callback a -> JSVal
foreign import javascript unsafe "captureThis($1, $2)" captureThis' :: JSVal -> This ps st -> JSVal

foreign import javascript unsafe "console.log($1)" js_printWhatever :: JSVal -> IO ()

-- |@console.log@ any value whatsoever.
printWhatever :: a -> IO ()
printWhatever = js_printWhatever . unsafeCoerce

-- |Make a 'Class' from a 'Spec' by calling React's @React.createClass@
foreign import javascript unsafe "React.createClass($1)" createClass :: Spec ps st -> Class ps

foreign import javascript unsafe "React.createElement.apply(this, [$1, $2].concat($3))" js_createElement :: Class ps -> Props ps -> JSVal -> Node

-- |Helper function which converts a value of a 'ToChildren' classed type down to a 'JSVal' to pass to React
asChildrenJSVal :: ToChildren children => children -> JSVal
asChildrenJSVal = maybe jsNull (jsval . fromArray) . toChildren

-- |Make a React component wrapped as a 'Node' by instantiating the 'Class' with the props and children given.
createElement :: (ToProps props, ToPropsOf props ~ ps, ToChildren children) => Class ps -> props -> children -> Node
createElement c ps = js_createElement c (toProps ps) . asChildrenJSVal

-- |A React element factory, kind of a partially applied 'Class'.
--
-- @ps@ is the type of the component's properties.
newtype Factory ps = Factory { fromFactory :: JSVal }

instance IsJSVal (Factory ps)

foreign import javascript "$1.apply(this, [$2].concat($3))" js_runFactory :: Factory ps -> Props ps -> JSVal -> Node

-- |Make a 'Factory' for a 'Class' using React's @React.createFactory@
--
-- A 'Factory' is essentially a partially applied @React.createElement@ though may reduce number of rerenders
-- induced by unique closures being created by GHCJS. TODO: Need to explain the details of the rerender issue.
foreign import javascript unsafe "React.createFactory($1)" createFactory :: Class ps -> Factory ps

-- |Run a 'Factory' to create a React component in the form of a 'Node', giving the props and children
-- that the component should use in raw JS-style form.
runFactory :: (ToProps props, ToPropsOf props ~ ps, ToChildren children) => Factory ps -> props -> children -> Node
runFactory f ps = js_runFactory f (toProps ps) . asChildrenJSVal

foreign import javascript unsafe "React.cloneElement($1, $2, $3)" js_cloneElement :: Node -> Props ps -> JSVal -> Node

-- |Clone an existing React element with some new props merged in, by using @React.cloneElement@.
--
-- **Warning:** The underlying React function @React.cloneElement@, which this wraps, merges the props of the
-- existing element with the given 'Props', which is why it's a literal 'Props' instead of @ToProps props@.
cloneElement :: ToChildren children => Node -> Props ps -> children -> Node
cloneElement n ps = js_cloneElement n ps . asChildrenJSVal

-- |Check if some 'JSVal' refers to a valid React element using @React.isValidElement@
foreign import javascript unsafe "React.isValidElement($1)" isValidElement :: JSVal -> IO Bool

{-
-- React.PropTypes
-- React.Children
-}
-- ReactDOM

foreign import javascript unsafe "ReactDOM.render($1, $2, $3)" js_render :: Node -> Element -> JSVal -> IO ()

-- |Render a React node by mounting it in the DOM under the specified element and possibly attaching some
-- cleanup callback to be executed when the component is rendered or updated.
render :: IsElement e => Node -> e -> Maybe (This ps st -> IO ()) -> IO ()
render re e mf = case mf of
  Nothing -> js_render re (toElement e) jsUndefined
  Just f -> do
    selfCleaner <- newIORef undefined
    cb <- syncCallback1 ContinueAsync $ \r -> do
      f $ This r
      cb <- readIORef selfCleaner
      releaseCallback cb
    writeIORef selfCleaner cb
    js_render re (toElement e) (captureThis cb)

foreign import javascript unsafe "ReactDOM.unmountComponentAtNode($1)" js_unmountComponentAtNode :: Element -> IO Bool

-- |Unmount and remove a React node mounted at some DOM element, yielding 'True' iff there was a React node
-- mounted.
unmountComponentAtNode :: IsElement e => e -> IO Bool
unmountComponentAtNode = js_unmountComponentAtNode . toElement

foreign import javascript unsafe "ReactDOM.findDOMNode($1)" js_findDOMNode :: Instance ps st -> IO (Nullable Element)

-- |Return the DOM element associated with an instance of a React component using @ReactDOM.findDOMNode@.
--
-- Intended only for use in specialty cases where the component abstraction is not enough and direct access to
-- the created DOM is required.
findDOMNode :: MonadIO m => Instance ps st -> m (Maybe Element)
findDOMNode c = liftIO (nullableToMaybe <$> js_findDOMNode c)

{-
-- React.Component
-}

foreign import javascript unsafe "$1['props']" js_getProps :: This ps st -> IO (Props ps)

-- |Get the props of this React component as the raw JS object.
getProps :: ReactM ps st (Props ps)
getProps = do
  v <- ask
  liftIO $ js_getProps v

foreign import javascript unsafe "$1['state']" js_getState :: This ps st -> IO (State st)

-- |A state object with a phantom type to distinguish state values for different components.
newtype State st = State { fromState :: Object.Object }
  deriving (IsJSVal)

instance ToJSVal (State st) where
  toJSVal (State o) = return $ coerce o
instance FromJSVal (State st) where
  fromJSVal = return . Just . coerce

-- |Get the state of the React component.
getState :: FromJSVal st => ReactM ps st (State st)
getState = liftIO . js_getState =<< ask

foreign import javascript unsafe "$1['setState']($2)" js_setState :: This ps st -> JSVal -> IO ()

-- |Set fields of the state for the component using some object value.
--
-- __Warning:__ React's @setState@ which this wraps will perform a shallow merge of the new object with the
-- existing object.
setState :: State st -> ReactM ps st ()
setState x = do
  v <- ask
  liftIO $ js_setState v (jsval x)

foreign import javascript unsafe "$1['replaceState']($2)" js_replaceState :: This ps st -> JSVal -> IO ()

-- |Set the state of the React component by coercing the state of type @st@ to JS using its 'ToJSVal' instance.
--
-- The state will be coerced without wrapping, so it must be a JS object as required by the React specification.
replaceState :: State st -> ReactM ps st ()
replaceState x = do
  ths <- ask
  liftIO $ js_replaceState ths (coerce x)

-- |Set the state of the React component by computing a new state using the current state and props.
--
-- __Warning:__ React's @setState@ which this wraps will perform a shallow merge of the new object with the
-- existing object.
setState' :: (State st -> Props ps -> State st) -> ReactM ps st ()
setState' f = do
  v <- ask
  liftIO $ do
    cb <- syncCallback2' $ \st ps -> do
      toJSVal $ f (State $ coerce st) (Props $ coerce ps)
    js_setState v (jsval cb)
    releaseCallback cb

-- forceUpdate :: f -> IO ()

-- Component Specs and Lifecycle
{-
newtype ReactM propPerms statePerms refPerms a = ReactM (ReaderT ReactThis IO a)
-}

-- |Type of actions during component rendering and other lifecycle callbacks which can read the current
-- component instance.
newtype ReactM ps st a = ReactM { fromReactM :: ReaderT (This ps st) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- |Run a 'ReactM' for some component instance.
runReactM :: MonadIO m => This ps st -> ReactM ps st a -> m a
runReactM t m = liftIO $ runReaderT (fromReactM m) t

deriving instance MonadBase IO (ReactM ps st)
instance MonadBaseControl IO (ReactM ps st) where
  type StM (ReactM ps st) a = a
  liftBaseWith f = ReactM $ liftBaseWith $ \q -> f (q . fromReactM)
  restoreM = ReactM . restoreM

instance MonadReader (This ps st) (ReactM ps st) where
  ask = ReactM ask
  local f = ReactM . local f . fromReactM

-- |A specification of a 'Class' which has been prepared usually with 'React.buildSpec' for use with
-- 'createClass'.
newtype Spec ps st = Spec { fromSpec :: Object.Object } -- [object Object], haha

instance IsJSVal (Spec ps st)

-- |Set a property of a JS object if 'Just', do nothing otherwise.
maybeSetProp :: JSString -> Maybe JSVal -> Object.Object -> IO ()
maybeSetProp propName mval o = case mval of
  Nothing -> return ()
  Just val -> Object.setProp propName val o
{-# INLINE maybeSetProp #-}

-- |Prop for disabling the React warning about using @contentEditable@ and @children@, when you know what you're
-- doing.
--
-- See https://facebook.github.io/react/docs/tags-and-attributes.html#html-attributes
suppressContentEditableWarning :: PropName Bool
suppressContentEditableWarning = PropName "suppressContentEditableWarning"

-- |Prop for dangerously setting the inner HTML of a component.
--
-- See https://facebook.github.io/react/tips/dangerously-set-inner-html.html
dangerouslySetInnerHTML :: PropName SanitizedHtml
dangerouslySetInnerHTML = PropName "dangerouslySetInnerHTML"

-- |Prop for setting the React component key in dynamic lists of children.
--
-- See https://facebook.github.io/react/docs/reconciliation.html#keys
key :: PropName JSString
key = PropName "key"

-- TODO support callback version of ref?

-- |Prop for setting a name to bind the backing DOM element for a component to @this.refs@.
--
-- See https://facebook.github.io/react/docs/more-about-refs.html#the-ref-string-attribute
ref :: PropName JSString
ref = PropName "ref"

-- |A single property assignment carrying the prop name and value, used for constructing a 'ReactProps' object
data Prop = Prop {-# UNPACK #-} !JSString {-# UNPACK #-} !JSVal

-- |Create a 'Prop' assignment for use with 'runFactory', 'createElement', and the various flavors of 'ReactProps'
-- building functions.
(.:) :: (PToJSVal a) => PropName a -> a -> Prop
(PropName k) .: v = Prop k (pToJSVal v)

-- |Build a 'Props' containing some ad-hoc 'Prop's.
--
-- For example:
--
-- >  buildProps [key .: "waffle", class .: "delicious"]
--
-- Generates:
--
-- >  {"key": "waffle", "class": "delicious"}
buildProps :: Foldable f => f Prop -> Props ps
buildProps ps = unsafePerformIO $ do
  o <- Object.create
  traverse_ (\(Prop k v) -> Object.setProp k v o) ps
  return $ Props o

-- |Build a 'Props' from some Haskell type @st@ along with some additional properties.
--
-- Given:
--
-- >  data MyProps = MyProps { waffles :: Int, syrup :: Int } deriving (Generic)
-- >  instance PToJSVal MyProps
--
-- An example:
--
-- >  props (MyProps 1 100) [key .: "tragedy"]
--
-- Generates:
--
-- >  { "waffles": 1, "syrup": "100", "key": "tragedy" }
props :: (PToJSVal ps, Foldable t) => ps -> t Prop -> Props ps
props pv ps = unsafePerformIO $ do
  eo <- Object.create
  o <- Object.create
  traverse_ (\(Prop k v) -> Object.setProp k v o) ps
  l <- toJSValListOf [unsafeCoerce eo, unsafeCoerce o, pToJSVal pv]
  merged <- assign $ unsafeCoerce l
  return $ unsafeCoerce merged

-- |Like 'props' which builds a 'Props' from some Haskell props @ps@ along with some additional properties, but
-- uses 'toJSVal' inside 'unsafePerformIO' which can be dangerous. By contrast 'props' uses 'pToJSVal'.
unsafeProps :: (ToJSVal ps, Foldable t) => ps -> t Prop -> Props ps
unsafeProps ps rest = unsafePerformIO $ do
  eo <- Object.create
  o <- Object.create
  traverse_ (\(Prop k v) -> Object.setProp k v o) rest
  pjv <- toJSVal ps
  l <- toJSValListOf [unsafeCoerce eo, unsafeCoerce o, pjv]
  merged <- assign $ unsafeCoerce l
  return $ unsafeCoerce merged

-- |Type of props which bind an event handler instead of some regular scalar value.
--
-- For example:
--
-- >  mySpec = spec $ do
-- >    sayHi <- eventHandler . const $ print "hello!"
-- >    pure $ do
-- >      this <- ask
-- >      button_ [onClick this sayHi] ["Click me"]
type EventProp ps st e = This ps st -> EventHandler ps st e -> Prop

-- |Bind an 'EventHandler' to a prop.
eventProp :: JSString -> This ps st -> EventHandler ps st e -> Prop
eventProp n t h = PropName n .: case h of
  HsEventHandler _ f -> captureThis' f t
  JSEventHandler f -> captureThis' f t

onCopy, onCut, onPaste :: EventProp ps st ClipboardEvent
onCopy = eventProp "onCopy"
onCut = eventProp "onCut"
onPaste = eventProp "onPaste"

onCompositionEnd, onCompositionStart, onCompositionUpdate :: EventProp ps st CompositionEvent
onCompositionEnd = eventProp "onCompositionEnd"
onCompositionStart = eventProp "onCompositionStart"
onCompositionUpdate = eventProp "onCompositionUpdate"

onKeyDown, onKeyPress, onKeyUp :: EventProp ps st KeyboardEvent
onKeyDown = eventProp "onKeyDown"
onKeyPress = eventProp "onKeyPress"
onKeyUp = eventProp "onKeyUp"

onFocus, onBlur :: EventProp ps st FocusEvent
onFocus = eventProp "onFocus"
onBlur = eventProp "onBlur"

onChange, onInput, onSubmit :: EventProp ps st FormEvent
onChange = eventProp "onChange"
onInput = eventProp "onInput"
onSubmit = eventProp "onSubmit"

onClick, onContextMenu, onDoubleClick, onDrag, onDragEnd, onDragEnter, onDragExit, onDragLeave, onDragOver, onDragStart, onDrop, onMouseDown, onMouseEnter, onMouseLeave, onMouseMove, onMouseOut, onMouseOver, onMouseUp :: EventProp ps st MouseEvent
onClick = eventProp "onClick"
onContextMenu = eventProp "onContextMenu"
onDoubleClick = eventProp "onDoubleClick"
onDrag = eventProp "onDrag"
onDragEnd = eventProp "onDragEnd"
onDragEnter = eventProp "onDragEnter"
onDragExit = eventProp "onDragExit"
onDragLeave = eventProp "onDragLeave"
onDragOver = eventProp "onDragOver"
onDragStart = eventProp "onDragStart"
onDrop = eventProp "onDrop"
onMouseDown = eventProp "onMouseDown"
onMouseEnter = eventProp "onMouseEnter"
onMouseLeave = eventProp "onMouseLeave"
onMouseMove = eventProp "onMouseMove"
onMouseOut = eventProp "onMouseOut"
onMouseOver = eventProp "onMouseOver"
onMouseUp = eventProp "onMouseUp"

onSelect :: EventProp ps st SelectionEvent
onSelect = eventProp "onSelect"

onTouchCancel, onTouchEnd, onTouchMove, onTouchStart :: EventProp ps st TouchEvent
onTouchCancel = eventProp "onTouchCancel"
onTouchEnd = eventProp "onTouchEnd"
onTouchMove = eventProp "onTouchMove"
onTouchStart = eventProp "onTouchStart"

onScroll :: EventProp ps st UIEvent
onScroll = eventProp "onScroll"

onWheel :: EventProp ps st WheelEvent
onWheel = eventProp "onWheel"

onAbort, onCanPlay, onCanPlayThrough, onDurationChange, onEmptied, onEncrypted, onEnded, onError, onLoadedData, onLoadedMetadata, onLoadStart, onPause, onPlay, onPlaying, onProgress, onRateChange, onSeeked, onSeeking, onStalled, onSuspend, onTimeUpdate, onVolumeChange, onWaiting :: EventProp ps st MediaEvent
onAbort = eventProp "onAbort"
onCanPlay = eventProp "onCanPlay"
onCanPlayThrough = eventProp "onCanPlayThrough"
onDurationChange = eventProp "onDurationChange"
onEmptied = eventProp "onEmptied"
onEncrypted = eventProp "onEncrypted"
onEnded = eventProp "onEnded"
-- TODO different event data when used for image event instead of media event
onError = eventProp "onError"
onLoadedData = eventProp "onLoadedData"
onLoadedMetadata = eventProp "onLoadedMetadata"
onLoadStart = eventProp "onLoadStart"
onPause = eventProp "onPause"
onPlay = eventProp "onPlay"
onPlaying = eventProp "onPlaying"
onProgress = eventProp "onProgress"
onRateChange = eventProp "onRateChange"
onSeeked = eventProp "onSeeked"
onSeeking = eventProp "onSeeking"
onStalled = eventProp "onStalled"
onSuspend = eventProp "onSuspend"
onTimeUpdate = eventProp "onTimeUpdate"
onVolumeChange = eventProp "onVolumeChange"
onWaiting = eventProp "onWaiting"

onLoad :: EventProp ps st ImageEvent
onLoad = eventProp "onLoad"
-- onError

onAnimationStart, onAnimationEnd, onAnimationIteration :: EventProp ps st AnimationEvent
onAnimationStart = eventProp "onAnimationStart"
onAnimationEnd = eventProp "onAnimationEnd"
onAnimationIteration = eventProp "onAnimationIteration"

onTransitionEnd :: EventProp ps st TransitionEvent
onTransitionEnd = eventProp "onTransitionEnd"

foreign import javascript "$1['persist']()" js_persist :: Object.Object -> IO Object.Object
foreign import javascript "$1['bubbles']" js_bubbles :: Object.Object -> IO Bool
foreign import javascript "$1['cancelable']" js_cancelable :: Object.Object -> IO Bool
foreign import javascript "$1['nativeEvent']" js_getNative :: Object.Object -> IO JSVal
foreign import javascript "$1['target']" js_target :: Object.Object -> IO EventTarget
foreign import javascript "$1['currentTarget']" js_currentTarget :: Object.Object -> IO EventTarget
foreign import javascript "$1['defaultPrevented']" js_defaultPrevented :: Object.Object -> IO Bool
foreign import javascript "$1['eventPhase']" js_eventPhase :: Object.Object -> IO Int
foreign import javascript "$1['isTrusted']" js_isTrusted :: Object.Object -> IO Bool
foreign import javascript "$1['preventDefault']()" js_preventDefault :: Object.Object -> IO ()
foreign import javascript "$1['isDefaultPrevented']()" js_isDefaultPrevented :: Object.Object -> IO Bool
foreign import javascript "$1['stopPropagation']()" js_stopPropagation :: Object.Object -> IO ()
foreign import javascript "$1['isPropagationStopped']()" js_isPropagationStopped :: Object.Object -> IO Bool
foreign import javascript "$1['timeStamp']" js_timeStamp :: Object.Object -> IO Int
foreign import javascript "$1['type']" js_type :: Object.Object -> IO JSString

-- |Class of events that a component can handle through one of its 'EventProp's.
class SyntheticEvent e where
  -- |The type of the native browser event associated with this synthetic React event.
  type NativeEvent e

  -- |Get the raw JS object for this event.
  eventVal :: e -> Object.Object
  eventVal = unsafeCoerce

  -- |Signal that this synthetic event object will be used outside of the immediate handler and should not be
  -- recycled. Usually, React keeps @SyntheticEvent@s in a pool and reuses them.
  persist :: MonadIO m => e -> m e
  persist = fmap unsafeCoerce . liftIO . js_persist . eventVal

  -- |Check whether this event will bubble through the DOM unless stopped via 'stopPropagation'
  bubbles :: MonadIO m => e -> m Bool
  bubbles = liftIO . js_bubbles . eventVal

  -- |Check whether this event can be cancelled via 'preventDefault' to prevent its default behavior in the
  -- browser.
  cancelable :: MonadIO m => e -> m Bool
  cancelable = liftIO . js_cancelable . eventVal

  -- |Get the current target of the event in bubbling, which is always the 'EventTarget' to which the event
  -- handler is attached.
  currentTarget :: MonadIO m => e -> m EventTarget
  currentTarget = liftIO . js_currentTarget . eventVal

  -- |Check whether the event has already had its default behavior prevented with 'preventDefault' during this
  -- handler.
  defaultPrevented :: MonadIO m => e -> m Bool
  defaultPrevented = liftIO . js_defaultPrevented . eventVal

  -- |Check which phase of event processing the event is currently in, one of none (0), capturing (1), at the
  -- event target (2), or bubbling.
  --
  -- TODO? use an ADT for this instead of 'Int'
  eventPhase :: MonadIO m => e -> m Int
  eventPhase = liftIO . js_eventPhase . eventVal

  -- |Check whether this event was generated by a user directly ('True') and is therefore a trustworthy
  -- representation of user action, or synthetically generated or modified by script ('False').
  isTrusted :: MonadIO m => e -> m Bool
  isTrusted = liftIO . js_isTrusted . eventVal

  -- |Extract the native event being wrapped by the @SyntheticEvent@
  nativeEvent :: MonadIO m => e -> m (NativeEvent e)
  nativeEvent = fmap unsafeCoerce . liftIO . js_getNative . eventVal

  -- |Prevent some default browser action the event would trigger, assuming this event is cancelable.
  preventDefault :: MonadIO m => e -> m ()
  preventDefault = liftIO . js_preventDefault . eventVal

  -- |Check whether the event has had its default behavior with 'preventDefault' either during the current
  -- handler or previously.
  isDefaultPrevented :: MonadIO m => e -> m Bool
  isDefaultPrevented = liftIO . js_isDefaultPrevented . eventVal

  -- |Stop propagation of this event through any further of the capturing and bubbling phases.
  stopPropagation :: MonadIO m => e -> m ()
  stopPropagation = liftIO . js_stopPropagation . eventVal

  -- |Check whether propagation of the event has been halted by use of 'stopPropagation'
  isPropagationStopped :: MonadIO m => e -> m Bool
  isPropagationStopped = liftIO . js_isPropagationStopped . eventVal

  -- |Fetch the primary target of the event which is the object that dispatched it in the first place.
  target :: MonadIO m => e -> m EventTarget
  target = liftIO . js_target . eventVal

  -- |Get the timestamp in milliseconds relative to some epoch.
  timeStamp :: MonadIO m => e -> m Int
  timeStamp = liftIO . js_timeStamp . eventVal

  -- |Get the string representing the type of event being processed.
  eventType_ :: MonadIO m => e -> m JSString
  eventType_ = liftIO . js_type . eventVal

{-
class ErrorEvent e where {}
-}

-- |Event data given when the clipboard event handlers are called: 'onCopy', 'onCut', and 'onPaste'.
newtype ClipboardEvent = ClipboardEvent { fromClipboardEvent :: Object.Object }
-- clipboardData :: ClipboardEvent -> m DataTransfer

instance IsJSVal ClipboardEvent

-- |Event data given for composition events: 'onCmopositionStart', 'onCompositionUpdate', and
-- 'onCompositionEnd'.
newtype CompositionEvent = CompositionEvent { fromCompositionEvent :: Object.Object }
instance IsJSVal CompositionEvent
instance SyntheticEvent CompositionEvent where
  type NativeEvent CompositionEvent = JSVal
-- data_ :: e -> m JSString

-- |Event data given for the keyboard events: 'onKeyDown', 'onKeyPress', and 'onKeyUp'.
newtype KeyboardEvent = KeyboardEvent { fromKeyboardEvent :: Object.Object }
instance IsJSVal KeyboardEvent
instance SyntheticEvent KeyboardEvent where
  type NativeEvent KeyboardEvent = Keyboard.KeyboardEvent
{-
altKey :: e -> m Bool
charCode :: e -> m Int
ctrlKey :: e -> m Bool
getModifierState :: Int -> m Bool
key :: e -> m JSString
keyCode :: e -> m Int
locale :: e -> m JSString
location :: e -> m Int
metaKey :: e -> m Bool
repeat :: e -> m Bool
shiftKey :: e -> m Bool
which :: e -> m Int
-}

-- |Event data given for the focus change events: 'onFocus' and 'onBlur'.
newtype FocusEvent = FocusEvent { fromFocusEvent :: Object.Object }
instance IsJSVal FocusEvent
instance SyntheticEvent FocusEvent where
  type NativeEvent FocusEvent = Focus.FocusEvent
-- relatedTarget :: e -> m EventTarget

-- |Event data given for form control events: 'onChange', 'onInput', and 'onSubmit'.
newtype FormEvent = FormEvent { fromFormEvent :: Object.Object }
instance IsJSVal FormEvent
instance SyntheticEvent FormEvent where
  type NativeEvent FormEvent = JSVal

-- |Event data given for mouse related events: 'onClick', 'onMouseDown', 'onMouseUp', etc.
newtype MouseEvent = MouseEvent { fromMouseEvent :: Object.Object }
instance IsJSVal MouseEvent
instance SyntheticEvent MouseEvent where
  type NativeEvent MouseEvent = Mouse.MouseEvent
{-
-- altKey
button :: e -> m Int
buttons :: e -> m Int
clientX :: e -> m Int
clientY :: e -> m Int
-- ctrlKey
-- getModifierState
-- metaKey
pageX :: e -> m Int
pageY :: e -> m Int
-- relatedTarget
screenX :: e -> m Int
screenY :: e -> m Int
-- shiftKey
-}

-- |Event data given when the 'onSelect' event handler is called.
newtype SelectionEvent = SelectionEvent { fromSelectionEvent :: Object.Object }
instance IsJSVal SelectionEvent
instance SyntheticEvent SelectionEvent where
  type NativeEvent SelectionEvent = JSVal

-- |Event data given with touchscreen events: 'onTouchStart', 'onTouchEnd', etc.
newtype TouchEvent = TouchEvent { fromTouchEvent :: Object.Object }
instance IsJSVal TouchEvent
instance SyntheticEvent TouchEvent where
  type NativeEvent TouchEvent = Touch.TouchEvent
-- altKey
-- changedTouches :: e -> m (Maybe TouchList)

-- |Event data given when the 'onScroll' event handler is called.
newtype UIEvent = UIEvent { fromUIEvent :: Object.Object }
instance IsJSVal UIEvent
instance SyntheticEvent UIEvent where
  type NativeEvent UIEvent = JSVal

-- |Event data given when the 'onWheel' handler is called.
newtype WheelEvent = WheelEvent { fromWheelEvent :: Object.Object }
instance IsJSVal WheelEvent
instance SyntheticEvent WheelEvent where
  type NativeEvent WheelEvent = JSVal

-- |Event data associated with media playback events: 'onPlay', 'onPause', 'onPlaying', etc.
newtype MediaEvent = MediaEvent { fromMediaEvent :: Object.Object }
instance IsJSVal MediaEvent
instance SyntheticEvent MediaEvent where
  type NativeEvent MediaEvent = JSVal

-- |Event data given when the 'onLoad' event handler is called due to an image load completing.
newtype ImageEvent = ImageEvent { fromImageEvent :: Object.Object }
instance IsJSVal ImageEvent
instance SyntheticEvent ImageEvent where
  type NativeEvent ImageEvent = JSVal

-- |Event data given for the animation related events: 'onAnimationStart', 'onAnimationIteration', and
-- 'onAnimationEnd'.
newtype AnimationEvent = AnimationEvent { fromAnimationEvent :: Object.Object }
instance IsJSVal AnimationEvent
instance SyntheticEvent AnimationEvent where
  type NativeEvent AnimationEvent = Animation.AnimationEvent

-- |Event data given when a transition ends and the 'onTransitionEnd' handler is called.
newtype TransitionEvent = TransitionEvent { fromTransitionEvent :: Object.Object }
instance IsJSVal TransitionEvent
instance SyntheticEvent TransitionEvent where
  type NativeEvent TransitionEvent = Transition.TransitionEvent

-- |Empty set of props.
noProps :: [Prop]
noProps = []

-- |A handle to the current React component
newtype This ps st = This { fromThis :: JSVal }

instance IsJSVal (This ps st)

foreign import javascript unsafe "$1['refs'][$2]" js_getRef :: This ps st -> JSString -> JSVal

-- |Get one of the @refs@ of the current component cast as a 'Instance' of whatever props and state type
-- you choose.
--
-- There's no checking that the returned Component actually uses the @ps@ and @st@ types
-- for its props and state, or that it conforms to the @t@ phantom type either, so a lot of safety
-- goes out the window here.
unsafeGetRef :: JSString -> ReactM ps st (Maybe (Instance ps' st'))
unsafeGetRef str = do
  this <- ask
  let r = js_getRef this str
  return $! if isTruthy r
    then Just $ Instance r
    else Nothing

foreign import javascript unsafe "($1 ? $1.props[$2] : null)" js_getProp :: This ps st -> JSString -> IO JSVal
foreign import javascript unsafe "$1[$2]" js_deref :: Props ps -> JSString -> IO JSVal

-- |Unform interface for getting a prop by some name from something which contains or represents props,
-- specifically 'This', 'ReactProps', or 'Properties'.
class GetProp a where
  -- |Get the property of the given name from a property carrier @a@, using the 'FromJSVal' instance to
  -- convert the property value into a Haskell type if it's present.
  getProp :: FromJSVal p => a -> PropName p -> Maybe p

  -- |Get the property of the given name and force it to be of the desired type using 'unsafeCoerce'
  unsafeGetProp :: a -> PropName p -> Maybe p

instance GetProp (This ps st) where
  getProp this (PropName pn) = unsafePerformIO $ do
    p <- js_getProp this pn
    if isTruthy p
      then fromJSVal p
      else return Nothing

  unsafeGetProp this (PropName pn) = unsafePerformIO $ do
    p <- js_getProp this pn
    return $ if isTruthy p
      then Just $ unsafeCoerce p
      else Nothing

instance GetProp (Props a) where
  getProp allProps (PropName pn) = unsafePerformIO $ do
    p <- js_deref allProps pn
    if isTruthy p
      then fromJSVal p
      else return Nothing

  unsafeGetProp allProps (PropName pn) = unsafePerformIO $ do
    p <- js_deref allProps pn
    return $ if isTruthy p
      then Just $ unsafeCoerce p
      else Nothing

-- |Class of types which can become 'Props' on demand, e.g. to create an element with 'createElement' or
-- 'runFactory'.
class ToProps a where
  -- The phantom type of 'Props' the @a@ converts into, e.g. @ToPropsOf (Properties ps) ~ ps@
  type ToPropsOf a :: *

  -- Convert to a 'Props' wrapper purely, with the resulting 'Props' uisng the designated phantom type
  -- 'ToPropsOf' @a@.
  toProps :: a -> Props (ToPropsOf a)

instance ToProps (Props ps) where
  type ToPropsOf (Props ps) = ps
  toProps = id
  {-# INLINE toProps #-}

instance ToProps OI.Object where
  type ToPropsOf OI.Object = OI.Object
  toProps = Props
  {-# INLINE toProps #-}

instance Foldable f => ToProps (f Prop) where
  type ToPropsOf (f Prop) = Prop
  toProps = buildProps

-- |Get the prop of this component by the given 'PropName' as a new 'Prop' to pass to some other component.
inheritProp :: This ps st -> PropName p -> Prop
inheritProp this (PropName p) = inheritProp' this p

-- |Get the prop of this component by the given 'JSString' as a new 'Prop' to pass to some other component.
inheritProp' :: This ps st -> JSString -> Prop
inheritProp' this str = Prop str . fromMaybe jsNull $ getProp this (PropName str)

-- |Class of types which can be converted to a sequence of children for a React node.
class ToChildren a where
  -- |Convert to a @Maybe (Array Node)@ to hand off to React, with @Nothing@ indicating @null@.
  toChildren :: a -> Maybe (Array Node)

instance ToChildren (Maybe (Array Node)) where
  toChildren = id
  {-# INLINE toChildren #-}

instance (Foldable f, ToNode n) => ToChildren (f n) where
  toChildren es
    | F.null es = Nothing
    | otherwise = Just . array . map node . F.toList $ es
  {-# INLINE toChildren #-}

-- |Class of types which can be converted to 'ReactNode's
class ToNode a where
  node :: a -> Node

instance ToNode S.Text where
  node = text . textToJSString
  {-# INLINE node #-}

instance ToNode L.Text where
  node = text . lazyTextToJSString
  {-# INLINE node #-}

instance ToNode JSString where
  node = Node . pToJSVal
  {-# INLINE node #-}

instance ToNode String where
  node = Node . pToJSVal
  {-# INLINE node #-}

instance ToNode Bool where
  node True = text ("true" :: JSString)
  node False = text ("false" :: JSString)
  {-# INLINE node #-}

-- |Convert a 'JSString' to a 'Node' representing a DOM text node to create.
--
-- A type constrained version of 'node'
text :: JSString -> Node
text = node
{-# INLINE text #-}
