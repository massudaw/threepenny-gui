module Graphics.UI.Threepenny.Events (
    -- * Synopsis
    -- | Events on DOM elements.

    -- * Convenience events
    onChangeE,onChangeEUI,valueChange,valueChangeUI, selectionChange,selectionChangeUI, checkedChange,checkedChangeUI,

    -- * Standard DOM events
    click, mousemove, mousedown, mouseup, hover, leave,
    focus, blur,
    KeyCode, keyup, keydown,
    ) where

import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Core
import Debug.Trace

silence = fmap (const ())

{-----------------------------------------------------------------------------
    Events
------------------------------------------------------------------------------}
-- | Event that occurs when the /user/ changes the value of the input element.
valueChange :: Element -> Event String
valueChange el = unsafeMapUI el (const $ get value el) (domEvent "keydown" el)

valueChangeUI :: Element -> UI (Event String)
valueChangeUI el = mapEventUI el (const $ get value el) (domEvent "keydown" el)

onChangeE :: Element -> Event String
onChangeE el = unsafeMapUI el (const $ get value el) (domEvent "onchange" el)

onChangeEUI :: Element -> UI (Event String)
onChangeEUI el = mapEventUI el (const $ get value el) (domEvent "onchange" el)

-- | Event that occurs when the /user/ changes the selection of a @<select>@ element.
selectionChange :: Element -> Event (Maybe Int)
selectionChange el = unsafeMapUI el ((const $ get selection el)) (click el)


selectionChangeUI :: Element -> UI (Event (Maybe Int))
selectionChangeUI el = mapEventUI el (const $ get selection el) (click el)

-- | Event that occurs when the /user/ changes the checked status of an input
-- element of type checkbox.
checkedChangeUI :: Element -> UI (Event Bool)
checkedChangeUI el = mapEventUI el (const $ get checked el) (click el)

checkedChange :: Element -> Event Bool
checkedChange el = unsafeMapUI el (const $ get checked el) (click el)

{-----------------------------------------------------------------------------
    DOM Events
------------------------------------------------------------------------------}
-- | Mouse click.
click :: Element -> Event ()
click = silence . domEvent "click"


-- | Mouse enters an element.
hover :: Element -> Event ()
hover = silence . domEvent "mouseenter"

-- | Event that periodically occurs while the mouse is moving over an element.
--
-- The event value represents the mouse coordinates
-- relative to the upper left corner of the element.
--
-- Note: The @<body>@ element responds to mouse move events,
-- but only in the area occupied by actual content,
-- not the whole browser window.
mousemove :: Element -> Event (Int,Int)
mousemove = fmap readCoordinates . domEvent "mousemove"

readCoordinates :: EventData -> (Int,Int)
readCoordinates json = (x,y)
    where [x,y] = unsafeFromJSON json

-- | Mouse down event.
-- The mouse coordinates are relative to the element.
mousedown :: Element -> Event (Int,Int)
mousedown = fmap readCoordinates . domEvent "mousedown"

-- | Mouse up event.
-- The mouse coordinates are relative to the element.
mouseup :: Element -> Event (Int,Int)
mouseup = fmap readCoordinates . domEvent "mouseup"

-- | Mouse leaving an element.
leave :: Element -> Event ()
leave = silence . domEvent "mouseleave"

-- | Element receives focus.
focus :: Element -> Event ()
focus = silence . domEvent "focus"

-- | Element loses focus.
blur :: Element -> Event ()
blur = silence . domEvent "blur"


type KeyCode = Int

-- | Key pressed while element has focus.
keydown :: Element -> Event KeyCode
keydown = fmap unsafeFromJSON . domEvent "keydown"

-- | Key released while element has focus.
keyup :: Element -> Event KeyCode
keyup   = fmap unsafeFromJSON . domEvent "keyup"
