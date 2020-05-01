module Graphics.UI.Threepenny.Events (
    -- * Synopsis
    -- | Events on DOM elements.

    -- * Convenience events
    onChangeE,valueChange, selectionChange, checkedChange,

    -- * Standard DOM events
    click,contextmenu, dblclick, mousewheel,mousemove, mousedown, mouseup,  
    hover, leave,
    focus, blur,
    KeyCode, keyup, keydown, keydownFilter
    ) where

import Graphics.UI.Threepenny.Attributes
import Graphics.UI.Threepenny.Core



{-----------------------------------------------------------------------------
    Events
------------------------------------------------------------------------------}
-- | Event that occurs when the /user/ changes the value of the input element.

valueChange :: Element -> UI (Event String)
valueChange el = domEventH "keydown" el (el # get valueFFI)


onChangeE :: Element -> UI (Event String)
onChangeE el = domEventH "change" el (el # get valueFFI )

-- | Event that occurs when the /user/ changes the selection of a @<select>@ element.


selectionChange :: Element -> UI (Event Int)
selectionChange el = domEventH "change" el (ffi "this.selectedIndex")

-- | Event that occurs when the /user/ changes the checked status of an input
-- element of type checkbox.
checkedChange :: Element -> UI (Event Bool)
checkedChange el =  domEventH "change" el (ffi "this.checked")

mousewheel :: Element -> UI (Event Int)
mousewheel el =   domEventH  "wheel" el  (ffi "function(){%1.preventDefault();if (%1.originalEvent.wheelDelta) {return %1.originalEvent.wheelDelta/120} else { return %1.originalEvent.deltaY/-3}}()" event)

{-----------------------------------------------------------------------------
    DOM Events
------------------------------------------------------------------------------}
-- | Mouse click.


click :: Element -> UI (Event ())
click el = domEventH "click" el (ffi "")

dblclick :: Element -> UI (Event ())
dblclick el = domEventH "dblclick" el (ffi "")

-- | Context menu event.
contextmenu :: Element -> UI (Event (Int,Int))
contextmenu el = domEventH "contextmenu"  el (coordinates el)

-- | Mouse enters an element.
hover :: Element -> UI (Event ())
hover el = domEventH "mouseenter" el (ffi "")


-- | Event that periodically occurs while the mouse is moving over an element.
--
-- The event value represents the mouse coordinates
-- relative to the upper left corner of the element.
--
-- Note: The @<body>@ element responds to mouse move events,
-- but only in the area occupied by actual content,
-- not the whole browser window.

coordinates :: Element -> JSFunction (Int,Int)
coordinates el = ffi "function(){var offset = $(%2).offset();var x  = %1.pageX - offset.left;var y = %1.pageY - offset.top;return [x, y]}()" event el

mousemove :: Element -> UI (Event (Int,Int))
mousemove el = domEventH "mousemove" el  (coordinates el)


-- | Mouse down event.
-- The mouse coordinates are relative to the element.
mousedown :: Element -> UI (Event (Int,Int))
mousedown el = domEventH "mousedown" el (coordinates el)

-- | Mouse up event.
-- The mouse coordinates are relative to the element.
mouseup :: Element -> UI (Event (Int,Int))
mouseup el = domEventH "mouseup" el (coordinates el)

-- | Mouse leaving an element.
leave :: Element -> UI (Event ())
leave el = domEventH "mouseleave" el  (ffi "")

-- | Element receives focus.
focus :: Element -> UI (Event ())
focus el = domEventH "focus" el (ffi "")

-- | Element loses focus.
blur :: Element -> UI (Event ())
blur el = domEventH "blur" el (ffi "")


type KeyCode = (Int,Bool,Bool,Bool)

-- | Key pressed while element has focus.
keydownFilter :: (Int,Bool,Bool,Bool) -> Element -> UI (Event KeyCode)
keydownFilter arg el = domEventAsync "keydown"  el (\j -> ffi "if ((%2.keyCode === %3[0]) && (%2.shiftKey === %3[1] ) && (%2.altKey === %3[2] ) && (%2.ctrlKey === %3[3]) )  { (%1)([%2.keyCode,%2.shiftKey,%2.altKey,%2.ctrlKey])}" j event arg )

keydown :: Element -> UI (Event KeyCode)
keydown el = domEventH "keydown"  el (ffi "[%1.keyCode,%1.shiftKey,%1.altKey,%1.ctrlKey]" event)

-- | Key released while element has focus.
keyup :: Element -> UI (Event KeyCode)
keyup   el = domEventH "keyup"  el (ffi "[%1.keyCode,%1.shiftKey,%1.altKey,%1.ctrlKey]" event)
