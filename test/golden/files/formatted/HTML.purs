module HTML where

import DOM as DOM

foo :: DOM.HTML
foo =
  render \_ -> do
    DOM.div []
      [ DOM.span
          [ DOM.className "foo"
          ]
          [ DOM.literal "This is the text"
          , DOM.literal "And some more"
          ]
      ]

bar :: DOM.HTML
bar =
  DOM.span
    [ DOM.className
        "bar"
    ]
    [ DOM.literal "Here's some more"
    , DOM.literal "And more even"
    ]

baz :: DOM.HTML
baz =
  DOM.button
    [ DOM.onClick mempty
    ]
    []

qux :: DOM.HTML
qux =
  DOM.button
    [DOM.onClick mempty]
    []
