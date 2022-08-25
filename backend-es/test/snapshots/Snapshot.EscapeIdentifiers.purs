module Snapshot.EscapeIdentifiers where

class Wat (sym :: Symbol)

instance Wat "a.b ?$$ \" →"
