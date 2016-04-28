; Playing with derive and hierarchies.
;
; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(ns multi.derive)

; Double-colon is a namespace-qualified keyword. Helps disambiguate tags passed
; to derive (since the relationships it registers are global, and otherwise
; a tag like :shape could collide between two completely unrelated namespaces).
; It's also possible to create local hierarchies with (make-hierarchy) instead
; of using the global one.
(eval :foo)
(eval ::foo)

; Define a hierarchy: (derive tag parent-tag)
(derive ::rect ::shape)
(derive ::square ::rect)
(derive ::ellipse ::shape)

; Check all descendants of ::shape
(descendants ::shape)

; isa? child parent
(isa? ::shape ::rect)
(isa? ::rect ::shape)
(isa? ::ellipse ::shape)
(isa? ::ellipse ::rect)

; isa? also accepts a vector and checks isa?-ness for each element
(isa? [::square ::rect] [::rect ::shape])
(isa? [::square ::rect] [::rect ::ellipse])

(deftype Shape [])
(deftype Rectangle [])
(deftype Ellipse [])
(deftype Triangle [])

(derive Rectangle ::myshape)
(derive Rectangle Shape)
