l := #( 199 200 208 210 200 207 240 269 260 263 ).
r := LinkedList new.
1 to: (l size) do: [ :i |
	                   (l atWrap: i-1) < (l atWrap: i)
	                                   ifTrue: [ r add: (l atWrap: i) ]
	                 ].
Transcript show: (r size); cr.
