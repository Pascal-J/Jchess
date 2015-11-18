amV_z_ =: (0 {:: [)`(1 {:: [)`]}  NB. dyad amend
boxscan =: &.>/(>@:) NB. applies u/ to any shape x and any shape y, as long as they are joined <x,<y
reduce =: 1 : '<"_1@[ ([: u boxscan ,) <@:]'

bmp =: (,. 4 4 <@$("2 0) 0 4) ,.~ (,:  4:^:(=&0)"0 leaf)   , <"2 (,  3:^:(=&1)"0) ((] ,: 2 (<3 3)} ])@{. , }.) 4 4 ($"1) 1 1 1 1 0 1 1 0 0 1 1 0 0 1 1 0 , 0 1 1 0 1 1 1 0 0 1 0 0 1 1 1 0 , 0 1 1 0 1 1 1 1 0 1 1 0 0 1 1 0 , 1 1 1 1 0 1 1 0 0 1 1 0 1 0 0 1 , 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 , 0 0 0 0 0 1 1 0 0 1 1 0 1 1 1 1 ,: 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0
bmp2 =: (,. 4 5 <@$("2 0) 0 4) ,.~ (,: 4:^:(=&0)"0 leaf) , <"2 (, 3:^:(=&1)"0) ((] ,: 2 (<3 4)} ])@{. , }.) 4 5 ($"1) 1 0 0 0 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 0 , 0 1 1 1 0 1 1 1 1 0 0 0 1 0 0 1 1 1 1 0 , 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 1 0 0 , 1 0 1 0 1 1 1 1 1 1 0 1 1 1 0 1 0 0 0 1 , 0 0 1 0 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 , 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 1 1 1 0 ,: 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 0 0 0
cbraw =. ( 2 | +/&i.~)8
border =: (a:, ;: ' a b c d e f g h') ,~ (|. <"0 >: i.8) ,. ]
toascii =: >@:(;@:(('.' #~ ".)^:(e.&'12345678')each) leaf@('/'&cut)) 
tobmp =: (8 8$' XsO.'{~leaf(,cbraw){"0 1|:@[{~[:,'rsnbqkpgRSNBQKPG.'i.])
ghostclear =: (] [`(amV~)@.(0< #@])  'Rr.' (] ,&<~ [ {~ 7 0 i."1 0 {.&>@])^:(0 <#@]) 4 <"1@$. $.@:(e.&'gG'"0))
ghostpawnCap =: (] amV~ '.' (; <) (>:@{. , {:)L:0@{:@[)`(] amV~ '.' (; <) (<:@{. , {:)L:0@{:@[)`]@.(2 <. (2 * 'pP' -.@e.~ {.@:(0 {:: [)) + 'gG' i. {:@:(0 {:: [))
ghostpawnAdd =: ]`(] amV~ ( (1 {:: [) <@:+ 0 ,~ _2 %~ (-/)@:({.every)@:}.@[) ,&<~ 'gG'{~'pP'i.{.@:(0{::[))@.(('pP'e.~{.@:(0{::[))*.2=|@(-/)@:({.every)@:}.@[)

moves=. ,/ > 'x-' ( [: ('87654321'&i.@{: , 'abcdefg'&i.@{.) each '-' cut rplc~) leaf > ' ' cut each  cutLF 0 : 0
e2-e4 c7-c5
f1-c4 g8-f6
c4xf7 e8xf7
e4-e5 d7-d5
e5xd6
)

   islowerg =: e.&'bnprgskq'
   isupperg =: e.&'BNPRGSKQ'
   islower =: e.&'bnprskq'
   isupper =: e.&'BNPRSKQ'
NB. capture routines x is from square idx, y is board. returns list of indexes that can be captured
Rcuts =: (1 (0}) each (8#0) <@:amV~("1)  1 ;"0 ])
Rspace =: (|:@:[ {~ {:@]) ; [ {~ {.@]
idxs =:(4$.$.)@:-.@i.
NB. returns maxlengths < > ^ V where infinity means till edge of board.
Rdirs =: ([: ;@;"1@((<0)&,^:(1=#)L:1)  ((']';'<:'){~(,-.)@(islower@[)) ((<./&:>L:1)@:(<./&:>leaf)@:(>:@(_:^:(0=#))@apply each))L:1(I.@islower;I.@isupper)@:((|.@]`(}.@]))@.([={.@]))leaf)
RdirsM =: ( 1 0 1 0 <;.(1) _1 1 _1 1 * Rdirs)"1
bounds =: 7&<.@(0&>.)
maxidx =: bounds leaf@(|."1@[ (<"1@(,. >)&{: , <"1@(,.~ >)&{.)"1 + each) 
Rmax =: ( idxs maxidx  [ RdirsM idxs ((Rcuts<;.1 each Rspace)~)"1 _ ])

Bspace =: ([ (] #~ e.&>) </.@:(|."1)@]) ,"0 [ (] #~ e.&>) </.@]
NB. returns maxlengths \< \> /> /< where infinity means till edge of board.
BdirsM =: (   [ > L:1@(<./@:,&:> L:1)@:( (({: ,~ >:leaf@{.)@] L:1)`(({. , >:leaf@{:)@] L:1)@.(islower@[)) ('|.leaf@]' ;'(}.leaf@])')  {.leaf@(_:^:(0=#)leaf)@(I.@islower;I.@isupper)@:apply each L:1 [(a:(_2{.,)]<;.1~1(0})-.@i.)leaf Bspace)"1 _
backtrack =: 2 : '] ([`]@.v"_) u' 
untilOB =: 2 : ' u backtrack(*./@:(_1&<)@] *. *./@:(8&>)@])(^:n)'
Bmax =: (|:@:(<"1)@((_1 _1;1 1;_1 1;1 _1)(,."0 1)<"1@idxs) 4 :' a + untilOB y b  [ ''a b''=. x'  each"1 1 ;"1@BdirsM)
