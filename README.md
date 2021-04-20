# Greek


Well, I write code recreationally, and this is yet another language
that I'll probably never finish.


Have a look at the (code)[code] file to see what I'm up to.
Here's what it was at the time of writing:


```haskell
a : Int;
a = 0;

b : Int;
b a b c = 0x1AF;

c = '\n ;
d = "foo bar\"";

e = 0b010101 ;

a : ∀ a b c. ∃ d. Either a (∃ x. f b) c;


List : Type;
List a = Nil | Cons a (List a) ;


Yoneda f a = Yoneda (∀ b. Arr (Arr a b) (f b))
;
```

What's up with the semicolons? It actually started out as a
very boring imperative language. We might go back to that too-
I don't have a goal!


#### Edit

```haskell
-- vi:syntax=haskell

foo f g a =
  f (g a) (g a) c

bar = 1
     2
    3
   4
  5
 6 -- hello
 7 {-
-} 8
 9


Γ Functor f
 foo = 1
 bar = 2
baz = 3


foo : a b c -> d e f -> g h i
bar : ((((a -> b) -> c) -> d) -> e) -> f
baz : a -> (b -> (c -> (d -> (e -> f))))


foo = a b c

infixr 0 :* +


foo =
 let
   a = 1
   b = 2
   c = 3
      4
     5
    6
 in a

bar = let in 2
baz = let a = let b = let c = 4 in 3 in 2 in 1
qux =
  let
  a =
   let
   b =
    let
    c = qux in c
       in b
  in a


foo =
  let Γ Functor f
        fmap : ∀ a b. (a -> b) -> f a -> fb
  in 2
```
