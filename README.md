# epicea-antivalue
*How to deal with odd outcomes from computations*

Antivalues let us break control flow locally and return an alternative result of a computation. They work a bit like exceptions, but there is only one type of antivalue, no type-based dispatch, and they cannot escape from functions or loops. Antivalues can be converted to regular values using ```anti``` and regular values can be converted to antivalues using ```anti```. Whenever an antivalue occurs, it interupts all expressions currently being evaluated, and propagates up the nested forms until it reaches a form that will handle it, such as ```either```, ```anti``` or ```export```. 

Even if they behave a bit like exceptions, this library generates standard Clojure code without exceptions from code that uses antivalues. That way, it can easily be made to work on different host platforms and we could hope that it will play well with other macro libraries, such as core.async (TODO: test that...). Unlike exceptions, antivalues can be associated to symbol using the ```let``` form, which makes it easy to identify the reason why some computation failed, instead of using the type-based dispatch mechanism of exceptions.

## Introductory example
Here is a small function to illustrate what the library does.
```clojure
(defn bmi [data]
  (top
   (let [mass (expect number? (:mass data))
         height (expect number? (:height data))]
     (either (/ mass (* height height))
             [:missing-mass (anti mass)]
             [:missing-height (anti height)]
             nil))))
```
It uses ```expect``` to validate the ```mass``` and ```height``` values in the map, and ```either``` to decide what to return. There is also a ```top``` form that surrounds everything and rewrites the code into regular clojure code. This small examples illustrates three useful features of this library:
  1. ```expect``` lets us validate data, and produces antivalues with invalid data.
  2. Antivalues can be bound to symbols using ```let```, so that we can identify the reason why something failed.
  3. ```either``` lets us choose the first expression that has a regular value, and that expression is then returned as a result from calling the function.

So if we call it with approprate arguments,
```clojure
(bmi {:mass 80 :height 1.94})
```
we get
```clojure
21.25624402168137
```
but with wrong arguments,
```clojure
(bmi {:mass 80 :height :kattskit})
```
we get
```clojure
[:missing-height :kattskit]
```

## Usage
We will be using the namespace ```epicea.antivalue.core```.

A regular Clojure value is just a value. The value of ```9``` is ```9```. To produce an antivalue, we use ```anti``` on the value:
```clojure
(anti 9)
```
This results in the *antivalue* of 9.

Here is an expression that evaluates to 9:
```clojure
(+ 4 5)
```
If we make one of the arguments an antivalue, the expression will evaluate to that antivalue, that is
```clojure
(+ (anti 4) 5)
```
evaluates to ```(anti 4)```.

```anti``` must always be wrapped inside ```either```, so that we always end up with a value (which is not an antivalue):
```clojure
(either (+ (anti 4) 5)
        :failure)
```
The form above will evaluate to the value ```:failure```. The top-most ```either``` form must always have at least one branch that will never produce an antivalue, so this will *not* compile:
```clojure
(either (+ (anti 4) 5))
```
Instead, you will get an error at macro expansion time.

Given an antivalue, we can turn it into a value again using anti:
```clojure
(either (anti (anti 4)) nil)
```
evaluates to ```4```, and 
```clojure
(either (anti (+ (anti 4) 5)) nil)
```
evaluates to ```4```, too.

```either``` can have more than two branches and each branch is visited in order until we encounter a branch with a value that is not an antivalue, for instance
```clojure
(either (anti 4) (anti 5) :a :b :c (anti 6) :d)
```
evaluates to ```:a``` which is the first value.

Expressions that produce antivalues can exist inside let-bindings, e.g.
```clojure        
(defn my-add [a b]
  (either
   (let [ax (if (number? a) a (anti a))
         bx (if (number? b) b (anti b))]
      (either (+ ax bx)
              [:bad-input :a (anti ax)]
              [:bad-input :b (anti bx)]
              nil))))
```
That is practical to identify the reason why we can't procede with a computation. Note that there are two ```either```. The outer ```either``` is only needed for the code transformations.

The ```expect``` macro tests if a function applied to a value is true and returns the value in that case, otherwise it produces an antivalue of that value. So the code here is equivalent to the above code.
```clojure
(defn my-add [a b]
  (either
   (let [ax (expect number? a)
         bx (expect number? b)]
     (either (+ ax bx)
              [:bad-input :a (anti ax)]
              [:bad-input :b (anti bx)]
              nil))))
```
In case we would actually need to work with an antivalue like a regular value, the ```export``` function will convert it to such a value. The following call
```clojure
(export (anti 3))
```
evaluates to
```clojure
#epicea.antivalue.core.Antivalue{:data 3}
```
And if we want to take any value and convert it to an antivalue if possible, there is ```import```. For instance, this call will take the raw representation of the antivalue previous exported and convert it to an ordinary value:
```clojure
(export (anti (import #epicea.antivalue.core.Antivalue{:data 3})))
```
so that we get ```3``` as result. The mechanism of ```import``` and ```export``` could be used to make antivalues cross function boundaries, but should probably be used with care. It might be better to explicitly use ```either``` to produce alternative return values if a computation fails.

See the [unit tests](test/epicea/antivalue/core_test.clj) for several examples of how the library can be used.

## Difference w.r.t exceptions

  * Unlike exceptions, antivalues are local. An antivalue produced inside a function cannot leak outside that function. This includes lambda functions.
  * Also, antivalues currently don't work with loops.
  * Whenever an antivalue occurs inside a let bounding form, instead of interupting the entire form as would have been the case with an exception, the antivalue is kept inside the bound symbol and only released once the bound symbol is evaluated.
  * There is only one type of antivalues. There is no type-based dispatch.

## Rationale

Exceptions cause confusion when they escape from functions, because the result of the function can either be a return value or an exception. If functions only produce return values but no exceptions, the code becomes simpler. But locally, exceptions can still be convenient because they *can* let us express the control flow more concisely and to the point. 

This library emulates a very simple form of exceptions that only work locally and makes it easy to deal with all the different forms of return values from calling functions, so that we can write code which is both robust, expressive and easy to reason about.

## License

Copyright © 2017 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
