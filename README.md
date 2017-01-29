# epicea-antivalue

To interupt the program flow locally with *antivalues*. A bit like a local exception mechanism. 
Actually, it is implemented using exceptions, but it could as well be implemented using just
code transformations by a macro. But it is easier to get it working with exceptions.

## Rationale

Exceptions seem to be common practice in for instance Java. In a functional programming language such as Clojure *the problem* is that they cause a lot of confusion: Should the result of the computation performed by a function be represented by a *return value*, or is it represented by an *exception* thrown by the function (or some function that it calls)? This is problematic, because it can be hard to simultaneously deal with these two types of outcomes. It is simpler if functions only produce return values.

Nevertheless, locally, exceptions can still be convenient to interupt control flow. For instance, maybe a user tries to log in on a system. We would expect that the user typed a valid user name, and from our user database we can retrieve a record for that user and check the password. It could however be that there is no user for that user name, and in that case, we would need to follow a different path than checking the provided password against a non-existant user. In that case, an exception based mechanism could help to break the control flow in a more convenient manner than using nested if- and let forms.

epicea/antivalues introduce *antivalues* that are similar to exceptions, but only work *locally* and not across function boundaries. Whenever a computation produces an antivalue, that computation is interupted. The antivalue produced inside a computation can then be either turned into a regular Clojure value using the ```anti``` form, or it can be ignored and another computation can continue instead, by using the ```either``` form. Furthermore, ```anti``` can also turn a regular clojure value into an ```anti```-value.

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
The form above will evaluate to the value ```:failure```. If we don't provide an alternative, it defaults to ```nil```, so
```clojure
(either (+ (anti 4) 5))
```
will evaluate to ```nil```.

Given an antivalue, we can turn it into a value again using anti:
```clojure
(either (anti (anti 4)))
```
evaluates to ```4```, and 
```clojure
(either (anti (+ (anti 4) 5)))
```
evaluates to ```4```, too.

```either```, can have several branches and each branch is visited in order until we encounter a branch with a value that is not an antivalue, for instance
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
              [:bad-input :b (anti bx)]))))
```
That is practical to identify the reason why we cant procede with a computation. Note that there are two ```either```. The outer ```either``` is only needed for the code transformations.

The ```expect``` macro tests if a function applied to a value is true and returns the value in that case, otherwise it produces an antivalue of that value. So the code here is equivalent to the above code.
```clojure
(defn my-add [a b]
  (either
   (let [ax (expect number? a)
         bx (expect number? b)]
     (either (+ ax bx)
              [:bad-input :a (anti ax)]
              [:bad-input :b (anti bx)]))))
```

## Difference w.r.t exceptions

  * Unlike exceptions, antivalues are local. An antivalue produced inside a function cannot leak outside that function. This includes lambda functions.
  * Also, antivalues currently don't work with loops.
  * Whenever an antivalue occurs inside a let bounding form, instead of interupting the entire form as would have been the case with an exception, the antivalue is kept inside the bound symbol and only released once the bound symbol is evaluated.
  * There is only one type of antivalues. There is no type-based dispatch.

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
