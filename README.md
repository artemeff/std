### std [![Build Status](https://secure.travis-ci.org/artemeff/std.png)](http://travis-ci.org/artemeff/std)

---

### std

### std_fn

```erlang
% function composition
Fn1 = std_fn:compose(
    [ fun(X) -> X + 3 end
    , fun(X) -> X * 4 end
    , fun(X) -> X - 2 end
    ]),
50 = Fn1(10),
62 = Fn1(13).

% partial application
Fn2 = std_fn:partial(fun lists:foldl/3,
    [fun(X, Acc) -> X * Acc end, 1]),
100 = Fn2([2, 5, 10]).

Fn3 = std_fn:partial(fun lists:foldl/3,
    [fun(X, Acc) -> X + Acc end]),
7 = Fn2(-10, [2, 5, 10]).
```


---

### Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
