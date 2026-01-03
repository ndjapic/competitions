program E_Square_Price;
{$mode objfpc}{$h+}{$inline on}
uses
    math;
const
    nn = 200 * 1000;
    eps = extended(0.1) / nn;

type
    generic TPrioQueue<T> = class
    public
        items: array of T;
        n: int32;
        constructor Create();
        function Compare(l, r: T): int64;
        procedure setItem(v: int32; x: T);
        procedure swim(v: int32; x: T);
        procedure enqueue(x: T);
        function prioChild(u: int32): int32;
        procedure sink(u: int32; x: T);
        procedure dequeue(u: int32);
    end;
    iPrioQueue = specialize TPrioQueue<int64>;

var
    n, i: int32;
    m, l, r, x: int64;
    p, k: array [1 .. nn] of int64;
    pq: iPrioQueue;

constructor TPrioQueue.Create();
begin
    setlength(items, 1);
    n := 0;
end;

function TPrioQueue.Compare(l, r: T): int64;
begin
    if extended(2*k[l]+1) * p[l] < extended(2*k[r]+1) * p[r] then
        result := -1
    else if extended(2*k[l]+1) * p[l] > extended(2*k[r]+1) * p[r] then
        result := 1
    else
        result := 0
end;

procedure TPrioQueue.setItem(v: int32; x: T);
begin
    items[v] := x;
end;

procedure TPrioQueue.swim(v: int32; x: T);
var
    u: int32;
begin
    u := (v-1) div 2;
    while (v > 0) and (Compare(x, items[u]) < 0) do begin
        setItem(v, items[u]);
        v := u;
        u := (v-1) div 2;
    end;
    setItem(v, x);
end;

procedure TPrioQueue.enqueue(x: T);
begin
    inc(n);
    if length(items) <= n then setlength(items, 2*n);
    swim(n-1, x);
end;

function TPrioQueue.prioChild(u: int32): int32;
var
    v: int32;
begin
    v := u * 2 + 1;
    if (v+1 < n) and (Compare(items[v+1], items[v]) < 0) then inc(v);
    result := v;
end;

procedure TPrioQueue.sink(u: int32; x: T);
var
    v: int32;
begin
    v := prioChild(u);
    while (v < n) and (Compare(items[v], x) < 0) do begin
        setItem(u, items[v]);
        u := v;
        v := prioChild(u);
    end;
    setItem(u, x);
end;

procedure TPrioQueue.dequeue(u: int32);
begin
    if length(items) >= 4*n then setlength(items, 2*n);
    dec(n);
    sink(u, items[n]);
end;

function isqrt(a: int64): int64; inline;
var
    x: int64;
begin
    if a < high(int32) then
        x := a
    else
        x := high(int32);
    while x * x > a do
        x := (x + a div x) div 2;
    isqrt := x;
end;

function f(x: int64): boolean;
var
    i: int32;
    s: extended;
begin
    s := 0;
    i := 1;
    result := true;
    while (i <= n) and result do begin
        k[i] := (x div p[i] +1) div 2;
        s := s + extended(1) * k[i] * k[i] * p[i];
        result := result and (s-m <= eps);
        inc(i);
    end;
end;

function g(x: int64): int64; inline;
var
    i: int32;
    s, t: extended;
begin
    s := 0;
    for i := 1 to n do begin
        k[i] := (x div p[i] +1) div 2;
        s := s + extended(1) * k[i] * k[i] * p[i];
        pq.enqueue(i);
    end;

    result := 0;
    while pq.n > 0 do begin

        i := pq.items[0];
        pq.dequeue(0);

        t := s + extended(2*k[i]+1) * p[i];
        if t-m <= eps then begin
            s := t;
            inc(k[i]);
        end;

        inc(result, k[i]);

    end;
end;

begin
    readln(n, m);
    pq := iPrioQueue.Create();

    for i := 1 to n do read(p[i]);
    readln;

    l := 0;
    r := m+1;
    while r-l > 1 do begin
        x := (l+r) div 2;
        if f(x) then
            l := x
        else
            r := x;
    end;

    writeln(g(l));
    pq.Free();
end.
