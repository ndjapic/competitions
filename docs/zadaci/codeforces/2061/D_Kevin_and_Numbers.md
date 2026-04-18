# Задатак: D_Kevin_and_Numbers.pas

```pascal
program D_Kevin_and_Numbers;
{$mode objfpc}{$h+}{$inline on}
uses
    math;
const
    nn = 200 * 1000;

type
    generic TPrioQueue<T> = class
    public
        items: array of T;
        n: int32;
        constructor Create();
        function Compare(l, r: T): int32; inline;
        procedure setItem(v: int32; x: T); inline;
        procedure swim(v: int32; x: T);
        procedure enqueue(x: T); inline;
        function prioChild(u: int32): int32;
        procedure sink(u: int32; x: T);
        procedure dequeue(u: int32); inline;
    end;
    iPrioQueue = specialize TPrioQueue<int64>;

var
    ntc, tci: int16;
    n, m, i, j, x: int32;
    a, b: array [1 .. nn] of int32;
    pqa, pqb: iPrioQueue;

constructor TPrioQueue.Create();
begin
    setlength(items, 1);
    n := 0;
end;

function TPrioQueue.Compare(l, r: T): int32;
begin
    result := - l + r;
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
    {if length(items) <= n then setlength(items, 2*n);}
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
    {if length(items) >= 4*n then setlength(items, 2*n);}
    dec(n);
    sink(u, items[n]);
end;

begin
    pqa := iPrioQueue.Create();
    pqb := iPrioQueue.Create();
    setlength(pqa.items, nn+1);
    setlength(pqb.items, nn+1);

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        for i := 1 to n do begin
            read(a[i]);
            pqa.enqueue(a[i]);
        end;
        readln;

        for j := 1 to m do begin
            read(b[j]);
            pqb.enqueue(b[j]);
        end;
        readln;

        while (0 < pqb.n) and (pqb.n <= pqa.n) and (pqa.items[0] <= pqb.items[0]) do
            if pqa.items[0] = pqb.items[0] then begin
                pqa.dequeue(0);
                pqb.dequeue(0);
            end else begin
                x := pqb.items[0];
                pqb.dequeue(0);
                pqb.enqueue(x div 2);
                pqb.enqueue((x+1) div 2);
            end;

        if (pqa.n = 0) and (pqb.n = 0) then
            writeln('Yes')
        else
            writeln('No');

        while pqa.n > 0 do pqa.dequeue(0);
        while pqb.n > 0 do pqb.dequeue(0);

    end;
end.

```
