# Problem: C_Asuna_and_the_Mosquitoes.pas

```pascal
program C_Asuna_and_the_Mosquitoes;
{$mode objfpc}{$h+}
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
    ntc, tci: int16;
    n, i: int32;
    x, ans0, ans1: int64;
    a: array [1 .. nn] of int64;
    pq0, pq1: iPrioQueue;

constructor TPrioQueue.Create();
begin
    setlength(items, 1);
    n := 0;
end;

function TPrioQueue.Compare(l, r: T): int64;
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

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        pq0 := iPrioQueue.Create();
        pq1 := iPrioQueue.Create();

        for i := 1 to n do
            if odd(a[i]) then
                pq1.enqueue(a[i])
            else
                pq0.enqueue(a[i]);

        ans0 := 0;
        if pq0.n > 0 then begin

            ans0 := pq0.items[0];
            pq0.dequeue(0);

            if pq1.n > 0 then begin
                x := pq1.items[0];
                pq1.dequeue(0);
                while x > 1 do begin
                    inc(ans0, x-1);
                    pq1.enqueue(1);
                    x := pq1.items[0];
                    pq1.dequeue(0);
                end;
                inc(ans0);

                while pq0.n > 0 do begin
                    x := pq0.items[0];
                    pq0.dequeue(0);
                    inc(ans0, x);
                end;

            end;

        end;

        while pq0.n > 0 do pq0.dequeue(0);
        while pq1.n > 0 do pq1.dequeue(0);

        pq0 := iPrioQueue.Create();
        pq1 := iPrioQueue.Create();

        for i := 1 to n do
            if odd(a[i]) then
                pq1.enqueue(a[i])
            else
                pq0.enqueue(a[i]);

        ans1 := 0;
        if pq1.n > 0 then begin

            ans1 := pq1.items[0];
            pq1.dequeue(0);

            if pq0.n > 0 then begin
                x := pq0.items[0];
                pq0.dequeue(0);
                inc(ans1, x-1);
                pq1.enqueue(1);

                x := pq1.items[0];
                pq1.dequeue(0);
                while x > 1 do begin
                    inc(ans1, x-1);
                    pq1.enqueue(1);
                    x := pq1.items[0];
                    pq1.dequeue(0);
                end;
                inc(ans1);
            end;

            while pq0.n > 0 do begin
                x := pq0.items[0];
                pq0.dequeue(0);
                inc(ans1, x);
            end;

        end;

        while pq0.n > 0 do pq0.dequeue(0);
        while pq1.n > 0 do pq1.dequeue(0);

        writeln(max(ans0, ans1));

    end;
end.

```
