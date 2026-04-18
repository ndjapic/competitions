# Задатак: B_Kevin_and_Geometry.pas

```pascal
program B_Kevin_and_Geometry;
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
        function Compare(l, r: T): int32;
        procedure setItem(v: int32; x: T);
        procedure swim(v: int32; x: T);
        procedure enqueue(x: T);
        function prioChild(u: int32): int32;
        procedure sink(u: int32; x: T);
        procedure dequeue(u: int32);
    end;
    iPrioQueue = specialize TPrioQueue<int32>;

var
    ntc, tci: int16;
    n, i, j, k: int32;
    found: boolean;
    a, d, pre, suf: array [1 .. nn] of int32;
    pq: iPrioQueue;

constructor TPrioQueue.Create();
begin
    setlength(items, 1);
    n := 0;
end;

function TPrioQueue.Compare(l, r: T): int32;
begin
    result := l - r;
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
    pq := iPrioQueue.Create();
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do begin
            read(a[i]);
            pq.enqueue(a[i]);
        end;
        readln;

        for i := 1 to n do begin
            a[i] := pq.items[0];
            pq.dequeue(0);
        end;

        for i := 1 to n-1 do
            d[i] := a[i+1] - a[i];

        pre[1] := 1;
        for i := 2 to n-1 do
            if d[i] < d[pre[i-1]] then
                pre[i] := i
            else
                pre[i] := pre[i-1];

        suf[n-1] := n-1;
        for i := n-2 downto 1 do
            if d[i] < d[suf[i+1]] then
                suf[i] := i
            else
                suf[i] := suf[i+1];

        i := 1;
        found := false;
        while (i < n) and not found do begin

            if d[i] = 0 then begin

                if (i > 2) and not found then begin
                    j := pre[i-2];
                    k := j+1;
                    found := d[j] < 2*a[i];
                end;

                if (i < n-2) and not found then begin
                    j := suf[i+2];
                    k := j+1;
                    found := d[j] < 2*a[i];
                end;

                if (1 < i) and (i < n-1) and not found then begin
                    j := i-1;
                    k := i+2;
                    found := a[k] - a[j] < 2*a[i];
                end;

            end;

            if not found then inc(i);
        end;

        if found then
            writeln(a[i], ' ', a[i+1], ' ', a[j], ' ', a[k])
        else
            writeln(-1);

    end;
end.

```
