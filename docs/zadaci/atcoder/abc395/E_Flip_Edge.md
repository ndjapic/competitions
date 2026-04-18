# Задатак: E_Flip_Edge.pas

```pascal
program E_Flip_Edge;
{$mode objfpc}{$h+}
uses
    Generics.Defaults, math;
const
    nn = 200 * 1000;
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;

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
    n, m, x, i, u, v: int32;
    adj: array [-nn .. nn] of int32;
    dist: array [-nn .. nn] of int64;
    sib, tar: array [-nn .. nn] of int32;
    pq: iPrioQueue;

constructor TPrioQueue.Create();
begin
    setlength(items, 1);
    n := 0;
end;

function TPrioQueue.Compare(l, r: T): int32;
begin
    if dist[l] < dist[r] then
        result := -1
    else if dist[l] > dist[r] then
        result := 1
    else
        result := 0;
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

procedure addarrow(u, v, i: int32);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

procedure readedges(n, m: int32);
var
    u, v, i: int32;
begin
    for v := -n to n do begin
        adj[v] := 0;
        dist[v] := inf;
    end;

    for i := 1 to m do begin
        readln(u, v);
        addarrow(u, v, i);
        addarrow(-v, -u, -i);
    end;
end;

begin
    readln(n, m, x);
    readedges(n, m);
    pq := iPrioQueue.Create();

    dist[1] := 0;
    pq.enqueue(1);

    while pq.n > 0 do begin
        u := pq.items[0];
        pq.dequeue(0);

        i := adj[u];
        while i <> 0 do begin
            v := tar[i];
            if dist[v] > dist[u] + 1 then begin
                dist[v] := dist[u] + 1;
                pq.enqueue(v);
            end;

            i := sib[i];
        end;

        v := -u;
        if dist[v] > dist[u] + x then begin
            dist[v] := dist[u] + x;
            pq.enqueue(v);
        end;

    end;

    writeln(min(dist[n], dist[-n]));
end.

```
