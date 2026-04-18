# Задатак: E_Reachable_Set.pas

```pascal
program E_Reachable_Set;
{$mode objfpc}{$h+}
const
    nn = 300 * 1000;

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
    n, m, i, u, v: int32;
    adj: array [1 .. nn] of int32;
    seen: array [1 .. nn] of boolean;
    sib, tar: array [-nn .. nn] of int32;
    dsu, size: array [1 .. nn] of int32;
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
    if not seen[x] then begin
        seen[x] := true;
        inc(n);
        if length(items) <= n then setlength(items, 2*n);
        swim(n-1, x);
    end;
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

function find(v: int32): int32;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
    find := dsu[v];
end;

procedure union2(u, v: int32);
begin
    dsu[v] := u;
    inc(size[u], size[v]);
end;

procedure union1(u, v: int32);
begin
    u := find(u);
    v := find(v);
    if u = v then
    else if size[u] > size[v] then
        union2(u, v)
    else
        union2(v, u);
end;

begin
    readln(n, m);
    pq := iPrioQueue.Create();

    for v := 1 to n do begin
        adj[v] := 0;
        seen[v] := false;
        dsu[v] := v;
        size[v] := 1;
    end;

    for i := 1 to m do begin
        readln(u, v);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;

    for u := 1 to n do begin

        while (pq.n > 0) and (pq.items[0] <= u) do
            pq.dequeue(0);

        i := adj[u];
        while i <> 0 do begin
            v := tar[i];
            if v < u then
                union1(u, v)
            else
                pq.enqueue(v);
            i := sib[i];
        end;

        if find(u) = find(1) then
            writeln(pq.n)
        else
            writeln(-1);

    end;
end.

```
