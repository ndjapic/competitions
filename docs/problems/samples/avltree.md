# Problem: avltree.pas

```pascal
program avltree;
{$MODE DELPHI}
uses
    math;

type
    tavltree<_T> = class
        x: _T;
        n: int32;
        h, b: int8;
        l, r: tavltree<_T>;
        class function count(t: tavltree<_T>): int32;
        class function height(t: tavltree<_T>): int8;
        class function balance(t: tavltree<_T>): int8;
        class procedure update(t: tavltree<_T>);
    end;

class function tavltree<_T>.count(t: tavltree<_T>): int32; inline;
begin
    if t = nil then
        result := 0
    else
        result := t.n;
end;

class function tavltree<_T>.height(t: tavltree<_T>): int8; inline;
begin
    if t = nil then
        result := 0
    else
        result := t.h;
end;

class function tavltree<_T>.balance(t: tavltree<_T>): int8; inline;
begin
    if t = nil then
        result := 0
    else
        result := t.b;
end;

class procedure tavltree<_T>.update(t: tavltree<_T>); inline;
begin
    if t <> nil then begin
        t.n := count(t.l) + count(t.r);
        t.b := height(t.r) - height(t.l);
        t.h := max(height(t.l), height(t.r)) + 1;
    end;
end;

var
    t: tavltree<int32>;

begin
    t := tavltree<int32>.create;
    t.free;
end.

```
