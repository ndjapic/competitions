# Задатак: E_Insert_or_Erase.pas

```pascal
program E_Insert_or_Erase;
uses
    math;
const
    maxa = 400 * 1001;
var
    n, q, i, k, ran, na, nt, x, y, p: int32;
    tp: int8;
    a: array [1 .. maxa] of record
        x, l, r: int32;
    end;
    t: array [1 .. maxa] of record
        x, p, l, r: int32;
    end;

procedure link(l, r: int32);
begin
    a[l].r := r;
    a[r].l := l;
end;

procedure ains(y, p: int32);
begin
    inc(na);
    link(na, a[p].r);
    link(p, na);
    a[na].x := y;
end;

procedure adel(l, p, r: int32);
begin
    a[l].r := a[p].r;
    a[r].l := a[p].l;
end;

procedure tins(y: int32);
var
    u, v: int32;
begin

    v := 1;
    y := y xor ran;

    while (v > 0) and (y <> t[v].x) do begin
        u := v;
        if y < t[u].x then
            v := t[u].l
        else
            v := t[u].r;
    end;

    if v = 0 then begin
        inc(nt);
        v := nt;
        if y < t[u].x then
            t[u].l := v
        else
            t[u].r := v;
        t[v].x := y;
    end;

    t[v].p := p;
    t[v].l := 0;
    t[v].r := 0;

end;

function find(x: int32): int32;
var
    u, v: int32;
begin
    v := 1;
    x := x xor ran;

    while x <> t[v].x do begin
        u := v;
        if x < t[u].x then
            v := t[u].l
        else
            v := t[u].r;
    end;

    find := t[v].p;
end;

begin
    randomize;
    ran := random(int32(1) shl 30);

    readln(n);
    read(x);

    a[1].r := 2;
    a[2].l := 1;
    na := 2;
    ains(x, 1);

    nt := 1;
    t[1].x := x;
    t[1].l := 0;
    t[1].r := 0;

    writeln('*  Entering for i *');

    for i := 2 to n do begin
        read(x);
        ains(x, na);
        tins(x);
    end;
    readln;

    writeln('*  Entering for k *');

    readln(q);
    for k := 1 to q do begin
        writeln('k=', k);
        read(tp);
        case tp of

            1: begin
                readln(x, y);
                p := find(x);
                ains(y, p);
            end;

            2: begin
                readln(x);
                p := find(x);
                adel(a[p].l, p, a[p].r);
            end;
        
        end;
    end;

    writeln('*  Entering while p *');

    p := a[1].r;
    while p <> 2 do begin
        write(a[p].x);
        p := a[p].r;
        if p <> 2 then write(' ');
    end;
    writeln;
end.

```
