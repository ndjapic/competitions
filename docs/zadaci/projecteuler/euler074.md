# Задатак: euler074.pas

```pascal
program Digit_factorial_chains;
const
    maxn = 1000 * 1000;
    maxx = 6*9*8*7*6*5*4*3*2*1 + 2;
var
    ntc, tci, l: int8;
    n, x, v, i, c: int32;
    loop: boolean;
    f: array [0 .. maxx] of int32;
    len: array [0 .. maxn] of int32;
    ans: array [1 .. maxn] of int32;
    t: array [1 .. 60] of record
        x, l, r: int32;
    end;

begin
    f[0] := 1;
    for n := 1 to 9 do f[n] := n * f[n-1];
    for n := 10 to maxx do f[n] := f[n mod 10] + f[n div 10];

    for n := 0 to maxn do begin

        t[1].x := n;
        t[1].l := 0;
        t[1].r := 0;

        len[n] := 1;
        x := n;
        loop := true;

        while loop do begin

            v := 1;
            x := f[x];
            while (x < t[v].x) and (t[v].l > 0) or (x > t[v].x) and (t[v].r > 0) do
                if (x < t[v].x) and (t[v].l > 0) then
                    v := t[v].l
                else if (x > t[v].x) and (t[v].r > 0) then
                    v := t[v].r;

            loop := x <> t[v].x;
            if loop then begin

                inc(len[n]);
                t[len[n]].x := x;
                t[len[n]].l := 0;
                t[len[n]].r := 0;

                if x < t[v].x then
                    t[v].l := len[n]
                else
                    t[v].r := len[n];

            end;

        end;

    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, l);

        c := 0;
        for x := 0 to n do
            if len[x] = l then begin;
                inc(c);
                ans[c] := x;
            end;

        if c = 0 then
            writeln(-1)
        else begin
            for i := 1 to c-1 do write(ans[i], ' ');
            writeln(ans[c]);
        end;

    end;
end.

```
